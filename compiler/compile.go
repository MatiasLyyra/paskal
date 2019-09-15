package compiler

import (
	"fmt"

	"github.com/MatiasLyyra/paskal/ast"
	"github.com/MatiasLyyra/paskal/types"
	"llvm.org/llvm/bindings/go/llvm"
)

type paskalValue struct {
	TypeValue types.Value
	Value     llvm.Value
}

type paskalFunction struct {
	Function      *ast.Function
	IsVarArg      bool
	FunctionValue llvm.Value
}

func (f paskalFunction) Argc() int {
	return len(f.Function.Params)
}

func (f paskalFunction) FunctionType() llvm.Type {
	var paramTypes []llvm.Type
	for _, param := range f.Function.Params {
		paramTypes = append(paramTypes, param.Type().LLVMType())
	}
	return llvm.FunctionType(f.Function.Return.LLVMType(), paramTypes, f.IsVarArg)
}

// Compiler for a module / translation unit. Should be created with NewCompiler function.
type Compiler struct {
	builder llvm.Builder
	Module  llvm.Module
	context llvm.Context
	// Function pass manager
	fPM llvm.PassManager
	// Moodule pass manager
	mPM          llvm.PassManager
	paskalModule *ast.Module
	tm           llvm.TargetMachine
	funcs        map[string]paskalFunction
	vars         map[string]paskalValue
	globalVars   map[string]paskalValue
}

// NewCompiler returns compiler for the given module.
// Optimization can be toggled with optLevel and sizeLevel which accept values between 0 and 3.
func NewCompiler(module *ast.Module, optLevel, sizeLevel int) *Compiler {
	c := &Compiler{
		Module:       llvm.NewModule(module.Name),
		builder:      llvm.NewBuilder(),
		funcs:        make(map[string]paskalFunction),
		vars:         make(map[string]paskalValue),
		globalVars:   make(map[string]paskalValue),
		paskalModule: module,
	}
	target, err := llvm.GetTargetFromTriple(llvm.DefaultTargetTriple())
	if err != nil {
		panic(err)
	}
	targetMachine := target.CreateTargetMachine(llvm.DefaultTargetTriple(), "x86-64", "", llvm.CodeGenLevelDefault, llvm.RelocDefault, llvm.CodeModelDefault)
	c.tm = targetMachine
	targetData := targetMachine.CreateTargetData()
	c.Module.SetDataLayout(targetData.String())
	targetData.Dispose()

	c.fPM = llvm.NewFunctionPassManagerForModule(c.Module)
	c.mPM = llvm.NewPassManager()
	c.context = c.Module.Context()
	pmb := llvm.NewPassManagerBuilder()
	pmb.SetOptLevel(optLevel)
	pmb.SetSizeLevel(sizeLevel)
	if optLevel > 0 {
		pmb.UseInlinerWithThreshold(1)
	}
	pmb.PopulateFunc(c.fPM)
	pmb.Populate(c.mPM)
	pmb.Dispose()
	return c
}

// Compile the module. After compilation the result can be fetched with Emit.
func (c *Compiler) Compile() error {
	for _, globalVar := range c.paskalModule.Vars {
		val := llvm.AddGlobal(c.Module, globalVar.Type().LLVMType(), globalVar.Name())
		val.SetInitializer(llvm.ConstNull(globalVar.Type().LLVMType()))
		c.globalVars[globalVar.Name()] = paskalValue{
			TypeValue: globalVar.Ref(),
			Value:     val,
		}
	}
	printfFunc := ast.NewFunction("printf", true)
	printfFunc.SetReturn(types.IntegerType)
	printfFunc.AddParam("buf", types.StringType)
	pPrintFunc := paskalFunction{
		IsVarArg: true,
		Function: printfFunc,
	}
	printf := llvm.AddFunction(c.Module, "printf", pPrintFunc.FunctionType())
	pPrintFunc.FunctionValue = printf
	c.funcs["printf"] = pPrintFunc

	err := c.compileFuncs()
	if err != nil {
		return err
	}
	err = llvm.VerifyModule(c.Module, llvm.AbortProcessAction)
	if err != nil {
		panic(err)
	}
	c.mPM.Run(c.Module)
	return nil
}

// Run function by name inside an interpreter.
func (c *Compiler) Run(funcName string, args []llvm.GenericValue) (llvm.GenericValue, error) {
	ee, err := llvm.NewInterpreter(c.Module)
	if err != nil {
		return llvm.GenericValue{}, err
	}
	functionVal := c.Module.NamedFunction(funcName)
	if functionVal.IsNil() {
		return llvm.GenericValue{}, fmt.Errorf("function %s does not exist", funcName)
	}
	return ee.RunFunction(functionVal, args), nil
}

// Emit compiled translation unit into byte slice.
func (c *Compiler) Emit(fileType llvm.CodeGenFileType) ([]byte, error) {
	mem, err := c.tm.EmitToMemoryBuffer(c.Module, fileType)
	if err != nil {
		return []byte{}, err
	}
	defer mem.Dispose()
	return mem.Bytes(), err
}

// Dispose releases LLVM resources held by the compiler. Compiler should not be used after this.
func (c *Compiler) Dispose() {
	c.tm.Dispose()
	c.builder.Dispose()
	// TODO: Check why disposing context causes program to crash in the tests
	// c.context.Dispose()
	c.Module.Dispose()
	c.fPM.Dispose()
	c.mPM.Dispose()
}

func (c *Compiler) compileFuncs() error {
	for _, function := range c.paskalModule.Funcs {
		pFunction := paskalFunction{
			Function: function,
		}
		val := llvm.AddFunction(c.Module, function.Name, pFunction.FunctionType())
		pFunction.FunctionValue = val
		c.funcs[function.Name] = pFunction
		err := c.compileFunctionBody(pFunction)
		if err != nil {
			return err
		}
		err = llvm.VerifyFunction(val, llvm.AbortProcessAction)
		if err != nil {
			panic(err)
		}
		c.fPM.RunFunc(val)
	}
	if c.paskalModule.Main != nil {
		pFunction := paskalFunction{
			Function: c.paskalModule.Main,
		}
		mainFn := llvm.AddFunction(c.Module, pFunction.Function.Name, pFunction.FunctionType())
		pFunction.FunctionValue = mainFn
		c.funcs[pFunction.Function.Name] = pFunction
		err := c.compileFunctionBody(pFunction)
		if err != nil {
			return err
		}
		err = llvm.VerifyFunction(mainFn, llvm.AbortProcessAction)
		if err != nil {
			panic(err)
		}
		c.fPM.RunFunc(mainFn)
	}
	return nil
}

func (c *Compiler) compileFunctionBody(f paskalFunction) error {
	entryBB := c.context.AddBasicBlock(f.FunctionValue, "entry")
	c.builder.SetInsertPoint(entryBB, entryBB.FirstInstruction())
	defer func() {
		c.vars = make(map[string]paskalValue)
	}()
	for i, paramValue := range f.FunctionValue.Params() {
		paramVar := f.Function.Params[i]
		paramValue.SetName(paramVar.Name() + "_arg")
		paramValueLocal := c.builder.CreateAlloca(paramVar.Type().LLVMType(), paramVar.Name())
		c.builder.CreateStore(paramValue, paramValueLocal)
		c.vars[paramVar.Name()] = paskalValue{
			TypeValue: paramVar.Ref(),
			Value:     paramValueLocal,
		}
	}
	for _, localVar := range f.Function.Vars {
		value := c.builder.CreateAlloca(localVar.Type().LLVMType(), localVar.Name())
		c.vars[localVar.Name()] = paskalValue{
			TypeValue: localVar.Ref(),
			Value:     value,
		}
	}
	retType := f.Function.Return.LLVMType()
	isVoid := retType == llvm.VoidType()
	funcName := f.Function.Name
	var retVal llvm.Value
	if !isVoid {
		retVal = c.builder.CreateAlloca(f.Function.Return.LLVMType(), f.Function.Name)
		c.vars[f.Function.Name] = paskalValue{
			TypeValue: types.NewVariable(funcName, f.Function.Return).Ref(),
			Value:     retVal,
		}
	}
	err := c.compileBlock(f.Function.Body)
	if err != nil {
		return err
	}
	if isVoid {
		c.builder.CreateRetVoid()
	} else {
		retVal = c.builder.CreateLoad(retVal, "retload")
		c.builder.CreateRet(retVal)
	}
	return nil
}

func (c *Compiler) compileBlock(block ast.Block) error {
	for _, expression := range block.Statements {
		err := c.compileNode(expression)
		if err != nil {
			return err
		}
	}
	return nil
}

func (c *Compiler) compileNode(node ast.Node) (err error) {
	switch stmt := node.(type) {
	case *ast.IfStatement:
		err = c.compileIfStmt(stmt)
	default:
		_, err = c.compileExpr(stmt, false)
	}
	return
}

func (c *Compiler) compileIfStmt(ifStmt *ast.IfStatement) error {
	condVal, err := c.compileExpr(ifStmt.Condition, true)
	parent := c.builder.GetInsertBlock().Parent()
	if err != nil {
		return err
	}
	condValType := condVal.TypeValue.Type()
	if !condValType.IsA(types.BooleanType) {
		return fmt.Errorf("cannot evaluate if statement on non boolean value")
	}
	trueBB := c.context.AddBasicBlock(parent, "trueblock")
	falseBB := c.context.AddBasicBlock(parent, "falseblock")
	mergeBB := c.context.AddBasicBlock(parent, "mergeblock")
	c.builder.CreateCondBr(condVal.Value, trueBB, falseBB)
	c.builder.SetInsertPoint(trueBB, trueBB.FirstInstruction())
	err = c.compileBlock(ifStmt.TrueBlock)
	if err != nil {
		return err
	}
	c.builder.CreateBr(mergeBB)
	c.builder.SetInsertPoint(falseBB, falseBB.FirstInstruction())
	if ifStmt.FalseBlock != nil {
		switch block := ifStmt.FalseBlock.(type) {
		case *ast.IfStatement:
			err = c.compileIfStmt(block)
		case ast.Block:
			err = c.compileBlock(block)
		default:
			panic(fmt.Sprintf("invalid value in else branch block %s", block))
		}
	}
	c.builder.CreateBr(mergeBB)
	c.builder.SetInsertPoint(mergeBB, mergeBB.FirstInstruction())
	return nil
}

func (c *Compiler) deref(val paskalValue) paskalValue {
	isPtr := val.TypeValue.Type().DerefType() != nil

	if isPtr {
		arr, isArr := val.TypeValue.Deref().Type().(*types.Array)
		if isArr && arr.Base.IsA(types.CharacterType) {
			return paskalValue{
				TypeValue: types.NewVariable("", arr.Base.RefType()),
				Value:     c.builder.CreateGEP(val.Value, []llvm.Value{llvm.ConstInt(llvm.Int32Type(), 0, true), llvm.ConstInt(llvm.Int32Type(), 0, true)}, ""),
			}
		}
		derefVal := c.builder.CreateLoad(val.Value, "deref")
		derefPaskalVal := paskalValue{
			TypeValue: val.TypeValue.Deref(),
			Value:     derefVal,
		}
		if _, ok := val.TypeValue.Type().DerefType().(*types.Array); ok {
			return c.deref(derefPaskalVal)
		}
		return derefPaskalVal
	}
	return val
}
