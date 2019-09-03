package compiler

import (
	"fmt"
	"reflect"

	"github.com/MatiasLyyra/paskal/ast"
	"github.com/MatiasLyyra/paskal/lexer"
	"github.com/MatiasLyyra/paskal/types"
	"llvm.org/llvm/bindings/go/llvm"
)

type PaskalValue struct {
	TypeValue types.Value
	Value     llvm.Value
}

type PaskalFunction struct {
	Function      *ast.Function
	IsVarArg      bool
	FunctionValue llvm.Value
}

func (f PaskalFunction) Argc() int {
	return len(f.Function.Params)
}

func (f PaskalFunction) FunctionType() llvm.Type {
	var paramTypes []llvm.Type
	for _, param := range f.Function.Params {
		paramTypes = append(paramTypes, param.Type().LLVMType())
	}
	return llvm.FunctionType(f.Function.Return.LLVMType(), paramTypes, f.IsVarArg)
}

type Compiler struct {
	builder      llvm.Builder
	Module       llvm.Module
	context      llvm.Context
	fPM          llvm.PassManager
	mPM          llvm.PassManager
	paskalModule *ast.Module
	tm           llvm.TargetMachine
	Funcs        map[string]PaskalFunction
	vars         map[string]PaskalValue
	globalVars   map[string]PaskalValue
}

func NewCompiler(module *ast.Module, optLevel, sizeLevel int) *Compiler {
	c := &Compiler{
		Module:       llvm.NewModule(module.Name),
		builder:      llvm.NewBuilder(),
		Funcs:        make(map[string]PaskalFunction),
		vars:         make(map[string]PaskalValue),
		globalVars:   make(map[string]PaskalValue),
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

func (c *Compiler) Compile() error {
	for _, globalVar := range c.paskalModule.Vars {
		val := llvm.AddGlobal(c.Module, globalVar.Type().LLVMType(), globalVar.Name())
		val.SetInitializer(llvm.ConstNull(globalVar.Type().LLVMType()))
		c.globalVars[globalVar.Name()] = PaskalValue{
			TypeValue: globalVar.Ref(),
			Value:     val,
		}
	}
	printfFunc := ast.NewFunction("printf", true)
	printfFunc.SetReturn(types.IntegerType)
	printfFunc.AddParam("buf", types.StringType)
	pPrintFunc := PaskalFunction{
		IsVarArg: true,
		Function: printfFunc,
	}
	printf := llvm.AddFunction(c.Module, "printf", pPrintFunc.FunctionType())
	pPrintFunc.FunctionValue = printf
	c.Funcs["printf"] = pPrintFunc

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

func (c *Compiler) Emit(fileType llvm.CodeGenFileType) ([]byte, error) {
	mem, err := c.tm.EmitToMemoryBuffer(c.Module, fileType)
	if err != nil {
		return []byte{}, err
	}
	defer mem.Dispose()
	return mem.Bytes(), err
}

func (c *Compiler) Dispose() {
	c.tm.Dispose()
	c.builder.Dispose()
	c.Module.Dispose()
	c.context.Dispose()
	c.fPM.Dispose()
	c.mPM.Dispose()
}

func (c *Compiler) compileFuncs() error {
	for _, function := range c.paskalModule.Funcs {
		pFunction := PaskalFunction{
			Function: function,
		}
		val := llvm.AddFunction(c.Module, function.Name, pFunction.FunctionType())
		pFunction.FunctionValue = val
		c.Funcs[function.Name] = pFunction
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
		pFunction := PaskalFunction{
			Function: c.paskalModule.Main,
		}
		mainFn := llvm.AddFunction(c.Module, pFunction.Function.Name, pFunction.FunctionType())
		pFunction.FunctionValue = mainFn
		c.Funcs[pFunction.Function.Name] = pFunction
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

func (c *Compiler) compileFunctionBody(f PaskalFunction) error {
	entryBB := c.context.AddBasicBlock(f.FunctionValue, "entry")
	c.builder.SetInsertPoint(entryBB, entryBB.FirstInstruction())
	defer func() {
		c.vars = make(map[string]PaskalValue)
	}()
	for i, paramValue := range f.FunctionValue.Params() {
		paramVar := f.Function.Params[i]
		paramValue.SetName(paramVar.Name() + "_arg")
		paramValueLocal := c.builder.CreateAlloca(paramVar.Type().LLVMType(), paramVar.Name())
		c.builder.CreateStore(paramValue, paramValueLocal)
		c.vars[paramVar.Name()] = PaskalValue{
			TypeValue: paramVar.Ref(),
			Value:     paramValueLocal,
		}
	}
	for _, localVar := range f.Function.Vars {
		value := c.builder.CreateAlloca(localVar.Type().LLVMType(), localVar.Name())
		c.vars[localVar.Name()] = PaskalValue{
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
		c.vars[f.Function.Name] = PaskalValue{
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
	condVal, err := c.compileExpr(ifStmt.Condition, false)
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

func (c *Compiler) compileExpr(node ast.Node, deref bool) (val PaskalValue, err error) {
	derefOverride := false
	switch expr := node.(type) {
	case *ast.BinaryExpression:
		val, err = c.compileBinaryExpr(expr.LHS, expr.RHS, expr.Op)
	case *ast.UnaryExpression:
		val, err = c.compileUnaryExpr(expr.Value, expr.Op)
		if expr.Op == lexer.AddressOf {
			derefOverride = true
		}
	case *ast.FunctionCall:
		val, err = c.compileFunctionCall(expr)
	case *ast.Subscript:
		val, err = c.compileSubscriptExpr(expr)
	case ast.IdentifierExpression:
		val, err = c.compileIdentifierExpr(expr)
	case ast.IntegerExpression:
		val, err = c.compileIntegerExpr(expr)
	case ast.BooleanExpression:
		val, err = c.compileBooleanExpr(expr)
	case ast.CharacterExpression:
		val, err = c.compileCharacterExpr(expr)
	case ast.RealExpression:
		val, err = c.compileRealExpr(expr)
	case ast.StringExpression:
		val, err = c.compileStringExpr(expr)
	default:
		panic(fmt.Sprintf("invalid expression %s", reflect.ValueOf(expr)))
	}
	if val.TypeValue != nil && deref && !derefOverride {
		val = c.deref(val)
	}
	return
}
func (c *Compiler) compileFunctionCall(fnCall *ast.FunctionCall) (PaskalValue, error) {
	var args []llvm.Value
	fnVal, ok := c.Funcs[fnCall.Name]
	if !ok {
		return PaskalValue{}, fmt.Errorf("function with name %s does not exist", fnCall.Name)
	}
	argcOk := fnVal.Argc() == len(fnCall.Args)
	varArgCallOk := fnVal.IsVarArg && fnVal.Argc() <= len(fnCall.Args)
	if !argcOk && !varArgCallOk {
		return PaskalValue{}, fmt.Errorf("incorrect amount of arguments to function %s, expected: %d got: %d", fnCall.Name, fnVal.Argc(), len(fnCall.Args))
	}
	for i, arg := range fnCall.Args {
		argVal, err := c.compileExpr(arg, true)
		if err != nil {
			return PaskalValue{}, err
		}
		argValType := argVal.TypeValue.Type()
		if i < len(fnVal.Function.Params) {
			paramValType := fnVal.Function.Params[i].Type()
			if !paramValType.IsA(argValType) {
				return PaskalValue{}, fmt.Errorf("incorrect argument type to function %s, expected: %s got %s", fnCall.Name, paramValType, argValType)
			}
		}
		args = append(args, argVal.Value)
	}
	var val llvm.Value
	ret := PaskalValue{
		TypeValue: types.NewVariable("", fnVal.Function.Return),
	}
	if fnVal.Function.Return.IsA(types.VoidType) {
		c.builder.CreateCall(fnVal.FunctionValue, args, "")
	} else {
		val = c.builder.CreateCall(fnVal.FunctionValue, args, "fncallret")
	}
	ret.Value = val
	return ret, nil
}
func (c *Compiler) compileSubscriptExpr(expr *ast.Subscript) (PaskalValue, error) {
	exprVal, err := c.compileExpr(expr.Value, false)
	if err != nil {
		return PaskalValue{}, err
	}
	exprValDeref := exprVal.TypeValue.Type().DerefType()
	if exprValDeref == nil {
		panic("non pointer indexing")
	}
	origArray, isArr := exprValDeref.(*types.Array)
	arr := origArray
	if !isArr {
		return PaskalValue{}, fmt.Errorf("cannot index non-array value of type: %s", exprVal.TypeValue.Type())
	}
	var gepIndexes = []llvm.Value{llvm.ConstInt(llvm.Int32Type(), 0, true)}
	for _, indexing := range expr.Indexing {
		indexValue, err := c.compileExpr(indexing, true)
		if err != nil {
			return PaskalValue{}, err
		}
		if !indexValue.TypeValue.Type().IsA(types.IntegerType) {
			return PaskalValue{}, fmt.Errorf("cannot index with non-integer value")
		}
		gepIndexes = append(gepIndexes, indexValue.Value)
	}
	elementPtr := c.builder.CreateGEP(exprVal.Value, gepIndexes, "gep")
	return PaskalValue{
		Value:     elementPtr,
		TypeValue: types.NewVariable("", arr.Base.RefType()),
	}, nil
}
func (c *Compiler) compileBinaryExpr(lhs, rhs ast.Node, op lexer.Kind) (PaskalValue, error) {
	var (
		lhsVal PaskalValue
		rhsVal PaskalValue
	)

	if op == lexer.Assignment {
		lhsVal, err := c.compileExpr(lhs, false)
		if err != nil {
			return PaskalValue{}, err
		}
		rhsVal, err = c.compileExpr(rhs, true)
		if err != nil {
			return PaskalValue{}, err
		}
		lhsType := lhsVal.TypeValue.Type()
		rhsType := rhsVal.TypeValue.Type()
		if lhsType.DerefType() == nil {
			return PaskalValue{}, fmt.Errorf("cannot assign to rvalue")
		}
		if !lhsType.DerefType().IsA(rhsType) {
			return PaskalValue{}, fmt.Errorf("cannot assign %s to %s", rhsType, lhsType.DerefType())
		}
		c.builder.CreateStore(rhsVal.Value, lhsVal.Value)
		return rhsVal, nil
	}
	lhsVal, err := c.compileExpr(lhs, true)
	if err != nil {
		return PaskalValue{}, err
	}
	rhsVal, err = c.compileExpr(rhs, true)
	if err != nil {
		return PaskalValue{}, err
	}
	lhsType := lhsVal.TypeValue.Type()
	rhsType := rhsVal.TypeValue.Type()
	if !lhsType.IsA(rhsType) {
		return PaskalValue{}, fmt.Errorf("invalid op: %s %s %s", lhsType, op, rhsType)
	}
	var opVal llvm.Value
	var opType types.Type

	switch op {
	case lexer.Mul:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.builder.CreateMul(lhsVal.Value, rhsVal.Value, "mul")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFMul(lhsVal.Value, rhsVal.Value, "fmul")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Div:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.builder.CreateSDiv(lhsVal.Value, rhsVal.Value, "sdiv")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFDiv(lhsVal.Value, rhsVal.Value, "fdiv")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Mod:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.builder.CreateSRem(lhsVal.Value, rhsVal.Value, "srem")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFRem(lhsVal.Value, rhsVal.Value, "frem")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Add:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.builder.CreateAdd(lhsVal.Value, rhsVal.Value, "add")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFAdd(lhsVal.Value, rhsVal.Value, "fadd")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Sub:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.builder.CreateSub(lhsVal.Value, rhsVal.Value, "sub")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFSub(lhsVal.Value, rhsVal.Value, "fsub")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Eq:
		opType = types.BooleanType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.builder.CreateICmp(llvm.IntEQ, lhsVal.Value, rhsVal.Value, "ieq")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFCmp(llvm.FloatOEQ, lhsVal.Value, rhsVal.Value, "feq")
		case lhsType.IsA(types.BooleanType):
			opVal = c.builder.CreateICmp(llvm.IntEQ, lhsVal.Value, rhsVal.Value, "booleq")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Ne:
		opType = types.BooleanType
		switch {
		case lhsType.IsA(types.IntegerType), lhsType.IsA(types.BooleanType):
			opVal = c.builder.CreateICmp(llvm.IntNE, lhsVal.Value, rhsVal.Value, "ine")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFCmp(llvm.FloatONE, lhsVal.Value, rhsVal.Value, "fne")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Lt:
		opType = types.BooleanType
		switch {
		case lhsType.IsA(types.IntegerType), lhsType.IsA(types.BooleanType):
			opVal = c.builder.CreateICmp(llvm.IntSLT, lhsVal.Value, rhsVal.Value, "ilt")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFCmp(llvm.FloatOLT, lhsVal.Value, rhsVal.Value, "flt")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Lte:
		opType = types.BooleanType
		switch {
		case lhsType.IsA(types.IntegerType), lhsType.IsA(types.BooleanType):
			opVal = c.builder.CreateICmp(llvm.IntSLE, lhsVal.Value, rhsVal.Value, "ilte")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFCmp(llvm.FloatOLT, lhsVal.Value, rhsVal.Value, "flte")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Gt:
		opType = types.BooleanType
		switch {
		case lhsType.IsA(types.IntegerType), lhsType.IsA(types.BooleanType):
			opVal = c.builder.CreateICmp(llvm.IntSGT, lhsVal.Value, rhsVal.Value, "igt")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFCmp(llvm.FloatOGT, lhsVal.Value, rhsVal.Value, "fgt")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Gte:
		opType = types.BooleanType
		switch {
		case lhsType.IsA(types.IntegerType), lhsType.IsA(types.BooleanType):
			opVal = c.builder.CreateICmp(llvm.IntSGE, lhsVal.Value, rhsVal.Value, "igte")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFCmp(llvm.FloatOGE, lhsVal.Value, rhsVal.Value, "fgte")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.And:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.builder.CreateAnd(lhsVal.Value, rhsVal.Value, "and")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Or:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.builder.CreateOr(lhsVal.Value, rhsVal.Value, "or")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.LAnd:
		opType = lhsType
		switch {
		case lhsType.IsA(types.BooleanType):
			opVal = c.builder.CreateAnd(lhsVal.Value, rhsVal.Value, "land")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.LOr:
		opType = lhsType
		switch {
		case lhsType.IsA(types.BooleanType):
			opVal = c.builder.CreateOr(lhsVal.Value, rhsVal.Value, "land")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	}
	return PaskalValue{
		TypeValue: types.NewVariable("", opType),
		Value:     opVal,
	}, nil
}

func (c *Compiler) compileUnaryExpr(expr ast.Node, op lexer.Kind) (PaskalValue, error) {
	unaryVal, err := c.compileExpr(expr, false)
	if err != nil {
		return PaskalValue{}, err
	}

	unaryValType := unaryVal.TypeValue.Type()
	var opVal llvm.Value
	switch op {
	case lexer.Deref:
		unaryValType = unaryValType.DerefType()
		if unaryValType == nil {
			return PaskalValue{}, fmt.Errorf("cannot deref type %s", unaryVal.TypeValue.Type())
		}
		opVal = c.builder.CreateLoad(unaryVal.Value, "ptrderef")
	case lexer.AddressOf:
		opVal = unaryVal.Value
		// opVal = c.Builder.CreateBitCast(unaryVal.Value, unaryValType.LLVMType(), "addressof")
	case lexer.LNot, lexer.Not:
		if !unaryValType.IsA(types.BooleanType) && !unaryValType.IsA(types.IntegerType) {
			return PaskalValue{}, fmt.Errorf("invalid op %s on type %s", op, unaryValType)
		}
		opVal = c.builder.CreateNot(unaryVal.Value, "not")
	default:
		return PaskalValue{}, fmt.Errorf("invalid unary operator %s", op)
	}
	return PaskalValue{
		TypeValue: types.NewVariable("", unaryValType),
		Value:     opVal,
	}, nil
}

func (c *Compiler) variableLookup(name string) (PaskalValue, error) {
	if val, ok := c.vars[name]; ok {
		return val, nil
	} else if val, ok := c.globalVars[name]; ok {
		return val, nil
	}
	return PaskalValue{}, fmt.Errorf("unknown identifier %s", name)
}

func (c *Compiler) compileIdentifierExpr(expr ast.IdentifierExpression) (PaskalValue, error) {
	name := string(expr)
	identifier, err := c.variableLookup(name)
	if err != nil {
		return PaskalValue{}, err
	}
	// identifier = c.deref(identifier)
	return identifier, nil
}

func (c *Compiler) compileIntegerExpr(expr ast.IntegerExpression) (PaskalValue, error) {
	return PaskalValue{
		TypeValue: types.NewVariable("", types.IntegerType),
		Value:     llvm.ConstInt(types.IntegerType.LLVMType(), uint64(expr), true),
	}, nil
}

func (c *Compiler) compileBooleanExpr(expr ast.BooleanExpression) (PaskalValue, error) {
	var val uint64
	if expr {
		val = 1
	}
	return PaskalValue{
		TypeValue: types.NewVariable("", types.BooleanType),
		Value:     llvm.ConstInt(types.BooleanType.LLVMType(), val, true),
	}, nil
}

func (c *Compiler) compileRealExpr(expr ast.RealExpression) (PaskalValue, error) {
	return PaskalValue{
		TypeValue: types.NewVariable("", types.RealType),
		Value:     llvm.ConstFloat(types.RealType.LLVMType(), float64(expr)),
	}, nil
}

func (c *Compiler) compileStringExpr(expr ast.StringExpression) (PaskalValue, error) {
	val := c.builder.CreateGlobalString(string(expr), "str")
	val.SetLinkage(llvm.PrivateLinkage)
	arrType := &types.Array{
		Base:         types.CharacterType,
		ElementCount: len(expr),
	}
	return PaskalValue{
		TypeValue: types.NewVariable("", arrType).Ref(),
		Value:     val,
	}, nil
}

func (c *Compiler) compileCharacterExpr(expr ast.CharacterExpression) (PaskalValue, error) {
	// Truncate chracters into a byte
	// TODO: Should be catched earlier in the parsing
	return PaskalValue{
		TypeValue: types.NewVariable("", types.CharacterType),
		Value:     llvm.ConstInt(types.CharacterType.LLVMType(), uint64(byte(expr)), true),
	}, nil
}

func (c *Compiler) deref(val PaskalValue) PaskalValue {
	isPtr := val.TypeValue.Type().DerefType() != nil

	if isPtr {
		arr, isArr := val.TypeValue.Deref().Type().(*types.Array)
		if isArr && arr.Base.IsA(types.CharacterType) {
			return PaskalValue{
				TypeValue: types.NewVariable("", arr.Base.RefType()),
				Value:     c.builder.CreateGEP(val.Value, []llvm.Value{llvm.ConstInt(llvm.Int32Type(), 0, true), llvm.ConstInt(llvm.Int32Type(), 0, true)}, ""),
			}
		}
		derefVal := c.builder.CreateLoad(val.Value, "deref")
		derefPaskalVal := PaskalValue{
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
