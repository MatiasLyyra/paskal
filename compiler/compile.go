package compiler

import (
	"fmt"
	"reflect"

	"github.com/MatiasLyyra/paskal/ast"
	"github.com/MatiasLyyra/paskal/lexer"
	"github.com/MatiasLyyra/paskal/types"
	"github.com/go-llvm/llvm"
)

type PaskalValue struct {
	TypeValue types.Value
	Value     llvm.Value
}

type PaskalFunction struct {
	Function      *ast.Function
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
	return llvm.FunctionType(f.Function.Return.LLVMType(), paramTypes, false)
}

type Compiler struct {
	Builder      llvm.Builder
	Module       llvm.Module
	Context      llvm.Context
	PassManager  llvm.PassManager
	PaskalModule *ast.Module
	Funcs        map[string]PaskalFunction
	Vars         map[string]PaskalValue
	GlobalVars   map[string]PaskalValue
}

func NewCompiler(module *ast.Module, optimize bool) *Compiler {
	c := &Compiler{
		Module:       llvm.NewModule(module.Name),
		Builder:      llvm.NewBuilder(),
		Funcs:        make(map[string]PaskalFunction),
		Vars:         make(map[string]PaskalValue),
		GlobalVars:   make(map[string]PaskalValue),
		PaskalModule: module,
	}
	c.PassManager = llvm.NewFunctionPassManagerForModule(c.Module)
	if optimize {
		c.PassManager.AddPromoteMemoryToRegisterPass()
		c.PassManager.AddInstructionCombiningPass()
		c.PassManager.AddReassociatePass()
		c.PassManager.AddDeadStoreEliminationPass()
		c.PassManager.AddGVNPass()
		c.PassManager.AddCFGSimplificationPass()
		c.PassManager.AddTailCallEliminationPass()
	}
	c.PassManager.InitializeFunc()
	c.Context = c.Module.Context()
	return c
}

func (c *Compiler) Compile() error {
	for _, globalVar := range c.PaskalModule.Vars {
		val := llvm.AddGlobal(c.Module, globalVar.Type().LLVMType(), globalVar.Name())
		val.SetInitializer(llvm.Undef(globalVar.Type().LLVMType()))
		c.GlobalVars[globalVar.Name()] = PaskalValue{
			TypeValue: globalVar.Ref(),
			Value:     val,
		}
	}
	err := c.compileFuncs()
	if err != nil {
		return err
	}
	err = llvm.VerifyModule(c.Module, llvm.AbortProcessAction)
	if err != nil {
		panic(err)
	}
	return nil
}

func (c *Compiler) compileFuncs() error {
	for _, function := range c.PaskalModule.Funcs {
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
		c.PassManager.RunFunc(val)
		err = llvm.VerifyFunction(val, llvm.AbortProcessAction)
		if err != nil {
			panic(err)
		}
	}
	return nil
}

func (c *Compiler) compileFunctionBody(f PaskalFunction) error {
	entryBB := c.Context.AddBasicBlock(f.FunctionValue, "entry")
	c.Builder.SetInsertPoint(entryBB, entryBB.FirstInstruction())
	defer func() {
		c.Vars = make(map[string]PaskalValue)
	}()
	for i, paramValue := range f.FunctionValue.Params() {
		paramVar := f.Function.Params[i]
		paramValue.SetName(paramVar.Name())
		c.Vars[paramVar.Name()] = PaskalValue{
			TypeValue: paramVar,
			Value:     paramValue,
		}
	}
	for _, localVar := range f.Function.Vars {
		value := c.Builder.CreateAlloca(localVar.Type().LLVMType(), localVar.Name())
		c.Vars[localVar.Name()] = PaskalValue{
			TypeValue: localVar.Ref(),
			Value:     value,
		}
	}
	retType := f.Function.Return.LLVMType()
	isVoid := retType == llvm.VoidType()
	funcName := f.Function.Name
	var retVal llvm.Value
	if !isVoid {
		retVal = c.Builder.CreateAlloca(f.Function.Return.LLVMType(), f.Function.Name)
		c.Vars[f.Function.Name] = PaskalValue{
			TypeValue: types.NewVariable(funcName, f.Function.Return).Ref(),
			Value:     retVal,
		}
	}
	err := c.compileBlock(f.Function.Body)
	if err != nil {
		return err
	}
	if isVoid {
		c.Builder.CreateRetVoid()
	} else {
		retVal = c.Builder.CreateLoad(retVal, "retload")
		c.Builder.CreateRet(retVal)
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
		_, err = c.compileExpr(stmt)
	}
	return
}

func (c *Compiler) compileIfStmt(ifStmt *ast.IfStatement) error {
	condVal, err := c.compileExpr(ifStmt.Condition)
	parent := c.Builder.GetInsertBlock().Parent()
	if err != nil {
		return err
	}
	condValType := condVal.TypeValue.Type()
	if !condValType.IsA(types.BooleanType) {
		return fmt.Errorf("cannot evaluate if statement on non boolean value")
	}
	trueBB := c.Context.AddBasicBlock(parent, "trueblock")
	falseBB := c.Context.AddBasicBlock(parent, "falseblock")
	mergeBB := c.Context.AddBasicBlock(parent, "mergeblock")
	c.Builder.CreateCondBr(condVal.Value, trueBB, falseBB)
	c.Builder.SetInsertPoint(trueBB, trueBB.FirstInstruction())
	err = c.compileBlock(ifStmt.TrueBlock)
	if err != nil {
		return err
	}
	c.Builder.CreateBr(mergeBB)
	c.Builder.SetInsertPoint(falseBB, falseBB.FirstInstruction())
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
	c.Builder.CreateBr(mergeBB)
	c.Builder.SetInsertPoint(mergeBB, mergeBB.FirstInstruction())
	return nil
}

func (c *Compiler) compileExpr(node ast.Node) (PaskalValue, error) {
	switch expr := node.(type) {
	case *ast.BinaryExpression:
		return c.compileBinaryExpr(expr.LHS, expr.RHS, expr.Op)
	case *ast.FunctionCall:
		return c.compileFunctionCall(expr)
	case ast.IdentifierExpression:
		return c.compileIdentifierExpr(expr)
	case ast.IntegerExpression:
		return c.compileIntegerExpr(expr)
	case ast.BooleanExpression:
		return c.compileBooleanExpr(expr)
	default:
		panic(fmt.Sprintf("invalid expression %s", reflect.ValueOf(expr)))
	}
}
func (c *Compiler) compileFunctionCall(fnCall *ast.FunctionCall) (PaskalValue, error) {
	var args []llvm.Value
	fnVal, ok := c.Funcs[fnCall.Name]
	if !ok {
		return PaskalValue{}, fmt.Errorf("function with name %s does not exist", fnCall.Name)
	}
	if fnVal.Argc() != len(fnCall.Args) {
		return PaskalValue{}, fmt.Errorf("incorrect amount of arguments to function %s, expected: %d got: %d", fnCall.Name, fnVal.Argc(), len(fnCall.Args))
	}
	for i, arg := range fnCall.Args {
		argVal, err := c.compileExpr(arg)
		if err != nil {
			return PaskalValue{}, err
		}
		argValType := argVal.TypeValue.Type()
		paramValType := fnVal.Function.Params[i].Type()
		if !paramValType.IsA(argValType) {
			return PaskalValue{}, fmt.Errorf("incorrect argument type to function %s, expected: %s got %s", fnCall.Name, paramValType, argValType)
		}
		args = append(args, argVal.Value)
	}
	var val llvm.Value
	ret := PaskalValue{
		TypeValue: types.NewVariable("", fnVal.Function.Return),
	}
	if fnVal.Function.Return.IsA(types.VoidType) {
		c.Builder.CreateCall(fnVal.FunctionValue, args, "")
	} else {
		val = c.Builder.CreateCall(fnVal.FunctionValue, args, "fncallret")
	}
	ret.Value = val
	return ret, nil
}

func (c *Compiler) compileBinaryExpr(lhs, rhs ast.Node, op lexer.Kind) (PaskalValue, error) {
	lhsVal, err := c.compileExpr(lhs)
	if err != nil {
		return PaskalValue{}, err
	}
	rhsVal, err := c.compileExpr(rhs)
	if err != nil {
		return PaskalValue{}, err
	}

	lhsType := lhsVal.TypeValue.Type()
	rhsType := rhsVal.TypeValue.Type()
	if op == lexer.Assignment {
		if lhsType.DerefType() == nil {
			return PaskalValue{}, fmt.Errorf("cannot assign to rvalue")
		}
		if !lhsType.DerefType().IsA(rhsType) {
			return PaskalValue{}, fmt.Errorf("cannot assign %s to %s", lhsType.DerefType(), rhsType)
		}
		c.Builder.CreateStore(rhsVal.Value, lhsVal.Value)
		return rhsVal, nil
	}
	lhsVal = c.deref(lhsVal)
	rhsVal = c.deref(rhsVal)
	lhsType = lhsVal.TypeValue.Type()
	rhsType = rhsVal.TypeValue.Type()
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
			opVal = c.Builder.CreateMul(lhsVal.Value, rhsVal.Value, "mul")
		case lhsType.IsA(types.RealType):
			opVal = c.Builder.CreateFMul(lhsVal.Value, rhsVal.Value, "fmul")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Div:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.Builder.CreateSDiv(lhsVal.Value, rhsVal.Value, "sdiv")
		case lhsType.IsA(types.RealType):
			opVal = c.Builder.CreateFDiv(lhsVal.Value, rhsVal.Value, "fdiv")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Mod:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.Builder.CreateSRem(lhsVal.Value, rhsVal.Value, "srem")
		case lhsType.IsA(types.RealType):
			opVal = c.Builder.CreateFRem(lhsVal.Value, rhsVal.Value, "frem")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Add:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.Builder.CreateAdd(lhsVal.Value, rhsVal.Value, "add")
		case lhsType.IsA(types.RealType):
			opVal = c.Builder.CreateFAdd(lhsVal.Value, rhsVal.Value, "fadd")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Sub:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.Builder.CreateSub(lhsVal.Value, rhsVal.Value, "sub")
		case lhsType.IsA(types.RealType):
			opVal = c.Builder.CreateFSub(lhsVal.Value, rhsVal.Value, "fsub")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Eq:
		opType = types.BooleanType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.Builder.CreateICmp(llvm.IntEQ, lhsVal.Value, rhsVal.Value, "ieq")
		case lhsType.IsA(types.RealType):
			opVal = c.Builder.CreateFCmp(llvm.FloatOEQ, lhsVal.Value, rhsVal.Value, "feq")
		case lhsType.IsA(types.BooleanType):
			opVal = c.Builder.CreateICmp(llvm.IntEQ, lhsVal.Value, rhsVal.Value, "booleq")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.And:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.Builder.CreateAnd(lhsVal.Value, rhsVal.Value, "and")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Or:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.Builder.CreateOr(lhsVal.Value, rhsVal.Value, "or")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.LAnd:
		opType = lhsType
		switch {
		case lhsType.IsA(types.BooleanType):
			opVal = c.Builder.CreateAnd(lhsVal.Value, rhsVal.Value, "land")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.LOr:
		opType = lhsType
		switch {
		case lhsType.IsA(types.BooleanType):
			opVal = c.Builder.CreateOr(lhsVal.Value, rhsVal.Value, "land")
		default:
			return PaskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	}
	return PaskalValue{
		TypeValue: types.NewVariable("", opType),
		Value:     opVal,
	}, nil
}

func (c *Compiler) compileIdentifierExpr(expr ast.IdentifierExpression) (PaskalValue, error) {
	name := string(expr)
	if val, ok := c.Vars[name]; ok {
		return val, nil
	} else if val, ok := c.GlobalVars[name]; ok {
		return val, nil
	}
	return PaskalValue{}, fmt.Errorf("unknown identifier %s", name)
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

func (c *Compiler) deref(val PaskalValue) PaskalValue {
	isPtr := val.TypeValue.Type().DerefType() == nil
	if !isPtr {
		derefVal := c.Builder.CreateLoad(val.Value, "deref")
		return PaskalValue{
			TypeValue: val.TypeValue.Deref(),
			Value:     derefVal,
		}
	}
	return val
}
