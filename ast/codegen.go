package ast

// import (
// 	"fmt"

// 	"github.com/go-llvm/llvm"
// )

// type Context struct {
// 	Builder     llvm.Builder
// 	Module      llvm.Module
// 	Context     llvm.Context
// 	PassManager llvm.PassManager
// 	Funcs       map[string]VariableType
// 	Vars        map[string]llvm.Value
// 	GlobalVars  map[string]llvm.Value
// }

// func NewContext() *Context {
// 	return &Context{
// 		Vars:       make(map[string]llvm.Value),
// 		GlobalVars: make(map[string]llvm.Value),
// 		Funcs:      make(map[string]VariableType),
// 	}
// }

// type PaskalValue struct {
// 	Value llvm.Value
// 	Type  VariableType
// }

// func (m *Module) Compile(ctx *Context, optimize bool) error {
// 	ctx.Module = llvm.NewModule(m.Name)
// 	ctx.Context = ctx.Module.Context()
// 	ctx.Builder = llvm.NewBuilder()

// 	pm := llvm.NewFunctionPassManagerForModule(ctx.Module)
// 	if optimize {
// 		pm.AddPromoteMemoryToRegisterPass()
// 		pm.AddInstructionCombiningPass()
// 		pm.AddReassociatePass()
// 		pm.AddGVNPass()
// 		pm.AddCFGSimplificationPass()
// 	}
// 	pm.InitializeFunc()
// 	ctx.PassManager = pm
// 	for _, variable := range m.vars {
// 		var llvmType llvm.Type
// 		switch variable.VarType {
// 		case Boolean:
// 			llvmType = llvm.Int1Type()
// 		case Integer:
// 			llvmType = llvm.Int32Type()
// 		case Real:
// 			llvmType = llvm.FloatType()
// 		case String:
// 			llvmType = llvm.PointerType(llvm.Int8Type(), 0)
// 		default:
// 			return fmt.Errorf("invalid type (%s) in module %s", variable.Name, m.Name)
// 		}
// 		val := llvm.AddGlobal(ctx.Module, llvmType, variable.Name)
// 		val.SetInitializer(llvm.Undef(llvmType))
// 		ctx.GlobalVars[variable.Name] = val
// 	}
// 	for _, function := range m.funcs {
// 		_, err := function.Codegen(ctx)
// 		if err != nil {
// 			return err
// 		}
// 	}
// 	if m.Main != nil {
// 		mainFnType := llvm.FunctionType(llvm.VoidType(), []llvm.Type{}, false)
// 		mainFn := llvm.AddFunction(ctx.Module, "main", mainFnType)
// 		entryBB := ctx.Context.AddBasicBlock(mainFn, "entry")
// 		ctx.Builder.SetInsertPoint(entryBB, entryBB.FirstInstruction())
// 		_, err := m.Main.Codegen(ctx)
// 		ctx.Builder.CreateRetVoid()
// 		if err != nil {
// 			return err
// 		}
// 		err = llvm.VerifyFunction(mainFn, llvm.AbortProcessAction)
// 		if err != nil {
// 			return err
// 		}
// 		ctx.PassManager.RunFunc(mainFn)
// 	}
// 	err := llvm.VerifyModule(ctx.Module, llvm.AbortProcessAction)
// 	if err != nil {
// 		return err
// 	}
// 	return nil
// }

// func (f *Function) Codegen(ctx *Context) (llvm.Value, error) {
// 	defer func() {
// 		ctx.Vars = make(map[string]llvm.Value)
// 	}()
// 	var paramTypes []llvm.Type
// 	for _, param := range f.params {
// 		paramType := param.VarType.LLVMType()
// 		paramTypes = append(paramTypes, paramType)
// 	}
// 	ctx.Funcs[f.Name] = f.Return
// 	fType := llvm.FunctionType(f.Return.LLVMType(), paramTypes, false)
// 	function := llvm.AddFunction(ctx.Module, f.Name, fType)
// 	for i, param := range function.Params() {
// 		param.SetName(f.params[i].Name)
// 		ctx.Vars[f.params[i].Name] = param
// 	}
// 	entryBB := ctx.Context.AddBasicBlock(function, "entry")
// 	ctx.Builder.SetInsertPoint(entryBB, entryBB.FirstInstruction())
// 	for _, variable := range f.vars {
// 		fVar := ctx.Builder.CreateAlloca(variable.VarType.LLVMType(), variable.Name)
// 		ctx.Vars[variable.Name] = fVar
// 	}
// 	if f.Return != Void {
// 		fVar := ctx.Builder.CreateAlloca(f.Return.LLVMType(), f.Name)
// 		ctx.Vars[f.Name] = fVar
// 	}
// 	_, err := f.Body.Codegen(ctx)
// 	if err != nil {
// 		return llvm.Value{}, err
// 	}
// 	if f.Return == Void {
// 		ctx.Builder.CreateRetVoid()
// 	} else {
// 		retVal := ctx.Vars[f.Name]
// 		retVal = ldaPtr(retVal, ctx)
// 		ctx.Builder.CreateRet(retVal)
// 	}
// 	err = llvm.VerifyFunction(function, llvm.AbortProcessAction)
// 	if err != nil {
// 		return llvm.Value{}, err
// 	}
// 	ctx.PassManager.RunFunc(function)
// 	return llvm.Value{}, nil
// }

// func (fnCall *FunctionCall) Codegen(ctx *Context) (llvm.Value, error) {
// 	function := ctx.Module.NamedFunction(fnCall.Name)
// 	if function.IsNull() {
// 		return llvm.Value{}, fmt.Errorf("function \"%s\" is not declared", fnCall.Name)
// 	}
// 	if function.ParamsCount() != len(fnCall.Args) {
// 		return llvm.Value{}, fmt.Errorf("function %s expects %d arguments, %d given",
// 			fnCall.Name,
// 			function.ParamsCount(),
// 			len(fnCall.Args))
// 	}
// 	var args []llvm.Value
// 	for _, arg := range fnCall.Args {
// 		argVal, err := arg.Codegen(ctx)
// 		if err != nil {
// 			return llvm.Value{}, err
// 		}
// 		// TODO: Validate argument type
// 		args = append(args, argVal)
// 	}
// 	var fnCallValue llvm.Value
// 	if ctx.Funcs[fnCall.Name] != Void {
// 		fnCallValue = ctx.Builder.CreateCall(function, args, "fncallvalue")
// 	} else {
// 		ctx.Builder.CreateCall(function, args, "")
// 	}
// 	return fnCallValue, nil
// }

// func (b *Block) Codegen(ctx *Context) (llvm.Value, error) {
// 	for _, stmt := range b.Statements {
// 		_, err := stmt.Codegen(ctx)
// 		if err != nil {
// 			return llvm.Value{}, err
// 		}
// 	}
// 	return llvm.Value{}, nil
// }

// func (ifStmt *IfStatement) Codegen(ctx *Context) (llvm.Value, error) {
// 	condVal, err := ifStmt.Condition.Codegen(ctx)
// 	if err != nil {
// 		return llvm.Value{}, err
// 	}
// 	if condVal.Type() != llvm.Int1Type() {
// 		return llvm.Value{}, fmt.Errorf("condition value must be boolean")
// 	}
// 	compVal := ctx.Builder.CreateICmp(llvm.IntEQ, condVal, llvm.ConstInt(llvm.Int1Type(), 1, false), "ifcond")
// 	trueBlock := ctx.Context.AddBasicBlock(ctx.Builder.GetInsertBlock().Parent(), "trueblock")
// 	falseBlock := ctx.Context.AddBasicBlock(trueBlock.Parent(), "falseblock")
// 	mergeBlock := ctx.Context.AddBasicBlock(falseBlock.Parent(), "mergeblock")
// 	ctx.Builder.CreateCondBr(compVal, trueBlock, falseBlock)
// 	ctx.Builder.SetInsertPoint(trueBlock, trueBlock.FirstInstruction())
// 	_, err = ifStmt.TrueBlock.Codegen(ctx)
// 	if err != nil {
// 		return llvm.Value{}, err
// 	}
// 	ctx.Builder.CreateBr(mergeBlock)
// 	ctx.Builder.SetInsertPoint(falseBlock, falseBlock.FirstInstruction())
// 	if ifStmt.FalseBlock != nil {
// 		_, err := ifStmt.FalseBlock.Codegen(ctx)
// 		if err != nil {
// 			return llvm.Value{}, err
// 		}
// 	}
// 	ctx.Builder.CreateBr(mergeBlock)
// 	ctx.Builder.SetInsertPoint(mergeBlock, mergeBlock.LastInstruction())
// 	return llvm.Value{}, nil
// }

// func (binExpr *BinaryExpression) Codegen(ctx *Context) (llvm.Value, error) {
// 	lhs, err := binExpr.LHS.Codegen(ctx)
// 	if err != nil {
// 		return llvm.Value{}, err
// 	}
// 	rhs, err := binExpr.RHS.Codegen(ctx)
// 	if err != nil {
// 		return llvm.Value{}, err
// 	}
// 	if lhs.Type() != rhs.Type() &&
// 		lhs.Type() != llvm.PointerType(rhs.Type(), 0) &&
// 		llvm.PointerType(lhs.Type(), 0) != rhs.Type() {
// 		return llvm.Value{}, fmt.Errorf("invalid operation %s on type %s and %s", binExpr.Op, lhs.Type(), rhs.Type())
// 	}
// 	switch binExpr.Op {
// 	case "+":
// 		lhs = ldaPtr(lhs, ctx)
// 		rhs = ldaPtr(rhs, ctx)
// 		if lhs.Type().TypeKind() == llvm.IntegerTypeKind {
// 			return ctx.Builder.CreateAdd(lhs, rhs, "tempadd"), nil
// 		} else if lhs.Type().TypeKind() == llvm.FloatTypeKind {
// 			return ctx.Builder.CreateFAdd(lhs, rhs, "tempfadd"), nil
// 		} else {
// 			return llvm.Value{}, fmt.Errorf("invalid op %s on type %s", binExpr.Op, lhs.Type())
// 		}
// 	case "-":
// 		lhs = ldaPtr(lhs, ctx)
// 		rhs = ldaPtr(rhs, ctx)
// 		if lhs.Type().TypeKind() == llvm.IntegerTypeKind {
// 			return ctx.Builder.CreateSub(lhs, rhs, "tempsub"), nil
// 		} else if lhs.Type().TypeKind() == llvm.FloatTypeKind {
// 			return ctx.Builder.CreateFSub(lhs, rhs, "tempfsub"), nil
// 		} else {
// 			return llvm.Value{}, fmt.Errorf("invalid op %s on type %s", binExpr.Op, lhs.Type())
// 		}
// 	case "*":
// 		lhs = ldaPtr(lhs, ctx)
// 		rhs = ldaPtr(rhs, ctx)
// 		if lhs.Type().TypeKind() == llvm.IntegerTypeKind {
// 			return ctx.Builder.CreateMul(lhs, rhs, "tempmul"), nil
// 		} else if lhs.Type().TypeKind() == llvm.FloatTypeKind {
// 			return ctx.Builder.CreateFMul(lhs, rhs, "tempfmul"), nil
// 		} else {
// 			return llvm.Value{}, fmt.Errorf("invalid op %s on type %s", binExpr.Op, lhs.Type())
// 		}
// 	case "/":
// 		lhs = ldaPtr(lhs, ctx)
// 		rhs = ldaPtr(rhs, ctx)
// 		if lhs.Type().TypeKind() == llvm.IntegerTypeKind {
// 			return ctx.Builder.CreateSDiv(lhs, rhs, "tempdiv"), nil
// 		} else if lhs.Type().TypeKind() == llvm.FloatTypeKind {
// 			return ctx.Builder.CreateFDiv(lhs, rhs, "tempfdiv"), nil
// 		} else {
// 			return llvm.Value{}, fmt.Errorf("invalid op %s on type %s", binExpr.Op, lhs.Type())
// 		}
// 	case "%":
// 		lhs = ldaPtr(lhs, ctx)
// 		rhs = ldaPtr(rhs, ctx)
// 		if lhs.Type().TypeKind() == llvm.IntegerTypeKind {
// 			return ctx.Builder.CreateSRem(lhs, rhs, "temprem"), nil
// 		} else if lhs.Type().TypeKind() == llvm.FloatTypeKind {
// 			return ctx.Builder.CreateFRem(lhs, rhs, "tempfrem"), nil
// 		} else {
// 			return llvm.Value{}, fmt.Errorf("invalid op %s on type %s", binExpr.Op, lhs.Type())
// 		}
// 	case "is":
// 		lhs = ldaPtr(lhs, ctx)
// 		rhs = ldaPtr(rhs, ctx)
// 		if lhs.Type().TypeKind() == llvm.IntegerTypeKind {
// 			return ctx.Builder.CreateICmp(llvm.IntEQ, lhs, rhs, "tempeq"), nil
// 		} else {
// 			return llvm.Value{}, fmt.Errorf("invalid op %s on type %s", binExpr.Op, lhs.Type())
// 		}
// 	case ":=":
// 		if lhs.Type().TypeKind() != llvm.PointerTypeKind {
// 			return llvm.Value{}, fmt.Errorf("cannot assign to rvalue")
// 		}
// 		rhs = ldaPtr(rhs, ctx)
// 		return ctx.Builder.CreateStore(rhs, lhs), nil
// 	}
// 	return llvm.Value{}, fmt.Errorf("binary op %s not implemented", binExpr.Op)
// }

// func ldaPtr(val llvm.Value, ctx *Context) llvm.Value {
// 	if val.Type().TypeKind() == llvm.PointerTypeKind {
// 		return ctx.Builder.CreateLoad(val, "tmpload")
// 	}
// 	return val
// }

// func (unaryExpr *UnaryExpression) Codegen(ctx *Context) (llvm.Value, error) {
// 	value, err := unaryExpr.Value.Codegen(ctx)
// 	if err != nil {
// 		return llvm.Value{}, err
// 	}
// 	valueTypeKind := value.Type().TypeKind()
// 	switch unaryExpr.Op {
// 	case "+":
// 	case "-":
// 		value = ldaPtr(value, ctx)
// 		if valueTypeKind == llvm.IntegerTypeKind || valueTypeKind == llvm.FloatTypeKind {
// 			value = ctx.Builder.CreateNeg(value, "tempneg")
// 		} else {
// 			return value, fmt.Errorf("cannot use \"-\" on non numeric value")
// 		}
// 	case "not":
// 		value = ldaPtr(value, ctx)
// 		if value.Type() == llvm.Int1Type() {
// 			value = ctx.Builder.CreateNot(value, "templnot")
// 		} else {
// 			return value, fmt.Errorf("cannot use \"not\" on non boolean value")
// 		}
// 	case "~":
// 		value = ldaPtr(value, ctx)
// 		if value.Type().TypeKind() == llvm.IntegerTypeKind {
// 			value = ctx.Builder.CreateNot(value, "tempnot")
// 		} else {
// 			return value, fmt.Errorf("cannot use \"~\" on non boolean value")
// 		}
// 	}
// 	return value, nil
// }

// func (iExpr IntegerExpression) Codegen(ctx *Context) (llvm.Value, error) {
// 	return llvm.ConstInt(Integer.LLVMType(), uint64(iExpr), true), nil
// }

// func (rExpr RealExpression) Codegen(ctx *Context) (llvm.Value, error) {
// 	return llvm.ConstFloat(Real.LLVMType(), float64(rExpr)), nil
// }

// func (bExpr BooleanExpression) Codegen(ctx *Context) (llvm.Value, error) {
// 	if bExpr {
// 		return llvm.ConstInt(Boolean.LLVMType(), 1, false), nil
// 	}
// 	return llvm.ConstInt(Boolean.LLVMType(), 0, false), nil
// }

// func (cExpr CharacterExpression) Codegen(ctx *Context) (llvm.Value, error) {
// 	return llvm.ConstInt(Boolean.LLVMType(), uint64(cExpr), true), nil
// }
// func (sExpr StringExpression) Codegen(ctx *Context) (llvm.Value, error) {
// 	return ctx.Builder.CreateGlobalString(string(sExpr), "tempstr"), nil
// }

// func (idExpr IdentifierExpression) Codegen(ctx *Context) (llvm.Value, error) {
// 	if val, ok := ctx.Vars[string(idExpr)]; ok {
// 		return val, nil
// 	} else if val, ok := ctx.GlobalVars[string(idExpr)]; ok {
// 		return val, nil
// 	} else if _, ok := ctx.Funcs[string(idExpr)]; ok {
// 		tempCall := &FunctionCall{Name: string(idExpr)}
// 		return tempCall.Codegen(ctx)
// 	}
// 	return llvm.Value{}, fmt.Errorf("unknown identifier %s", idExpr)
// }
