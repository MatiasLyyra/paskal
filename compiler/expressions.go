package compiler

import (
	"fmt"
	"reflect"

	"github.com/MatiasLyyra/paskal/ast"
	"github.com/MatiasLyyra/paskal/lexer"
	"github.com/MatiasLyyra/paskal/types"
	"llvm.org/llvm/bindings/go/llvm"
)

func (c *Compiler) compileExpr(node ast.Node, deref bool) (val paskalValue, err error) {
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
func (c *Compiler) compileFunctionCall(fnCall *ast.FunctionCall) (paskalValue, error) {
	var args []llvm.Value
	fnVal, ok := c.funcs[fnCall.Name]
	if !ok {
		return paskalValue{}, fmt.Errorf("function with name %s does not exist", fnCall.Name)
	}
	argcOk := fnVal.Argc() == len(fnCall.Args)
	varArgCallOk := fnVal.IsVarArg && fnVal.Argc() <= len(fnCall.Args)
	if !argcOk && !varArgCallOk {
		return paskalValue{}, fmt.Errorf("incorrect amount of arguments to function %s, expected: %d got: %d", fnCall.Name, fnVal.Argc(), len(fnCall.Args))
	}
	for i, arg := range fnCall.Args {
		argVal, err := c.compileExpr(arg, true)
		if err != nil {
			return paskalValue{}, err
		}
		argValType := argVal.TypeValue.Type()
		if i < len(fnVal.Function.Params) {
			paramValType := fnVal.Function.Params[i].Type()
			if !paramValType.IsA(argValType) {
				return paskalValue{}, fmt.Errorf("incorrect argument type to function %s, expected: %s got %s", fnCall.Name, paramValType, argValType)
			}
		}
		args = append(args, argVal.Value)
	}
	var val llvm.Value
	ret := paskalValue{
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
func (c *Compiler) compileSubscriptExpr(expr *ast.Subscript) (paskalValue, error) {
	exprVal, err := c.compileExpr(expr.Value, false)
	if err != nil {
		return paskalValue{}, err
	}
	exprValDeref := exprVal.TypeValue.Type().DerefType()
	if exprValDeref == nil {
		panic("non pointer indexing")
	}
	origArray, isArr := exprValDeref.(*types.Array)
	arr := origArray
	if !isArr {
		return paskalValue{}, fmt.Errorf("cannot index non-array value of type: %s", exprVal.TypeValue.Type())
	}
	var gepIndexes = []llvm.Value{llvm.ConstInt(llvm.Int32Type(), 0, true)}
	for _, indexing := range expr.Indexing {
		indexValue, err := c.compileExpr(indexing, true)
		if err != nil {
			return paskalValue{}, err
		}
		if !indexValue.TypeValue.Type().IsA(types.IntegerType) {
			return paskalValue{}, fmt.Errorf("cannot index with non-integer value")
		}
		gepIndexes = append(gepIndexes, indexValue.Value)
	}
	elementPtr := c.builder.CreateGEP(exprVal.Value, gepIndexes, "gep")
	return paskalValue{
		Value:     elementPtr,
		TypeValue: types.NewVariable("", arr.Base.RefType()),
	}, nil
}
func (c *Compiler) compileBinaryExpr(lhs, rhs ast.Node, op lexer.Kind) (paskalValue, error) {
	var (
		lhsVal paskalValue
		rhsVal paskalValue
	)

	if op == lexer.Assignment {
		lhsVal, err := c.compileExpr(lhs, false)
		if err != nil {
			return paskalValue{}, err
		}
		rhsVal, err = c.compileExpr(rhs, true)
		if err != nil {
			return paskalValue{}, err
		}
		lhsType := lhsVal.TypeValue.Type()
		rhsType := rhsVal.TypeValue.Type()
		if lhsType.DerefType() == nil {
			return paskalValue{}, fmt.Errorf("cannot assign to rvalue")
		}
		if !lhsType.DerefType().IsA(rhsType) {
			return paskalValue{}, fmt.Errorf("cannot assign %s to %s", rhsType, lhsType.DerefType())
		}
		c.builder.CreateStore(rhsVal.Value, lhsVal.Value)
		return rhsVal, nil
	}
	lhsVal, err := c.compileExpr(lhs, true)
	if err != nil {
		return paskalValue{}, err
	}
	rhsVal, err = c.compileExpr(rhs, true)
	if err != nil {
		return paskalValue{}, err
	}
	lhsType := lhsVal.TypeValue.Type()
	rhsType := rhsVal.TypeValue.Type()
	if !lhsType.IsA(rhsType) {
		return paskalValue{}, fmt.Errorf("invalid op: %s %s %s", lhsType, op, rhsType)
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
			return paskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Div:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.builder.CreateSDiv(lhsVal.Value, rhsVal.Value, "sdiv")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFDiv(lhsVal.Value, rhsVal.Value, "fdiv")
		default:
			return paskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Mod:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.builder.CreateSRem(lhsVal.Value, rhsVal.Value, "srem")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFRem(lhsVal.Value, rhsVal.Value, "frem")
		default:
			return paskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Add:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.builder.CreateAdd(lhsVal.Value, rhsVal.Value, "add")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFAdd(lhsVal.Value, rhsVal.Value, "fadd")
		default:
			return paskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Sub:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.builder.CreateSub(lhsVal.Value, rhsVal.Value, "sub")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFSub(lhsVal.Value, rhsVal.Value, "fsub")
		default:
			return paskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
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
			return paskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Ne:
		opType = types.BooleanType
		switch {
		case lhsType.IsA(types.IntegerType), lhsType.IsA(types.BooleanType):
			opVal = c.builder.CreateICmp(llvm.IntNE, lhsVal.Value, rhsVal.Value, "ine")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFCmp(llvm.FloatONE, lhsVal.Value, rhsVal.Value, "fne")
		default:
			return paskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Lt:
		opType = types.BooleanType
		switch {
		case lhsType.IsA(types.IntegerType), lhsType.IsA(types.BooleanType):
			opVal = c.builder.CreateICmp(llvm.IntSLT, lhsVal.Value, rhsVal.Value, "ilt")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFCmp(llvm.FloatOLT, lhsVal.Value, rhsVal.Value, "flt")
		default:
			return paskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Lte:
		opType = types.BooleanType
		switch {
		case lhsType.IsA(types.IntegerType), lhsType.IsA(types.BooleanType):
			opVal = c.builder.CreateICmp(llvm.IntSLE, lhsVal.Value, rhsVal.Value, "ilte")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFCmp(llvm.FloatOLT, lhsVal.Value, rhsVal.Value, "flte")
		default:
			return paskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Gt:
		opType = types.BooleanType
		switch {
		case lhsType.IsA(types.IntegerType), lhsType.IsA(types.BooleanType):
			opVal = c.builder.CreateICmp(llvm.IntSGT, lhsVal.Value, rhsVal.Value, "igt")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFCmp(llvm.FloatOGT, lhsVal.Value, rhsVal.Value, "fgt")
		default:
			return paskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Gte:
		opType = types.BooleanType
		switch {
		case lhsType.IsA(types.IntegerType), lhsType.IsA(types.BooleanType):
			opVal = c.builder.CreateICmp(llvm.IntSGE, lhsVal.Value, rhsVal.Value, "igte")
		case lhsType.IsA(types.RealType):
			opVal = c.builder.CreateFCmp(llvm.FloatOGE, lhsVal.Value, rhsVal.Value, "fgte")
		default:
			return paskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.And:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.builder.CreateAnd(lhsVal.Value, rhsVal.Value, "and")
		default:
			return paskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.Or:
		opType = lhsType
		switch {
		case lhsType.IsA(types.IntegerType):
			opVal = c.builder.CreateOr(lhsVal.Value, rhsVal.Value, "or")
		default:
			return paskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.LAnd:
		opType = lhsType
		switch {
		case lhsType.IsA(types.BooleanType):
			opVal = c.builder.CreateAnd(lhsVal.Value, rhsVal.Value, "land")
		default:
			return paskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	case lexer.LOr:
		opType = lhsType
		switch {
		case lhsType.IsA(types.BooleanType):
			opVal = c.builder.CreateOr(lhsVal.Value, rhsVal.Value, "land")
		default:
			return paskalValue{}, fmt.Errorf("invalid op %s for type %s", op, lhsType)
		}
	}
	return paskalValue{
		TypeValue: types.NewVariable("", opType),
		Value:     opVal,
	}, nil
}

func (c *Compiler) compileUnaryExpr(expr ast.Node, op lexer.Kind) (paskalValue, error) {
	unaryVal, err := c.compileExpr(expr, false)
	if err != nil {
		return paskalValue{}, err
	}

	unaryValType := unaryVal.TypeValue.Type()
	var opVal llvm.Value
	switch op {
	case lexer.Deref:
		unaryValType = unaryValType.DerefType()
		if unaryValType == nil {
			return paskalValue{}, fmt.Errorf("cannot deref type %s", unaryVal.TypeValue.Type())
		}
		opVal = c.builder.CreateLoad(unaryVal.Value, "ptrderef")
	case lexer.AddressOf:
		opVal = unaryVal.Value
		// opVal = c.Builder.CreateBitCast(unaryVal.Value, unaryValType.LLVMType(), "addressof")
	case lexer.LNot, lexer.Not:
		if !unaryValType.IsA(types.BooleanType) && !unaryValType.IsA(types.IntegerType) {
			return paskalValue{}, fmt.Errorf("invalid op %s on type %s", op, unaryValType)
		}
		opVal = c.builder.CreateNot(unaryVal.Value, "not")
	default:
		return paskalValue{}, fmt.Errorf("invalid unary operator %s", op)
	}
	return paskalValue{
		TypeValue: types.NewVariable("", unaryValType),
		Value:     opVal,
	}, nil
}
