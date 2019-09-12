package compiler

import (
	"fmt"

	"github.com/MatiasLyyra/paskal/ast"
	"github.com/MatiasLyyra/paskal/types"
	"llvm.org/llvm/bindings/go/llvm"
)

func (c *Compiler) compileIdentifierExpr(expr ast.IdentifierExpression) (paskalValue, error) {
	name := string(expr)
	if val, ok := c.vars[name]; ok {
		return val, nil
	} else if val, ok := c.globalVars[name]; ok {
		return val, nil
	}
	return paskalValue{}, fmt.Errorf("unknown identifier %s", name)
}

func (c *Compiler) compileIntegerExpr(expr ast.IntegerExpression) (paskalValue, error) {
	return paskalValue{
		TypeValue: types.NewVariable("", types.IntegerType),
		Value:     llvm.ConstInt(types.IntegerType.LLVMType(), uint64(expr), true),
	}, nil
}

func (c *Compiler) compileBooleanExpr(expr ast.BooleanExpression) (paskalValue, error) {
	var val uint64
	if expr {
		val = 1
	}
	return paskalValue{
		TypeValue: types.NewVariable("", types.BooleanType),
		Value:     llvm.ConstInt(types.BooleanType.LLVMType(), val, true),
	}, nil
}

func (c *Compiler) compileRealExpr(expr ast.RealExpression) (paskalValue, error) {
	return paskalValue{
		TypeValue: types.NewVariable("", types.RealType),
		Value:     llvm.ConstFloat(types.RealType.LLVMType(), float64(expr)),
	}, nil
}

func (c *Compiler) compileStringExpr(expr ast.StringExpression) (paskalValue, error) {
	val := c.builder.CreateGlobalString(string(expr), "str")
	val.SetLinkage(llvm.PrivateLinkage)
	arrType := &types.Array{
		Base:         types.CharacterType,
		ElementCount: len(expr),
	}
	return paskalValue{
		TypeValue: types.NewVariable("", arrType).Ref(),
		Value:     val,
	}, nil
}

func (c *Compiler) compileCharacterExpr(expr ast.CharacterExpression) (paskalValue, error) {
	// Truncate rune into a byte
	// TODO: Should be catched earlier in the parsing if rune literal doesn't fit into byte
	return paskalValue{
		TypeValue: types.NewVariable("", types.CharacterType),
		Value:     llvm.ConstInt(types.CharacterType.LLVMType(), uint64(byte(expr)), true),
	}, nil
}
