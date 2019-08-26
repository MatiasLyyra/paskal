package main

import (
	"bytes"
	"fmt"

	"github.com/MatiasLyyra/paskal/ast"
	"github.com/MatiasLyyra/paskal/lexer"
	"github.com/MatiasLyyra/paskal/parser"
	"github.com/shurcooL/go-goon"
)

var code = `
program main
var
PI : Integer

function test : integer
BEGIN
	PI := 123
	test := ~1
END

BEGIN
	test
END
`

func main() {
	execute(code)
}
func execute(code string) {
	tokens, err := lexer.Tokenize(bytes.NewBufferString(code))
	if err != nil {
		fmt.Printf("ERROR: %s\n", err)
		return
	}
	mod, err := parser.Module(tokens)
	if err != nil {
		fmt.Printf("Parse error: %s\n", err)
		return
	}
	ctx := ast.NewContext()
	goon.Dump(mod.Compile(ctx, true))
	ctx.Module.Dump()
	// engine, err := llvm.NewExecutionEngine(ctx.Module)
	// if err != nil {
	// 	fmt.Println(err.Error())
	// }

	// // run the function!
	// funcResult := engine.RunFunction(ctx.Module.NamedFunction("test"), []llvm.GenericValue{})
	// fmt.Printf("%f\n", funcResult.Float(llvm.FloatType()))
}
