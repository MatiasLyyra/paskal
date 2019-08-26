package main

import (
	"bytes"
	"fmt"

	"github.com/MatiasLyyra/paskal/compiler"
	"github.com/MatiasLyyra/paskal/lexer"
	"github.com/MatiasLyyra/paskal/parser"
)

var code = `
program main
var
PI : InTEger
isPaskal : Boolean

function factorial(x : integer) : integer
BEGIN
	if x is 0 then begin
		factorial := 0
	end else begin
		factorial := x * factorial(x - 1)
	end
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
	c := compiler.NewCompiler(mod, true)
	err = c.Compile()
	if err != nil {
		fmt.Printf("Compilation error: %s\n", err)
	}
	c.Module.Dump()
	// ctx := ast.NewContext()
	// goon.Dump(mod.Compile(ctx, true))
	// ctx.Module.Dump()
	// engine, err := llvm.NewExecutionEngine(ctx.Module)
	// if err != nil {
	// 	fmt.Println(err.Error())
	// }

	// // run the function!
	// funcResult := engine.RunFunction(ctx.Module.NamedFunction("test"), []llvm.GenericValue{})
	// fmt.Printf("%f\n", funcResult.Float(llvm.FloatType()))
}
