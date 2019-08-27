package main

import (
	"bytes"
	"fmt"

	"github.com/MatiasLyyra/paskal/compiler"
	"github.com/MatiasLyyra/paskal/lexer"
	"github.com/MatiasLyyra/paskal/parser"
	"github.com/llvm/llvm-project/llvm/bindings/go/llvm"
)

var code = `
program main
var
PI : InTEger
isPaskal : Boolean

function factorial : integer
BEGIN
	if true then begin
		factorial := 1
	end else begin
		factorial := 1
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
	c := compiler.NewCompiler(mod, false)
	err = c.Compile()
	if err != nil {
		fmt.Printf("Compilation error: %s\n", err)
	}
	llvm.NewTargetData()
	c.Module.Dump()
	engine, err := llvm.NewInterpreter(c.Module)
	if err != nil {
		fmt.Println(err.Error())
	}

	// run the function!
	funcResult := engine.RunFunction(c.Module.NamedFunction("factorial"), []llvm.GenericValue{})
	fmt.Printf("%d\n", int32(funcResult.Int(true)))
}
