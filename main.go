package main

import (
	"bytes"
	"fmt"

	"github.com/MatiasLyyra/paskal/lexer"
	"github.com/MatiasLyyra/paskal/parser"
)

var code = `
program main
var
foo, bar : integer
const PI : REAL

procedure baz(a b z: integer d : boolean pi : real);
var
x : boolean;
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
	fmt.Println(mod)
}
