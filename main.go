package main

import (
	"bytes"
	"fmt"

	"github.com/MatiasLyyra/paskal/lexer"
)

var code = `
readln (a, b, c);
s := (a + b + c)/2.0;
area := sqrt(s * (s - a)*(s-b)*(s-c));
writeln(area);   
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
	fmt.Printf("Tokens: %s\n", tokens)
}
