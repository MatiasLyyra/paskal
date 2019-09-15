package compiler_test

import (
	"bytes"
	"testing"

	"llvm.org/llvm/bindings/go/llvm"

	"github.com/MatiasLyyra/paskal/compiler"
	"github.com/MatiasLyyra/paskal/lexer"
	"github.com/MatiasLyyra/paskal/parser"

	"github.com/MatiasLyyra/paskal/types"
)

var testsCases = []struct {
	desc       string
	code       string
	result     string
	resultType types.Type
}{
	{
		desc: "Integer return works",
		code: `
program test

function testFunc() : integer
begin
		testFunc := 42
end
`,
		result: "42",
	},
}

func TestIntegerReturns(t *testing.T) {
	testCases := []struct {
		desc     string
		code     string
		result   int32
		function string
	}{
		{
			desc: "Basic return works",
			code: `
program test

function testFunc() : integer
begin
	testFunc := 42
end
`,
			function: "testfunc",
			result:   42,
		},
		{
			desc: "Return with if statement",
			code: `
program test2

function testFunc() : integer
var test : boolean
begin
	test := false
	if test then
		testFunc := 123
	else
		testFunc := 321
end
`,
			function: "testfunc",
			result:   321,
		},
		{
			desc: "Return works with integer arrays",
			code: `
program test3
var arr : array[2] of integer
function testFunc() : integer
var temp : ^integer
begin
	temp := @arr[0]
	^temp := 123
	arr[1] := 321
	testFunc := arr[0] + arr[1]
end
`,
			function: "testfunc",
			result:   444,
		},
		{
			desc: "Function call works with integer return",
			code: `
program test3
function anotherFunc : integer
begin
	anotherFunc := 6
end
function testFunc() : integer
begin
	testFunc := 10 - anotherFunc()
end
`,
			function: "testfunc",
			result:   4,
		},
		{
			desc: "Procedure can be used to modify value",
			code: `
program test3
var value : integer
procedure mutate
begin
	value := 6
end
function testFunc() : integer
begin
	value := 0
	mutate()
	testFunc := value
end
`,
			function: "testfunc",
			result:   6,
		},
	}
	llvm.InitializeNativeTarget()
	for _, tC := range testCases {
		t.Run(tC.desc, func(t *testing.T) {
			tokens, err := lexer.Tokenize(bytes.NewBufferString(tC.code))
			if err != nil {
				t.Fatal(err)
			}
			module, err := parser.Module(tokens)
			if err != nil {
				t.Fatal(err)
			}
			c := compiler.NewCompiler(module, 0, 0)
			defer c.Dispose()
			err = c.Compile()
			if err != nil {
				t.Fatal(err)
			}
			val, err := c.Run(tC.function, []llvm.GenericValue{})
			defer val.Dispose()
			if err != nil {
				t.Fatal(err)
			}
			testResult := int32(val.Int(true))
			if testResult != tC.result {
				t.Fatalf("expected function %s to return %d but got %d", tC.function, tC.result, testResult)
			}
		})
	}
}
