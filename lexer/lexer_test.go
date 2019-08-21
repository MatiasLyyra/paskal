package lexer_test

import (
	"bytes"
	"testing"

	"github.com/MatiasLyyra/paskal/lexer"
)

func TestLexer(t *testing.T) {
	testCases := []struct {
		desc     string
		code     string
		expected []lexer.Kind
	}{
		{
			desc: "Basic lexer test",
			code: `program foobar;
uses foo, bar;
var
val : integer;
pi : real;
`,
			expected: []lexer.Kind{
				lexer.Program, lexer.Identifier, lexer.SemiColon,
				lexer.Uses, lexer.Identifier, lexer.Comma, lexer.Identifier, lexer.SemiColon,
				lexer.Var,
				lexer.Identifier, lexer.Colon, lexer.Integer, lexer.SemiColon,
				lexer.Identifier, lexer.Colon, lexer.Real, lexer.SemiColon,
				lexer.EOF,
			},
		},
		{
			desc: "Comments are ignored",
			code: `// program foobar;
{
uses foo, bar;
var
val : integer;
pi : real;
}
{* integer *}
{*program*}
// {*Test*} uses program
`,
			expected: []lexer.Kind{lexer.EOF},
		},
		{
			desc: "Function",
			code: `
function myLine(x: real): real;
begin
	myLine := 0.5 * x + 2;
end;
`,
			expected: []lexer.Kind{
				lexer.Function, lexer.Identifier, lexer.LParen, lexer.Identifier, lexer.Colon, lexer.Real, lexer.RParen, lexer.Colon, lexer.Real, lexer.SemiColon,
				lexer.Begin,
				lexer.Identifier, lexer.Assignment, lexer.RealConstant, lexer.Mul, lexer.Identifier, lexer.Add, lexer.IntegerConstant, lexer.SemiColon,
				lexer.End, lexer.SemiColon,
				lexer.EOF,
			},
		},
		{
			desc: "Procedure",
			code: `
procedure foo;
var
x : boolean;
begin
	x := false or true;
	if x then begin
			x := x and false;
			exit;
	end;
	else begin
			foo("x is x", 'f');
	end;
end;
			`,
			expected: []lexer.Kind{
				lexer.Procedure, lexer.Identifier, lexer.SemiColon,
				lexer.Var,
				lexer.Identifier, lexer.Colon, lexer.Boolean, lexer.SemiColon,
				lexer.Begin,
				lexer.Identifier, lexer.Assignment, lexer.BooleanConstant, lexer.LOr, lexer.BooleanConstant, lexer.SemiColon,
				lexer.If, lexer.Identifier, lexer.Then, lexer.Begin,
				lexer.Identifier, lexer.Assignment, lexer.Identifier, lexer.LAnd, lexer.BooleanConstant, lexer.SemiColon,
				lexer.Exit, lexer.SemiColon,
				lexer.End, lexer.SemiColon,
				lexer.Else, lexer.Begin,
				lexer.Identifier, lexer.LParen, lexer.StringConstant, lexer.Comma, lexer.CharacterConstant, lexer.RParen, lexer.SemiColon,
				lexer.End, lexer.SemiColon,
				lexer.End, lexer.SemiColon,
				lexer.EOF,
			},
		},
	}
	for _, tC := range testCases {
		t.Run(tC.desc, func(t *testing.T) {
			tokens, err := lexer.Tokenize(bytes.NewBufferString(tC.code))
			if err != nil {
				t.Errorf("unexpected error: %s", err)
			}
			if len(tC.expected) != len(tokens) {
				t.Fatalf("expected %d tokens got %d", len(tC.expected), len(tokens))
			}
			for i, token := range tokens {
				if token.Kind != tC.expected[i] {
					t.Errorf("expected token '%s' got '%s'", tC.expected[i], token.Kind)
				}
			}
		})
	}
}
