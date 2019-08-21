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
