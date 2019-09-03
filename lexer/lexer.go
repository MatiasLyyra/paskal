package lexer

import (
	"bytes"
	"fmt"
	"io"
	"strings"
	"text/scanner"
	"unicode"
)

type lexer struct {
	scanner scanner.Scanner
	buf     *bytes.Buffer
	current rune
}

func (lex *lexer) skipWhiteSpace() {
	for unicode.IsSpace(lex.current) {
		lex.advance(false)
	}
}

func (lex *lexer) bufferString() string {
	return strings.ToLower(lex.buf.String())
}

func (lex *lexer) advance(save bool) {
	if lex.current != scanner.EOF {
		char := lex.scanner.Next()
		if save {
			lex.buf.WriteRune(lex.current)
		}
		lex.current = char
	}
}

func (lex *lexer) isDigit() bool {
	return unicode.IsDigit(lex.current)
}

func (lex *lexer) isLetter() bool {
	return unicode.IsLetter(lex.current)
}

func (lex *lexer) isOperator() bool {
	return lex.current == '|' ||
		lex.current == '&' ||
		lex.current == '~' ||
		lex.current == '*' ||
		lex.current == '%' ||
		lex.current == '/' ||
		lex.current == '+' ||
		lex.current == '-' ||
		lex.current == ':' ||
		lex.current == '=' ||
		lex.current == '(' ||
		lex.current == ')' ||
		lex.current == '[' ||
		lex.current == ']' ||
		lex.current == ';' ||
		lex.current == '.' ||
		lex.current == '^' ||
		lex.current == '<' ||
		lex.current == '>' ||
		lex.current == '@' ||
		lex.current == ','
}

func (lex *lexer) scanDigit() Token {
	for lex.isDigit() {
		lex.advance(true)
	}
	if lex.current == '.' {
		lex.advance(true)
		for lex.isDigit() {
			lex.advance(true)
		}
		return lex.makeToken(RealConstant)
	}
	return lex.makeToken(IntegerConstant)
}

func (lex *lexer) scanIdentifier() Token {
	for lex.isLetter() || lex.isDigit() || lex.current == '_' {
		lex.advance(true)
	}
	switch lex.bufferString() {
	case "program":
		return lex.makeToken(Program)
	case "uses":
		return lex.makeToken(Uses)
	case "const":
		return lex.makeToken(Const)
	case "var":
		return lex.makeToken(Var)
	case "procedure":
		return lex.makeToken(Procedure)
	case "function":
		return lex.makeToken(Function)
	case "begin":
		return lex.makeToken(Begin)
	case "end":
		return lex.makeToken(End)
	case "if":
		return lex.makeToken(If)
	case "then":
		return lex.makeToken(Then)
	case "else":
		return lex.makeToken(Else)
	case "goto":
		return lex.makeToken(Goto)
	case "result":
		return lex.makeToken(Result)
	case "exit":
		return lex.makeToken(Exit)
	case "string", "integer", "real", "boolean", "character":
		return lex.makeToken(PrimitiveType)
	// Operators
	case "and":
		return lex.makeToken(LAnd)
	case "or":
		return lex.makeToken(LOr)
	case "not":
		return lex.makeToken(LNot)
	case "is":
		return lex.makeToken(Eq)
	// Types
	case "array":
		return lex.makeToken(Array)
	case "of":
		return lex.makeToken(Of)
	case "true", "false":
		return lex.makeToken(BooleanConstant)
	}
	return lex.makeToken(Identifier)
}

func (lex *lexer) skipMultiComment() error {
	lex.advance(false)
	for lex.current != '}' && lex.current != scanner.EOF {
		lex.advance(false)
	}
	if lex.current == scanner.EOF {
		return fmt.Errorf("expectng } found EOF")
	}
	lex.advance(false)
	return nil
}

func (lex *lexer) skipToNewLine() {
	for lex.current != '\n' &&
		lex.current != scanner.EOF {
		lex.advance(false)
	}
}

func (lex *lexer) scanStringConstant() (Token, error) {
	lex.advance(false)
	for lex.current != '"' {
		if lex.current == '\n' {
			return lex.makeToken(Invalid),
				fmt.Errorf("missing \"")
		}
		lex.advance(true)
	}
	lex.advance(false)
	return lex.makeToken(StringConstant), nil
}

func (lex *lexer) scanCharacterConstant() (Token, error) {
	lex.advance(false)
	for lex.current != '\'' {
		if lex.current == '\n' {
			return lex.makeToken(Invalid),
				fmt.Errorf("missing '")
		}
		lex.advance(true)
	}
	lex.advance(false)
	return lex.makeToken(CharacterConstant), nil
}

func (lex *lexer) makeToken(kind Kind) Token {
	token := Token{
		Content: lex.buf.String(),
		Kind:    kind,
	}
	if kind != CharacterConstant && kind != StringConstant {
		token.Content = strings.ToLower(token.Content)
	}
	lex.buf.Reset()
	return token
}

func (lex *lexer) scanSpecial() Token {
	var kind Kind
	needsSave := true
	switch lex.current {
	case '*':
		kind = Mul
	case '/':
		lex.advance(true)
		needsSave = false
		if lex.current == '/' {
			lex.buf.Reset()
			lex.skipToNewLine()
			return lex.makeToken(Comment)
		}
		kind = Div
	case '%':
		kind = Mod
	case '+':
		kind = Add
	case '-':
		kind = Sub
	case '|':
		kind = Or
	case '&':
		kind = And
	case '=':
		kind = Assignment
		if lex.current == '=' {
			kind = Eq
			lex.advance(true)
		}
	case '!':
		kind = LNot
		if lex.current == '=' {
			kind = Ne
			lex.advance(true)
		}
	case '<':
		kind = Lt
		if lex.current == '=' {
			kind = Lte
			lex.advance(true)
		}
	case '>':
		kind = Gt
		if lex.current == '=' {
			kind = Gte
			lex.advance(true)
		}
	case '~':
		kind = Not
	case '^':
		kind = Deref
	case '@':
		kind = AddressOf
	case ':':
		lex.advance(true)
		kind = Colon
		needsSave = false
		if lex.current == '=' {
			kind = Assignment
			lex.advance(true)
		}
	case '(':
		kind = LParen
	case ')':
		kind = RParen
	case '[':
		kind = LBracket
	case ']':
		kind = RBracket
	case ';':
		kind = SemiColon
	case '.':
		kind = FullStop
	case ',':
		kind = Comma
	}
	if needsSave {
		lex.advance(true)
	}
	return lex.makeToken(kind)
}

func (lex *lexer) next() (Token, error) {
	lex.skipWhiteSpace()
	if lex.current == scanner.EOF {
		lex.advance(true)
		return lex.makeToken(EOF), nil
	}
	if lex.current == '{' {
		err := lex.skipMultiComment()
		return lex.makeToken(Comment), err
	}
	if lex.isDigit() {
		return lex.scanDigit(), nil
	}
	if lex.isLetter() {
		return lex.scanIdentifier(), nil
	}
	if lex.isOperator() {
		return lex.scanSpecial(), nil
	}
	if lex.current == '"' {
		return lex.scanStringConstant()
	}
	if lex.current == '\'' {
		return lex.scanCharacterConstant()
	}
	return Token{},
		fmt.Errorf("invalid token %s", lex.buf.String())
}

func Tokenize(r io.Reader) ([]Token, error) {
	tokens := make([]Token, 0)
	lex := &lexer{
		buf: bytes.NewBufferString(""),
	}
	lex.scanner.Init(r)
	var (
		err error
		t   Token
	)
	lex.advance(false)
	for lex.current != scanner.EOF {
		t, err = lex.next()
		if err != nil {
			return tokens, err
		}
		// Throw away comments, semicolons and commas
		// No need for them!
		isJunk := t.Kind == Comment ||
			t.Kind == SemiColon ||
			t.Kind == Comma
		if !isJunk {
			tokens = append(tokens, t)
		}
	}
	return tokens, err
}
