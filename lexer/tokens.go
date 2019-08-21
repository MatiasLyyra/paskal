package lexer

import (
	"fmt"
)

type Kind int
type Category int

const (
	Operator Category = iota
	None
)

const (
	// Keywords
	Program Kind = iota
	Uses
	Const
	Var
	Procedure
	Function
	Begin
	End
	If
	Then
	Else
	Goto
	Result
	Exit
	// Types
	PrimitiveType
	StringConstant
	IntegerConstant
	RealConstant
	BooleanConstant
	CharacterConstant
	Identifier
	// Operators
	Mul
	Div
	Mod
	Add
	Sub
	// Bonary operators
	Or
	And
	Not
	// Logical operators
	LOr
	LAnd
	LNot
	Eq
	// Misc
	Assignment
	LParen
	RParen
	SemiColon
	FullStop
	Colon
	Comma
	EOF
	Comment
	Invalid
)

func (k Kind) String() (s string) {
	switch k {
	case Program:
		s = "program"
	case Uses:
		s = "uses"
	case Const:
		s = "const"
	case Var:
		s = "var"
	case Procedure:
		s = "procedure"
	case Function:
		s = "function"
	case Begin:
		s = "begin"
	case End:
		s = "end"
	case If:
		s = "if"
	case Then:
		s = "then"
	case Else:
		s = "else"
	case Goto:
		s = "goto"
	case Result:
		s = "result"
	case Exit:
		s = "exit"
	case PrimitiveType:
		s = "primitive type"
	case StringConstant:
		s = "string value"
	case IntegerConstant:
		s = "integer value"
	case RealConstant:
		s = "real value"
	case BooleanConstant:
		s = "boolean value"
	case CharacterConstant:
		s = "character value"
	case Identifier:
		s = "identifier"
	case Mul:
		s = "*"
	case Div:
		s = "/"
	case Mod:
		s = "%"
	case Add:
		s = "+"
	case Sub:
		s = "-"
	case Or:
		s = "|"
	case And:
		s = "&"
	case Not:
		s = "~"
	case LOr:
		s = "or"
	case LAnd:
		s = "and"
	case LNot:
		s = "not"
	case Eq:
		s = "is"
	case Assignment:
		s = ":="
	case LParen:
		s = "("
	case RParen:
		s = ")"
	case SemiColon:
		s = ";"
	case FullStop:
		s = "."
	case Colon:
		s = ":"
	case Comma:
		s = ","
	case EOF:
		s = "EOF"
	case Comment:
		s = "comment"
	case Invalid:
		s = "invalid"
	}
	return
}

type Token struct {
	Kind    Kind
	Content string
}

func (t Token) String() string {
	kindStr := t.Kind.String()
	if kindStr == t.Content {
		return fmt.Sprintf("%s", t.Content)
	}
	return fmt.Sprintf("(%s %s)", t.Kind, t.Content)
}
