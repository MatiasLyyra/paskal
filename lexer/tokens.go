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
	String
	StringConstant
	Integer
	IntegerConstant
	Real
	RealConstant
	Boolean
	BooleanConstant
	Character
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

type Token struct {
	Kind    Kind
	Content string
}

func (t Token) String() string {
	return fmt.Sprintf("(%s %s)", t.Kind, t.Content)
}
