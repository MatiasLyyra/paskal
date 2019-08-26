package parser

import (
	"fmt"

	"github.com/MatiasLyyra/paskal/lexer"
)

type parser struct {
	tokens []lexer.Token
	idx    int
}

func (p *parser) peek() lexer.Token {
	if p.idx >= len(p.tokens) {
		return lexer.Token{Kind: lexer.EOF}
	}
	return p.tokens[p.idx]
}
func (p *parser) peekAt(offet int) lexer.Token {
	if offet+p.idx < len(p.tokens) {
		return p.tokens[offet+p.idx]
	}
	return lexer.Token{Kind: lexer.EOF}
}
func (p *parser) consume() lexer.Token {
	prevToken := p.peek()
	p.idx++
	return prevToken
}

func (p *parser) accept(kinds ...lexer.Kind) bool {
	for _, kind := range kinds {
		if p.peek().Kind == kind {
			return true
		}
	}
	return false
}

func (p *parser) is(target lexer.Kind, kinds []lexer.Kind) bool {
	for _, kind := range kinds {
		if kind == target {
			return true
		}
	}
	return false
}

func (p *parser) consumeIfAccept(kind ...lexer.Kind) (lexer.Token, bool) {
	if p.accept(kind...) {
		return p.consume(), true
	}
	return lexer.Token{}, false
}

func (p *parser) require(kind ...lexer.Kind) (lexer.Token, error) {
	if p.accept(kind...) {
		return p.consume(), nil
	}
	return lexer.Token{}, fmt.Errorf("expected token %s found %s", kind, p.peek().Kind)
}

type precedence struct {
	priority   int
	rightAssoc bool
}

var opPrecedence map[lexer.Kind]precedence
var binaryOps []lexer.Kind
var unaryOps []lexer.Kind

func init() {
	opPrecedence = map[lexer.Kind]precedence{
		lexer.Mul: {
			priority: 7,
		},
		lexer.Div: {
			priority: 7,
		},
		lexer.Mod: {
			priority: 7,
		},
		lexer.Add: {
			priority: 6,
		},
		lexer.Sub: {
			priority: 6,
		},
		lexer.Eq: {
			priority: 5,
		},
		lexer.And: {
			priority: 4,
		},
		lexer.Or: {
			priority: 3,
		},
		lexer.LAnd: {
			priority: 2,
		},
		lexer.LOr: {
			priority: 1,
		},
		lexer.Assignment: {
			priority:   0,
			rightAssoc: true,
		},
	}
	for kind := range opPrecedence {
		binaryOps = append(binaryOps, kind)
	}
	unaryOps = []lexer.Kind{
		lexer.Add,
		lexer.Sub,
		lexer.Not,
		lexer.LNot,
	}
}
