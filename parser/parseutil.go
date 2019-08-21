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

func (p *parser) accept(kind lexer.Kind) bool {
	return p.peek().Kind == kind
}

func (p *parser) consumeIfAccept(kind lexer.Kind) (lexer.Token, bool) {
	if p.accept(kind) {
		return p.consume(), true
	}
	return lexer.Token{}, false
}

func (p *parser) require(kind lexer.Kind) (lexer.Token, error) {
	if p.peek().Kind == kind {
		return p.consume(), nil
	}
	return lexer.Token{}, fmt.Errorf("expected token %s found %s", kind, p.peek().Kind)
}
