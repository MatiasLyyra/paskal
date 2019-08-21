package parser

import (
	"fmt"

	"github.com/MatiasLyyra/paskal/ast"
	"github.com/MatiasLyyra/paskal/lexer"
)

func Module(tokens []lexer.Token) (module *ast.Module, err error) {
	p := &parser{
		tokens: tokens,
	}
	_, err = p.require(lexer.Program)
	if err != nil {
		return
	}
	moduleToken, err := p.require(lexer.Identifier)
	if err != nil {
		return
	}
	module = ast.NewModule(moduleToken.Content)
	err = p.variableDeclarations(module)
	if err != nil {
		return
	}
	err = p.functionSection(module)
	if err != nil {
		return
	}
	_, err = p.require(lexer.EOF)
	return
}

func (p *parser) functionSection(module *ast.Module) error {
	for p.accept(lexer.Function) || p.accept(lexer.Procedure) {
		hasReturn := p.consume().Content == "function"
		funcNameTok, err := p.require(lexer.Identifier)
		if err != nil {
			return err
		}
		function := ast.NewFunction(funcNameTok.Content, hasReturn)
		if _, found := p.consumeIfAccept(lexer.LParen); found {
			for !p.accept(lexer.RParen) {
				ids, typeName, err := p.variableList()
				if err != nil {
					return err
				}
				for _, id := range ids {
					err := function.AddParam(id, typeName)
					if err != nil {
						return err
					}
				}
			}
			if _, err := p.require(lexer.RParen); err != nil {
				return err
			}
		}
		if hasReturn {
			if _, err := p.require(lexer.Colon); err != nil {
				return err
			}
			if !p.accept(lexer.Identifier) && !p.accept(lexer.PrimitiveType) {
				return fmt.Errorf("function %s requires return value type", function.Name)
			}
		} else {
			function.Return = ast.Void
		}
		err = p.variableDeclarations(function)
		if err != nil {
			return err
		}
		err = module.AddFunc(function)
		if err != nil {
			return err
		}
	}
	return nil
}

func (p *parser) variableDeclarations(vc ast.VarContainer) error {
	for p.accept(lexer.Var) || p.accept(lexer.Const) {
		var (
			varFound   bool
			constFound bool
		)
		if p.accept(lexer.Var) {
			varFound = true
		} else if p.accept(lexer.Const) {
			constFound = true
		} else {
			panic("internal error: unknown token")
		}
		p.consume()
		ids, typeName, err := p.variableList()
		if err != nil {
			return err
		}
		for _, id := range ids {
			var err error
			if varFound {
				err = vc.AddVar(id, typeName)
			} else if constFound {
				err = vc.AddConst(id, typeName)
			}
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func (p *parser) variableList() (ids []string, typeName string, err error) {
	if p.accept(lexer.Identifier) {
		ids = append(ids, p.consume().Content)
		for !p.accept(lexer.Colon) {
			if !p.accept(lexer.Identifier) {
				break
			}
			ids = append(ids, p.consume().Content)
		}
		if _, err = p.require(lexer.Colon); err != nil {
			return
		}
		if !p.accept(lexer.PrimitiveType) && !p.accept(lexer.Identifier) {
			err = fmt.Errorf("expected type found %s", p.peek())
			return
		}
		typeName = p.consume().Content
	}
	return
}
