package parser

import (
	"fmt"
	"strconv"

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
	if p.accept(lexer.Begin) {
		main, err := p.blockStatement()
		if err != nil {
			return nil, err
		}
		module.Main = main
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
			function.SetReturn(p.consume().Content)
		} else {
			function.Return = ast.Void
		}
		err = p.variableDeclarations(function)
		if err != nil {
			return err
		}
		body, err := p.blockStatement()
		if err != nil {
			return err
		}
		if body == nil {
			return fmt.Errorf("empty function body on function %s", function.Name)
		}
		function.Body = body
		err = module.AddFunc(function)
		if err != nil {
			return err
		}
	}
	return nil
}

func (p *parser) blockStatement() (ast.Node, error) {
	_, blockStartFound := p.consumeIfAccept(lexer.Begin)
	var value ast.Node
	if blockStartFound {
		block := &ast.Block{}
		for !p.accept(lexer.End) {
			stmt, err := p.controlStatement()
			if err != nil {
				return nil, err
			}
			block.Statements = append(block.Statements, stmt)
		}
		p.consume()
		value = block
	} else {
		var err error
		value, err = p.Expression()
		if err != nil {
			return nil, err
		}
	}
	return value, nil
}

func (p *parser) controlStatement() (ast.Node, error) {
	if p.accept(lexer.If) {
		return p.ifStatement()
	}
	return p.Expression()
}

func (p *parser) ifStatement() (ast.Node, error) {
	var (
		trueBlock  ast.Node
		falseBlock ast.Node
	)
	_, err := p.require(lexer.If)
	if err != nil {
		return nil, err
	}
	condExpr, err := p.Expression()
	if err != nil {
		return nil, err
	}
	_, err = p.require(lexer.Then)
	if err != nil {
		return nil, err
	}
	trueBlock, err = p.blockStatement()
	if err != nil {
		return nil, err
	}
	if p.accept(lexer.Else) {
		p.consume()
		var err error
		if p.accept(lexer.If) {
			falseBlock, err = p.ifStatement()
		} else {
			falseBlock, err = p.blockStatement()
		}
		if err != nil {
			return nil, err
		}
	}
	return &ast.IfStatement{
		Condition:  condExpr,
		TrueBlock:  trueBlock,
		FalseBlock: falseBlock,
	}, nil
}

func (p *parser) Expression() (ast.Node, error) {
	lhs, err := p.unaryExpression()
	if err != nil {
		return nil, err
	}
	return p.binaryExpression(lhs, 0)
}
func (p *parser) binaryExpression(lhs ast.Node, min int) (ast.Node, error) {
	opPrec, _ := opPrecedence[p.peek().Kind]
	for p.accept(binaryOps...) && opPrec.priority >= min {
		op := p.peek()
		p.consume()
		opPrec = opPrecedence[op.Kind]
		rhs, err := p.unaryExpression()
		if err != nil {
			return nil, err
		}
		nextPrec, _ := opPrecedence[p.peek().Kind]
		for p.accept(binaryOps...) {
			greater := (nextPrec.priority > opPrec.priority) || (nextPrec.rightAssoc && (nextPrec.priority == opPrec.priority))
			if !greater {
				break
			}
			rhs, err = p.binaryExpression(rhs, nextPrec.priority)
			if err != nil {
				return nil, err
			}
			nextPrec, _ = opPrecedence[p.peek().Kind]
		}
		lhs = &ast.BinaryExpression{
			LHS: lhs,
			RHS: rhs,
			Op:  op.Content,
		}
	}
	return lhs, nil
}

func (p *parser) unaryExpression() (ast.Node, error) {
	var (
		value ast.Node
		err   error
	)
	if p.accept(lexer.LParen) {
		p.consume()
		value, err = p.Expression()
		if err != nil {
			return nil, err
		}
		_, err = p.require(lexer.RParen)
		if err != nil {
			return nil, err
		}
	} else if p.accept(unaryOps...) {
		unOp := p.consume()
		value, err = p.Expression()
		if err != nil {
			return nil, err
		}
		value = &ast.UnaryExpression{
			Op:    unOp.Content,
			Value: value,
		}
	} else if p.accept(lexer.IntegerConstant) {
		tok := p.consume()
		tmp, _ := strconv.ParseInt(tok.Content, 10, 32)
		value = ast.IntegerExpression(tmp)
	} else if p.accept(lexer.RealConstant) {
		tok := p.consume()
		tmp, _ := strconv.ParseFloat(tok.Content, 32)
		value = ast.RealExpression(tmp)
	} else if p.accept(lexer.CharacterConstant) {
		tok := p.consume()
		value = ast.CharacterExpression([]rune(tok.Content)[0])
	} else if p.accept(lexer.BooleanConstant) {
		tok := p.consume()
		switch tok.Content {
		case "true":
			value = ast.BooleanExpression(true)
		case "false":
			value = ast.BooleanExpression(false)
		default:
			return nil, fmt.Errorf("invalid boolean value %s", tok.Content)
		}
	} else if p.accept(lexer.StringConstant) {
		tok := p.consume()
		value = ast.StringExpression(tok.Content)
	} else if p.accept(lexer.Identifier) {
		tok := p.consume()
		value = ast.IdentifierExpression(tok.Content)
		if p.accept(lexer.LParen) {
			p.consume()
			var arguments []ast.Node
			for !p.accept(lexer.RParen) {
				arg, err := p.Expression()
				if err != nil {
					return nil, err
				}
				arguments = append(arguments, arg)
			}
			p.consume()
			value = &ast.FunctionCall{
				Name: tok.Content,
				Args: arguments,
			}
		}
	} else if p.accept(unaryOps...) {
		op := p.consume().Content
		value, err = p.Expression()
		if err != nil {
			return nil, err
		}
		value = &ast.UnaryExpression{
			Op:    op,
			Value: value,
		}
	} else {
		return nil, fmt.Errorf("expected value got %s", p.peek().Content)
	}
	return value, nil
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
		for {
			ids, typeName, err := p.variableList()
			if err != nil {
				return err
			}
			if len(ids) == 0 {
				break
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
