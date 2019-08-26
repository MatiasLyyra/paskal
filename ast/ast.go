package ast

import (
	"fmt"

	"github.com/MatiasLyyra/paskal/types"

	"github.com/MatiasLyyra/paskal/lexer"
)

type empty struct{}

type Node interface {
	// Codegen(ctx *Context) (llvm.Value, error)
}

type Module struct {
	Name        string
	Main        Node
	Vars        []types.Value
	Consts      []types.Value
	Funcs       []*Function
	varLookup   map[string]empty
	constLookup map[string]empty
	funcLookup  map[string]empty
}

type VarContainer interface {
	NameExists(name string) bool
	AddVar(name, varType string) error
	AddConst(name, constType string) error
}

func NewModule(name string) *Module {
	return &Module{
		Name:        name,
		varLookup:   make(map[string]empty),
		constLookup: make(map[string]empty),
		funcLookup:  make(map[string]empty),
	}
}
func (m *Module) NameExists(name string) bool {
	_, foundVar := m.varLookup[name]
	_, foundConst := m.constLookup[name]
	return foundVar || foundConst
}
func (m *Module) AddFunc(function *Function) error {
	_, funcExists := m.funcLookup[function.Name]
	if m.NameExists(function.Name) || funcExists {
		return fmt.Errorf("variable, constant or function with name %s is already defined", function.Name)
	}
	m.Funcs = append(m.Funcs, function)
	m.funcLookup[function.Name] = empty{}
	return nil
}
func (m *Module) AddVar(name, varType string) error {
	if m.NameExists(name) {
		return fmt.Errorf("variable or constant with name %s is already defined", name)
	}
	m.Vars = append(m.Vars, types.NewVariable(name, types.BasicTypeFromString(varType)))
	m.varLookup[name] = empty{}
	return nil
}
func (m *Module) AddConst(name, constType string) error {
	if m.NameExists(name) {
		return fmt.Errorf("variable or constant with name %s is already defined", name)
	}
	m.Consts = append(m.Consts, types.NewConst(name, types.BasicTypeFromString(constType)))
	m.constLookup[name] = empty{}
	return nil
}

type Function struct {
	Name        string
	hasReturn   bool
	Return      types.Type
	Vars        []types.Value
	Params      []types.Value
	varLookup   map[string]empty
	paramLookup map[string]empty
	Body        Block
}

func NewFunction(name string, hasReturn bool) *Function {
	return &Function{
		Name:        name,
		hasReturn:   hasReturn,
		varLookup:   make(map[string]empty),
		paramLookup: make(map[string]empty),
	}
}

type FunctionCall struct {
	Name string
	Args []Node
}

func (f *Function) NameExists(name string) bool {
	_, foundVar := f.varLookup[name]
	_, foundParam := f.paramLookup[name]
	var foundReturnVar bool
	if f.hasReturn {
		foundReturnVar = f.Name == name
	}
	return foundVar || foundParam || foundReturnVar
}

func (f *Function) AddVar(name, varType string) error {
	if f.NameExists(name) {
		return fmt.Errorf("variable or constant with name %s is already defined", name)
	}
	f.Vars = append(f.Vars, types.NewVariable(name, types.BasicTypeFromString(varType)))
	f.varLookup[name] = empty{}
	return nil
}

func (f *Function) SetReturn(retType string) {
	f.Return = types.BasicTypeFromString(retType)
}

func (f *Function) AddParam(name, paramType string) error {
	if f.NameExists(name) {
		return fmt.Errorf("variable or constant with name %s is already defined", name)
	}
	f.Params = append(f.Params, types.NewVariable(name, types.BasicTypeFromString(paramType)))
	f.paramLookup[name] = empty{}
	return nil
}

func (f *Function) AddConst(name, constType string) error {
	return fmt.Errorf("invalid const value in function / procedure")
}

type Block struct {
	Statements []Node
}

type IfStatement struct {
	Condition  Node
	TrueBlock  Block
	FalseBlock Node
}

type BinaryExpression struct {
	Op  lexer.Kind
	LHS Node
	RHS Node
}

type UnaryExpression struct {
	Op    lexer.Kind
	Value Node
}

type ParenExpression struct {
	Value Node
}

type IntegerExpression int
type RealExpression float32
type StringExpression string
type BooleanExpression bool
type CharacterExpression rune
type IdentifierExpression string
