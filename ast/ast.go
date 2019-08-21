package ast

import "fmt"

type VariableType int

const (
	Integer VariableType = iota
	Character
	Boolean
	String
	Real
	UserDefined
	Void
)

type Variable struct {
	Name    string
	VarType VariableType
}

func stringToVariableType(name string) VariableType {
	switch name {
	case "integer":
		return Integer
	case "character":
		return Character
	case "boolean":
		return Boolean
	case "string":
		return String
	case "real":
		return Real
	default:
		return UserDefined
	}
}

type empty struct{}

type Identifier struct {
	Name string
	Type VariableType
}

type Module struct {
	Name        string
	vars        []Variable
	consts      []Variable
	funcs       []*Function
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
	m.funcs = append(m.funcs, function)
	m.funcLookup[function.Name] = empty{}
	return nil
}
func (m *Module) AddVar(name, varType string) error {
	if m.NameExists(name) {
		return fmt.Errorf("variable or constant with name %s is already defined", name)
	}
	m.vars = append(m.vars,
		Variable{
			Name:    name,
			VarType: stringToVariableType(varType),
		},
	)
	m.varLookup[name] = empty{}
	return nil
}
func (m *Module) AddConst(name, constType string) error {
	if m.NameExists(name) {
		return fmt.Errorf("variable or constant with name %s is already defined", name)
	}
	t := stringToVariableType(constType)
	if t == UserDefined {
		return fmt.Errorf("invalid const type %s on constant %s", constType, name)
	}
	m.consts = append(m.consts,
		Variable{
			Name:    name,
			VarType: stringToVariableType(constType),
		},
	)
	m.constLookup[name] = empty{}
	return nil
}

type Function struct {
	Name        string
	hasReturn   bool
	Return      VariableType
	vars        []Variable
	params      []Variable
	varLookup   map[string]empty
	paramLookup map[string]empty
}

func NewFunction(name string, hasReturn bool) *Function {
	return &Function{
		Name:        name,
		hasReturn:   hasReturn,
		varLookup:   make(map[string]empty),
		paramLookup: make(map[string]empty),
	}
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
	f.vars = append(f.vars,
		Variable{
			Name:    name,
			VarType: stringToVariableType(varType),
		},
	)
	f.varLookup[name] = empty{}
	return nil
}

func (f *Function) AddParam(name, paramType string) error {
	if f.NameExists(name) {
		return fmt.Errorf("variable or constant with name %s is already defined", name)
	}
	f.params = append(f.params,
		Variable{
			Name:    name,
			VarType: stringToVariableType(paramType),
		},
	)
	f.paramLookup[name] = empty{}
	return nil
}

func (f *Function) AddConst(name, constType string) error {
	return fmt.Errorf("invalid const value in function / procedure")
}
