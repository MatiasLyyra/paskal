package types

import (
	"fmt"

	"github.com/go-llvm/llvm"
)

type BasicKind int

const (
	Integer BasicKind = iota
	Real
	Character
	Boolean
	Void
)

var (
	IntegerType   *BasicType
	RealType      *BasicType
	CharacterType *BasicType
	BooleanType   *BasicType
	VoidType      *BasicType
)

type Pointer struct {
	Base Type
}

type BasicType struct {
	Kind BasicKind
}

type Type interface {
	LLVMType() llvm.Type
	DerefType() Type
	RefType() Type
	IsA(Type) bool
	String() string
}

type Value interface {
	IsConst() bool
	Name() string
	Type() Type
	Ref() Value
	Deref() Value
}

type Variable struct {
	varType Type
	name    string
}

func BasicTypeFromString(typeName string) (bt *BasicType) {
	bt = &BasicType{}
	switch typeName {
	case "integer":
		bt.Kind = Integer
	case "boolean":
		bt.Kind = Boolean
	case "real":
		bt.Kind = Real
	case "character":
		bt.Kind = Character
	default:
		panic(fmt.Sprintf("invalid type %s", typeName))
	}
	return
}

func NewVariable(name string, varType Type) *Variable {
	return &Variable{
		name:    name,
		varType: varType,
	}
}

type Const struct {
	name      string
	constType Type
}

func NewConst(name string, constType Type) *Const {
	return &Const{
		name:      name,
		constType: constType,
	}
}

func (v *Variable) Type() Type    { return v.varType }
func (v *Variable) Name() string  { return v.name }
func (v *Variable) IsConst() bool { return false }
func (v *Variable) Ref() Value {
	return NewVariable(v.name, v.varType.RefType())
}
func (v *Variable) Deref() Value {
	if v.varType.DerefType() == nil {
		return nil
	}
	return NewVariable(v.name, v.varType.DerefType())
}

func (c *Const) Type() Type    { return c.constType }
func (c *Const) Name() string  { return c.name }
func (c *Const) IsConst() bool { return true }
func (c *Const) Ref() Value {
	return NewConst(c.name, c.constType.RefType())
}
func (c *Const) Deref() Value {
	if c.constType.DerefType() == nil {
		return nil
	}
	return NewConst(c.name, c.constType.DerefType())
}

func (t *Pointer) LLVMType() llvm.Type { return llvm.PointerType(t.Base.LLVMType(), 0) }
func (t *Pointer) DerefType() Type     { return t.Base }
func (t *Pointer) RefType() Type       { return &Pointer{Base: t} }
func (t *Pointer) String() string      { return fmt.Sprintf("*%s", t.Base.String()) }
func (t *Pointer) IsA(t2 Type) bool {
	t2Ptr, ok := t2.(*Pointer)
	if !ok {
		return false
	}
	return t.Base.IsA(t2Ptr.Base)
}

func (t *BasicType) IsA(t2 Type) bool {
	t2Bt, ok := t2.(*BasicType)
	if !ok {
		return false
	}
	return t.Kind == t2Bt.Kind
}
func (t *BasicType) LLVMType() llvm.Type {
	switch t.Kind {
	case Integer:
		return llvm.Int32Type()
	case Real:
		return llvm.DoubleType()
	case Character:
		return llvm.Int8Type()
	case Boolean:
		return llvm.Int1Type()
	case Void:
		return llvm.VoidType()
	default:
		panic(fmt.Sprintf("invalid type %s", t.Kind))
	}
}
func (t *BasicType) String() string {
	switch t.Kind {
	case Integer:
		return "integer"
	case Real:
		return "real"
	case Character:
		return "character"
	case Boolean:
		return "boolean"
	case Void:
		return "void"
	default:
		return "unknown"
	}
}
func (t *BasicType) DerefType() Type { return nil }
func (t *BasicType) RefType() Type   { return &Pointer{Base: t} }

func init() {
	IntegerType = &BasicType{
		Kind: Integer,
	}
	BooleanType = &BasicType{
		Kind: Boolean,
	}
	RealType = &BasicType{
		Kind: Real,
	}
	CharacterType = &BasicType{
		Kind: Character,
	}
	VoidType = &BasicType{
		Kind: Void,
	}
}
