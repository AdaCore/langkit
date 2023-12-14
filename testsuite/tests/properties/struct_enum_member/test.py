"""
Test having enums as part of structs.
"""

from langkit.dsl import (
    ASTNode, Enum, EnumValue, Field, Struct, T, UserField, abstract
)
from langkit.expressions import AbstractProperty, Property, Self

from utils import build_and_run


class DeclKind(Enum):
    func = EnumValue()
    var = EnumValue()
    none = EnumValue(is_default=True)


class DeclAndKind(Struct):
    dcl = UserField(type=T.Decl)
    knd = UserField(type=T.DeclKind)


class FooNode(ASTNode):
    pass


@abstract
class Decl(FooNode):
    decl_kind = AbstractProperty(DeclKind, public=True)
    with_kind = Property(
        DeclAndKind.new(dcl=Self, knd=Self.decl_kind),
        public=True
    )


class VarDecl(Decl):
    name = Field()
    expr = Field()

    decl_kind = Property(DeclKind.var)


class FuncDecl(Decl):
    name = Field()
    args = Field()
    expr = Field()

    decl_kind = Property(DeclKind.func)


@abstract
class Expression(FooNode):
    pass


class Literal(Expression):
    token_node = True


class Name(Expression):
    token_node = True


class Plus(Expression):
    left = Field()
    right = Field()


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
