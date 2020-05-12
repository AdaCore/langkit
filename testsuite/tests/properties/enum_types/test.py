"""
Test basic features for enumeration types.
"""

from langkit.dsl import ASTNode, Enum, EnumValue, Field, abstract
from langkit.expressions import AbstractProperty, Property, langkit_property

from utils import build_and_run


class DeclKind(Enum):
    func = EnumValue()
    var = EnumValue()


class FooNode(ASTNode):

    @langkit_property(public=True)
    def identity(k=DeclKind):
        return k


@abstract
class Decl(FooNode):
    decl_kind = AbstractProperty(DeclKind, public=True)


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


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
