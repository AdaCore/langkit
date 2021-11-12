"""Check that the generic Ada API works as expected."""

from langkit.dsl import ASTNode, Field, Struct, T, UserField, abstract
from langkit.expressions import (
    CharacterLiteral,
    No,
    Property,
    Self,
    langkit_property,
)

from utils import build_and_run


class Point(Struct):
    x = UserField(type=T.BigInt)
    y = UserField(type=T.BigInt)


class NodeResult(Struct):
    n = UserField(type=T.Example)


# Create a struct that is not exposed just to check that it does not show up in
# the public introspection API.
class PrivatePoint(Struct):
    x = UserField(type=T.BigInt)
    y = UserField(type=T.BigInt)


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property()
    def to_public(p=T.PrivatePoint):
        return Point.new(x=p.x, y=p.y)

    @langkit_property(public=True)
    def prop(p=T.Point):
        return Self.to_public(PrivatePoint.new(x=p.x, y=p.y))

    @langkit_property(public=True)
    def result():
        return T.NodeResult.new(n=Self)

    # Test for primitive types
    id_bool = Property(lambda id=T.Bool: id, public=True)
    id_int = Property(lambda id=T.Int: id, public=True)
    id_bigint = Property(lambda id=T.BigInt: id, public=True)
    id_char = Property(lambda id=T.Character: id, public=True)
    id_token = Property(lambda id=T.Token: id, public=True)
    id_sym = Property(lambda id=T.Symbol: id, public=True)
    id_unit = Property(lambda id=T.AnalysisUnit: id, public=True)
    id_root_node = Property(lambda id=T.FooNode: id, public=True)
    id_name = Property(lambda id=T.Name: id, public=True)

    # Test for enums
    id_unit_kind = Property(lambda id=T.AnalysisUnitKind: id, public=True)

    # Test for arrays
    id_node_array = Property(lambda id=T.FooNode.entity.array: id, public=True)
    id_expr_array = Property(lambda id=T.Expr.entity.array: id, public=True)
    id_bigint_array = Property(lambda id=T.BigInt.array: id, public=True)
    id_unit_array = Property(lambda id=T.AnalysisUnit.array: id, public=True)

    # Test for default values
    id_dflt_bool = Property(lambda id=(T.Bool, True): id, public=True)
    id_dflt_int = Property(lambda id=(T.Int, 42): id, public=True)
    id_dflt_char = Property(
        lambda id=(T.Character, CharacterLiteral('\x00')): id,
        public=True)
    id_dflt_root_node = Property(lambda id=(T.FooNode, No(T.FooNode)): id,
                                 public=True)


class VarDecl(FooNode):
    name = Field()
    value = Field()


class Name(FooNode):
    token_node = True


@abstract
class Expr(FooNode):
    pass


class Addition(Expr):
    lhs = Field()
    rhs = Field()


class Number(Expr):
    token_node = True


class Ref(Expr):
    name = Field()


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    ada_main=["analysis.adb", "introspection_types.adb",
              "introspection_values.adb"]
)
print("Done")
