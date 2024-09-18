"""Check that the generic Ada API works as expected."""

from langkit.dsl import (
    ASTNode,
    AbstractField,
    Field,
    MetadataField,
    NullField,
    Struct,
    T,
    UserField,
    abstract,
    env_metadata,
    synthetic,
)
from langkit.expressions import (
    AbstractKind,
    ArrayLiteral,
    BigIntLiteral,
    CharacterLiteral,
    CreateCopyNodeBuilder,
    CreateSynthNodeBuilder,
    Entity,
    If,
    No,
    Property,
    PropertyError,
    Self,
    String,
    langkit_property,
    lazy_field,
)

from utils import build_and_run


@env_metadata
class Metadata(Struct):
    md1 = MetadataField(type=T.Bool, use_in_eq=True)
    md2 = MetadataField(type=T.Bool, use_in_eq=False)


class Point(Struct):
    label = UserField(type=T.String)
    x = UserField(type=T.BigInt)
    y = UserField(type=T.BigInt)


class NodeResult(Struct):
    n = UserField(type=T.Example)
    e = UserField(type=T.Example.entity)


# Create a struct that is not exposed just to check that it does not show up in
# the public introspection API.
class PrivatePoint(Struct):
    x = UserField(type=T.BigInt)
    y = UserField(type=T.BigInt)


class FooNode(ASTNode):
    pass


@abstract
class BaseExample(FooNode):
    # Check for proper member inheritance handling: the introspection API
    # should list "fld_1" first and "fld_2" for BaseExample, but in the
    # opposite order for Example.
    fld_1 = AbstractField(type=T.Name)
    fld_2 = AbstractField(type=FooNode)

    @langkit_property(return_type=T.Bool,
                      public=True,
                      kind=AbstractKind.abstract)
    def id_bool(id=T.Bool):
        pass


class Example(BaseExample):

    fld_2 = NullField()
    fld_1 = Field()

    @langkit_property()
    def to_public(p=T.PrivatePoint):
        return Point.new(label=String("from private"), x=p.x, y=p.y)

    @langkit_property(public=True)
    def prop(p=T.Point):
        return Self.to_public(PrivatePoint.new(x=p.x, y=p.y))

    @langkit_property(public=True)
    def result():
        return T.NodeResult.new(n=Self, e=Entity)

    @langkit_property(public=True)
    def may_raise(value=T.Int, do_raise=T.Bool):
        return If(do_raise, PropertyError(T.Int, "from may_raise"), value)

    # Test for primitive types
    id_bool = Property(lambda id=T.Bool: id, public=True)
    id_int = Property(lambda id=T.Int: id, public=True)
    id_bigint = Property(lambda id=T.BigInt: id, public=True)
    id_char = Property(lambda id=T.Character: id, public=True)
    id_token = Property(lambda id=T.Token: id, public=True)
    id_sloc = Property(lambda id=T.SourceLocation: id, public=True)
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

    # Test for iterators
    create_bigint_iterator = Property(
        ArrayLiteral([BigIntLiteral(1), BigIntLiteral(2), BigIntLiteral(3)])
        .to_iterator,
        public=True
    )
    id_bigint_iterator = Property(lambda id=T.BigInt.iterator: id, public=True)

    # Test for default values
    id_dflt_bool = Property(lambda id=(T.Bool, True): id, public=True)
    id_dflt_int = Property(lambda id=(T.Int, 42): id, public=True)
    id_dflt_char = Property(
        lambda id=(T.Character, CharacterLiteral('\x00')): id,
        public=True)
    id_dflt_root_node = Property(lambda id=(T.FooNode, No(T.FooNode)): id,
                                 public=True)

    @langkit_property(public=True)
    def with_md(md1=T.Bool, md2=T.Bool):
        return Example.entity.new(node=Entity.node, info=T.entity_info.new(
            rebindings=Entity.info.rebindings,
            md=T.Metadata.new(md1=md1, md2=md2),
            from_rebound=Entity.info.from_rebound
        ))

    @lazy_field(public=True)
    def create_synth_node():
        return CreateSynthNodeBuilder(
            T.SynthNode,
            foo=CreateCopyNodeBuilder(No(T.FooNode)),
        ).build()


class NullQual(FooNode):
    enum_node = True
    qualifier = True


class VarDecl(FooNode):
    is_null = Field()
    name = Field()
    value = Field()


class Def(FooNode):
    name = Field(type=T.Name)
    args = Field(type=T.Name.list)
    expr = Field(type=T.Expr)


class Name(FooNode):
    token_node = True


@abstract
class Expr(FooNode):
    pass


class Addition(Expr):
    lhs = Field()
    rhs = Field()


class Call(Expr):
    name = Field(type=T.Name)
    args = Field(type=T.Expr.list)


class Number(Expr):
    token_node = True


class Ref(Expr):
    name = Field()


@synthetic
class SynthNode(FooNode):
    foo = Field(type=T.FooNode, nullable=True)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=[
        "analysis.adb",
        "introspection_types.adb",
        "introspection_values.adb",
        "hash.adb",
    ],
    types_from_lkt=True,
    generate_unparser=True,
)
print("Done")
