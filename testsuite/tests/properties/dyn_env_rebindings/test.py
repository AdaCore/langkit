"""
Check that using custom rebindings in an inner_env_assoc inside a
DynamicLexicalEnv works as intended. In particular, check that it behaves
safely when any of lexical envs in the rebindings entry become stale.
"""

from langkit.dsl import ASTNode, Annotations, Field, StructType, T, abstract
from langkit.envs import EnvSpec, add_env, add_to_env
from langkit.expressions import (
    AbstractKind, DynamicLexicalEnv, Entity, No, Self, Var, current_env,
    langkit_property, lazy_field
)

from utils import build_and_run


class Metadata(StructType):
    pass


class FooNode(ASTNode):
    pass


class Identifier(FooNode):
    token_node = True


@abstract
class TypeExpr(FooNode):
    @langkit_property(kind=AbstractKind.abstract, public=True,
                      return_type=T.TypeDecl.entity)
    def resolve():
        pass


class TypeRef(TypeExpr):
    name = Field(type=T.Identifier)

    @langkit_property()
    def resolve():
        return Entity.children_env.get_first(Self.name.symbol).match(
            lambda alias=T.TypeAlias.entity: alias.type_expr.resolve,
            lambda other: other.cast(T.TypeDecl)
        )


class TypeInstantiation(TypeExpr):
    generic_type = Field(type=T.TypeExpr)
    actual_type = Field(type=T.TypeExpr)

    @lazy_field()
    def instantiation_env():
        return DynamicLexicalEnv(
            TypeInstantiation.instantiation_bindings
        )

    @langkit_property(memoized=True, return_type=T.inner_env_assoc.array)
    def instantiation_bindings():
        ent = Var(Self.as_bare_entity)
        gen = Var(ent.generic_type.resolve.cast(T.GenericStruct))
        act = Var(ent.actual_type.resolve)
        return T.inner_env_assoc.new(
            key=gen.type_param.name.symbol,
            value=act.node,
            rebindings=act.info.rebindings
        ).singleton

    @langkit_property()
    def resolve():
        gen = Var(Entity.generic_type.resolve.cast(T.GenericStruct))
        return T.GenericStruct.entity.new(
            node=gen.node,
            info=T.entity_info.new(
                rebindings=gen.info.rebindings.append_rebinding(
                    gen.node.children_env,
                    Self.instantiation_env
                ),
                from_rebound=gen.info.from_rebound,
                md=gen.info.md,
            )
        )


@abstract
class TypeDecl(FooNode):
    name = Field(type=T.Identifier)

    env_spec = EnvSpec(
        add_to_env(T.env_assoc.new(
            key=Self.name.symbol,
            value=Self,
            dest_env=current_env(),
            metadata=No(T.Metadata),
        )),
        add_env()
    )


class BareStruct(TypeDecl):
    pass


class GenericFormalTypeDecl(TypeDecl):
    pass


class GenericStruct(TypeDecl):
    type_param = Field(type=T.GenericFormalTypeDecl)

    annotations = Annotations(rebindable=True)

    @langkit_property(return_type=T.TypeDecl.entity, public=True)
    def actual_type():
        return Entity.children_env.get_first(
            Self.type_param.name.symbol
        ).cast(TypeDecl)


class TypeAlias(TypeDecl):
    type_expr = Field(type=T.TypeExpr)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
