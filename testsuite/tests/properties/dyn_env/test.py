"""
Check that the DynamicLexicalEnv expression works as expected.
"""

from langkit.dsl import ASTNode, Field, StructType, T, abstract
from langkit.envs import EnvSpec, add_to_env
from langkit.expressions import (DynamicLexicalEnv, No, Self, Var,
                                 langkit_property, lazy_field)

from utils import build_and_run


class Metadata(StructType):
    pass


class FooNode(ASTNode):
    pass


class Identifier(FooNode):
    token_node = True


class FunDecl(FooNode):
    name = Field(type=T.Identifier)
    args = Field(type=T.Identifier.list)

    env_spec = EnvSpec(
        add_to_env(T.env_assoc.new(
            key=Self.name.symbol,
            val=Self,
            dest_env=No(T.LexicalEnv),
            metadata=No(T.Metadata),
        ))
    )


class CallExpr(FooNode):
    name = Field(type=T.Identifier)
    args = Field(type=T.Expr.list)

    @lazy_field(return_type=T.LexicalEnv)
    def args_env():
        result = DynamicLexicalEnv(CallExpr.resolver)
        return result

    @langkit_property(return_type=T.inner_env_assoc.array)
    def resolver():
        decl = Var(Self.node_env.get_first(Self.name).cast(T.FunDecl))
        return decl.args.map(lambda i, a: T.inner_env_assoc.new(
            key=a.symbol, val=Self.args.at(i), metadata=No(T.env_md)
        ))

    @langkit_property(public=True, return_type=T.Expr.entity)
    def get(name=T.Symbol):
        return Self.args_env.get_first(name).cast(T.Expr)


@abstract
class Expr(FooNode):
    pass


class Literal(Expr):
    token_node = True


class Ref(Expr):
    name = Field(type=T.Identifier)


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main='main.adb')
print('Done')
