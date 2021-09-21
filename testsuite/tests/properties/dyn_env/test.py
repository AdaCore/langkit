"""
Check that the DynamicLexicalEnv expression works as expected.
"""

from langkit.dsl import ASTNode, Field, StructType, T, abstract
from langkit.envs import EnvSpec, add_to_env
from langkit.expressions import (
    DynamicLexicalEnv, Entity, No, Self, Var, current_env,
    langkit_property, lazy_field
)

from utils import build_and_run


class Metadata(StructType):
    pass


class FooNode(ASTNode):
    pass


class Identifier(FooNode):
    token_node = True


class ConsDecl(FooNode):
    name = Field(type=T.Identifier)
    cons_expr = Field(type=T.Expr)

    env_spec = EnvSpec(
        add_to_env(T.env_assoc.new(
            key=Self.name.symbol,
            val=Self,
            dest_env=current_env(),
            metadata=No(T.Metadata),
        ))
    )


class ArgSpec(FooNode):
    name = Field(type=T.Identifier)
    arg_expr = Field(type=T.Expr)


class FunDecl(FooNode):
    name = Field(type=T.Identifier)
    args = Field(type=T.ArgSpec.list)

    env_spec = EnvSpec(
        add_to_env(T.env_assoc.new(
            key=Self.name.symbol,
            val=Self,
            dest_env=current_env(),
            metadata=No(T.Metadata),
        ))
    )


class CallExpr(FooNode):
    name = Field(type=T.Identifier)
    args = Field(type=T.Expr.list)

    # Properties that return dynamic lexical envs

    @lazy_field(return_type=T.LexicalEnv)
    def args_env():
        return DynamicLexicalEnv(CallExpr.args_assocs_getter)

    @lazy_field(return_type=T.LexicalEnv)
    def arg_exprs_env():
        return DynamicLexicalEnv(CallExpr.arg_exprs_assocs_getter,
                                 Expr.resolve)

    # Getter of env associations for both dynamic lexical envs

    @langkit_property(return_type=T.inner_env_assoc.array)
    def args_assocs_getter():
        """
        For each argument, associate its name to the expression passed in this
        call.
        """
        decl = Var(Self.node_env.get_first(Self.name).cast(T.FunDecl))
        return decl.args.map(lambda i, a: T.inner_env_assoc.new(
            key=a.name.symbol, val=Self.args.at(i), metadata=No(T.env_md)
        ))

    @langkit_property(return_type=T.inner_env_assoc.array)
    def arg_exprs_assocs_getter():
        """
        For each argument, associate its name to its default expression.
        """
        decl = Var(Self.node_env.get_first(Self.name).cast(T.FunDecl))
        return decl.args.map(lambda a: T.inner_env_assoc.new(
            key=a.name.symbol, val=a.arg_expr.node, metadata=No(T.env_md)
        ))

    # Entry points for the test program

    @langkit_property(public=True, return_type=T.Expr.entity)
    def get_arg(name=T.Symbol):
        return Self.args_env.get_first(name).cast(T.Expr)

    @langkit_property(public=True, return_type=T.Expr.entity)
    def get_arg_expr(name=T.Symbol):
        return Self.arg_exprs_env.get_first(name).cast(T.Expr)


@abstract
class Expr(FooNode):

    @langkit_property(return_type=T.Expr.entity)
    def resolve():
        return Entity.match(
            lambda l=T.Literal: l,

            lambda r=T.Ref:
            Entity.node_env.get_first(r.name.symbol).cast(T.ConsDecl)
            .cons_expr.resolve,
        )


class Literal(Expr):
    token_node = True


class Ref(Expr):
    name = Field(type=T.Identifier)


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main='main.adb')
print('Done')
