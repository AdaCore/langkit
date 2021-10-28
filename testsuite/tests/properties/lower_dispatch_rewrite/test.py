"""
Test that the lowering pass for properties dispatching works properly rewrites
references to all transformed properties.
"""

from langkit.dsl import ASTNode, Field, T, abstract, synthetic
from langkit.envs import EnvSpec, add_env, add_to_env_kv, reference
from langkit.expressions import No, Self, langkit_property

from utils import build_and_run


@abstract
class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


@abstract
class Expr(FooNode):
    pass


@abstract
class Atom(Expr):
    pass


class Lit(Atom):
    token_node = True


class Ref(Atom):
    name = Field()

    @langkit_property(public=True)
    def resolve():
        return Self.node_env.get_first(Self.name.symbol)

    @langkit_property(public=True)
    def dummy():
        return Self.referenced_env.get(Self.name.symbol)

    @langkit_property()
    def referenced_env():
        return No(T.LexicalEnv)


@synthetic
class MiddleRef(Ref):

    @langkit_property()
    def referenced_env():
        return No(T.LexicalEnv)


class DerivedRef(MiddleRef):
    @langkit_property()
    def referenced_env():
        return Self.unit.root.node_env.get_first(Self.name.symbol).children_env


class Plus(Expr):
    lhs = Field()
    rhs = Field()


class Var(FooNode):
    name = Field()
    expr = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, val=Self),
    )


class Def(FooNode):
    name = Field()
    imports = Field()
    vars = Field()
    expr = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, val=Self),
        add_env(),
        reference(Self.imports.map(lambda i: i.cast(T.FooNode)),

                  # If PropertyDef rewriting omits the following references,
                  # env lookup will never reach DerivedRef.referenced_env, so
                  # resolution will sometimes fail to reach definition.
                  T.MiddleRef.referenced_env)
    )


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              lkt_semantic_checks=True)
print('Done')
