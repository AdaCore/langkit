"""
Test that the lowering pass for properties dispatching works properly on all
kinds of root properties:

  * concrete, abstract and runtime-checked abstract;
  * using Entity and using Self.
"""

from langkit.dsl import ASTNode, Field, T, abstract
from langkit.envs import EnvSpec, add_to_env_kv
from langkit.expressions import (AbstractProperty, DynamicVariable, Entity,
                                 Self, langkit_property)

from utils import build_and_run


dyn_var = DynamicVariable('dyn_var', T.FooNode)


@abstract
class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


@abstract
class Expr(FooNode):

    evaluate_abstract = AbstractProperty(T.Int, public=True)
    evaluate_rtcheck = AbstractProperty(T.Int, public=True,
                                        runtime_check=True)

    @langkit_property(public=True)
    def evaluate_concrete():
        return 1

    evaluate_entity = AbstractProperty(T.Int, public=True)


@abstract
class Atom(Expr):
    pass


class Lit(Atom):
    token_node = True

    @langkit_property()
    def evaluate_abstract():
        return 1

    @langkit_property()
    def evaluate_rtcheck():
        return 1

    @langkit_property(public=True)
    def evaluate_entity():
        return 1


class Ref(Atom):
    name = Field()

    @langkit_property(public=True)
    def resolve():
        return Self.node_env.get_first(Self.name.symbol).cast_or_raise(T.Def)

    @langkit_property()
    def evaluate_abstract():
        return Self.resolve.expr.evaluate_abstract

    @langkit_property(public=True)
    def evaluate_concrete():
        return Self.resolve.expr.evaluate_concrete

    @langkit_property(public=True)
    def evaluate_entity():
        return Entity.resolve.expr.evaluate_entity


class Plus(Expr):
    lhs = Field()
    rhs = Field()

    @langkit_property()
    def evaluate_abstract():
        return Self.lhs.evaluate_abstract + Self.rhs.evaluate_abstract

    @langkit_property()
    def evaluate_rtcheck():
        return Self.lhs.evaluate_rtcheck + Self.rhs.evaluate_rtcheck

    @langkit_property(public=True)
    def evaluate_concrete():
        return Self.lhs.evaluate_concrete + Self.rhs.evaluate_concrete

    @langkit_property(public=True)
    def evaluate_entity():
        return Entity.lhs.evaluate_entity + Entity.rhs.evaluate_entity


class Def(FooNode):
    name = Field()
    expr = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, val=Self)
    )


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              lkt_semantic_checks=True)
print('Done')
