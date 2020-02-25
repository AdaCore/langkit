"""
Test the "get all" procedure of lexical envs, and in particular its
determinism.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import (ASTNode, Field, T, UserField, has_abstract_list,
                         synthetic)
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import No, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(return_type=T.FooNode.entity.array,
                      public=True)
    def env_get_all():
        return Self.children_env.get(symbol=No(T.Symbol))

    @langkit_property(return_type=T.FooNode, memoized=True)
    def make_id(s=T.Symbol):
        return SynthId.new(sym=s)

    @langkit_property(return_type=T.FooNode)
    def unique_id(s=T.Symbol):
        return Self.unit.root.make_id(s)


@synthetic
class SynthId(FooNode):
    sym = UserField(type=T.Symbol, public=False)

    @langkit_property(return_type=T.String, public=True)
    def get_sym():
        return Self.sym.image


class Id(FooNode):
    token_node = True


@has_abstract_list
class Insert(FooNode):
    sym = Field(type=T.Id)
    node = Field(type=T.Id)

    env_spec = EnvSpec(add_to_env_kv(
        Self.sym.symbol,
        Self.unique_id(Self.node.symbol)
    ))


class Program(Insert.list):
    env_spec = EnvSpec(add_env())


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
