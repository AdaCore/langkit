"""
Test iterating over a huge number of elements.

This shows that an iterator should be used in place of an array when one wants
to return a huge number of elements, as calling ``p_get_all_examples`` from
the public APIs crashes in this instance.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.envs import EnvSpec, add_to_env_kv
from langkit.expressions import (
    Self, langkit_property
)

from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(public=True, return_type=T.FooNode.entity.array)
    def get_all_examples():
        return Self.children_env.get("example")

    @langkit_property(public=True, return_type=T.FooNode.entity.iterator)
    def iter_examples():
        return Self.get_all_examples.to_iterator


class Example(FooNode):
    env_spec = EnvSpec(
        add_to_env_kv("example", Self)
    )


build_and_run(lkt_file='expected_concrete_syntax.lkt',
              ada_main='main.adb')
print('Done')
