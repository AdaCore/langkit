from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.envs import EnvSpec, set_initial_env
from langkit.expressions import Self

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    env_spec = EnvSpec(
        set_initial_env(Self.parent.parent.children_env)
    )


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='script.py')
