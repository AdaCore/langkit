"""
Check that set_initial_env trying to set a non-primary environment raises a
Property_Error.
"""

from langkit.dsl import ASTNode
from langkit.envs import EnvSpec, set_initial_env
from langkit.expressions import Self, direct_env

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True

    env_spec = EnvSpec(
        set_initial_env(direct_env(Self.parent.children_env.env_orphan)),
    )


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
