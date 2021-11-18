"""
Check that add_to_env on a non-primary environment raises a Property_Error. It
used to crash with a Constraint_Error.
"""

from langkit.dsl import ASTNode
from langkit.envs import EnvSpec, add_env, add_to_env
from langkit.expressions import Self, direct_env, new_env_assoc

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True

    env_spec = EnvSpec(
        add_env(),
        add_to_env(
            new_env_assoc(
                key="foo",
                value=Self,
                dest_env=direct_env(Self.children_env.env_orphan),
            )
        )
    )


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
