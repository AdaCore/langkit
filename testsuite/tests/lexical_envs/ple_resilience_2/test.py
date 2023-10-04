"""
Check that Self_Env in nodes have are default initialized to the empty
environment when Populate_Lexical_Env haven't processed them yet.
"""

from langkit.dsl import ASTNode, Field
from langkit.envs import EnvSpec, do
from langkit.expressions import Self

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    pass


class Def(FooNode):
    example = Field()

    env_spec = EnvSpec(
        # Try to access something inside the children't env, which PLE did not
        # reach yet, to make sure it is not null (i.e. initialized). If it is
        # null, this will raise a constaint error, and thus some failure will
        # be visible in the Python API.
        do(Self.example.children_env.env_node),
    )


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
