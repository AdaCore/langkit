"""
Test that the "is_visible_from" operation properly raises a PropertyError for
invalid input.
"""

from langkit.dsl import ASTNode, Bool, has_abstract_list
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import EmptyEnv, If, Self, Var, langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True)
    def prop(empty1=Bool, empty2=Bool):
        arg1 = Var(If(empty1, EmptyEnv, Self.children_env))
        arg2 = Var(If(empty2, EmptyEnv, Self.children_env))
        return arg1.is_visible_from(arg2)


@has_abstract_list
class Name(FooNode):
    token_node = True

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.symbol, value=Self),
        add_env()
    )


class Scope(Name.list):
    env_spec = EnvSpec(add_env())


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
