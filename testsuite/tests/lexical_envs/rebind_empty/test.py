"""
Check that rebinding the empty environment works (it used to crash with a
Constraint_Error).
"""

from langkit.dsl import ASTNode
from langkit.envs import EnvSpec, add_env
from langkit.expressions import No, T, Var, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True

    env_spec = EnvSpec(add_env())

    @langkit_property(public=True, return_type=T.FooNode.entity.array)
    def rebind(from_node=T.FooNode.entity, to_node=T.FooNode.entity):

        # Build non-null rebindings
        rbdng = Var(No(T.EnvRebindings).append_rebinding(
            from_node.children_env,
            to_node.children_env
        ))

        # Call rebind_env on them (this is the important part, the rest is
        # cosmetic).
        return No(T.LexicalEnv).rebind_env(rbdng).get("foo")


build_and_run(lkt_file="expected_concrete_syntax.lkt",
              py_script="main.py")
print('Done')
