"""
Check that an invalid node type passed to the "through" property in a
"reference" env action triggers a Property_Error. It used to trigger an
assertion error.
"""

from langkit.dsl import ASTNode, Field, T
from langkit.envs import EnvSpec, add_env, add_to_env_kv, reference
from langkit.expressions import No, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Decl(FooNode):
    token_node = True

    @langkit_property()
    def refd_env():
        return No(T.LexicalEnv)

    env_spec = EnvSpec(add_to_env_kv(key=Self.symbol, value=Self))

    @langkit_property(public=True)
    def lookup(name=T.Symbol):
        return Self.node_env.get(name)


class Scope(FooNode):
    decls = Field(type=Decl.list)

    env_spec = EnvSpec(
        add_env(),
        reference(
            # This passes a Scope node to a Decl property, so we expect a
            # Property_Error whenever we try to lookup envs.
            nodes=Self.cast(FooNode).singleton,
            through=Decl.refd_env,
        ),
    )


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print('Done')
