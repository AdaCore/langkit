"""
Check that creating domains with arrays of non-root entity types generates
valid and correctly running code.
"""

from langkit.dsl import ASTNode, Field, T, UserField
from langkit.envs import EnvSpec, add_to_env_kv
from langkit.expressions import Self, Var, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Definition(FooNode):
    name = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, val=Self)
    )


class Name(FooNode):
    token_node = True

    ref_var = UserField(type=T.LogicVar, public=False)

    @langkit_property(public=True)
    def resolve():
        candidates = Var(
            Self.node_env.get(Self).map(lambda d: d.cast(T.Definition))
        )
        return Self.ref_var.domain(candidates).solve

    @langkit_property(public=True)
    def definition():
        return Self.ref_var.get_value.cast(T.Definition)


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
