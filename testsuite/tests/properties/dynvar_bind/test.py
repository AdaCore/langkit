from langkit.dsl import ASTNode, Field, LexicalEnv
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import DynamicVariable, Self, langkit_property

from utils import build_and_run


Env = DynamicVariable('env', LexicalEnv)


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


class Decl(FooNode):
    name = Field()
    refs = Field()

    env_spec = EnvSpec(
        add_to_env_kv(
            key=Self.name.symbol, value=Self
        ),
        add_env()
    )


class Ref(FooNode):
    name = Field()

    env_spec = EnvSpec(
        add_to_env_kv(
            key=Self.name.symbol, value=Self
        )
    )

    @langkit_property(public=True)
    def resolve():
        return Env.bind(Self.parent.parent.node_env,
                        Env.get(Self.name.symbol).at(0))


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
