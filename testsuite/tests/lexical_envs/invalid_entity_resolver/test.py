"""
Test the handling of analysis units in the properties DSL.
"""

from langkit.dsl import ASTNode, Field, LexicalEnv, T
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import (DynamicVariable, Property, Self,
                                 langkit_property)

from utils import emit_and_print_errors


Env = DynamicVariable('env', LexicalEnv)


def run(name, prop):
    print('== {} =='.format(name))

    class FooNode(ASTNode):
        resolve_ref = prop

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

        env_spec = EnvSpec(add_to_env_kv(
            key=Self.name.symbol, value=Self,
            resolver=FooNode.resolve_ref
        ))

        @langkit_property(public=True)
        def resolve():
            return Self.node_env.get(Self.name).at(0)

    emit_and_print_errors(lkt_file='foo.lkt')


run('Bad return type', Property(Self.node_env.get('foo')))
run('Has dynamic variable', Property(Self.node_env.get('foo').at(0),
                                     dynamic_vars=[Env]))
run('Has arguments', Property(lambda i=T.Int:
                              Self.node_env.get('foo').at(i)))
print('Done')
