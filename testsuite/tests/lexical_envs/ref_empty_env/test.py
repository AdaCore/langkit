"""
Check that referenced environments that resolve to Empty_Env are properly
cached.

The special case of Empty_Env used never to be cached, which caused exponential
time in lexical env lookup. This testcase checks that 1) they are cached
through making sure the lexical env does not hang (test timeout) and 2) they
are invalidated when appropriate (a unit is (re)parsed).
"""

from langkit.dsl import ASTNode, Field, T, abstract
from langkit.envs import EnvSpec, add_env, add_to_env_kv, reference
from langkit.expressions import No, Self, Var, langkit_property

from utils import build_and_run, unparse_all_script


class FooNode(ASTNode):
    pass


class Identifier(FooNode):
    token_node = True

    @langkit_property(return_type=T.FooNode.entity)
    def resolve():
        return Self.children_env.get(Self.symbol).at(0)


class Block(FooNode):
    name = Field(type=T.Identifier)
    items = Field(type=T.Item.list)

    env_spec = EnvSpec(
        add_to_env_kv(Self.name.symbol, Self),
        add_env(names=[Self.name.symbol]),
    )


@abstract
class Item(FooNode):
    pass


class Decl(Item):
    name = Field(type=T.Identifier)

    env_spec = EnvSpec(add_to_env_kv(Self.name.symbol, Self))


class Open(Item):
    name = Field(type=T.Identifier)

    env_spec = EnvSpec(
        reference(nodes=[Self.cast(T.FooNode)], through=T.Open.resolve),
    )

    @langkit_property(return_type=T.LexicalEnv)
    def resolve():
        n = Var(Self.name.resolve())
        return n.then(lambda n: n.children_env, No(T.LexicalEnv))


class Ref(Item):
    name = Field(type=T.Identifier)

    @langkit_property(return_type=T.FooNode.entity, public=True)
    def resolve():
        return Self.name.resolve()


build_and_run(lkt_file='expected_concrete_syntax.lkt',
              unparse_script=unparse_all_script, py_script='main.py')
print('Done')
