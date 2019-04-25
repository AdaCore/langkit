"""
Check that creating domains with arrays of non-root entity types generates
valid and correctly running code.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, T, UserField
from langkit.envs import EnvSpec, add_to_env_kv
from langkit.expressions import Self, langkit_property
from langkit.parsers import Grammar, List, Or

from lexer_example import Token
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
        candidates = (Self.node_env.get(Self)
                      .map(lambda d: d.cast(T.Definition)))
        return Self.ref_var.domain(candidates).solve

    @langkit_property(public=True)
    def definition():
        return Self.ref_var.get_value.cast(T.Definition)


fg = Grammar('main_rule')
fg.add_rules(
    name=Name(Token.Identifier),
    main_rule=List(Or(Definition('def', fg.name), fg.name), sep=','),
)
build_and_run(fg, 'main.py')
print('Done')
