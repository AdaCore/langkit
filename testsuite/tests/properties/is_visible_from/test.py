"""
Test that the "is_visible_from" operation properly raises a PropertyError for
invalid input.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Bool, T, has_abstract_list
from langkit.envs import EnvSpec, add_to_env_kv, add_env
from langkit.expressions import EmptyEnv, If, New, Self, Var, langkit_property
from langkit.parsers import Grammar, List

from lexer_example import Token
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
        add_to_env_kv(key=Self.symbol, val=Self),
        add_env()
    )


class Scope(Name.list):
    env_spec = EnvSpec(add_env())


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(foo_grammar.name, list_cls=Scope),
    name=Name(Token.Identifier),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
