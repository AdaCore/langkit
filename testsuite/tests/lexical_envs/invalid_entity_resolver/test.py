"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, LexicalEnvType, T
from langkit.envs import EnvSpec, add_to_env, add_env
from langkit.expressions import (DynamicVariable, New, Property, Self,
                                 langkit_property)
from langkit.parsers import Grammar, List

from lexer_example import Token
from utils import emit_and_print_errors


Env = DynamicVariable('env', LexicalEnvType)


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
            add_to_env(
                New(T.env_assoc, key=Self.name.symbol, val=Self)
            ),
            add_env()
        )

    class Ref(FooNode):
        name = Field()

        env_spec = EnvSpec(add_to_env(
            New(T.env_assoc, key=Self.name.symbol, val=Self),
            resolver=FooNode.resolve_ref
        ))

        @langkit_property(public=True)
        def resolve():
            return Self.node_env.get(Self.name).at(0)

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=List(grammar.decl),
        decl=Decl(
            Name(Token.Identifier),
            '(', List(grammar.ref, empty_valid=True), ')'
        ),
        ref=Ref(Name(Token.Identifier)),
    )
    emit_and_print_errors(grammar)

run('Bad return type', Property(Self.node_env.get('foo')))
run('Has dynamic variable', Property(Self.node_env.get('foo').at(0),
                                     dynamic_vars=[Env]))
run('Has arguments', Property(lambda i=T.IntegerType:
                              Self.node_env.get('foo').at(i)))
print('Done')
