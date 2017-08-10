from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, abstract
from langkit.expressions import Property, Self
from langkit.parsers import Grammar, Opt, Tok

from lexer_example import Token
from os import path
from utils import emit_and_print_errors


def run(name, expr_fn):
    """
    Emit and print the errors we get for the below grammar with "expr_fn" as a
    property in Example.
    """
    Diagnostics.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))

    @abstract
    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        name = Field()

        prop = Property(expr_fn)

    class Name(FooNode):
        tok = Field()

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=Example('example', Opt(grammar.name)),
        name=Name(Tok(Token.Identifier, keep=True)),
    )
    emit_and_print_errors(grammar)
    print('')


run('No argument', Self.name.then(lambda: Self))
run('Two arguments', Self.name.then(lambda x, y: x.foo(y)))
run('Default value', Self.name.then(lambda name=None: name))

print('Done')
