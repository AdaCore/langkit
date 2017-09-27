"""
Test that Symbol bindings in the Python API are properly working.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, T
from langkit.expressions import Entity, Property, langkit_property
from langkit.parsers import Grammar, List, Or, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):

    @langkit_property(public=True)
    def count(seq=T.Example.entity.array):
        return seq.length


class Sequence(FooNode.list):
    all_items = Property(Entity.map(lambda i: i), public=True)
    example_items = Property(Entity.filtermap(
        lambda i: i.cast_or_raise(T.Example),
        lambda i: i.is_a(T.Example)
    ), public=True)


class Example(FooNode):
    pass


class Null(FooNode):
    pass


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(foo_grammar.node, list_cls=Sequence),
    node=Or(foo_grammar.example, foo_grammar.null),
    example=Example(Tok(Token.Example)),
    null=Null(Tok(Token.Null)),
)

build_and_run(foo_grammar, 'main.py')
print('Done')
