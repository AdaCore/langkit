"""
Test that ref-counted fields in structs are properly bound in C/Python APIs.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.compiled_types import (
    ASTNode, Field, Struct, T, root_grammar_class
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import New, Property, Self
from langkit.parsers import Grammar, List, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    pass


class Names(Struct):
    label = Field(type=T.Name)
    name_list = Field(type=T.Name.array_type())


class ListDecl(FooNode):
    label = Field(type=T.Name)
    name_list = Field(type=T.Name.list_type())

    to_struct = Property(
        New(Names,
            label=Self.label,
            name_list=Self.name_list.map(lambda n: n)),
        public=True
    )


class Name(FooNode):
    tok = Field(type=T.Token)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(ListDecl(foo_grammar.name, '(', foo_grammar.names, ')')),
    names=List(foo_grammar.name, sep=',', empty_valid=True),
    name=Name(Tok(Token.Identifier, keep=True)),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
