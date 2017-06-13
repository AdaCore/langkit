"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import DiagnosticError, Diagnostics
from langkit.dsl import (ASTNode, BoolType, Field, Struct, T, UserField,
                         env_metadata, root_grammar_class)
from langkit.parsers import Grammar, Tok

from lexer_example import Token
from utils import emit_and_print_errors, reset_langkit


def run(md_constructor):
    """
    Emit and print he errors we get for the below grammar. `md_constructor` is
    called to create the lexical environment metadata.
    """

    print('== {} =='.format(md_constructor.__name__))
    Diagnostics.set_lang_source_dir(os.path.abspath(__file__))

    @root_grammar_class
    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        pass

    def lang_def():
        foo_grammar = Grammar('main_rule')
        foo_grammar.add_rules(main_rule=Example(Tok(Token.Example)))
        return foo_grammar

    try:
        md_constructor()
    except DiagnosticError:
        reset_langkit()
    else:
        emit_and_print_errors(lang_def)
    print('')


def not_a_struct():
    @env_metadata
    class Metadata(object):
        pass


def two_md():
    @env_metadata
    class Metadata(Struct):
        pass

    del Metadata

    @env_metadata
    class Metadata(Struct):
        pass


def bad_name():
    @env_metadata
    class BadName(Struct):
        pass


def bad_field():
    @env_metadata
    class Metadata(Struct):
        fld = Field(type=BoolType)


def bad_type():
    @env_metadata
    class Metadata(Struct):
        fld = UserField(type=T.FooNode)


run(not_a_struct)
run(two_md)
run(bad_name)
run(bad_field)
run(bad_type)
print('Done')
