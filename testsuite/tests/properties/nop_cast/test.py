from __future__ import absolute_import, division, print_function

from langkit.compiled_types import ASTNode, T, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.expressions import Property, Self
from langkit.parsers import Grammar, Row

from os import path
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    pass


class ExampleNode(FooNode):
    prop = Property(Self.cast(T.ExampleNode), public=True)


def lang_def():
    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(
        main_rule=Row('example') ^ ExampleNode
    )
    return foo_grammar

emit_and_print_errors(lang_def)
print('Done')
