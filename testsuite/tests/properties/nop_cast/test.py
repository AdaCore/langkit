from langkit.compiled_types import ASTNode, TypeRepo, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.expressions import Property, Self
from langkit.parsers import Grammar, Row, List, Tok

from lexer_example import Token
from os import path
from utils import emit_and_print_errors, reset_langkit


T = TypeRepo()
Diagnostics.set_lang_source_dir(path.abspath(__file__))


@root_grammar_class
class FooNode(ASTNode):
    pass


class ExampleNode(FooNode):
    prop = Property(Self.cast(T.ExampleNode))


foo_grammar = Grammar()
foo_grammar.main_rule_name = 'main_rule'
foo_grammar.add_rules(
    main_rule=Row('example') ^ ExampleNode
)
emit_and_print_errors(foo_grammar)
print 'Done'
