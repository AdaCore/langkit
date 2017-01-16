from langkit.compiled_types import ASTNode, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.expressions import Property
from langkit.parsers import Grammar, Row

from os import path
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(path.abspath(__file__))


def make_lang_def(lit):
    def lang_def():
        @root_grammar_class()
        class FooNode(ASTNode):
            b = Property(lit)

        foo_grammar = Grammar('main_rule')
        foo_grammar.add_rules(main_rule=Row('example') ^ FooNode)
        return foo_grammar
    return lang_def

print 'Valid case'
emit_and_print_errors(make_lang_def(12))
print 'Invalid case'
emit_and_print_errors(make_lang_def("lol"))
print('')
print 'Done'
