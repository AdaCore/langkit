from langkit.compiled_types import ASTNode, abstract, root_grammar_class
from langkit.parsers import Grammar, Row

from utils import emit_and_print_errors


@abstract
@root_grammar_class
class FooNode(ASTNode):
    pass


class ExampleNode(FooNode):
    pass


class UnreferencedNode(FooNode):
    pass


foo_grammar = Grammar()
foo_grammar.main_rule_name = 'main_rule'
foo_grammar.add_rules(
    main_rule=Row('example') ^ ExampleNode
)


emit_and_print_errors(foo_grammar)
print 'Done'
