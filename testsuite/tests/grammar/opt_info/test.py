"""
Test that the is_optional property on parse fields works as expected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.parsers import Grammar, Opt, Or

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    pass


class ExampleWrapper(FooNode):
    opt_field = Field()
    non_opt_field = Field()


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Or(foo_grammar.rule_1, foo_grammar.rule_2),
    rule_1=ExampleWrapper(Opt(Example("example")), Example("example")),
    rule_2=ExampleWrapper(Example("example"), Example("example"))
)

emit_and_print_errors(foo_grammar)

for field in (ExampleWrapper.opt_field, ExampleWrapper.non_opt_field):
    print("Field {} is {}".format(
        field,
        "optional" if field.is_optional else "not optional"
    ))

print('Done')
