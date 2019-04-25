"""
Test that the is_optional property on parse fields works as expected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.parsers import DontSkip, Grammar, Null, Opt, Or

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    pass


class ExampleWrapper(FooNode):
    # Optional fields
    field_opt = Field()
    field_or = Field()
    field_defer = Field()
    field_null = Field()
    field_dont_skip = Field()

    # Non optional fields
    field_opt_bool = Field()
    field_transform = Field()


class HasExample(FooNode):
    enum_node = True
    qualifier = True


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Or(foo_grammar.rule_1, foo_grammar.rule_2),

    rule_1=ExampleWrapper(
        Opt(Example("example")),
        Or(Example("example"), Null(Example)),
        foo_grammar.sub_rule,
        Null(Example),
        DontSkip(Opt(Example("example"))),

        Opt(Example("example")).as_bool(HasExample),
        Example("example")
    ),
    rule_2=ExampleWrapper(
        Example("example"),
        Example("example"),
        Example("example"),
        Example("example"),
        Example("example"),

        HasExample("example"),
        Example("example")
    ),

    sub_rule=Opt(Example("example"))
)

emit_and_print_errors(foo_grammar)

fields = [
    ExampleWrapper.field_opt,
    ExampleWrapper.field_or,
    ExampleWrapper.field_defer,
    ExampleWrapper.field_null,
    ExampleWrapper.field_dont_skip,

    ExampleWrapper.field_opt_bool,
    ExampleWrapper.field_transform
]

for field in fields:
    print("Field {} is {}".format(
        field,
        "optional" if field.is_optional else "not optional"
    ))

print('Done')
