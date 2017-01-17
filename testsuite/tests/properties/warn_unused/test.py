from langkit.compiled_types import (
    ASTNode, BoolType, Field, T, root_grammar_class
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import (
    Let, Property, Self, Var, langkit_property, ignore
)
from langkit.parsers import Grammar, List, Or, Row

from os import path
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    pass


class Example(FooNode):
    prop = Property(True)


class ExampleList(FooNode):
    items = Field()

    @langkit_property()
    def unused_param(ex_param=T.FooNode, ex_used_param=T.FooNode):
        ignore(ex_used_param)
        return Self.items.at(0)

    @langkit_property()
    def unused_block_var():
        ex_var = Var(Self)
        # The whole point of this is to test that ex_var is properly flagged by
        # langkit as being unused.
        del ex_var
        return Self.items

    @langkit_property()
    def unused_let_var():
        return Let(lambda ex_list=Self.items: Self.items)

    unused_loop_var = Property(
        Self.items.map(lambda ex_item: True)
    )

    unused_then_var = Property(
        Self.items.then(lambda ex_items: Self.items.at(0))
    )

    unused_match_var = Property(
        Self.items.map(
            lambda item: item.match(
                lambda ex=T.Example: True,
                lambda ex_list=T.ExampleList:
                    ex_list.unused_match_var.all(lambda b: b),
                lambda _: False,
            )
        ),
        type=BoolType.array_type(),
    )


def lang_def():
    foo_grammar = Grammar('item')
    foo_grammar.add_rules(
        item=Or(foo_grammar.example, foo_grammar.example_list),
        example=Row('example') ^ Example,
        example_list=ExampleList(
            '(', List(foo_grammar.item), ')'
        )
    )
    return foo_grammar


emit_and_print_errors(lang_def)
print 'Done'
