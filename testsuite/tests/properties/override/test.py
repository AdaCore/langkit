"""
Test that when a property B overrides a property A, B's return type must be a
subtype of A's.
"""

import os.path

from langkit.compiled_types import (
    ASTNode, TypeRepo, abstract, root_grammar_class
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import AbstractProperty, No, Property
from langkit.parsers import Grammar, Or, Row

from utils import emit_and_print_errors, reset_langkit


def run(name, astnode_fn):
    """
    Emit and print the errors we get for the below grammar with "match_expr" as
    a property in ExampleNode.
    """

    Diagnostics.set_lang_source_dir(os.path.abspath(__file__))

    print('== {} =='.format(name))
    reset_langkit()

    T = TypeRepo()
    astnode = astnode_fn(T)

    @abstract
    @root_grammar_class
    class FooNode(ASTNode):
        pass

    @abstract
    class MiddleNode(FooNode):
        get_random_node = AbstractProperty(type=T.MiddleNode)

    class ExampleNode(MiddleNode):
        get_random_node = Property(No(astnode))

    @abstract
    class NullNode(FooNode):
        pass

    def lang_def():
        foo_grammar = Grammar('main_rule')
        foo_grammar.add_rules(
            main_rule=Or(Row('example') ^ ExampleNode,
                         Row('null') ^ NullNode)
        )
        return foo_grammar

    emit_and_print_errors(lang_def)
    print('')


# Incomplete set of matchers
run('FooNode', lambda T: T.FooNode)
run('MiddleNode', lambda T: T.MiddleNode)
run('NullNode', lambda T: T.NullNode)

print 'Done'
