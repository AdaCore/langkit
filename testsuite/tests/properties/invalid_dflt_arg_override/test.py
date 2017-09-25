from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, BoolType
from langkit.expressions import langkit_property
from langkit.parsers import Grammar

from os import path
from utils import emit_and_print_errors


def run(name, arg_parent, arg_overriding):
    """
    Emit and print the errors we get for the below grammar with "arg_parent"
    and "arg_overriding" "arg" as the first argument in FooNode.prop and
    Example.prop.
    """

    Diagnostics.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        @langkit_property(public=True)
        def prop(a=arg_parent):
            return a

    class Example(FooNode):
        @langkit_property()
        def prop(a=arg_overriding):
            return a

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=Example('example'),
    )
    emit_and_print_errors(grammar)
    print('')


run('Missing default value', (BoolType, True), BoolType)
run('Extra default value',   BoolType,         (BoolType, True))
run('Wrong value',           (BoolType, True), (BoolType, False))
run('Correct',               (BoolType, True), (BoolType, True))

print('Done')
