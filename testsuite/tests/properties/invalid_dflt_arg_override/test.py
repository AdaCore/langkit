from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Bool
from langkit.expressions import AbstractKind, langkit_property
from langkit.parsers import Grammar

from utils import emit_and_print_errors


def run(name, arg_parent, arg_overriding):
    """
    Emit and print the errors we get for the below grammar with "arg_parent"
    and "arg_overriding" "arg" as the first argument in FooNode.prop and
    Example.prop.
    """

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        @langkit_property(public=True, return_type=Bool,
                          kind=AbstractKind.abstract)
        def prop(a=arg_parent):
            pass

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


run('Missing default value', (Bool, True), Bool)
run('Extra default value',   Bool,         (Bool, True))
run('Wrong value',           (Bool, True), (Bool, False))
run('Correct',               (Bool, True), (Bool, True))

print('Done')
