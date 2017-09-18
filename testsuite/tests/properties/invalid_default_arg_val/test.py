from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, BoolType, T
from langkit.expressions import DynamicVariable, Property, Self
from langkit.parsers import Grammar

from os import path
from utils import emit_and_print_errors


dyn_var = DynamicVariable('dyn_var', BoolType)


def run(name, expr, calling_expr=True):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    Diagnostics.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        p = Property(expr)
        calling = Property(calling_expr)

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=Example('example'),
    )
    emit_and_print_errors(grammar)
    print('')


# Test proper validation for default arguments
run('Bad tuple', lambda a=(BoolType, ): a)
run('Bad expr', lambda a=(BoolType, BoolType): a)
run('Bad type', lambda a=(BoolType, 1): a)
run('Bad Self use', lambda a=(T.FooNode, Self): a)
run('Bad dynamic variable use', lambda a=(BoolType, dyn_var): a)

# Test proper validation for argument passing with default arguments
run('Missing argument (1)',
    lambda a=BoolType, b=(BoolType, True): a.and_then(b),
    Self.p())
run('Missing argument (2)',
    lambda a=BoolType, b=(BoolType, True): a.and_then(b),
    Self.p(b=True))

run('Bad keyword argument',
    lambda a=BoolType, b=(BoolType, True): a.and_then(b),
    Self.p(a=1, b=True))

run('Too many arguments (1)',
    lambda a=BoolType, b=(BoolType, True): a.and_then(b),
    Self.p(True, False, b=False))
run('Too many arguments (2)',
    lambda a=BoolType, b=(BoolType, True): a.and_then(b),
    Self.p(True, a=False, b=False))
run('Too many arguments (3)',
    lambda a=BoolType, b=(BoolType, True): a.and_then(b),
    Self.p(True, False, c=False))
run('Too many arguments (4)',
    lambda a=BoolType, b=(BoolType, True): a.and_then(b),
    Self.p(True, False, 1, a=False))

print('Done')
