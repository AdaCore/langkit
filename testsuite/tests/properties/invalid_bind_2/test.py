from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T, UserField
from langkit.expressions import Bind, Entity, Self, langkit_property
from langkit.parsers import Grammar

from utils import emit_and_print_errors


def run(name, equation_expr):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        var1 = UserField(type=T.LogicVarType, public=False)
        var2 = UserField(type=T.LogicVarType, public=False)

    class ExampleNode(FooNode):
        @langkit_property(public=True)
        def resolve():
            return equation_expr.solve

    grammar = Grammar('main_rule')
    grammar.add_rules(main_rule=ExampleNode('example'))
    emit_and_print_errors(grammar)
    print('')


# Valid cases
run('Bind(logic_var, logic_var)', Bind(Self.var1, Self.var2))
run('Bind(logic_var, entity)', Bind(Self.var1, Entity))

# Invalid cases
run('Bind(entity, logic_var)', Bind(Entity, Self.var1))
run('Bind(logic_var, bool)', Bind(Self.var1, True))
print('Done')
