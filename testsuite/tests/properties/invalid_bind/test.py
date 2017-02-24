from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from langkit.compiled_types import (
    ASTNode, root_grammar_class, LogicVarType, UserField, T
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import Bind, Let, Property, Self, langkit_property
from langkit.parsers import Grammar, Row, Or

from os import path
from utils import emit_and_print_errors


def run(name, prop_expr):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    Diagnostics.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))

    prop = None

    @root_grammar_class()
    class FooNode(ASTNode):
        ref_var = UserField(LogicVarType, public=False)
        type_var = UserField(LogicVarType, public=False)

    class BarNode(FooNode):

        @langkit_property(public=False)
        def main_prop():
            return Bind(Self.type_var, Self.ref_var, eq_prop=prop)

        wrapper = Property(Let(lambda _=Self.main_prop: Self), public=True)

    class BazNode(FooNode):
        prop = Property(12, public=True)
        prop2 = Property(True, public=True)
        prop3 = Property(lambda _=T.BarNode: True, public=True)
        prop4 = Property(lambda other=T.BazNode: Self == other, public=True)

    def lang_def():
        foo_grammar = Grammar('main_rule')
        foo_grammar.add_rules(
            main_rule=Or(
                Row('example') ^ BarNode,
                Row('example') ^ BazNode,
            )
        )
        return foo_grammar

    prop = eval(prop_expr)
    emit_and_print_errors(lang_def)
    print('')


run("Incorrect bind eq_prop 1", "BazNode.fields.prop")
run("Incorrect bind eq_prop 2", "BazNode.fields.prop2")
run("Incorrect bind eq_prop 3", "BazNode.fields.prop3")
run("Correct bind eq_prop", "BazNode.fields.prop4")
print('Done')
