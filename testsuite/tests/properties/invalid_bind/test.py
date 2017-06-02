from __future__ import absolute_import, division, print_function

from langkit.compiled_types import (
    ASTNode, LexicalEnvType, root_grammar_class, LogicVarType, UserField, T
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import (Bind, DynamicVariable, Property, Self, Var,
                                 langkit_property, ignore)
from langkit.parsers import Grammar, Row, Or

from os import path
from utils import emit_and_print_errors


Env = DynamicVariable('env', LexicalEnvType)


def run(name, prop_expr):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    Diagnostics.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))

    prop = eval(prop_expr)

    @root_grammar_class()
    class FooNode(ASTNode):
        ref_var = UserField(LogicVarType, public=False)
        type_var = UserField(LogicVarType, public=False)

    class BarNode(FooNode):
        main_prop = Property(Bind(Self.type_var, Self.ref_var, eq_prop=prop))

        @langkit_property(public=True)
        def wrapper():
            _ = Var(Self.main_prop)
            ignore(_)
            return Self

    class BazNode(FooNode):
        prop = Property(12, warn_on_unused=False)
        prop2 = Property(True, warn_on_unused=False)

        @langkit_property(warn_on_unused=False)
        def prop3(_=T.BarNode):
            return True

        @langkit_property(warn_on_unused=False)
        def prop4(other=T.BazNode.entity()):
            return Self.as_entity == other

        @langkit_property(warn_on_unused=False, dynamic_vars=[Env])
        def prop5(other=T.BazNode.entity()):
            return other.node_env == Env

    def lang_def():
        foo_grammar = Grammar('main_rule')
        foo_grammar.add_rules(
            main_rule=Or(
                Row('example') ^ BarNode,
                Row('example') ^ BazNode,
            )
        )
        return foo_grammar

    emit_and_print_errors(lang_def)
    print('')


run('Incorrect bind eq_prop 1', 'T.BazNode.fields.prop')
run('Incorrect bind eq_prop 2', 'T.BazNode.fields.prop2')
run('Incorrect bind eq_prop 3', 'T.BazNode.fields.prop3')
run('Correct bind eq_prop 4', 'T.BazNode.fields.prop4')
run('Correct bind eq_prop 5', 'T.BazNode.fields.prop5')
print('Done')
