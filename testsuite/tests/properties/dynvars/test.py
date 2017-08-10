from __future__ import absolute_import, division, print_function

import itertools
import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, LexicalEnvType, BoolType, abstract
from langkit.expressions import (AbstractProperty, DynamicVariable, Literal,
                                 Property, Self)
from langkit.parsers import Grammar, Row

from utils import emit_and_print_errors


Env = DynamicVariable('env', LexicalEnvType)


def run(abstract_dyn_vars, concrete_dyn_vars):
    """
    Emit and print the errors we get for the below grammar for the given
    abstract property/concrete property dynamic variables.
    """

    def fmt_value(dyn_var):
        if dyn_var is None:
            return 'default'
        elif dyn_var == []:
            return 'no dynamic variable'
        else:
            return 'with dynamic variables'

    print('== abstract: {}, concrete: {} =='.format(
        fmt_value(abstract_dyn_vars),
        fmt_value(concrete_dyn_vars)
    ))
    Diagnostics.set_lang_source_dir(os.path.abspath(__file__))

    class RootNode(ASTNode):
        pass

    @abstract
    class AbstractNode(RootNode):
        prop = AbstractProperty(BoolType, dynamic_vars=abstract_dyn_vars)

        use_prop = Property(Env.bind(Self.node_env, Self.prop), public=True)

    class ConcreteNode(AbstractNode):
        prop = Property(Literal(True), dynamic_vars=concrete_dyn_vars)

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=Row('example') ^ ConcreteNode,
    )

    if emit_and_print_errors(grammar):
        for fld in (AbstractNode.prop, ConcreteNode.prop):
            print('  {}: {}'.format(fld.qualname,
                                    fmt_value(fld.dynamic_vars)))
    Env.unfreeze()
    print('')


dynvars_values = (None, [Env], [])
for p1, p2 in itertools.product(dynvars_values, dynvars_values):
    run(p1, p2)

print('Done')
