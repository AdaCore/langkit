from __future__ import absolute_import, division, print_function

from langkit.dsl import AnalysisUnitKind, ASTNode, T
from langkit.expressions import Try, langkit_property
from langkit.parsers import Grammar

from utils import emit_and_print_errors


def run(name, expr):
    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):

        @langkit_property(public=True, return_type=T.Bool)
        def p():
            return expr

    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(
        main_rule=Example('example'),
    )
    emit_and_print_errors(foo_grammar)
    print('')

run('Heterogeneous types', Try(1, True))
run('Not nullable type', Try(AnalysisUnitKind.unit_body))
print('Done')
