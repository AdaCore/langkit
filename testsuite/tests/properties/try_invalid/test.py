from langkit.dsl import ASTNode, AnalysisUnitKind, T
from langkit.expressions import Try, langkit_property

from utils import emit_and_print_errors


def run(name, expr):
    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):

        @langkit_property(public=True, return_type=T.Bool)
        def p():
            return expr

    emit_and_print_errors(lkt_file='foo.lkt')
    print('')

run('Heterogeneous types', Try(1, True))
run('Not nullable type', Try(AnalysisUnitKind.unit_body))
print('Done')
