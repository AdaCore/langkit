"""
Test the "unique" array operation in the DSL (valid use).
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True

    @langkit_property(public=True, return_type=T.AnalysisUnit.array)
    def test(a=T.AnalysisUnit.array):
        return a.unique


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              lkt_semantic_checks=True)
print('Done')
