"""
Check that ".to_symbol" works as expected.
"""

from langkit.compile_context import LibraryEntity
from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True

    @langkit_property(public=True, return_type=T.Bool)
    def compare_as_symbols(s1=T.String, s2=T.String):
        return s1.to_symbol == s2.to_symbol


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    ada_main='main.adb',
    symbol_canonicalizer=LibraryEntity('Libfoolang.Helpers',
                                       'Canonicalize_Symbol')
)
print('Done')
