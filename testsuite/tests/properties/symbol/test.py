"""
Test that ".symbol" raises a property error on null nodes or when symbol
canonicalization fails.
"""

from langkit.compile_context import LibraryEntity
from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True)
    def prop(e=T.Example):
        return e.symbol


class Example(FooNode):
    token_node = True


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              symbol_canonicalizer=LibraryEntity('Pkg', 'Canonicalize'))
print('Done')
