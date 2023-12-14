"""
Test that Symbol bindings in the Python API are properly working.
"""

from langkit.compile_context import LibraryEntity
from langkit.dsl import ASTNode, Symbol
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True

    @langkit_property(public=True, return_type=Symbol)
    def sym(sym=Symbol):
        return sym


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    symbol_canonicalizer=LibraryEntity('Pkg', 'Canonicalize'),
    types_from_lkt=True,
)
print('Done')
