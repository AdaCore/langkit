"""
"""
from langkit.dsl import ASTNode
from langkit.expressions import langkit_property

from utils import emit_and_print_errors


class FooNode(ASTNode):
    """
    Docstring with `invalid_title_ref`.
    """

    @langkit_property(public=True)
    def prop():
        """
        Docstring with `invalid_title_ref`.
        """
        return 1

    @langkit_property(public=True)
    def prop2():
        """
        Docstring with an `rst error in it.
        """
        return 2

    @langkit_property(public=True)
    def prop3():
        """
        Docstring with an unsupported block.

        >>> 1 + 1
        """
        return 3


class Example(FooNode):
    pass


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
