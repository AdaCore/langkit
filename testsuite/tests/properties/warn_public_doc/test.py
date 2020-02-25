from __future__ import absolute_import, division, print_function

from langkit.diagnostics import WarningSet
from langkit.dsl import ASTNode
from langkit.expressions import AbstractKind, T, langkit_property

from utils import emit_and_print_errors


class FooNode(ASTNode):
    """
    Root node.
    """

    # This property is documented, so it should not have a warning
    @langkit_property(public=True, return_type=T.Bool,
                      kind=AbstractKind.abstract)
    def doc_prop():
        """
        This property is documented.
        """
        pass

    # This property is undocumented, so it should have a warning
    @langkit_property(public=True, return_type=T.Bool,
                      kind=AbstractKind.abstract)
    def undoc_prop():
        pass

    # This property is undocumented, so it should have a warning
    @langkit_property(public=True, return_type=T.Bool,
                      kind=AbstractKind.abstract)
    def will_doc_prop():
        pass


class Example(FooNode):
    """
    Example node.
    """

    # This property is undocumented but it inherits a documented one, so it
    # should not have a warning.
    @langkit_property(public=True)
    def doc_prop():
        return True

    # This property is undocumented, so it should have a warning
    @langkit_property(public=True)
    def undoc_prop():
        return True

    # This property is documented, so it should not have a warning
    @langkit_property(public=True)
    def will_doc_prop():
        """
        This property is documented.
        """
        return True


emit_and_print_errors(lkt_file='foo.lkt', warning_set=WarningSet())
print('Done')
