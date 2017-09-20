from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics, WarningSet
from langkit.dsl import ASTNode
from langkit.expressions import langkit_property
from langkit.parsers import Grammar

from os import path
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(path.abspath(__file__))


class FooNode(ASTNode):

    # This property is documented, so it should not have a warning
    @langkit_property(public=True)
    def doc_prop():
        """
        This property is documented.
        """
        return True

    # This property is undocumented, so it should have a warning
    @langkit_property(public=True)
    def undoc_prop():
        return True

    # This property is undocumented, so it should have a warning
    @langkit_property(public=True)
    def will_doc_prop():
        return True


class Example(FooNode):

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


grammar = Grammar('item')
grammar.add_rules(item=Example('example'))
emit_and_print_errors(grammar, warning_set=WarningSet())
print('Done')
