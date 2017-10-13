"""
Test that calling a property on a null node literal generates valid Ada code.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, T
from langkit.expressions import No, langkit_property
from langkit.parsers import Grammar

from utils import build


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):

    @langkit_property(public=True)
    def eval_unit():
        return No(T.FooNode).prop

    @langkit_property()
    def prop():
        return True


class Example(FooNode):
    pass


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Example('example'),
)
build(foo_grammar)
print('Done')
