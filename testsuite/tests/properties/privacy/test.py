from __future__ import absolute_import, division, print_function

import itertools
import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, BoolType, abstract
from langkit.expressions import AbstractProperty, Literal, Property, Self
from langkit.parsers import Grammar

from utils import emit_and_print_errors


def run(abstract_public, concrete_public):
    """
    Emit and print the errors we get for the below grammar for the given
    privacy levels.
    """

    fmt_privacy = {
        None: 'default',
        True: 'public',
        False: 'private',
    }
    print('== abstract: {}, concrete: {} =='.format(
        fmt_privacy[abstract_public],
        fmt_privacy[concrete_public]
    ))
    Diagnostics.set_lang_source_dir(os.path.abspath(__file__))

    class RootNode(ASTNode):
        pass

    @abstract
    class AbstractNode(RootNode):
        prop = AbstractProperty(BoolType, public=abstract_public)

        public_prop = Property(Self.prop, public=True)

    class ConcreteNode(AbstractNode):
        prop = Property(Literal(True), public=concrete_public)

        public_prop = Property(Self.prop, public=True)

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=ConcreteNode('example'),
    )

    if emit_and_print_errors(grammar):
        for fld in (AbstractNode.prop, ConcreteNode.prop):
            print('  {}: {}'.format(fld.qualname, fmt_privacy[fld.is_public]))
    print('')


privacy_levels = (None, True, False)
for p1, p2 in itertools.product(privacy_levels, privacy_levels):
    run(p1, p2)

print('Done')
