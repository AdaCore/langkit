from __future__ import absolute_import, division, print_function

import itertools

from langkit.dsl import ASTNode, Bool, abstract
from langkit.expressions import AbstractProperty, Literal, Property, Self

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

    class RootNode(ASTNode):
        pass

    @abstract
    class AbstractNode(RootNode):
        prop = AbstractProperty(Bool, public=abstract_public)

        public_prop = Property(Self.prop, public=True)

    class ConcreteNode(AbstractNode):
        prop = Property(Literal(True), public=concrete_public)

        public_prop = Property(Self.prop, public=True)

    class OtherConcreteNode(AbstractNode):
        prop = Property(False)

    if emit_and_print_errors(lkt_file='foo.lkt'):
        for fld in (AbstractNode.prop, ConcreteNode.prop):
            print('  {}: {}'.format(fld.qualname,
                                    fmt_privacy[fld.original_is_public]))
    print('')


privacy_levels = (None, True, False)
for p1, p2 in itertools.product(privacy_levels, privacy_levels):
    run(p1, p2)

print('Done')
