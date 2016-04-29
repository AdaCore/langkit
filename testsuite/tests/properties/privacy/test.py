import itertools
import os.path

from langkit.compiled_types import (
    ASTNode, BoolType, Field, root_grammar_class
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import AbstractProperty, Literal, Property
from langkit.parsers import Grammar, Row

from lexer_example import Token, foo_lexer
from utils import emit_and_print_errors, reset_langkit


def run(abstract_private, concrete_private):
    """
    Emit and print the errors we get for the below grammar for the given
    privacy levels.
    """

    fmt_privacy = {
        None: 'default',
        True: 'private',
        False: 'public',
    }
    print '== abstract: {}, concrete: {} =='.format(
        fmt_privacy[abstract_private],
        fmt_privacy[concrete_private]
    )
    reset_langkit()
    Diagnostics.set_lang_source_dir(os.path.abspath(__file__))

    @root_grammar_class
    class AbstractNode(ASTNode):
        prop = AbstractProperty(BoolType, private=abstract_private)

    class ConcreteNode(AbstractNode):
        prop = Property(Literal(True), private=concrete_private)

    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(
        main_rule=Row('example') ^ ConcreteNode,
    )
    if emit_and_print_errors(foo_grammar):
        for fld in (AbstractNode._fields['prop'],
                    ConcreteNode._fields['prop']):
            print '  {}: {}'.format(fld.qualname, fmt_privacy[fld.is_private])
    print('')


privacy_levels = (None, True, False)
for p1, p2 in itertools.product(privacy_levels, privacy_levels):
    run(p1, p2)

print 'Done'
