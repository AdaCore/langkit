import itertools
import os.path

from langkit.compiled_types import (
    ASTNode, BoolType, root_grammar_class
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import AbstractProperty, Literal, Property
from langkit.parsers import Grammar, Row

from utils import emit_and_print_errors


def run(abstract_has_implicit_env, concrete_has_implicit_env):
    """
    Emit and print the errors we get for the below grammar for the given
    "has_implicit_env" attribute values.
    """

    fmt_value = {
        None: 'default',
        True: 'implicit env',
        False: 'no implicit env',
    }
    print '== abstract: {}, concrete: {} =='.format(
        fmt_value[abstract_has_implicit_env],
        fmt_value[concrete_has_implicit_env]
    )
    Diagnostics.set_lang_source_dir(os.path.abspath(__file__))

    @root_grammar_class()
    class RootNode(ASTNode):
        pass

    class AbstractNode(RootNode):
        prop = AbstractProperty(BoolType,
                                has_implicit_env=abstract_has_implicit_env,
                                public=True)

    class ConcreteNode(AbstractNode):
        prop = Property(Literal(True),
                        has_implicit_env=concrete_has_implicit_env)

    def lang_def():
        foo_grammar = Grammar('main_rule')
        foo_grammar.add_rules(
            main_rule=Row('example') ^ ConcreteNode,
        )
        return foo_grammar

    if emit_and_print_errors(lang_def):
        for fld in (AbstractNode._fields['prop'],
                    ConcreteNode._fields['prop']):
            print '  {}: {}'.format(fld.qualname,
                                    fmt_value[fld.has_implicit_env])
    print('')


attribute_values = (None, True, False)
for p1, p2 in itertools.product(attribute_values, attribute_values):
    run(p1, p2)

print 'Done'
