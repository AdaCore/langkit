from __future__ import absolute_import, division, print_function

from langkit.compiled_types import ASTNode, root_grammar_class, LongType
from langkit.diagnostics import Diagnostics
from langkit.expressions import Property
from langkit.parsers import Grammar, Row

from os import path
from utils import emit_and_print_errors


def run(name, prop_lambda):
    """
    Emit and print the errors we get for the below grammar with `prop_lambda`
    as the `expr` argument for a Property.
    """

    global FooNode

    Diagnostics.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))

    @root_grammar_class()
    class FooNode(ASTNode):
        pass

    class BarNode(FooNode):
        prop = Property(prop_lambda, warn_on_unused=False)

    def lang_def():
        foo_grammar = Grammar('main_rule')
        foo_grammar.add_rules(main_rule=Row('example') ^ BarNode)
        return foo_grammar

    emit_and_print_errors(lang_def)
    print('')


run("Correct code", lambda: True)
run("Incorrect property definition 1", ["Lol this is obviously wrong"])
run("Incorrect property definition 2", lambda x, *y, **z: "pouet")
run("Incorrect property definition 3", lambda x, y=LongType: x)
run("Incorrect property definition 4",
    lambda Node=LongType, Lex_Env=LongType: Node)
run("Incorrect property definition 5", lambda a=["Obviously wrong"]: a)
run("Incorrect property definition 6", lambda a=Diagnostics: a)

print('Done')
