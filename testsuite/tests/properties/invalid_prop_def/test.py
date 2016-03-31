from langkit.compiled_types import (
    ASTNode, root_grammar_class, NodeMacro, LongType
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import Property
from langkit.parsers import Grammar, Row

from os import path
from utils import emit_and_print_errors, reset_langkit


def run(name, *args):
    """
    Emit and print the errors we get for the below grammar with *args as
    a list of NodeMacro classes to use on BarNode.

    This will not only check the consistency of Property diagnostics, but also
    that the SLOCs generated for NodeMacros are good, ie. they will reference
    the original definition site.
    """

    global FooNode

    Diagnostics.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))
    reset_langkit()

    @root_grammar_class
    class FooNode(ASTNode):
        pass

    class BarNode(FooNode):
        _macros = args

    foo_grammar = Grammar()
    foo_grammar.main_rule_name = 'main_rule'
    foo_grammar.add_rules(main_rule=Row('example') ^ BarNode)
    emit_and_print_errors(foo_grammar)
    print('')


run("Correct code")


class Macro1(NodeMacro):
    propyprop = Property(["Lol this is obviously wrong"])
run("Incorrect property definition 1", Macro1)


class Macro2(NodeMacro):
    propyprop = Property(lambda x, *y, **z: "pouet")
run("Incorrect property definition 2", Macro2)


class Macro3(NodeMacro):
    propyprop = Property(lambda x, y=LongType: x)
run("Incorrect property definition 3", Macro3)


class Macro4(NodeMacro):
    propyprop = Property(lambda Node=LongType, Lex_Env=LongType: Node)
run("Incorrect property definition 4", Macro4)


class Macro5(NodeMacro):
    propyprop = Property(lambda a=["Obviously wrong"]: a)
run("Incorrect property definition 5", Macro5)


class Macro6(NodeMacro):
    propyprop = Property(lambda a=NodeMacro: a)
run("Incorrect property definition 6", Macro6)

print 'Done'
