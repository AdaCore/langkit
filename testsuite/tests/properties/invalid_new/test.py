from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, LongType, Struct, UserField
from langkit.expressions import (Literal, New, No, Property, Self, Var,
                                 langkit_property)
from langkit.parsers import Grammar, Row

from os import path
from utils import emit_and_print_errors


def run(name, expr):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    global FooNode, BarNode, MyStruct

    Diagnostics.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))

    class MyStruct(Struct):
        a = UserField(type=LongType)
        b = UserField(type=LongType)

    class FooNode(ASTNode):
        pass

    class BarNode(FooNode):
        @langkit_property(public=True)
        def public_prop():
            struct = Var(Self.prop)
            return struct.a + struct.b

        prop = Property(expr)

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=Row('example') ^ BarNode,
    )
    emit_and_print_errors(grammar)
    print('')


run("Correct code", lambda: New(MyStruct, a=Literal(12), b=Literal(15)))
run("Incorrect new 1", lambda: New(MyStruct, a=Literal(12)))
run("Incorrect new 2", lambda: New(MyStruct, a=Literal(12), b=No(FooNode)))
run("Incorrect new 1", lambda: New(MyStruct, a=Literal(12), b=Literal(15),
                                   c=Literal(19)))
print('Done')
