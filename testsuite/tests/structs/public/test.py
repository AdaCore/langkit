"""
Test that structure types are properly bound in public APIs.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import (ArrayLiteral, BigIntLiteral, New, Self,
                                 langkit_property)
from langkit.parsers import Grammar, List

from utils import build_and_run


class FooNode(ASTNode):
    pass


class MyStruct(Struct):
    entity_field = UserField(type=T.FooNode)
    array_field = UserField(type=T.FooNode.entity.array)
    bigint_field = UserField(type=T.BigInt)


class Example(FooNode):
    token_node = True

    @langkit_property(public=True)
    def get_struct():
        return New(MyStruct,
                   entity_field=Self,
                   array_field=ArrayLiteral([
                       Self.cast(T.FooNode).as_bare_entity,
                       Self.parent.as_bare_entity]),
                   bigint_field=BigIntLiteral(10**100))

    @langkit_property(public=True)
    def struct_identity(s=MyStruct):
        return s


grammar = Grammar('main_rule')
grammar.add_rules(main_rule=List(Example('example')))

build_and_run(grammar, py_script='main.py', ada_main='main.adb')
print('')
print('Done')
