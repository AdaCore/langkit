"""
Test that OCaml API is properly working.
"""

from __future__ import absolute_import, division, print_function

from langkit.compile_context import LibraryEntity
from langkit.dsl import (
    ASTNode, Enum, EnumValue, Field, Symbol, T, has_abstract_list
)
from langkit.expressions import (
    ArrayLiteral, CharacterLiteral, Entity, Property, langkit_property
)
from langkit.parsers import Grammar, List, Or

from lexer_example import Token
from utils import build_and_run


class Color(Enum):
    Red = EnumValue()
    Green = EnumValue()
    Blue = EnumValue()


@has_abstract_list
class FooNode(ASTNode):

    @langkit_property(public=True)
    def count(seq=T.Example.entity.array):
        return seq.length

    @langkit_property(public=True)
    def get_a(c=(T.Character, CharacterLiteral('a'))):
        return c

    @langkit_property(public=True)
    def get_eacute(c=(T.Character, CharacterLiteral(u'\xe9'))):
        return c

    @langkit_property(public=True)
    def identity(c=T.Character):
        return c

    @langkit_property(public=True)
    def double(c=T.Character):
        return ArrayLiteral([c, c], T.Character)

    @langkit_property(public=True)
    def same_color(c=Color):
        return c


class Sequence(FooNode.list):
    all_items = Property(Entity.map(lambda i: i), public=True)
    example_items = Property(Entity.filtermap(
        lambda i: i.cast_or_raise(T.Example),
        lambda i: i.is_a(T.Example)
    ), public=True)


class Example(FooNode):
    pass


class Null(FooNode):
    pass


class Var(FooNode):
    arg = Field(type=Sequence)


class Ident(FooNode):
    token_node = True

    @langkit_property(public=True, return_type=Symbol)
    def sym(sym=Symbol):
        return sym


class StringLiteral(FooNode):
    pass

foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(foo_grammar.node, list_cls=Sequence),
    node=Or(foo_grammar.example, foo_grammar.null, foo_grammar.var,
            foo_grammar.ident, foo_grammar.string),
    example=Example('example'),
    null=Null('null'),
    var=Var('var', '(', foo_grammar.main_rule, ')'),
    ident=Ident(Token.Identifier),
    string=StringLiteral(Token.String)
)

build_and_run(foo_grammar, ocaml_main='main',
              symbol_canonicalizer=LibraryEntity('Pkg', 'Canonicalize'))
print('Done')
