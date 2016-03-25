from langkit.compiled_types import (
    ASTNode, BoolType, Field, abstract, root_grammar_class
)
from langkit.diagnostics import LangSourceDir
from langkit.expressions import Literal, Match, Property, Self
from langkit.parsers import Grammar, Or, Row, Tok

from lexer_example import Token, foo_lexer
from os import path
from utils import emit_and_print_errors, reset_langkit


def run(name, match_expr):
    """
    Emit and print the errors we get for the below grammar with "match_expr" as
    a property in ExampleNode.
    """

    global Compound, Expression, FooNode, NullNode, Number

    LangSourceDir.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))
    reset_langkit()

    @abstract
    @root_grammar_class
    class FooNode(ASTNode):
        pass

    @abstract
    class BodyNode(FooNode):
        pass

    class NullNode(BodyNode):
        pass

    @abstract
    class Expression(BodyNode):
        pass

    class Number(Expression):
        tok = Field()

    class Compound(Expression):
        prefix = Field()
        suffix = Field()

    class ExampleNode(FooNode):
        body = Field()

        prop = Property(match_expr)

    foo_grammar = Grammar()
    foo_grammar.main_rule_name = 'main_rule'
    foo_grammar.add_rules(
        main_rule=Row(
            'example',
            Or(foo_grammar.expression,
               Row('null') ^ NullNode)
        ) ^ ExampleNode,

        number=Tok(Token.Number) ^ Number,

        expression=Or(
            Row(foo_grammar.number,
                ',',
                foo_grammar.expression) ^ Compound,
            foo_grammar.number
        ),
    )

    emit_and_print_errors(foo_grammar)
    print('')


run('Missing NullNode', lambda: Self.body.match(
    lambda e=Expression: Literal(0),
))
run('Missing Expression', lambda: Self.body.match(
    lambda e=NullNode: Literal(0),
))
run('Missing Compound', lambda: Self.body.match(
    lambda e=NullNode: Literal(0),
    lambda e=Number:   Literal(1),
))
run('Invalid type', lambda: Self.body.match(
    lambda e=NullNode:   Literal(0),
    lambda e=Expression: Literal(1),
    lambda e=BoolType:   Literal(1),
))
run('Irrelevant AST node', lambda: Self.body.match(
    lambda e=NullNode:   Literal(0),
    lambda e=Expression: Literal(1),
    lambda e=FooNode:    Literal(1),
))

print 'Done'
