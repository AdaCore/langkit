from langkit.compiled_types import (
    ASTNode, BoolType, Field, abstract, root_grammar_class
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import Literal, Property, Self
from langkit.parsers import Grammar, Or, Row, Tok

from lexer_example import Token
from os import path
from utils import emit_and_print_errors


def run(name, match_expr):
    """
    Emit and print the errors we get for the below grammar with "match_expr" as
    a property in ExampleNode.
    """

    global BodyNode, Compound, Expression, FooNode, NullNode, Number

    Diagnostics.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))

    @abstract
    @root_grammar_class()
    class FooNode(ASTNode):
        prop = Property(Literal(0))

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

    def lang_def():
        foo_grammar = Grammar('main_rule')
        foo_grammar.add_rules(
            main_rule=Row(
                'example',
                Or(foo_grammar.expression,
                   Row('null') ^ NullNode)
            ) ^ ExampleNode,

            number=Tok(Token.Number, keep=True) ^ Number,

            expression=Or(
                Row(foo_grammar.number,
                    ',',
                    foo_grammar.expression) ^ Compound,
                foo_grammar.number
            ),
        )
        return foo_grammar

    emit_and_print_errors(lang_def)
    print('')


# Incomplete set of matchers
run('Missing NullNode', lambda: Self.body.match(
    lambda e=Expression: e.prop,
))
run('Missing Expression', lambda: Self.body.match(
    lambda e=NullNode: e.prop,
))
run('Missing Compound', lambda: Self.body.match(
    lambda e=NullNode: e.prop,
    lambda e=Number:   e.prop,
))

# Invalid matchers
run('Invalid type', lambda: Self.body.match(
    lambda e=NullNode:   e.prop,
    lambda e=Expression: e.prop,
    lambda e=BoolType:   e.prop,
))
run('Irrelevant AST node', lambda: Self.body.match(
    lambda e=NullNode:   e.prop,
    lambda e=Expression: e.prop,
    lambda e=FooNode:    e.prop,
))

# Unreachable matchers
run('Default case after full coverage', lambda: Self.body.match(
    lambda e=BodyNode: e.prop,
    lambda _:          Literal(1),
))

run('Node after default case (1)', lambda: Self.body.match(
    lambda _:          Literal(0),
    lambda e=BodyNode: e.prop,
))
run('Node after default case (2)', lambda: Self.body.match(
    lambda _:        Literal(0),
    lambda e=Number: e.prop,
))

run('Node after full coverage (1)', lambda: Self.body.match(
    lambda e=BodyNode: e.prop,
    lambda e=Number:   e.prop,
))
run('Node after full coverage (2)', lambda: Self.body.match(
    lambda e=NullNode: e.prop,
    lambda e=Number:   e.prop,
    lambda e=Compound: e.prop,
    lambda e=BodyNode: e.prop,
))

print 'Done'
