"""
Check that precise types for fields of synthetized nodes account for types
coming from synthetization properties.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, T, abstract, synthetic
from langkit.expressions import AbstractKind, New, Self, langkit_property
from langkit.parsers import Grammar, Or

from lexer_example import Token
from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


@abstract
class AbstractHolder(FooNode):
    f = Field(type=T.Expr)


class ParsedHolder(AbstractHolder):
    pass


# Case of a field that only synthetic nodes can hold
@synthetic
class SynthNode(FooNode):
    f = Field(type=T.Expr)


# Case of a field that both synthetic and parsed nodes can hold
@synthetic
class SynthHolder(AbstractHolder):
    pass


@abstract
class Expr(FooNode):

    @langkit_property(public=True, return_type=FooNode,
                      kind=AbstractKind.abstract)
    def synth():
        pass


class Literal(Expr):
    token_node = True

    @langkit_property(memoized=True)
    def synth():
        return New(SynthNode, f=Self)

    @langkit_property(public=True, memoized=True)
    def holder():
        return New(SynthHolder, f=Self)


class Name(Expr):
    token_node = True

    @langkit_property(memoized=True)
    def synth():
        return New(SynthNode, f=Self)


g = Grammar('main_rule')
g.add_rules(
    main_rule=Or(g.literal, g.name, g.holder),
    literal=Literal(Token.Number),
    name=Name(Token.Identifier),
    holder=ParsedHolder('(', g.name, ')')
)
ctx = emit_and_print_errors(g)
nodes = {n.dsl_name: n for n in ctx.astnode_types}

for node_name in ['SynthNode', 'AbstractHolder']:
    node = nodes[node_name]
    fields = {f.original_name.lower: f for f in node.get_fields()}
    f = fields['f']
    print('Precise types for {}:'.format(f.qualname))
    for t in f.precise_types.minimal_matched_types:
        print('  * {}'.format(t.dsl_name))

print('Done')
