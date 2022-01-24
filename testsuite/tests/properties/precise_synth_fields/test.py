"""
Check that precise types for fields of synthetized nodes account for types
coming from synthetization properties.
"""

from langkit.dsl import (
    ASTNode, Field, T, abstract, has_abstract_list, synthetic
)
from langkit.expressions import AbstractKind, New, Self, langkit_property

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


@abstract
@has_abstract_list
class AbstractHolder(FooNode):
    f = Field(type=T.Expr)


class ParsedHolder(AbstractHolder):
    pass


class HolderList(AbstractHolder.list):
    pass


@abstract
class AbstractManyHolder(FooNode):
    f = Field(type=AbstractHolder.list)


class ParsedManyHolder(AbstractManyHolder):
    @langkit_property(public=True, memoized=True)
    def synth():
        return New(SyntheticManyHolder, f=Self.f)


# Case of a field that only synthetic nodes can hold
@synthetic
class SynthNode(FooNode):
    f = Field(type=T.Expr)


# Case of a field that both synthetic and parsed nodes can hold
@synthetic
class SynthHolder(AbstractHolder):
    pass


@synthetic
class SyntheticManyHolder(AbstractManyHolder):
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


ctx = emit_and_print_errors(lkt_file='foo.lkt')
nodes = {n.dsl_name: n for n in ctx.astnode_types}

for node_name in ['SynthNode', 'AbstractHolder', 'AbstractManyHolder']:
    node = nodes[node_name]
    fields = {f.original_name: f for f in node.get_fields()}
    f = fields['f']

    if not f.type.is_list_type:
        print('Precise types for {}:'.format(f.qualname))
        for t in f.precise_types.minimal_matched_types:
            print('  * {}'.format(t.dsl_name))
    else:
        print('Precise elements types for {}:'.format(f.qualname))
        for t in f.precise_element_types.minimal_matched_types:
            print('  * {}'.format(t.dsl_name))

print('Done')
