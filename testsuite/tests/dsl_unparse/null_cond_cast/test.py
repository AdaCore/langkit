from langkit.dsl import ASTNode, T
from langkit.expressions import Self, langkit_property

from utils import emit_and_print_errors, unparse_script


class FooNode(ASTNode):

    @langkit_property()
    def ident():
        return Self

    @langkit_property()
    def static_ident(n=T.FooNode):
        return n

    @langkit_property(public=True)
    def p1():
        return Self._.cast_or_raise(T.Example)

    @langkit_property(public=True)
    def p2():
        return (
            Self.static_ident(Self)
            ._.cast_or_raise(T.Example.list)
            .filtermap(lambda e: e.as_bare_entity, lambda e: e.parent.is_null)
        )

    @langkit_property(public=True)
    def p3(unit=T.AnalysisUnit):
        return unit._.root._.cast_or_raise(T.Example).ident


class Example(FooNode):
    token_node = True


emit_and_print_errors(
    lkt_file='expected_concrete_syntax.lkt', unparse_script=unparse_script
)
print('Done')
