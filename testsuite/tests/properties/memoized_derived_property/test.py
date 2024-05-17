from langkit.dsl import ASTNode, Field, T
from langkit.expressions import Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(activate_tracing=True)
    def identity(x=T.Int):
        return x

    @langkit_property(public=True, memoized=True)
    def foo(x=T.Int):
        return Self.identity(x)


class Block(FooNode):
    example1 = Field(type=T.Example1)
    example2 = Field(type=T.Example2)


class Example1(FooNode):
    @langkit_property()
    def foo(x=T.Int):
        return Self.identity(x) + 1


class Example2(FooNode):
    @langkit_property(memoized=True)
    def foo(x=T.Int):
        return Self.identity(x) + 2


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print('Done')
