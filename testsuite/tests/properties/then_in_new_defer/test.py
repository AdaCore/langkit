"""
Check that ".then" is properly compiled when used as an operand of a ".new"
whose prefix is a defered ("T.*") type. This used to skip the "prepare"
compilation pass, which resulted in a Langkit crash.
"""

from langkit.dsl import ASTNode, Field, T, synthetic
from langkit.expressions import If, No, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


@synthetic
class SynthNode(FooNode):
    f = Field(type=T.FooNode)


class Example(FooNode):
    token_node = True

    @langkit_property(return_type=T.FooNode.entity, public=True)
    def p1():
        return Self.p2(No(T.FooNode)).f.as_bare_entity

    @langkit_property(return_type=T.SynthNode, memoized=True)
    def p2(n=T.FooNode):
        # Wrap the Defer creation in an If expression so that it is not
        # resolved too soon in order to trigger the bug.
        return If(
            True,
            T.SynthNode.new(
                f=n.then(
                    lambda n: n,
                    default_val=Self,
                ),
            ),
            No(T.SynthNode),
        )


build_and_run(lkt_file="expected_concrete_syntax.lkt", py_script="main.py")
print("Done")
