"""
Check that accessing the "expr" field on the result of a "cast" expression
works as expected. Due to the way "cast" expressions are represented in the
DSL, this used to compute a stripped expression.
"""

from langkit.dsl import ASTNode, Field
from langkit.expressions import Self, T, langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True)
    def prop():
        # The "cast" expression is represented with a Cast instance, which had
        # an "expr" attribute, so "X.cast(...).expr" used to return "X", and so
        # part of the expression was ignored. If it still was, the following
        # would try to evaluate the ".flag" field on Self, i.e. on a FooNode,
        # which is invalid, so this example would not compile.
        return Self.cast(T.ExprHolder).expr.flag


class ExprHolder(FooNode):
    expr = Field()


class Example(FooNode):
    token_node = True

    @langkit_property(public=True)
    def flag():
        return True


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
