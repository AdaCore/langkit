"""
Test that covers syntactic problems uncovered during transition to concrete
syntax, that were not covered by the existing testsuite.
"""

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import (
    ArrayLiteral, Let, Self, String, Var, ignore, langkit_property
)

from utils import build_and_run, unparse_all_script


class FooNode(ASTNode):
    pass


class KV(Struct):
    key = UserField(T.Symbol)
    val = UserField(T.Symbol)


class Example(FooNode):

    @langkit_property(public=True)
    def null_match():
        """
        Test that null conditional + match expr expands correctly.
        """
        return Self._.match(
            lambda e=Example: e
        )

    @langkit_property(public=True)
    def test_symlit_escape():
        """
        Test that sym literals with quotes in them are properly unparsed.
        """
        return "\"=\""

    @langkit_property(public=True)
    def test_dotexpr_lhs():
        """
        Test various valid dotexpr's LHS.
        """
        a = Var(ArrayLiteral([1]).find(lambda v: v == 1))
        b = Var(Let(lambda b=[1, 2]: b).find(lambda v: v == 1))
        c = Var(String("hello").concat(String(" world")))
        ignore(b)
        ignore(c)
        return a

    @langkit_property(public=True)
    def test_1():
        """
        Tests that block delimiter prevents a syntactic ambiguity.
        """
        a = Var(12)
        return [a]

    @langkit_property(public=True)
    def test_2():
        """
        Tests that reserved names are properly avoided in constructor syntax.
        """
        return KV.new(key="hey", val="you")


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              unparse_script=unparse_all_script)
print('Done')
