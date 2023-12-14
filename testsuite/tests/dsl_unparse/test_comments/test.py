from langkit.diagnostics import WarningSet
from langkit.dsl import ASTNode, T, abstract
from langkit.expressions import (
    And, Cond, If, Let, Or, Property, Self, Var, langkit_property
)

from utils import build_and_run, default_warning_set


@abstract
class TestNode(ASTNode):
    """
    Root node class for Test AST nodes.
    """
    pass


class ExampleNode(TestNode):
    """
    Example node.

    Second line.
    """
    @langkit_property(return_type=T.Int, public=True)
    def test_no_var():
        "docstring."
        # hello
        return 42

    prop_inline = Property(
        # wow!
        43,
        public=True
    )
    prop_lambda = Property(
        # identity
        lambda x=T.Int:
        # yep
        x,
        public=True
    )

    @langkit_property(public=True)
    def test_cond(x=T.Int):
        # incredible function
        return Cond(
            # case 0
            x == 0,
            # identity
            x,

            # case 1
            x == 1,

            # increment
            x + 1,

            # otherwise double
            x + x
        )

    @langkit_property(return_type=T.Int, public=True)
    def test_if():
        # define x
        x = Var(
            # let's see... maybe 24?
            24
        )

        # call foo
        return If(
            # no way!
            True,

            # haha
            If(
                # second
                False,
                # nope
                12,
                # finally!
                Self.test_cond(
                    # arg
                    x
                )
            ),

            # hoho
            24
        )

    @langkit_property(return_type=T.Int, public=True)
    def test_match():
        return Self.match(
            # first case
            lambda x=T.NodeType1: 0,

            # node type 2!
            lambda y=T.NodeType2: 1,

            # well otherwise...
            lambda _: 2
        )

    @langkit_property(return_type=T.Int, public=True)
    def test_let():
        # let it go!!!
        return Let(
            # where is this comment?
            lambda
            # define x
            x=42,
            # define y
            y=24:
            # compute
            x + y
        )

    @langkit_property(return_type=T.Bool, public=True)
    def test_bool_binops(x=T.Bool, y=T.Bool):
        return And(
            # hehe
            And(
                # hoho
                x  # hihi
                & y,
                x
            ) | x,
            # wow
            Or(
                x,
                x.any_of(
                    # True
                    True,
                    # False
                    False
                )
            )
        )


class NodeType1(ExampleNode):
    pass


class NodeType2(ExampleNode):
    pass


warning_set = default_warning_set.clone()
warning_set.disable(WarningSet.unused_node_type)
warning_set.disable(WarningSet.unused_bindings)

build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    warning_set=warning_set,
    types_from_lkt=True,
)
print('Done')
