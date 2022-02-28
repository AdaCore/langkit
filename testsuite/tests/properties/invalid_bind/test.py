"""
Check that invalid ``conv_prop`` arguments for ``Bind`` expressions are
properly rejected.
"""

from langkit.dsl import ASTNode, LogicVar, T, UserField
from langkit.expressions import (
    Bind, DynamicVariable, Entity, Property, Self, langkit_property
)

from utils import emit_and_print_errors


def run(name, conv_prop):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    dyn_node1 = DynamicVariable("dyn_node1", T.FooNode)
    dyn_node2 = DynamicVariable("dyn_node2", T.FooNode)

    print("== {} ==".format(name))

    conv_prop = eval(conv_prop)

    class FooNode(ASTNode):
        ref_var = UserField(LogicVar, public=False)
        type_var = UserField(LogicVar, public=False)

    class BarNode(FooNode):
        main_prop = Property(
            dyn_node1.bind(
                Self,
                Bind(Self.type_var, Self.ref_var, conv_prop=conv_prop),
            ),
            warn_on_unused=False,
        )

    class BazNode(FooNode):
        prop1 = Property(12, warn_on_unused=False)

        @langkit_property(warn_on_unused=False)
        def prop2(_=T.Bool):
            return Entity

        @langkit_property(warn_on_unused=False, dynamic_vars=[dyn_node2])
        def prop3():
            return dyn_node2.as_bare_entity

        @langkit_property(warn_on_unused=False, dynamic_vars=[dyn_node1])
        def prop4(_=(T.Bool, True)):
            return dyn_node1.as_bare_entity

        @langkit_property(warn_on_unused=False, dynamic_vars=[dyn_node1])
        def prop5():
            return dyn_node1.as_bare_entity

    emit_and_print_errors(lkt_file="foo.lkt")
    print("")


run("Bad return type", "T.BazNode.prop1")
run("Bad arity", "T.BazNode.prop2")
run("Unbound dynamic vars", "T.BazNode.prop3")
run("Valid default arg", "T.BazNode.prop4")
run("Valid", "T.BazNode.prop5")
print('Done')
