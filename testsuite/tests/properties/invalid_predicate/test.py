"""
Test that Prediate works well with default argument values.
"""

from langkit.dsl import ASTNode, Bool, LogicVar, T, UserField
from langkit.expressions import Predicate, Self, Var, ignore, langkit_property

from utils import emit_and_print_errors


def run(name, *pred_args):
    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):

        var1 = UserField(LogicVar, public=False)
        var2 = UserField(LogicVar, public=False)

        @langkit_property(warn_on_unused=False)
        def pred1(n=T.FooNode.entity):
            return n.is_null

        @langkit_property(warn_on_unused=False)
        def pred2(n=T.FooNode.entity, b=(Bool, False)):
            return n.is_null & b

        @langkit_property(public=True)
        def prop():
            ignore(Var(Predicate(*pred_args)))
            return Self.as_bare_entity

    emit_and_print_errors(lkt_file='foo.lkt')
    print('')


run('No logic var', T.Example.pred1)
run('One missing logic var', T.Example.pred1, Self.var1)
run('Bad argument type', T.Example.pred1, Self.var1, True)
run('Too many arguments', T.Example.pred1, Self.var1, Self.var2, True)
run('Ok (no default var)', T.Example.pred1, Self.var1, Self.var2)

run('Ok (one unpassed default var)', T.Example.pred2, Self.var1, Self.var2)
run('Ok (one passed default var)', T.Example.pred2, Self.var1, Self.var2, True)

print('Done')
