"""
Check use of bindings in predicate expressions are considered when computing
"unused bindings".
"""

from langkit.dsl import ASTNode, T, UserField
from langkit.expressions import Predicate, Self, langkit_property

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    var = UserField(T.LogicVar, public=False)

    @langkit_property()
    def pred(i=T.Int):
        return i == 0

    @langkit_property(public=True)
    def prop(i=T.Int):
        return Predicate(T.Example.pred, Self.var, i).solve


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
