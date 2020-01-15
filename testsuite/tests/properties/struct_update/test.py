"""
Check that struct update expressions work as expected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import BigIntLiteral, langkit_property
from langkit.parsers import Grammar, List

from utils import build_and_run


class KV(Struct):
    key = UserField(type=T.String)
    val = UserField(type=T.BigInt)


class FooNode(ASTNode):

    @langkit_property(public=True)
    def increment(kv=KV):
        return kv.update(val=kv.val + BigIntLiteral(1))


class Example(FooNode):
    token_node = True


g = Grammar('main_rule')
g.add_rules(main_rule=List(Example('example')))
build_and_run(g, py_script='main.py')
print('Done')
