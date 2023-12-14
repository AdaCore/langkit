"""
Check that struct update expressions work as expected.
"""

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import BigIntLiteral, langkit_property

from utils import build_and_run


class KV(Struct):
    key = UserField(type=T.String)
    value = UserField(type=T.BigInt)


class FooNode(ASTNode):

    @langkit_property(public=True)
    def increment(kv=KV):
        return kv.update(value=kv.value + BigIntLiteral(1))


class Example(FooNode):
    token_node = True


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
