"""
Check that invalid struct updates are properly rejected.
"""

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import BigIntLiteral, langkit_property

from utils import emit_and_print_errors


def run(name, expr):
    print('== {} =='.format(name))

    class KV(Struct):
        key = UserField(type=T.String)
        val = UserField(type=T.BigInt)

    class FooNode(ASTNode):
        @langkit_property(public=True)
        def increment(kv=KV):
            return expr(kv)

    class Example(FooNode):
        token_node = True

    emit_and_print_errors(lkt_file='foo.lkt')
    print('')


big_1 = BigIntLiteral(1)

run('Not a struct', lambda kv: big_1.update(val=kv))
run('Invalid field', lambda kv: kv.update(nosuchfield=big_1))
run('Invalid type', lambda kv: kv.update(key=big_1))

print('Done')
