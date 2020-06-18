"""
Check that default argument values are correctly handled in the properties
dispach lowering pass.
"""

from langkit.dsl import ASTNode, Bool, Field, abstract
from langkit.expressions import Not, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Identifier(FooNode):
    token_node = True


@abstract
class Decl(FooNode):
    name = Field(Identifier)

    @langkit_property(public=True)
    def prop(arg=(Bool, False)):
        return arg


class VarDecl(Decl):
    @langkit_property(public=True)
    def prop(arg=(Bool, False)):
        return Not(arg)


class FunDecl(Decl):
    pass


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              types_from_lkt=True)
print('')
print('Done')
