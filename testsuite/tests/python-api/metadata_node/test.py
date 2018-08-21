"""
Test that metadata fields that contain nodes are properly bound in the Python
API.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Struct, T, UserField, env_metadata
from langkit.expressions import New, No, Self, langkit_property
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


@env_metadata
class Metadata(Struct):
    node = UserField(type=FooNode)


class Example(FooNode):
    token_node = True

    @langkit_property(public=True)
    def get():
        md = New(T.Metadata, node=Self)
        info = New(T.EntityInfo, md=md, rebindings=No(T.EnvRebindingsType))
        return New(T.FooNode.entity, node=Self, info=info)


grammar = Grammar('main_rule')
grammar.add_rules(main_rule=Example('example'))

build_and_run(grammar, 'main.py')
print('')
print('Done')
