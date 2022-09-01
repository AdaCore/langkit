"""
Test that invalid environment metadata structs are properly rejected.
"""

import langkit
from langkit.diagnostics import DiagnosticError
from langkit.dsl import ASTNode, MetadataField, Struct, T, env_metadata

from utils import emit_and_print_errors


def run(md_constructor):
    """
    Emit and print he errors we get for the below grammar. `md_constructor` is
    called to create the lexical environment metadata.
    """

    print('== {} =='.format(md_constructor.__name__))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        pass

    try:
        md_constructor()
    except DiagnosticError:
        langkit.reset()
    else:
        emit_and_print_errors(lkt_file='foo.lkt')
    print('')


def not_a_struct():
    @env_metadata
    class Metadata(object):
        pass


def two_md():
    @env_metadata
    class Metadata(Struct):
        pass

    del Metadata

    @env_metadata
    class Metadata(Struct):
        pass


def bad_name():
    @env_metadata
    class BadName(Struct):
        pass


def bad_type():
    @env_metadata
    class Metadata(Struct):
        fld = MetadataField(type=T.AnalysisUnit, use_in_eq=True)


run(not_a_struct)
run(two_md)
run(bad_name)
run(bad_type)
print('Done')
