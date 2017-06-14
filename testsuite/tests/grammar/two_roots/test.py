from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import DiagnosticError, Diagnostics
from langkit.dsl import ASTNode

Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):
    pass

try:
    class OtherNode(ASTNode):
        pass
except DiagnosticError:
    pass

print('Done')
