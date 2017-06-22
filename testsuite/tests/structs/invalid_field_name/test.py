"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import DiagnosticError, Diagnostics
from langkit.dsl import BoolType, Struct, UserField


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))

try:
    class MyStruct(Struct):
        A = UserField(type=BoolType)
except DiagnosticError:
    pass  # If we get here, a diagnostic should be emitted on standard output
else:
    print('Got no diagnostic error...')
print('Done')
