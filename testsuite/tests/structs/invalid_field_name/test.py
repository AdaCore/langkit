"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

from langkit.diagnostics import DiagnosticError
from langkit.dsl import Bool, Struct, UserField


try:
    class MyStruct(Struct):
        A = UserField(type=Bool)
except DiagnosticError:
    pass  # If we get here, a diagnostic should be emitted on standard output
else:
    print('Got no diagnostic error...')
print('Done')
