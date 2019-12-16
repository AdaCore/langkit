from __future__ import absolute_import, division, print_function

from collections import namedtuple
import ctypes
import sys

import libfoolang
from libfoolang import _py2to3


print('main.py: Running...')

# Import the C API helper
ttls = libfoolang._c_lib.foo_text_to_locale_string
ttls.argtypes = [ctypes.POINTER(libfoolang._text)]
ttls.restype = ctypes.c_char_p

# Create an input value and run it
text = libfoolang._text._unwrap(u'Hello')
result = ttls(ctypes.byref(text))

print('> {}'.format(repr(result)))

print('main.py: Done.')
