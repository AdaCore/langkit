"""
Test the semantic part of liblktlang: Name and type resolution for the moment.
This is just a wrapper around a call to ``lkt_resolve``.
"""

from __future__ import absolute_import, division, print_function

import os
from os import path as P
import subprocess
import sys


LKT_LIB_DIR = P.join(os.environ['LANGKIT_ROOT_DIR'], 'contrib', 'lkt')

subprocess.check_call([
    sys.executable, P.join(LKT_LIB_DIR, 'lkt_resolve.py'), 'test.lkt'
])
