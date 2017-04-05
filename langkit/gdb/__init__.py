"""
GDB helpers to debug generated libraries.
"""

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)


def setup():
    """
    Register helpers in GDB internals. This should be run when the generated
    library is actually loaded in GDB.
    """
    pass
