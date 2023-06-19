"""
Test that the Java bindings correctly implement node equality and hashing,
in particular in the presence of metadata.
"""

import common
from utils import build_and_run


common  # avoid pyflakes "unused" warning

build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    ni_main='BindingsTests'
)

print('Done')
