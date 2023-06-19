"""
Test that Java API is properly working.
"""

from langkit.compile_context import LibraryEntity
from utils import build_and_run

import common


common  # avoid pyflakes "unused" warning

build_and_run(lkt_file='expected_concrete_syntax.lkt',
              ni_main='BindingsTests',
              symbol_canonicalizer=LibraryEntity('Pkg', 'Canonicalize'))
print('Done')
