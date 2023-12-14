"""
Test that Java API is properly working.
"""

from langkit.compile_context import LibraryEntity
from utils import build_and_run

import common


common  # avoid pyflakes "unused" warning

build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    java_main="BindingsTests",
    default_unit_provider=LibraryEntity(
        "Libfoolang.Helpers", "Create_Unit_Provider",
    ),
    symbol_canonicalizer=LibraryEntity("Pkg", "Canonicalize"),
    types_from_lkt=True,
)
print("Done")
