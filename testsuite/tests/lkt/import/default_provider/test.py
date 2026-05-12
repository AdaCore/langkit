"""
Test the default Lkt unit provider.
"""

from contextlib import contextmanager
import os

import liblktlang as L


def fetch_foo(provider: L.UnitProvider):
    ctx = L.AnalysisContext(unit_provider=provider)
    unit = ctx.get_from_provider("foo", L.AnalysisUnitKind.unit_body)
    root = unit.root
    if root is not None:
        print('"foo" unit resolved to', os.path.relpath(unit.filename))
        print(f"  {unit.text!r}")
    else:
        print('"foo" unit not found...')
    print()


@contextmanager
def setenv(key, value):
    old_value = os.environ.get(key)
    os.environ[key] = value
    yield
    if old_value is None:
        del os.environ[key]
    else:
        os.environ[key] = old_value


src_dir = os.path.abspath("src")
other_src_dir = os.path.abspath("other_src")

# Test with environment variables
print("===== Test with environment variables =====\n")
print('== Fetch "foo" in Lkt mode with LKT_PATH')
with setenv("LKT_PATH", src_dir):
    fetch_foo(L.UnitProvider.create_default(L.LanguageMode.lkt))

print('== Fetch "foo" in LKQL mode with LKQL_PATH')
with setenv("LKQL_PATH", src_dir):
    fetch_foo(L.UnitProvider.create_default(L.LanguageMode.lkql))

print('== Fetch "foo" in Lkt mode with LKQL_PATH')
with setenv("LKQL_PATH", src_dir):
    fetch_foo(L.UnitProvider.create_default(L.LanguageMode.lkt))

print('== Fetch "foo" in LKQL mode with LKT_PATH')
with setenv("LKT_PATH", src_dir):
    fetch_foo(L.UnitProvider.create_default(L.LanguageMode.lkql))

print('== Fetch "foo" in Lkt mode with no environment variables')
fetch_foo(L.UnitProvider.create_default(L.LanguageMode.lkt))

# Test with explicit directories
print("===== Test with explicit directories =====\n")
print('== Fetch "foo" in Lkt mode in "src" dir')
fetch_foo(L.UnitProvider.from_directories(L.LanguageMode.lkt, [src_dir]))

print('== Fetch "foo" in Lkt mode in "other_src" dirs')
fetch_foo(L.UnitProvider.from_directories(L.LanguageMode.lkt, [other_src_dir]))

print('== Fetch "foo" in LKQL mode in "src" dir')
fetch_foo(L.UnitProvider.from_directories(L.LanguageMode.lkql, [src_dir]))

print('== Fetch "foo" in LKQL mode in "other_src" dir')
fetch_foo(
    L.UnitProvider.from_directories(L.LanguageMode.lkql, [other_src_dir])
)

print('== Fetch "foo" in Lkt mode in no dirs')
fetch_foo(L.UnitProvider.from_directories(L.LanguageMode.lkt, []))

print("main.py: Done")
