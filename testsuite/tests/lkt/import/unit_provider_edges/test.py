"""
Test edge cases for Liblktlang's unit provider.
"""

import os.path

import liblktlang as L


ctx = L.AnalysisContext()

for name, kind in [
    # Lkt has bodies only: check what requiring another kind of unit kind does
    ("foo", L.AnalysisUnitKind.unit_specification),
    # Check the case where the requested unit does not exist
    ("bar", L.AnalysisUnitKind.unit_body),
    # Check requesting a unit with an invalid name ('-' is not allowed)
    ("foo-bar", L.AnalysisUnitKind.unit_body),
]:
    print("#", name, kind)
    print()
    u = ctx.get_from_provider(name, kind)
    print("Got unit", repr(os.path.basename(u.filename)))
    for d in u.diagnostics:
        print(u.format_gnu_diagnostic(d))
    print()

print("Done")
