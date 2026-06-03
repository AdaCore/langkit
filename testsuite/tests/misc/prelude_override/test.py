"""
Check that overriding the prelude does not prevent name resolution.

The `LktNode.p_prelude_unit` property used not to run PLE on the prelude in
this case.
"""

import sys

import liblktlang as lkt


ctx = lkt.AnalysisContext()

# Provide a minimal prelude so that the simple Lkt source below can be typed
prelude = ctx.get_from_buffer(
    "__prelude",
    """
    @builtin struct Int {}
    @builtin generic[T] struct Entity {}
    @builtin generic[T] struct NodeBuilder {}
    @builtin generic[T] struct Stream {}
    """,
)
if prelude.diagnostics:
    for d in prelude.diagnostics:
        print(prelude.format_gnu_diagnostic(d))
    sys.exit(1)

unit = ctx.get_from_buffer("foo.lkt", "val x: Int = 1")

for d in unit.root.findall(lkt.ValDecl):
    if d.p_xref_entry_point:
        results = d.p_solve_enclosing_context
        print(results)
