"""
Check that module docstrings are correctly decoded.
"""

import glob

import liblktlang as lkt


ctx = lkt.AnalysisContext()

for filename in sorted(glob.glob("*.lkt")):
    print(f"== {filename} ==")
    print()

    u = ctx.get_from_file(filename)
    if u.diagnostics:
        for d in u.diagnostics:
            print(u.format_gnu_diagnostic(d))
        print()
        continue

    u.root.dump()
    print()

    if u.root.f_doc is None:
        print("No module-level documentation")

    else:
        result = u.root.f_doc.p_denoted_value
        if result.has_error:
            print(f"{result.error_sloc}: {result.error_message}")
        else:
            print(f"doc = {ascii(result.value)}")

    print()

print("Done")
