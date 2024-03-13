"""
Check that string literals are correctly decoded.
"""

import liblktlang as lkt


ctx = lkt.AnalysisContext()

for filename in [
    "valid.lkt",
    "escape_single_quote.lkt",
    "escape_invalid_char.lkt",
    "escape_invalid_digit.lkt",
    "backslash_quote.lkt",
]:
    print(f"== {filename} ==")
    print("")

    u = ctx.get_from_file(filename)
    if u.diagnostics:
        for d in u.diagnostics:
            print(u.format_gnu_diagnostic(d))
    else:
        for decl in u.root.findall(lkt.ValDecl):
            print(f"{decl.f_syn_name.text} = ", end="")
            result = decl.f_val.p_denoted_value
            if result.has_error:
                print(f"ERROR: {result.error_sloc}: {result.error_message}")
            else:
                print(ascii(result.value))

    print("")

print("Done")
