"""
Check that character literals are correctly decoded.
"""

import liblktlang as lkt


ctx = lkt.AnalysisContext()

for filename in [
    "valid.lkt",
    "empty.lkt",
    "escape_double_quote.lkt",
    "escape_invalid_char.lkt",
    "escape_invalid_digit.lkt",
]:
    print(f"== {filename} ==")
    print("")

    u = ctx.get_from_file(filename)
    if u.diagnostics:
        for d in u.diagnostics:
            print(u.format_gnu_diagnostic(d))
    else:
        for decl in u.root.findall(lkt.ValDecl):
            name = decl.f_syn_name.text
            result = decl.f_expr.p_denoted_value
            if result.has_error:
                print(f"{name}:{result.error_sloc}: {result.error_message}")
            else:
                print(f"{name} = {ascii(result.value)}")

    print("")

print("Done")
