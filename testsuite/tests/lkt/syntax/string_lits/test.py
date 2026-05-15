"""
Check that string literals are correctly decoded.
"""

import liblktlang as lkt


def process_file(filename: str) -> lkt.AnalysisUnit:
    print(f"== {filename} ==")
    print("")

    u = ctx.get_from_file(filename)
    if u.diagnostics:
        for d in u.diagnostics:
            print(u.format_gnu_diagnostic(d))
        return None
    return u


def process_literal(label: str, value: lkt.DecodedStringValue) -> None:
    print(f"{label} = ", end="")
    if value.has_error:
        print(f"ERROR: {value.error_sloc}: {value.error_message}")
    else:
        print(ascii(value.value))


ctx = lkt.AnalysisContext()

for filename in [
    "valid.lkt",
    "escape_single_quote.lkt",
    "escape_invalid_char.lkt",
    "escape_invalid_digit.lkt",
    "backslash_quote.lkt",
    "block_escape_invalid_char.lkt",
    "block_missing_space.lkt",
    "block_trailing_space.lkt",
]:
    u = process_file(filename)
    if u is not None:
        for decl in u.root.findall(lkt.ValDecl):
            process_literal(decl.f_syn_name.text, decl.f_expr.p_denoted_value)

    print("")


u = process_file("regex_pattern.lkt")
if u is not None:
    for p in u.root.findall(lkt.RegexPattern):
        process_literal(p.text, p.p_denoted_value)

print("Done")
