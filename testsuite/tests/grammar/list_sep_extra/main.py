import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()

for kw in ["kwA", "kwB", "kwC", "kwD"]:
    print(f"== {kw} ==")
    print()
    for text in [
        "",
        " ,",
        " a",
        " a ,",
        " , a",
        " , a ,",
        " a, b, c",
        " , a, b, c",
        " a, b, c ,",
        " , a, b, c ,",
    ]:
        u = ctx.get_from_buffer("buffer", buffer=kw + text + " ;")
        status = "ERROR" if u.diagnostics else "OK   "
        print(f"  {status} {u.text}")
    print()

# Regression test: the token range for the list node used to be the trailing
# ";" whereas it should be "bar" (the list parser for kwE does not accept
# trailing separators).
text = "kwE foo; bar;"
print(f"== Token range for: {text} ==")
print()
u = ctx.get_from_buffer("buffer", buffer=text)
for d in u.diagnostics:
    print("ERROR:", d)
print("Root:", u.root)
print("Token start:", u.root.token_start)
print("Token end:", u.root.token_end)
print()

print("main.py: Done.")
