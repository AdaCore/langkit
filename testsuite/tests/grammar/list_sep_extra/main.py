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

print("main.py: Done.")
