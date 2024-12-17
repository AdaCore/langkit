import libfoolang


print("main.py: Running...")

u = libfoolang.AnalysisContext().get_from_buffer("main.txt", b"foo1 @foo2")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    assert False
for t in u.iter_tokens():
    print(ascii(str(t)))
print()

print("main.py: Done.")
