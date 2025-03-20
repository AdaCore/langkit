import sys

import libfoolang


def load_as(filename, buffer):
    result = ctx.get_from_buffer(filename, buffer=buffer)
    if result.diagnostics:
        for d in result.diagnostics:
            print("{}:{}".format(result.filename, d))
        sys.exit(1)
    result.populate_lexical_env()
    return result


def load_main():
    return load_as("main.txt", b"foo { foo.bar }")


def load_a():
    return load_as("a.txt", b"foo.bar {}\n foo.bar {}")


def load_b():
    return load_as("b.txt", b"foo.bar {}")


print("main.py: Running...")

for label, loaders in [
    ("Loading a.txt first", [load_main, load_a, load_b]),
    ("Loading b.txt first", [load_main, load_b, load_a]),
]:
    print("== {} ==".format(label))
    ctx = libfoolang.AnalysisContext()
    for l in loaders:
        l()
    u = ctx.get_from_file("main.txt")
    sys.stdout.flush()
    u._dump_lexical_env()
    print("")

    for r in u.root.findall(libfoolang.Ref):
        print("{} resolves to {}".format(r, r.p_resolve))
    print("")

print("main.py: Done.")
