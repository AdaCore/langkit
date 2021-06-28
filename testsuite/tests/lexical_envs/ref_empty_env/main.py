import libfoolang


print('main.py: Running...')


ctx = libfoolang.AnalysisContext()


def load(fn):
    u = ctx.get_from_file(fn)
    assert not u.diagnostics
    u.populate_lexical_env()
    return u


def check():
    for n in u1.root.findall(libfoolang.Ref):
        print("  {} -> {}".format(n, n.p_resolve))


print("After loading u1.txt")
u1 = load("u1.txt")
check()

print("After loading u2.txt")
u2 = load("u2.txt")
check()

print('main.py: Done.')
