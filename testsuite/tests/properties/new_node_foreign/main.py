import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
ctx.discard_errors_in_populate_lexical_env(False)


def load(filename, buffer):
    u = ctx.get_from_buffer(filename, buffer)
    if u.diagnostics:
        for d in u.diagnostics:
            print(d)
        sys.exit(1)
    return u


root = load("root.txt", "def root {}")
root.populate_lexical_env()

foo = load("foo.txt", "root {}")
print("Evaluating .f_synth...")
try:
    print("->", foo.root[0].f_synth)
except libfoolang.PropertyError as exc:
    print("Got a property error: {}".format(exc))

print("main.py: Done.")
