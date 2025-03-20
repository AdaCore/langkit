import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
ctx.discard_errors_in_populate_lexical_env(False)


def process(name, buffer):
    print("== {} ==".format(name))

    u = ctx.get_from_buffer(name, buffer)
    if u.diagnostics:
        for d in u.diagnostics:
            print("{}:{}".format(name, d))
        sys.exit(1)

    try:
        u.populate_lexical_env()
    except libfoolang.PropertyError as exc:
        print("   Got a {}: {}".format(type(exc).__name__, exc))
    else:
        print("   All good...")
    print("")


# This deals with only local nodes and environments: there is no error here
process(
    "foo",
    b"""
    foo {
        bar {}
    }

    foo.baz {}
""",
)

# Having set_initial_env return a foreign environment is not allowed by default
process(
    "foo.qux",
    b"""
    foo.qux {}
""",
)

print("main.py: Done.")
