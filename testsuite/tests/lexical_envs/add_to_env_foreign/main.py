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

    +foo (foo.bar)
""",
)

# Adding a node from the current unit to a foreign unit's lexical env is
# allowed.
process(
    "add_to_foreign",
    b"""
    foo.bar
""",
)

# Adding a foreign node to the current unit's lexical env is an error
process(
    "add_foreign_node",
    b"""
    +foo.bar
""",
)

# Adding a local node to the current unit's lexical environment is allowed, but
# when the metadata contains a foreign node, this is an error.
process(
    "add_foreign_md",
    b"""
    local {}
    +local (foo)
""",
)

# Adding a synthetic node to a foreign env is forbidden
process(
    "add_foreign_synth",
    b"""
    .foo.bar
""",
)

print("main.py: Done.")
