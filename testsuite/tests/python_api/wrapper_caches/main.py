import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
unit = ctx.get_from_buffer("foo.txt", b"example (example)")

# Make sure that the exact context wraper is re-used over and over
assert unit.context is ctx

if unit.diagnostics:
    for d in unit.diagnostics:
        print(d)
    sys.exit(1)

root = unit.root
child = root[0]

# Make sure that the exact unit/node wrapper is re-used over and over
assert child.parent is root
assert root[0] is child
assert root.unit is unit

# Make sure trying to use a stale reference raises an error
print("Reparsing...")
unit.reparse(b"example (example) # reparsed")
for name, computation in [
    (".parent", lambda n: n.parent),
    ("[0]", lambda n: n[0]),
    ("str()", lambda n: str(n)),
]:
    print("Trying to compute: {}...".format(name))
    try:
        computation(root)
    except libfoolang.StaleReferenceError:
        print("   StaleReferenceError raised!")
    else:
        print("   No error raised...")

# ... however the equality/hashing methods should not, to allow stale
# references in dicts/sets to be free'd after the reparse.
assert isinstance(hash(root), int)
assert root == root

print("main.py: Done.")
