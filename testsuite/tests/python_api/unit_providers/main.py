# Test that custom unit providers defined using the Python bidnings work as
# expected.

import sys

import libfoolang


print("main.py: Running...")

sources = {
    "m0.txt": b"{ a b c }",
    "m1.txt": b"{ a b c }\n{ a b c }",
    "refs.txt": b"{ ref m.a ref m.b ref m.c }",
}


class CustomUnitProvider(libfoolang.UnitProvider):
    def __init__(self, mapping):
        self.mapping = mapping

        self.requested = set()

    def unit_location(self, name, kind):
        # The implementation around unit providers can do redundant queries. To
        # avoid noisy test baselines, show the request only once.
        key = (name, kind)
        if key not in self.requested:
            print("!!! CustomUnitProvider.unit_location")
            print(f"    name={name!r}")
            print(f"    kind={kind}")
            self.requested.add(key)
        return self.mapping[name]


def create_context(mapping):
    up = CustomUnitProvider(mapping)
    ctx = libfoolang.AnalysisContext(unit_provider=up)
    for filename, buffer in sources.items():
        ctx.get_from_buffer(filename, buffer)
    return ctx


for mapping in [
    {"m": ("m0.txt", 0)},
    {"m": ("m1.txt", 0)},
    {"m": ("m1.txt", 1)},
    # File exists, but the PLE root index is out of bounds
    {"m": ("m0.txt", 1)},
    # Missing mapping item: our custom unit provider will propagate an
    # exception.
    {},
    # Invalid types
    {"m": ("m0.txt", "foo")},
    {"m": (3.14, 1)},
    {"m": "nonsense"},
]:
    print(f"# {mapping}")
    print()
    ctx = create_context(mapping)
    for block in ctx.get_from_file("refs.txt").root:
        for ref in block.f_items:
            sys.stdout.flush()
            try:
                block = ref.p_resolve
            except libfoolang.PropertyError:
                result = "<PropertyError>"
            else:
                result = str(block)
            print(f"  {ref.text}: {result}")
    print()

print("main.py: Done.")
