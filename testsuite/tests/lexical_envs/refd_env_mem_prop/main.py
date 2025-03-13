import sys

import libfoolang


print("main.py: Running...")

ctx = libfoolang.AnalysisContext()
base = ctx.get_from_buffer(
    "base.txt",
    """
    def base {
       def fo {}
    }
    """,
)

main = ctx.get_from_buffer(
    "main.txt",
    """
    def main {
        +base
        +foo
    }
""",
)

for u in [base, main]:
    if u.diagnostics:
        for d in u.diagnostics:
            print(d)
        sys.exit(1)

base.populate_lexical_env()
base_ref, foo_ref = main.root[0].f_content

print("resolving `base`:", base_ref.p_lookup)
print("resolving `foo`:", foo_ref.p_lookup)

print("=== Reparsing base ===")
base = ctx.get_from_buffer(
    "base.txt",
    """
    def base {
       def foo {}
    }
    """,
)

print("resolving `base`:", base_ref.p_lookup)
print("resolving `foo`:", foo_ref.p_lookup)  # used to return None


print("main.py: Done.")
