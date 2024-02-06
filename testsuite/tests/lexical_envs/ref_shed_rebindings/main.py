import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", """
    def base {
        def foo {
            def test {}
        }
        +foo
    }

    def new_env {}
""")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

base, new_env = u.root

print("lookup 'test' from base: " + str(base.p_lookup("test")))

rebound_base = base.p_with_rebinding(base, new_env)

print("rebound base: " + str(rebound_base))

# The following query used to return the naked declaration `test`, which is
# not what we expect since we are doing the query from inside an instantiation
# of `base` which encapsulates `test`: therefore the lookup must return the
# declaration of `test` with the same rebindings as those on `base`.
print("lookup 'test' from rebound base: " + str(rebound_base.p_lookup("test")))

print('main.py: Done.')
