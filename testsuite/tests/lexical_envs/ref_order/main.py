import sys

import libfoolang


print("main.py: Running...")

print("")
print("===== Functional Test =====")
print("")

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer(
    "main.txt",
    """
    def foo {
        def bar {
            def baz {}
        }
    }
    def test_1 {
        +foo
        +bar
    }
    def test_2 {
        +bar
        +foo
    }
""",
)
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

test_1 = u.root[1]
test_2 = u.root[2]

# The lookup from test_1 should work because it can go through env referenced
# by bar which was found in the env referenced by foo.
# However the lookup from test_2 should return None, because the env
# referenced by bar should not have been found (since it would have required
# looking into foo is referenced after it).
# When working on nodes without rebindings, this was already the observed
# behavior, but "by chance": it is because a referenced env resolves and
# caches its env immediately after being added, so in this case bar was not
# yet added when resolving foo.
print("lookup 'baz' from test_1: " + str(test_1.p_lookup("baz")))
print("lookup 'baz' from test_2: " + str(test_2.p_lookup("baz")))

test_1 = test_1.p_with_rebindings
test_2 = test_2.p_with_rebindings

# Now with rebindings: the two lookups should behave exactly like they did
# without rebindings, so the first one should succeed and the second one should
# fail. However the second one used to "work" as well: since referenced envs
# where only cached for nodes without empty entity info, these lookups would
# trigger recomputation of the referenced env each time. Hence, when resolving
# the env referenced by bar, the lookup did go through foo to find it.
print("lookup 'baz' from test_1: " + str(test_1.p_lookup("baz")))
print("lookup 'baz' from test_2: " + str(test_2.p_lookup("baz")))

print("")
print("===== Performance Test =====")
print("")

u = ctx.get_from_buffer(
    "main.txt",
    """
    def a { def foo {} }
    def b {}
    def c {}
    def d {}
    def e {}
    def f {}
    def g {}
    def h {}
    def i {}
    def j {}
    def k {}
    def l {}
    def m {}
    def n {}
    def o {}
    def p {}
    def q {}
    def r {}
    def s {}
    def t {}
    def u {}
    def v {}
    def w {}
    def x {}
    def y {}
    def z { def bar {} }
    def test {
      +a
      +b
      +c
      +d
      +e
      +f
      +g
      +h
      +i
      +j
      +k
      +l
      +m
      +n
      +o
      +p
      +q
      +r
      +s
      +t
      +u
      +v
      +w
      +x
      +y
      +z
    }
""",
)
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

test = u.root[-1].p_with_rebindings
print("lookup 'foo' from test: " + str(test.p_lookup("foo")))
print("lookup 'bar' from test: " + str(test.p_lookup("bar")))

print("main.py: Done.")
