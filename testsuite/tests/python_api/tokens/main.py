"""
Test token-related primitives.
"""

import sys

import libfoolang


print("main.py: Running...")


def parse(filename, content):
    result = ctx.get_from_buffer(filename, content)
    if result.diagnostics:
        print("Errors in {}:".format(filename))
        for d in result.diagnostics:
            print("  {}".format(d))
        sys.exit(1)
    return result


ctx = libfoolang.AnalysisContext()
u = parse("foo.txt", b" (a (b c d)) ")
u2 = parse("bar.txt", b"()")

tokens = []
t = u.first_token
while t is not None:
    tokens.append(t)
    t = t.next

# Print all tokens, for output clarity
print("Tokens:")
for t in tokens:
    print("   {}".format(t))
print("")

# Print the whole text buffer
print("Input source buffer:\n   {}".format(repr(u.text)))
print("")

# Test Token's comparison operations
assert tokens[0] != tokens[1]
assert not (tokens[0] == tokens[1])
assert tokens[0] == tokens[0]

assert tokens[0] < tokens[1]
assert not (tokens[0] < tokens[0])
assert not (tokens[1] < tokens[0])

assert tokens[0] <= tokens[1]
assert tokens[0] <= tokens[0]
assert not (tokens[1] <= tokens[0])

assert tokens[1] > tokens[0]
assert not (tokens[0] > tokens[0])
assert not (tokens[0] > tokens[1])

assert tokens[1] >= tokens[0]
assert tokens[0] >= tokens[0]
assert not (tokens[0] >= tokens[1])

# Test Token.range_until
assert list(tokens[0].range_until(tokens[-1])) == tokens
assert list(tokens[1].range_until(tokens[0])) == []
assert list(tokens[1].range_until(tokens[1])) == tokens[1:2]
assert list(tokens[1].range_until(tokens[2])) == tokens[1:3]

# Test the hash operation (sets are hash-based) and the ordering operators
# (sorted).
token_set = set(tokens) | set(tokens[0].range_until(tokens[-1]))
assert sorted(token_set) == tokens

print("== Test Token.text_range ==")
for t1, t2 in [
    (tokens[1], tokens[0]),
    (tokens[0], tokens[0]),
    (tokens[0], tokens[1]),
    (tokens[1], tokens[4]),
]:
    print(
        "Token.text_range({}, {}):\n   {}".format(
            t1, t2, repr(libfoolang.Token.text_range(t1, t2))
        )
    )
print("")

# Ordering operations and .range_until must raise an error when working on
# tokens from different analysis units.
print("== Test unit consistency checks ==")
for func in [libfoolang.Token.__lt__, libfoolang.Token.range_until]:
    try:
        func(u.first_token, u2.first_token)
    except ValueError as exc:
        print("{} raised ValueError:\n   {}".format(func.__name__, exc))
    else:
        assert False
print("")

# Ordering operations and .range_until must raise an error when provided
# non-token values.
print("== Test type consistency checks ==")
for func in [
    libfoolang.Token.__lt__,
    libfoolang.Token.__le__,
    libfoolang.Token.__gt__,
    libfoolang.Token.__ge__,
    libfoolang.Token.range_until,
    libfoolang.Token.text_range,
]:
    try:
        func(u.first_token, 42)
    except TypeError as exc:
        print(
            "{} raised {}:\n   {}".format(
                func.__name__, type(exc).__name__, exc
            )
        )
    else:
        assert False
print("")

# Equality operations must handle them correctly however
assert not (u.first_token == u2.first_token)
assert u.first_token != u2.first_token

print("== Test properties returning tokens ==")
print("token_start:", u.root.token_start)
print("token_end:", u.root.token_end)
print("")


print("main.py: Done.")
