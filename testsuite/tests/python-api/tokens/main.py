"""
Test that Tokens' "container primitives" (hash and < operators) properly work.
"""

from __future__ import absolute_import, division, print_function

import sys

print('main.py: Running...')


import libfoolang


def parse(filename, content):
    result = ctx.get_from_buffer(filename, content)
    if result.diagnostics:
        print('Errors in {}:'.format(filename))
        for d in result.diagnostics:
            print('  {}'.format(d))
        sys.exit(1)
    return result


ctx = libfoolang.AnalysisContext()
u = parse('foo.txt', '(a (b c d))')
u2 = parse('bar.txt', '()')
r = u.root
tokens = sorted(r.tokens)

print('Tokens:')
for t in tokens:
    print('  ', t)

# Test both that Token.range_until properly works and that the equality
# operator also works.
other_tokens = list(r.token_start.range_until(r.token_end))
assert tokens == other_tokens

# This tests both the hash operation (sets are hash-based) and the ordering
# operators (sorted).
token_set = set(tokens) | set(other_tokens)
assert sorted(token_set) == tokens


# Test that ordering operators don't work for tokens that come from different
# units.
try:
    u.first_token < u2.first_token
except ValueError as exc:
    print('< on incompatible tokens raised ValueError: {}'.format(exc))
else:
    print('< on incompatible tokens raised no exception: FAIL')


# ... but the equality operator should work in this case
try:
    u.first_token == u2.first_token
except ValueError as exc:
    print('== on incompatible tokens raised ValueError: {}'.format(exc))
else:
    print('== on incompatible tokens raised no exception')


print('main.py: Done.')
