import sys

import libfoolang


print('main.py: Running...')


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('input', """
def a {
    nocat { b     }
    cat1  { b c   }
    cat2  { b   d }
    example
}
""")
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


def var_image(v):
    return '<Var {}, line {}>'.format(v.text, v.sloc_range.start.line)


example = u.root[-1]
for lookup in ('p_lookup_all', 'p_lookup_none', 'p_lookup_1', 'p_lookup_2'):
    print('With {}:'.format(lookup))
    for name in ('a', 'b', 'c', 'd'):
        lookup_prop = getattr(example, lookup)
        print('  ({}) -> {}'.format(name,
                                    [var_image(v) for v in lookup_prop(name)]))
    print('')

print('main.py: Done.')
