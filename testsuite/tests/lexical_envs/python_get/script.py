import sys

import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_file('source.txt')
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

u.populate_lexical_env()
o = u.root.find(lambda n: isinstance(n, libfoolang.Def) and n.f_id.text == 'o')
q = o.parent.parent
foo = q.parent.parent

nodes = [foo, q, o]
symbols = ['foo', 'p', 't', 'q', 'o', 'nosymbol']
symbol_width = max(len(sym) for sym in symbols)


def def_img(node):
    assert isinstance(node, libfoolang.Def)
    return '<Def {} line {}>'.format(node.f_id.text,
                                     node.sloc_range.start.line)


for node in nodes:
    print('== From {} =='.format(def_img(node)))

    for sym in symbols:
        elts = node.children_env.get(sym)
        print('  {} : {}'.format(
            sym.ljust(symbol_width),
            (', '.join(def_img(elt.el) for elt in elts)
             if elts else
             '<none>')
        ))

print 'Done.'
