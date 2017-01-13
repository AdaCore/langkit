print 'main.py: Running...'


import sys

import libfoolang


source = """
    a (a0, a1, a2)
    b (b0)
    c ()
    d (d0, d2, d3, d4)
"""


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('<buffer>', source)
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)

u.populate_lexical_env()
name_lists = [name_list.p_to_struct
              for name_list in u.root.findall(libfoolang.ListDecl)]

print 'Name lists:'
for nl in name_lists:
    print '  * {}: [{}]'.format(
        nl.f_label.f_tok.text,
        ', '.join(n.f_tok.text for n in nl.f_name_list)
    )

print 'main.py: Done.'
