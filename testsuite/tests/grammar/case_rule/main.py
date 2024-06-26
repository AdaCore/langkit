import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()

for label, text in (
    ('single-char', "'c'"),
    ('simple-attr', "a'b"),
    ('char-dot', "'a'.b"),
    ('id-char', "a'b'"),
    ('unicode-id-char', "\xe9'\U0001f642'"),
    ('access', "access T"),
    ('tick-access', "T'access"),
):
    print('== {} =='.format(label))
    u = ctx.get_from_buffer('{}.txt'.format(label), text)
    if u.diagnostics:
        for d in u.diagnostics:
            print(d)
        print('--')
    for t in u.iter_tokens():
        print(ascii(str(t)))
    print('')

print('main.py: Done.')
