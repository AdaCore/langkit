import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('foo.txt', b'{example example')

print("Diagnostics:")
for d in u.diagnostics:
    print(d)
print("")

print("Token range:")
text_range = libfoolang.Token.text_range(u.first_token, u.last_token)
print(repr(text_range))
print("")

print('main.py: Done.')
