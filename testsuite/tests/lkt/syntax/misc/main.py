import libfoolang


print('main.py: Starting...')

ctx = libfoolang.AnalysisContext()
unit = ctx.get_from_file('leading_trivia.txt')
print(unit.lookup_token(libfoolang.Sloc(3, 1)))

print('main.py: Done.')
print('')
