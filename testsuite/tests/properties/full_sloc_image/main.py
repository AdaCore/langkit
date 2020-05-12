import libfoolang


ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer('foo.txt', 'example')
print(libfoolang._py2to3.text_repr(u.root.full_sloc_image))
