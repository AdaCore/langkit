import gc

import libfoolang


print('main.py: Running...')


def check(label, t):
    print(f"{label}:")
    try:
        print(f"-> {t}")
    except libfoolang.StaleReferenceError:
        print("Got a StaleReferenceError")
    print("")


# Create an analysis unit and get a reference to one of its tokens. Then
# perform the only legitimate use of this token referenc.
u = libfoolang.AnalysisContext().get_from_buffer('main.txt', b"example")
t = u.first_token
check("Valid", t)

# Reparse the analysis unit, making the token reference stale even though the
# analysis context and its token data handlers are still the same.
u.reparse(b"# example")
check("After reparse", t)

# Now destroy the analysis unit (and its context), making the token reference
# stale.
t = u.first_token
del u
gc.collect()
check("After context destruction", t)

print('main.py: Done.')
