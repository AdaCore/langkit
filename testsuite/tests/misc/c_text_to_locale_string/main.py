import ctypes

import libfoolang


print("main.py: Running...")

# Import the C API helper
ttls = libfoolang._c_lib.foo_text_to_locale_string
ttls.argtypes = [ctypes.POINTER(libfoolang._text)]
ttls.restype = ctypes.c_char_p

# Create an input value and run it
text = libfoolang._text._unwrap("Hello")
result = ttls(ctypes.byref(text))

print("> {}".format(result))

print("main.py: Done.")
