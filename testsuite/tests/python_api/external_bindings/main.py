import ctypes
import sys

from libfoolang import AnalysisContext, AnalysisUnit

# To make our life easy, this test includes the "user API" in libfoolang
# itself. Real life users will use their own shared library.
from libfoolang import _c_lib as user_api


def flush():
    sys.stdout.flush()
    sys.stderr.flush()


def load_function(name, argtypes, restype):
    result = getattr(user_api, name)
    result.argtypes = argtypes
    result.restype = restype
    return result


print("main.py: Starting...")
print("")

ctx = AnalysisContext()
u = ctx.get_from_buffer("foo.txt", buffer=b"example example")

print("== Check_Context ==")
print("")
check_context = load_function(
    "foo_check_context",
    [AnalysisContext._c_type, ctypes.POINTER(AnalysisContext._c_type)],
    None,
)
flush()

c_ctx = AnalysisContext._unwrap(ctx)
c_rctx = AnalysisContext._c_type()
check_context(c_ctx, ctypes.byref(c_rctx))
rctx = AnalysisContext._wrap(c_rctx)
assert ctx == rctx

print("== Check_Unit ==")
print("")
check_unit = load_function(
    "foo_check_unit",
    [AnalysisUnit._c_type, ctypes.POINTER(AnalysisUnit._c_type)],
    None,
)
flush()

c_u = AnalysisUnit._unwrap(u)
c_ru = AnalysisUnit._c_type()
check_unit(c_u, ctypes.byref(c_ru))
ru = AnalysisUnit._wrap(c_ru)
assert u == ru

print("main.py: Done.")
