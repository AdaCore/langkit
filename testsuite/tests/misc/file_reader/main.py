import sys


print("main.py: Starting...")

import libfoolang


fr = libfoolang.FileReader.my_file_reader()
ctx = libfoolang.AnalysisContext(file_reader=fr)


def print_title(label):
    print(label)
    print("=" * len(label))
    print("")


def dump(u):
    if u.diagnostics:
        print("Errors:")
        for d in u.diagnostics:
            print("  {}".format(d))
    else:
        print("Success: {}".format(repr(u.text)))


def parse(filename, charset):
    sys.stdout.flush()
    u = ctx.get_from_file(filename, charset)
    dump(u)


print_title("Parsing foo.txt")
parse("foo.txt", "")
print("")

print_title("Parsing error.txt")
parse("error.txt", "some-charset")
print("")

print_title("Parsing the internal unit")
u = ctx.get_internal_unit
dump(u)
print("")

assert u == ctx.get_from_file("__internal_unit")

print_title("Reparsing the internal unit")
try:
    ctx.get_from_file("__internal_unit", reparse=True)
except libfoolang.PreconditionFailure as exc:
    print("PreconditionFailure: {}".format(exc))
print("")

print_title("Using buffer-based parsing APIs")
print(".get_from_buffer:")
try:
    ctx.get_from_buffer("from_buffer.txt", buffer=b"example")
except libfoolang.PreconditionFailure as exc:
    print("PreconditionFailure: {}".format(exc))
print("")

print(".reparse:")
u = ctx.get_from_file("foo.txt")
try:
    u.reparse(buffer=b"foobar")
except libfoolang.PreconditionFailure as exc:
    print("PreconditionFailure: {}".format(exc))
print("")

print("main.py: Done.")
sys.stdout.flush()
