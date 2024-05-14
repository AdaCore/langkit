import sys

import libfoolang


def print_diagnostic(diag):
    msg = diag.template
    for arg in diag.args:
        msg = msg.replace("{}", str(arg), 1)
    msg = str(diag.location.sloc_range) + ":" + msg
    for ctx in diag.contexts:
        msg += f"\n  with {ctx.ref_node.text} => {ctx.decl_node.text}"
    print(msg)
    print()


print('main.py: Running...')

main_program = """
def foo(number, number)
def foo(string, string)

foo(1, 2)
foo("a", "b")
foo(1, "b")
foo("a", 2)
"a" : number
"""

ctx = libfoolang.AnalysisContext()
u = ctx.get_from_buffer("main.txt", buffer=main_program)
if u.diagnostics:
    for d in u.diagnostics:
        print(d)
    sys.exit(1)


for node in u.root.findall(libfoolang.Resolvable):
    print(f"-- Resolving {node} --")
    result = node.p_resolve
    if result.success:
        print("success")
    else:
        for diag in result.diagnostics:
            print_diagnostic(diag)
    print()

print('main.py: Done.')
