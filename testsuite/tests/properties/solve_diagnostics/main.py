import re
import sys

import libfoolang


template_re = re.compile(r"(?P<literal_brace>\{\{)|(?P<placeholder>\{\})")


def print_diagnostic(diag):
    template = diag.message_template
    next_arg = 0
    msg = ""
    remainder_index = 0
    for m in template_re.finditer(template):
        msg += template[remainder_index : m.start()]
        remainder_index = m.end()

        groups = m.groupdict()
        if groups["literal_brace"]:
            msg += "{"
        else:
            assert groups["placeholder"]
            msg += str(diag.args[next_arg])
            next_arg += 1

    msg = str(diag.location.sloc_range) + ":" + msg
    for ctx in diag.contexts:
        msg += f"\n  with {ctx.ref_node.text} => {ctx.decl_node.text}"
    print(msg)
    print()


def print_result(result):
    if result.success:
        print("success")
    else:
        for diag in result.diagnostics:
            print_diagnostic(diag)


print("main.py: Running...")

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
    print_result(node.p_resolve)
    print()


print("-- test_escaping")
print_result(u.root.f_stmts[0].f_name.p_test_escaping_resolve)

print("main.py: Done.")
