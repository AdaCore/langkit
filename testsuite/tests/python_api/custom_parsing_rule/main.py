import sys

import libfoolang as lfl


print("main.py: Running...")


def parse(filename, content, rule):
    result = ctx.get_from_buffer(filename, content, rule=rule)
    if result.diagnostics:
        print("Errors in {}:".format(filename))
        for d in result.diagnostics:
            print("  {}".format(d))
        sys.exit(1)
    return result


ctx = lfl.AnalysisContext()

n1 = parse("foo1.txt", b"def f(x) = x + 2", lfl.default_grammar_rule).root
n2 = parse("foo2.txt", b"3", lfl.GrammarRule.expr_rule).root

print("{} ({})".format(n1.text, type(n1)))
print("{} ({})".format(n2.text, type(n2)))

print("main.py: Done.")
