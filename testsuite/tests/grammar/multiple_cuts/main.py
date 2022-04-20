import libfoolang


inputs = [
    ('complete case 1', "def a"),
    ('complete case 2', "def a (b)"),
    ('complete case 3', "def a (b) {c}"),
    ('complete case 4', "var a"),
    ('complete case 5', "var a (b)"),
    ('complete case 6', "var a (b, c, d)"),
    ('complete case 7', ". a (b)"),
    ('complete case 8', ". a (b) {c}"),
    ('complete case 9', ", a b"),
    ('complete case 10', "(a) , b c"),
    # The def and var rules check that incomplete results are produced
    # regarding the presence of several cut parsers.
    ('incomplete case 1', "def"),
    ('incomplete case 2', "def a (b"),
    ('incomplete case 3', "def a (b) {c"),
    ('incomplete case 4', "def a ("),
    ('incomplete case 5', "def a (b) {"),
    ('incomplete case 6', "def a ( {"),
    ('incomplete case 7', "def a (b {c"),
    ('incomplete case 8', "var"),
    ('incomplete case 9', "var a ("),
    ('incomplete case 10', "var a ()"),
    ('incomplete case 11', "var a (b, c, d"),
    # The dot rule checks that an incomplete result is produced if only the
    # optional part can set the no_backtracing variable.
    ('incomplete case 12', ". a (b"),
    ('incomplete case 13', ". a (b) {"),
    ('incomplete case 14', ". a ( {"),
    # The comma rule is similar to the dot one but the optional part is at the
    # beginning of the rule.
    ('incomplete case 15', ", b"),
    ('incomplete case 16', "(a) , b"),
    ('incomplete case 17', "(a , b"),
]

ctx = libfoolang.AnalysisContext()

for name, text in inputs:
    print(f"=== {name}: {text} ===")
    print()
    u = ctx.get_from_buffer("buffer", buffer=text)

    for d in u.diagnostics:
        print(d)
    u.root.dump()
    print()
