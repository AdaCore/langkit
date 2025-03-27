import sys

import libfoolang


print("main.py: Running...")

test_case_number = 0


def test_case(**units):
    global test_case_number
    test_case_number = test_case_number + 1
    ctx = libfoolang.AnalysisContext()

    header = "Test case {}".format(test_case_number)
    print(header)
    print("=" * len(header))
    print("")

    # Prepopulate all units so that results take every unit into account
    for unit_name, unit_content in units.items():
        u = ctx.get_from_buffer(unit_name, unit_content)
        u.populate_lexical_env()

    for unit_name, unit_content in units.items():
        print(unit_name)
        print("-" * len(unit_name))
        print("")
        u = ctx.get_from_buffer(unit_name, unit_content)
        if u.diagnostics:
            for d in u.diagnostics:
                print(d)
            sys.exit(1)
        for x in u.root.p_env_get_all:
            print("    def {}({})".format(x.f_name.text, x.sloc_range))
        print("")


test_case(
    unit_1=b"""
def program {
    def node_a
    def node_b
    def node_c
    def node_d
    def node_e
}
"""
)

test_case(
    unit_1=b"""
def program {
    def node_d
    def node_b
    def node_a
    def node_c
    def node_e
}
"""
)

test_case(
    unit_1=b"""
def program {
    def node_a
    def node_b
    def node_a
    def node_b
}
"""
)

test_case(
    unit_1=b"""
def program {
    def node_b
    def node_a
    def node_b
    def node_a
}
"""
)

test_case(
    unit_1=b"def unit_1 {def node_c def node_d}",
    unit_2=b"def unit_2 {def node_b def node_a}",
)


print("main.py: Done.")
