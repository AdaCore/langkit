"""
Check that control-flow commands work correctly for recursive properties.

When executed, control flow commands ("next", "out", ...) must not stop inside
recursive calls: they must trigger in the same call.
"""

from helpers import (
    break_lkt,
    run_continue,
    run_foonext,
    run_fooout,
    start_gdb,
)


gdb = start_gdb("recursive_cf")


def setup(label: str):
    # Run the test program until the given label
    gdb.test("start recursive_cf", None)
    break_lkt(label)
    run_continue()

    # Delete the breakpoint so that it does not trigger during recursive calls:
    # we want to check that execution skips them.
    gdb.test("delete", None)

    # Sanity check that we end up in the right stack frame
    check_in_root()


def check_in_root():
    gdb.test("foostate node", "node = <FooNodeList main.txt:1:1-1:27>")


########
# next #
########

setup("recursive_inner_loop")
run_foonext(
    "<FooNode.test_recursive_cf at test.lkt:@NUMBER> evaluated"
    " to: 2"
    "\n"
    "\nNow evaluating <.map at test.lkt:@NUMBER>"
)
check_in_root()

#######
# out #
#######

setup("recursive_inner_loop")
run_fooout(
    "<FooNode.test_recursive_cf at test.lkt:@NUMBER> evaluated"
    " to: 2"
    "\n"
    "\nNow evaluating <.map at test.lkt:@NUMBER>"
)
check_in_root()
run_fooout(
    "<.map at test.lkt:@NUMBER> evaluated to:"
    " Integer array of length 2 = {2, 1}"
    "\n"
    "\nNow evaluating <ValDecl at test.lkt:@NUMBER>"
)
check_in_root()
