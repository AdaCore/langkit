"""
Checks for the control-flow handling commands and the "state" command.
"""

from helpers import (
    run_continue,
    run_foobreak,
    run_foonext,
    run_fooout,
    run_foosi,
    start_gdb,
)


gdb = start_gdb("control_flow")


# State when not in a property
gdb.test("foostate", "Selected frame is not in a property.")

#
# First property call: "next" command
#

# Check state at the start of the property
run_foobreak("FooNode.test_control_flow")
run_continue()
gdb.test(
    "foostate",
    """\
Running FooNode.test_control_flow
from @...test.lkt:@NUMBER
  node = <Example main.txt:1:1-1:8>
  i = 1""",
)

# Also check printing a specific binding
gdb.test("foostate i", "i = 1")
gdb.test("foostate nosuchvar", "No binding called nosuchvar")

# Check state at various points during the property execution
run_foonext("Now evaluating <.parent at test.lkt:@NUMBER>")
run_foonext(
    """\
<.parent at test.lkt:@NUMBER> evaluated to: <FooNodeList main.txt:1:1-1:23>

Now evaluating <.children at test.lkt:@NUMBER>"""
)
run_foonext(
    """\
<.children at test.lkt:@NUMBER> evaluated to: Bare_Foo_Node\
 array of length 3 = {@...}

Now evaluating <ValDecl at test.lkt:@NUMBER>"""
)
run_foonext("Now evaluating <.map at test.lkt:@NUMBER>")
run_foonext("Now evaluating <FooNode.parents at test.lkt:@NUMBER>")
gdb.test(
    "foostate",
    """\
Running FooNode.test_control_flow
from @...test.lkt:@NUMBER
  node = <Example main.txt:1:1-1:8>
  i = 1

Currently evaluating <ValDecl at test.lkt:@NUMBER>
from @...test.lkt:@NUMBER
  nodes = Bare_Foo_Node array of length 3 =\
 {<Example main.txt:1:1-1:8>, <Example main.txt...
  <.parent at test.lkt:@NUMBER> -> <FooNodeList main.txt:1:1-1:23>
  <.children at test.lkt:@NUMBER> -> Bare_Foo_Node array of length 3 =\
 {<Example main.txt:1:1-1:8>, <Example main.txt...

Currently evaluating <ValDecl at test.lkt:@NUMBER>
from @...test.lkt:@NUMBER

Currently evaluating <.map at test.lkt:@NUMBER>
from @...test.lkt:@NUMBER
  n = <Example main.txt:1:1-1:8>

Currently evaluating <FooNode.parents at test.lkt:@NUMBER>
from @...test.lkt:@NUMBER""",
)

#
# Second property call: "out" command
#

# Try to go out of the expression while none is running
run_continue()
gdb.test("fooout", "Not evaluating any expression currently")
for i in range(5):
    run_foonext(None)
run_fooout(
    """\
<FooNode.parents at test.lkt:@NUMBER> evaluated to:\
 Bare_Foo_Node array of length 2 = {@...

Now evaluating <.length at test.lkt:@NUMBER>"""
)
run_fooout(
    """
<.length at test.lkt:@NUMBER> evaluated to: 2

Now evaluating <.children at test.lkt:@NUMBER>"""
)
run_fooout(
    """\
<.children at test.lkt:@NUMBER> evaluated to: \
Bare_Foo_Node array of length 0

Now evaluating <ValDecl at test.lkt:@NUMBER>"""
)
run_fooout(
    """\
<ValDecl at test.lkt:@NUMBER> evaluated to: 1

Now evaluating <Arithmetic '+' at test.lkt:@NUMBER>"""
)
run_fooout(
    """\
<Arithmetic '+' at test.lkt:@NUMBER> evaluated to: 3

Now evaluating <.map at test.lkt:@NUMBER>"""
)
run_fooout(
    """\
<.map at test.lkt:@NUMBER> evaluated to: Integer array of length 3 = {@...

Now evaluating <ValDecl at test.lkt:@NUMBER>"""
)
run_fooout(
    """\
<ValDecl at test.lkt:@NUMBER> evaluated to: 5

Now evaluating <ValDecl at test.lkt:@NUMBER>"""
)
run_fooout("<ValDecl at test.lkt:@NUMBER> evaluated to: 5")
gdb.test("fooout", "Not evaluating any expression currently")

#
# Third property call: "si" command
#

# Try to step inside while no expression is running (property just started)
run_continue()
run_foosi("Now evaluating <.parent at test.lkt:@NUMBER>")
run_foosi(
    """\
<.parent at test.lkt:@NUMBER> evaluated to: \
<FooNodeList main.txt:1:1-1:23>

Now evaluating <.children at test.lkt:@NUMBER>"""
)
run_foosi(
    """\
<.children at test.lkt:@NUMBER> evaluated to:\
 Bare_Foo_Node array of length 3 = {@...

Now evaluating <ValDecl at test.lkt:@NUMBER>"""
)
run_foosi("Now evaluating <.map at test.lkt:@NUMBER>")
run_foosi("Now evaluating <FooNode.parents at test.lkt:@NUMBER>")
run_foosi(
    """\
<FooNode.parents at test.lkt:@NUMBER> evaluated to:\
 Bare_Foo_Node array of length 2 = {@...

Now evaluating <.length at test.lkt:@NUMBER>"""
)
run_foosi(
    """\
<.length at test.lkt:@NUMBER> evaluated to: 2

Now evaluating <.children at test.lkt:@NUMBER>"""
)
run_foosi(
    """\
<.children at test.lkt:@NUMBER> evaluated to: \
Bare_Foo_Node array of length 0

Now evaluating <ValDecl at test.lkt:@NUMBER>"""
)
run_foosi("Now evaluating <FooNode.control_flow_helper at test.lkt:@NUMBER>")

# This should get us into the call to the control_flow_helper property
gdb.test(
    "foosi",
    "@/(Thread \\d+ hit )?/Breakpoint @...,"
    " libfoolang.implementation.foo_node_p_control_flow_helper@...",
)
