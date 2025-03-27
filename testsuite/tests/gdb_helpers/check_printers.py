"""
Checks for pretty-printers.
"""

from helpers import (
    break_dsl,
    check_var_state,
    run_continue,
    run_foobreak,
    start_gdb,
)


gdb = start_gdb("printers")


# Check pretty-printing for analysis units
run_foobreak("FooNode.id_unit")
run_continue()
gdb.print_expr("u", "<AnalysisUnit @...main.txt>")
run_continue()
gdb.print_expr("u", "null")

# Check pretty-printing for nodes
node_img = "<Example main.txt:1:1-1:8>"
run_foobreak("FooNode.id_node")
run_continue()
gdb.print_expr("n", node_img)
run_continue()
gdb.print_expr("n", "null")

# Check pretty-printing for nodes
break_dsl("test_strings")
run_continue()
check_var_state("empty", '""')
check_var_state("foo", '"foo"')
check_var_state("s_quote", '"\'"')
check_var_state("d_quote", '""""')
check_var_state("quote_mix", '"""\'"')
check_var_state("lf", '"["00000a"]"')
check_var_state("nul", '"["000000"]"')

# Check pretty-printing for symbols

break_dsl("test_symbols")
run_continue()
check_var_state("empty", "No_Symbol")
check_var_state("foo", '"foo"')

# Check pretty-printing for rebindings
break_dsl("test_rebindings")
run_continue()
check_var_state("null_var", "<Rebindings null>")
check_var_state("r1", "<Rebindings [main.txt:1:9]>")
check_var_state("r2", "<Rebindings [main.txt:1:17]>")
check_var_state("concat", "<Rebindings [main.txt:1:9, main.txt:1:17]>")

# Check pretty-printing for lexical envs
break_dsl("test_envs")
run_continue()
check_var_state("null_var", "<LexicalEnv root>")
check_var_state("primary", f"<LexicalEnv (primary) for {node_img}>")
check_var_state(
    "orphan",
    "<LexicalEnv (orphaned)>"
    f" = {{[orphaned] = <LexicalEnv (primary) for {node_img}>}}",
)
check_var_state(
    "group",
    "<LexicalEnv (grouped)>"
    f" = {{<LexicalEnv (primary) for {node_img}>,"
    " <LexicalEnv root>}",
)
check_var_state(
    "rebound",
    "<LexicalEnv (rebound)>"
    " = {[rebindings] = <Rebindings [main.txt:1:9]>,"
    f" [rebound_env] = <LexicalEnv (primary) for {node_img}>}}",
)

# Check pretty-printing for entities
break_dsl("test_entities")
run_continue()
check_var_state("null_root", "null")
check_var_state("null_example", "null")
check_var_state("ent_root", "<| Example main.txt:1:1-1:8 |>")
check_var_state("ent_example", "<| Example main.txt:1:1-1:8 |>")
check_var_state("rebound", "<| Example main.txt:1:1-1:8 [main.txt:1:9] |>")

# Check pretty-printing for arrays
break_dsl("test_arrays")
run_continue()
check_var_state("empty", "Bare_Foo_Node array of length 0")
check_var_state("single", f"Bare_Foo_Node array of length 1 = {{{node_img}}}")
check_var_state(
    "complete",
    "Bare_Foo_Node array of length 3"
    " = {<Example main.txt:1:1-1:8>,"
    " <Example main.txt:1:9-1:16>,"
    " <Example main.txt:1:17-1:23>}",
)

# Check pretty-printing for vectors
break_dsl("test_vectors")
run_continue()
run_continue()
gdb.print_expr(
    "map_result_vec",
    "Bare_Foo_Node_Array_Access vector of length 1 = {"
    "\n  Bare_Foo_Node array of length 0}",
)

# Check pretty-printing for tokens
break_dsl("test_tokens")
run_continue()
check_var_state("null_var", "No_Token")
check_var_state("first", "<Token 7 1/0 at 1:1-1:8 'example'>")
