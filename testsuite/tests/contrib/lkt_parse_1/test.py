"""
Test langkit's new concrete syntax parser.

RA22-015: This is a temporary test, we'll get rid of it when tests are ported
to the new syntax.
"""

from __future__ import absolute_import, division, print_function

import os
from os import path as P
import subprocess
import sys


LK_LIB_DIR = P.join(os.environ['LANGKIT_ROOT_DIR'], 'contrib', 'lkt')
TESTS_DIR = P.join(os.environ['LANGKIT_ROOT_DIR'], 'testsuite', 'tests')

o = subprocess.check_output(
    [sys.executable, P.join(LK_LIB_DIR, 'manage.py'), 'make', '-P'],
    # CWD is the lib's directory so that lib is generated in the lib's dir, not
    # in the test dir.
    cwd=LK_LIB_DIR
)

tests = sorted(((P.join(root, f), P.basename(root))
                for root, _, files in os.walk(TESTS_DIR)
                for f in files
                if f == 'expected_concrete_syntax.lkt'), key=lambda x: x[1])

# TODO: for the moment we'll use a whitelist for tests, eventually we want to
# parse them all.
test_whitelist = {
    'abstract_fields', 'add_to_env_foreign', 'add_to_env_mult_dests',
    'analysis_unit', 'analysis_unit_from_node', 'array_types',
    'auto_ple_dispatcher', 'auto_populate', 'bare_lexing',
    'c_text_to_locale_string', 'character', 'custom_parsing_rule',
    'dflt_arg_val', 'dynvar_bind', 'dynvars_dflt', 'entity_cast', 'entity_eq',
    'entity_field_access', 'entity_length', 'entity_map', 'enum_node_inherit',
    'env_get_all', 'exposed_bare_nodes', 'external', 'field_introspection',
    'foreign_env_md', 'ghost_nodes', 'hashes', 'import_argcount', 'is_a',
    'let_underscore', 'lifetimes', 'logging', 'lookup_token', 'lower_dispatch',
    'lower_dispatch_rewrite', 'map_index', 'memoized_big_table',
    'memoized_env', 'memoized_inf_recurs', 'memoized_unit', 'new_node',
    'newline', 'node_comparison', 'node_conversion',
    'node_env_concrete_subclass', 'node_env_empty', 'node_negative_index',
    'node_none_check', 'node_type_introspection', 'null_list_get',
    'ple_resilience', 'ple_resilience_2', 'populate_error', 'pred_kind_in',
    'properties_introspection', 'qualifier_sloc_range', 'rebindings',
    'ref_after_reparse', 'rewriting', 'siblings', 'stack_overflow',
    'symbol_type', 'tokens', 'trailing_garbage', 'unbounded_string_buffer',
    'unicode_buffer', 'unit_canon', 'unit_filename', 'unparse_or_skip',
    'wrapper_caches'
}

whitelisted_tests = [t for t in tests if t[1] in test_whitelist]

for full_syntax_path, test_name in whitelisted_tests:
    header = 'Parsing concrete syntax for test {}'.format(test_name)
    print("{}\n{}\n".format(header, "=" * len(header)))
    sys.stdout.flush()
    subprocess.check_call(
        [P.join(LK_LIB_DIR, 'build', 'bin', 'parse'), "-f",
         full_syntax_path]
    )
    print()
