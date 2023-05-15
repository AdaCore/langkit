#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libfoolang.h"

#include "utils_exc.h"
#include "utils_text.h"

int
main (void)
{
  // Create the source code buffer.
  const char *buffer = "example\nexample\n";
  const size_t buffer_size = strlen (buffer);

  // Declaring the working variables.
  foo_analysis_context context;
  foo_analysis_unit unit;

  foo_base_entity root;
  foo_node_kind_enum node_kind;
  foo_base_entity *children;
  unsigned children_count;

  foo_token *tokens;
  char *token_kind_name;

  foo_text text;

  // Create the analysis context.
  context = foo_allocate_analysis_context ();
  foo_initialize_analysis_context (
    /* context= */ context,
    /* charset= */ NULL,
    /* file_reader= */ NULL,
    /* unit_provider= */ NULL,
    /* event_handler= */ NULL,
    /* with_trivia= */ 1,
    /* tab_stop= */ 8
  );
  abort_on_exception ();

  // Create the analysis unit.
  unit = foo_get_analysis_unit_from_buffer (
    /* context= */ context,
    /* filename= */ "main.txt",
    /* charset= */ NULL,
    /* buffer= */ buffer,
    /* buffer_size= */ buffer_size,
    /* foo_grammar_rule= */ foo_default_grammar_rule
  );
  abort_on_exception ();

  // Get the root node.
  foo_unit_root (unit, &root);
  abort_on_exception ();

  // Verify the root kind.
  node_kind = foo_node_kind (&root);
  abort_on_exception ();
  foo_kind_name (node_kind, &text);
  abort_on_exception ();
  fprint_text (stdout, &text, 1);
  printf ("\n");
  foo_destroy_text (&text);
  abort_on_exception ();

  // Assert that the root's unit is the same as the original unit.
  foo_analysis_unit other_unit = foo_node_unit (&root);
  abort_on_exception ();
  if (other_unit != unit) {
    printf ("Analysis units should be equal!\n");
  }

  // Get the root children.
  children_count = foo_node_children_count (&root);
  printf ("Root children count = %u\n", children_count);
  children = (foo_base_entity *) malloc (
    children_count * sizeof (foo_base_entity)
  );
  for (unsigned i = 0; i < children_count; i++) {
    foo_node_child (&root, i, &children[i]);
    abort_on_exception ();
    node_kind = foo_node_kind (&children[i]);
    abort_on_exception ();
    foo_kind_name (node_kind, &text);
    abort_on_exception ();
    printf ("Child %u of root = ", i + 1);
    fprint_text (stdout, &text, 1);
    printf ("\n");
    foo_destroy_text (&text);
    abort_on_exception ();
  }

  // Get the tokens.
  tokens = (foo_token *) malloc (
    children_count * sizeof (foo_token)
  );
  for (unsigned i = 0; i < children_count; i++) {
    foo_foo_node_token_start (&children[i], &tokens[i]);
    abort_on_exception ();
    token_kind_name = foo_token_kind_name (tokens[i].kind);
    abort_on_exception ();
    printf(
      "Start token of the %u child = \"%s\"\n",
      i + 1,
      token_kind_name
    );
    free (token_kind_name);
  }

  // Assert that the two first tokens are equivalent.
  foo_bool equivalence = foo_token_is_equivalent (
    &tokens[0],
    &tokens[1]
  );
  if (equivalence == 0) {
    printf ("Tokens should be equivalent!\n");
  }

  // Free the context.
  foo_context_decref (context);

  // Free the children and tokens.
  free (children);
  free (tokens);

  puts ("main.c: Done.");
  return 0;
}
