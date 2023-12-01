#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libfoolang.h"

#include "utils_exc.h"
#include "utils_text.h"

static void
print_node (foo_node *node)
{
  foo_text text;

  foo_node_image (node, &text);
  abort_on_exception ();

  fprint_text (stdout, &text, 0);

  foo_destroy_text (&text);
  abort_on_exception ();
}

int
main (void)
{
  const char *buffer = "example\nexample\n";
  const size_t buffer_size = strlen (buffer);

  foo_analysis_context context;
  foo_analysis_unit unit;

  foo_node root, root_copy;
  foo_node_kind_enum node_kind;
  foo_node *children;
  unsigned children_count;

  foo_token *tokens;
  int token_kind;
  char *token_kind_name;

  foo_text text;

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

  unit = foo_get_analysis_unit_from_buffer (
    /* context= */ context,
    /* filename= */ "main.txt",
    /* charset= */ NULL,
    /* buffer= */ buffer,
    /* buffer_size= */ buffer_size,
    /* foo_grammar_rule= */ foo_default_grammar_rule
  );
  abort_on_exception ();

  /* Get the root node and check its kind.  */
  foo_unit_root (unit, &root);
  abort_on_exception ();
  node_kind = foo_node_kind (&root);
  abort_on_exception ();
  foo_kind_name (node_kind, &text);
  abort_on_exception ();
  fprint_text (stdout, &text, 1);
  printf ("\n");
  foo_destroy_text (&text);
  abort_on_exception ();

  /* Check that the root's unit is the same as the original unit.  */
  foo_analysis_unit other_unit = foo_node_unit (&root);
  abort_on_exception ();
  if (other_unit != unit)
  {
    printf ("Analysis units should be equal!\n");
  }

  /* Check that "create_bare_entity" works as expected.  */
  foo_create_bare_entity (root.node, &root_copy);
  abort_on_exception ();
  printf ("Bare entity created from the root node: ");
  print_node (&root_copy);
  puts ("");

  /* Print the list of children for the root node.  */
  children_count = foo_node_children_count (&root);
  printf ("Root children count = %u\n", children_count);
  children = (foo_node *) malloc (children_count * sizeof (foo_node));
  for (unsigned i = 0; i < children_count; i++)
  {
    foo_node_child (&root, i, &children[i]);
    abort_on_exception ();
    printf ("Child %u of root = ", i + 1);
    print_node (&children[i]);
    puts ("");
  }

  /* Print the start token for each child.  */
  tokens = (foo_token *) malloc (children_count * sizeof (foo_token));
  for (unsigned i = 0; i < children_count; i++)
  {
    foo_foo_node_token_start (&children[i], &tokens[i]);
    abort_on_exception ();
    token_kind = foo_token_get_kind (&tokens[i]);
    abort_on_exception ();
    token_kind_name = foo_token_kind_name (token_kind);
    abort_on_exception ();
    printf ("Start token of the %u child = \"%s\"\n", i + 1, token_kind_name);
    free (token_kind_name);
  }

  /* Check that the two first tokens are equivalent.  */
  foo_bool equivalence = foo_token_is_equivalent (&tokens[0], &tokens[1]);
  if (! equivalence)
  {
    printf ("Tokens should be equivalent!\n");
  }

  foo_context_decref (context);
  free (children);
  free (tokens);

  puts ("main.c: Done.");
  return 0;
}
