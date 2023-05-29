#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libfoolang.h"

#include "utils_exc.h"
#include "utils_text.h"

void
c_main (void)
{
  // Declaring the buffer
  const char *buffer = "example";
  const size_t buffer_size = strlen (buffer);
  const char *name = "main.txt";

  // Declaring the working variables
  foo_analysis_context context;
  foo_analysis_unit unit;
  foo_base_entity root;
  foo_text text;

  // Initialize the analysis context
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

  // Create the analysis unit
  unit = foo_get_analysis_unit_from_buffer (
    /* context= */ context,
    /* filename= */ name,
    /* charset= */ NULL,
    /* buffer= */ buffer,
    /* buffer_size= */ buffer_size,
    /* foo_grammar_rule= */ foo_default_grammar_rule
  );
  abort_on_exception ();

  // Get the unit root
  foo_unit_root (unit, &root);
  abort_on_exception ();

  // Set the dummy field to an arbitrary value to ensure that it is being
  // ignored by the Ada implementation.
  root.info.md.dummy = 42;

  // Get the root text
  foo_node_image (&root, &text);
  abort_on_exception ();

  printf ("ROOT IMAGE = ");
  fprint_text (stdout, &text, 1);
  printf ("\n");
  foo_destroy_text (&text);

  // Call the identity function on an empty structure.  This is supposed to be
  // a no-op since it returns an empty result with no side effect. Do the call
  // to make sure it does not crash.
  foo_internal_my_struct source;
  foo_internal_my_struct res;
  foo_example_p_identity (&root, &source, &res);
  abort_on_exception ();

  // Get a new non empty struct
  foo_internal_non_empty_struct non_empty;
  foo_example_p_new_non_empty_struct (&root, &non_empty);
  abort_on_exception ();

  // Get the big integer from the non-empty and display it
  foo_big_integer big_integer;
  foo_example_p_get_big_int (&root, &non_empty, &big_integer);
  abort_on_exception ();

  foo_big_integer_text (big_integer, &text);
  abort_on_exception ();

  printf ("BIG INTEGER = ");
  fprint_text (stdout, &text, 1);
  printf ("\n");
  foo_destroy_text (&text);
  abort_on_exception ();

  // Get the character from the non-empty and display it
  uint32_t character;
  foo_example_p_get_char (&root, &non_empty, &character);
  abort_on_exception ();

  printf ("CHARACTER = %u\n", character);
}
