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
  const char *buffer = "my_ident";
  const size_t buffer_size = strlen (buffer);

  // Declaring the working variables.
  foo_analysis_context context;
  foo_analysis_unit unit;

  foo_node root;

  foo_text text;
  foo_symbol_type sym;
  foo_symbol_type_array sym_array;

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

  // Get the symbol for the root node
  foo_example_p_sym (&root, &sym);
  foo_symbol_text (&sym, &text);
  fprint_text (stdout, &text, 1);
  printf ("\n");
  foo_destroy_text (&text);

  // Get symbol array
  foo_example_p_sym_array(&root, &sym_array);
  for (int i = 0; i < sym_array->n; i++) {
      foo_symbol_type sym = sym_array->items[i];
      foo_symbol_text (&sym, &text);
      fprint_text (stdout, &text, 1);
      printf ("\n");
      foo_destroy_text (&text);
  }
  foo_symbol_type_array_dec_ref(sym_array);

  // Free the context.
  foo_context_decref (context);

  puts ("main.c: Done.");
  return 0;
}
