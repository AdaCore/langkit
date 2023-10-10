#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libfoolang.h"

#include "utils_exc.h"

static void
print_ints (foo_int_array ints)
{
  bool first = true;
  printf ("[");
  for (int i = 0; i < ints->n; ++i)
    {
      if (first)
	first = false;
      else
	printf (", ");
      printf ("%d", ints->items[i]);
    }
  printf ("]");
}

int
main (void)
{
  const char *buffer = "example\n";
  const size_t buffer_size = strlen (buffer);

  foo_analysis_context context;
  foo_analysis_unit unit;
  foo_node root;
  foo_int_array ints1, ints2;

  context = foo_allocate_analysis_context ();
  abort_on_exception ();

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

  foo_unit_root (unit, &root);
  abort_on_exception ();

  /* Create an example array of ints.  */
  ints1 = foo_int_array_create (3);
  abort_on_exception ();
  ints1->items[0] = 10;
  ints1->items[1] = 20;
  ints1->items[2] = 25;

  /* Call the property to use it.  */
  printf ("p_int_array_id(");
  print_ints (ints1);
  printf (") = ");
  foo_foo_node_p_int_array_id (&root, ints1, &ints2);
  abort_on_exception ();

  /* Release that array of ints.  */
  foo_int_array_dec_ref (ints1);
  abort_on_exception ();

  /* Print the property result and release it.  */
  print_ints (ints2);
  foo_int_array_dec_ref (ints2);
  abort_on_exception ();
  printf ("\n");

  foo_context_decref (context);

  puts ("main.c: Done.");
  return 0;
}
