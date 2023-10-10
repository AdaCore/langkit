#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libfoolang.h"

#include "utils_exc.h"

static void
print_int_array (foo_int_array ints)
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

static void
print_int_iter (foo_int_iterator ints)
{
  bool first = true;
  int item;
  while (foo_int_iterator_next (ints, &item))
    {
      abort_on_exception ();
      if (first)
	first = false;
      else
	printf (", ");
      printf ("%d", item);
    }
  abort_on_exception ();
}

int
main (void)
{
  const char *buffer = "example\n";
  const size_t buffer_size = strlen (buffer);

  foo_analysis_context context;
  foo_analysis_unit unit;
  foo_base_entity root;
  foo_int_array int_array;
  foo_int_iterator int_iter;

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
  int_array = foo_int_array_create (3);
  abort_on_exception ();
  int_array->items[0] = 10;
  int_array->items[1] = 20;
  int_array->items[2] = 25;

  /* Call the property to use it.  */
  printf ("Iterating on p_int_array_to_iter(");
  print_int_array (int_array);
  printf (") = ");
  foo_foo_node_p_int_array_to_iter (&root, int_array, &int_iter);
  abort_on_exception ();

  /* Release that array of ints.  */
  foo_int_array_dec_ref (int_array);
  abort_on_exception ();

  /* Iterate on the property result and release it.  */
  print_int_iter (int_iter);
  foo_int_iterator_dec_ref (int_iter);
  abort_on_exception ();
  printf ("\n");

  foo_context_decref (context);

  puts ("main.c: Done.");
  return 0;
}
