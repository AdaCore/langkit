#include <stdio.h>
#include <stdlib.h>

#include "libfoolang.h"

static foo_stack_trace
process_exc (void)
{
  const foo_exception *exc = foo_get_last_exception ();
  if (exc != NULL)
    {
      char *str;
      int i, size;
      void **elements;
      foo_stack_trace result;

      str = foo_exception_name (exc->kind);
      printf ("Exception name: %s\n", str);
      free (str);
      puts ("");

      size = foo_stack_trace_size (exc->stack_trace);
      elements = malloc (sizeof (void *) * size);
      for (i = 0; i < size; ++i)
	elements[i] = foo_stack_trace_element (exc->stack_trace, i);
      result = foo_create_stack_trace (size, elements);
      free (elements);
      return result;
    }

  puts ("Got no exception...");
  return NULL;
}

static void
print_stack_trace (const char *label, foo_stack_trace trace)
{
  char *str;

  printf ("Symbolized stack trace for %s: BEGIN\n", label);
  str = foo_symbolize_stack_trace (trace);
  puts (str);
  free (str);
  puts ("END\n");
}

int
main (void)
{
  foo_node entity;
  foo_token token;
  foo_stack_trace t1, t2;

  puts ("Create a first stack trace...");

  foo_unit_root (NULL, &entity);
  t1 = process_exc ();

  puts ("Create a second stack trace...");
  foo_unit_first_token (NULL, &token);
  t2 = process_exc ();

  print_stack_trace ("foo_unit_root", t1);
  print_stack_trace ("foo_unit_first_token", t2);
  foo_destroy_stack_trace (t1);
  foo_destroy_stack_trace (t2);

  puts ("main.c: Done.");
  return 0;
}
