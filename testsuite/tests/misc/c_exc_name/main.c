#include <stdio.h>
#include <stdlib.h>

#include "libfoolang.h"

int
main (void)
{
  const foo_exception *exc;
  foo_node entity;

  foo_unit_root (NULL, &entity);
  exc = foo_get_last_exception ();
  if (exc != NULL)
    {
      char *exc_name = foo_exception_name (exc->kind);
      printf ("Got %s\n", exc_name);
      free (exc_name);
    }

  puts ("main.c: Done.");
  return 0;
}
