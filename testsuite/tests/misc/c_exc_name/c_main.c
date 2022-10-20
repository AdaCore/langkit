#include <stdio.h>
#include <stdlib.h>

#include "libfoolang.h"

void
c_main (void)
{
  const foo_exception *exc;
  foo_base_entity entity;

  foo_unit_root (NULL, &entity);
  exc = foo_get_last_exception ();
  if (exc != NULL)
    {
      char *exc_name = foo_exception_name (exc->kind);
      printf ("Got %s\n", exc_name);
      free (exc_name);
    }
}
