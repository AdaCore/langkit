#include <stdio.h>
#include <stdlib.h>

#include <libfoolang.h>

#include "utils_exc.h"

/* Print the exception that was raised by the last API call.  When
   there was no such exception, print nothing if OR_SILENT, and print "Got no
   exception" otherwise.  Return if there was an exception.  */

bool
print_exception (bool or_silent)
{
  const foo_exception *exc = foo_get_last_exception ();
  if (exc != NULL)
    {
      char *exc_name = foo_exception_name (exc->kind);
      printf ("Got an exception (%s):\n  %s\n",
	      exc_name,
	      exc->information);
      free (exc_name);
    }
  else if (! or_silent)
    puts ("Got no exception\n");
  return false;
}

/* If the last API call raised an error, print it and abort the process.  */

void
abort_on_exception (void)
{
  if (print_exception (true))
    exit (1);
}
