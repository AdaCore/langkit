#ifndef UTILS_EXC_H
#define UTILS_EXC_H

#include <stdbool.h>
#include <stdio.h>

#include "utils_lib.h"

/* Print the exception that was raised by the last API call.  When
   there was no such exception, print nothing if OR_SILENT, and print "Got no
   exception" otherwise.  Return if there was an exception.  */

static bool
print_exception (bool or_silent)
{
  const LIB_SYMBOL (exception) *exc = LIB_SYMBOL(get_last_exception) ();
  if (exc != NULL)
    {
      char *exc_name = LIB_SYMBOL (exception_name) (exc->kind);
      printf ("Got an exception (%s):\n  %s\n",
	      exc_name,
	      exc->information);
      free (exc_name);
      return true;
    }
  else if (! or_silent)
    puts ("Got no exception\n");
  return false;
}

/* If the last API call raised an error, print it and abort the process.  */

static void
abort_on_exception (void)
{
  if (print_exception (true))
    exit (1);
}

#endif /* UTILS_EXC_H */
