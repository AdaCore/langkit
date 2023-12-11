#include <stdlib.h>
#include <string.h>

#include "utils_exc.h"
#include "utils_unit.h"

/* Return the base filename for UNIT.  */

char *unit_basename (foo_analysis_unit unit)
{
  char *filename;
  char *basename;
  char *result;
  size_t len;
  char *c;

  filename = foo_unit_filename (unit);
  abort_on_exception ();

  basename = filename;
  for (c = basename; *c; ++c)
    if (*c == '/' || *c == '\\')
      basename = c + 1;

  len = strlen (basename) + 1;
  result = malloc (len);
  memcpy (result, basename, len);
  free (filename);
  return result;
}

/* Output on STREAM an image of UNIT.  */

void
fprint_unit (FILE *stream, foo_analysis_unit unit)
{
  /* Get the base name for this anaysis unit.  This is necessary to ensure the
     test output is determinisitic.  */
  char *basename = unit_basename (unit);

  fprintf (stream, "<AnalysisUnit %s>", basename);
  free (basename);
}
