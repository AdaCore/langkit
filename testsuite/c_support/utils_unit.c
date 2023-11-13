#include <stdlib.h>

#include "utils_exc.h"
#include "utils_unit.h"

/* Output on STREAM an image of UNIT.  */

void
fprint_unit(FILE *stream, foo_analysis_unit unit) {
  /* Get the base name for this anaysis unit.  This is necessary to ensure the
     test output is determinisitic.  */
  char *filename;
  char *basename;
  char *c;

  filename = foo_unit_filename (unit);
  abort_on_exception ();

  basename = filename;
  for (c = basename; *c; ++c)
    if (*c == '/' || *c == '\\')
      basename = c + 1;

  fprintf (stream, "<AnalysisUnit %s>", basename);
  free (filename);
}
