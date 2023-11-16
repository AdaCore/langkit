#ifndef UTILS_UNIT_H

#include <stdio.h>

#include <libfoolang.h>

extern char *unit_basename (foo_analysis_unit unit);
extern void fprint_unit (FILE *stream, foo_analysis_unit unit);

#endif /* UTILS_UNIT_H */
