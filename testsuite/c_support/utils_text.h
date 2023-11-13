#ifndef UTILS_TEXT_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include <libfoolang.h>

extern void fprint_text (FILE *stream, foo_text *text, bool with_quotes);

#endif /* UTILS_TEXT_H */
