#ifndef UTILS_TEXT_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include <libfoolang.h>

extern void fprint_char (FILE *stream, uint32_t c, bool with_quotes);
extern void fprint_text (FILE *stream, foo_text *text, bool with_quotes);
extern void print_indented_buffer (const char *prefix, foo_text *text);
extern void alloc_text (const char *buffer, foo_text *text);
extern void free_text (foo_text *text);

#endif /* UTILS_TEXT_H */
