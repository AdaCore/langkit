#include <stdlib.h>
#include <string.h>

#include "utils_text.h"

void
fprint_char (FILE *stream, uint32_t c, bool with_quotes)
{
  if ((with_quotes && c == '"')
      || c == '\\')
    {
      fprintf(stream, "\\%c", (char) c);
    }
  else if (0x20 <= c && c <= 0x7f)
    {
      fputc(c, stream);
    }
  else if (c <= 0xff)
    {
      fprintf(stream, "\\x");
      fprintf(stream, "%02x", c);
    }
  else if (c <= 0xffff)
    {
      fprintf(stream, "\\u");
      fprintf(stream, "%02x", c >> 8);
      fprintf(stream, "%02x", c & 0xff);
    }
  else
    {
      fprintf(stream, "\\U");
      fprintf(stream, "%02x", c >> 24);
      fprintf(stream, "%02x", (c >> 16) & 0xff);
      fprintf(stream, "%02x", (c >> 8) & 0xff);
      fprintf(stream, "%02x", c & 0xff);
    }
}

/* Output on STREAM the text in TEXT, escaping non-ASCII characters with
   Python-style sequences. If WITH_QUOTES is false, do not output the boundary
   quotes.  */

void
fprint_text (FILE *stream, foo_text *text, bool with_quotes)
{
  unsigned i;

  if (with_quotes)
    fputc('"', stream);

  for (i = 0; i < text->length; i++)
    fprint_char (stream, text->chars[i], with_quotes);

  if (with_quotes)
    fputc('"', stream);
}

/* Print a text buffer on the standard output. Special characters are escaped
   except \n, which is printed as a real line break. Print PREFIX before each
   written line.  */

void
print_indented_buffer (const char *prefix, foo_text *text)
{
  bool first = true;
  size_t i;

  for (i = 0; i < text->length; i++)
    {
      const uint32_t c = text->chars[i];

      if (first)
	printf ("%s", prefix);
      first = false;
      if (c == '\n')
	{
	  first = true;
	  puts ("");
	}
      else
        fprint_char (stdout, c, 0);
    }
  if (! first)
    puts ("");
}

/* Helper to allocate a text object.  */

void
alloc_text (const char *buffer, foo_text *text)
{
  size_t i;

  text->length = strlen (buffer);
  text->chars = malloc (4 * text->length);
  text->is_allocated = 1;

  for (i = 0; i < text->length; ++i)
    text->chars[i] = buffer[i];
}

/* Helper to free a text object allocated with ALLOC_TEXT.  */

void
free_text (foo_text *text)
{
  free (text->chars);
}
