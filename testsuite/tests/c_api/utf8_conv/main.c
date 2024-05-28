#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libfoolang.h"

#include "utils_exc.h"
#include "utils_text.h"

const char *some_string = "AA \xc3\xa9 BB";
const size_t some_string_length = 8;

const char *some_char = "\xf2\xab\xb3\x9e";
const size_t some_char_length = 4;

static void
print_bytes (const char *buffer, size_t length)
{
  printf ("UTF8[");
  for (size_t i = 0; i < length; ++i)
    {
      if (i > 0)
	printf (" ");
      printf ("%02hhx", buffer[i]);
      if (buffer[i] >= 0x20 && (unsigned char) buffer[i] <= 0x7f)
	printf (" '%c'", buffer[i]);
    }
  puts ("]");
}

static void
check (int bit, uint32_t base)
{
  uint32_t buffer[1] = { 1 << bit | base };
  foo_text src_text = {&buffer[0], 1, 0};

  char *bytes;
  size_t length;
  foo_text dest_text;

  printf ("Bit %2i: ", bit + 1);
  printf ("UTF32[%08llx] -> ", (long long) buffer[0]);

  foo_text_to_utf8 (&src_text, &bytes, &length);
  abort_on_exception ();

  print_bytes(bytes, length);

  foo_text_from_utf8 (bytes, length, &dest_text);
  abort_on_exception ();

  if (dest_text.length != 1)
    printf ("ERROR: mismatching text buffer size (got %i, expected 1)\n",
	    (int) dest_text.length);
  else if (dest_text.chars[0] != buffer[0])
    printf ("ERROR: mismatching text buffer contents (got %08llx)\n",
	    (long long) dest_text.chars[0]);

  foo_destroy_text (&dest_text);
  abort_on_exception ();

  free (bytes);
}

int
main (void)
{
  foo_text text;
  foo_string_type string;
  uint32_t chr;
  char *bytes;
  size_t length;
  int i;

  /* Check that all codepoint bits are well preserved during the UTF32->UTF8
     and UTF8->UTF32 conversion.  */

  for (i = 0; i < 21; ++i)
    check (i, 0);
  puts ("");

  puts ("# 2 bytes");
  for (i = 0; i < 11; ++i)
    check (i, 1 << 7);
  puts ("");

  puts ("# 3 bytes");
  for (i = 0; i < 16; ++i)
    check (i, 1 << 11);
  puts ("");

  puts ("# 4 bytes");
  for (i = 0; i < 20; ++i)
    check (i, 1 << 17);
  check (20, 0xffff);
  puts ("");

  /* Now check the converters on real world text/string/char.  */
  printf ("UTF8 before:\n  ");
  print_bytes (some_string, some_string_length);
  puts ("");

  puts ("# Conversion to text");
  foo_text_from_utf8 (some_string, some_string_length, &text);
  abort_on_exception ();

  fprint_text (stdout, &text, 0);
  puts ("");

  puts ("... Conversion back to UTF-8");
  foo_text_to_utf8 (&text, &bytes, &length);
  abort_on_exception ();

  foo_destroy_text (&text);
  abort_on_exception ();

  print_bytes (bytes, length);

  free (bytes);
  puts ("");

  puts ("# Conversion to string");
  foo_string_from_utf8 (some_string, some_string_length, &string);
  abort_on_exception ();

  /* String and text have very similar representations.  */
  text.chars = &string->content[0];
  text.length = string->length;
  fprint_text (stdout, &text, 0);
  puts ("");

  puts ("... Conversion back to UTF-8");
  foo_string_to_utf8 (string, &bytes, &length);
  abort_on_exception ();

  foo_string_dec_ref (string);
  abort_on_exception ();

  print_bytes (bytes, length);

  free (bytes);
  puts ("");

  printf ("UTF8 before:\n  ");
  print_bytes (some_char, some_char_length);
  puts ("");

  puts ("# Conversion to character");
  foo_char_from_utf8 (some_char, some_char_length, &chr);
  abort_on_exception ();

  text.chars = &chr;
  text.length = 1;
  fprint_text (stdout, &text, 0);
  puts ("");

  puts ("... Conversion back to UTF-8");
  foo_char_to_utf8 (chr, &bytes, &length);
  abort_on_exception ();

  print_bytes (bytes, length);

  free (bytes);
  puts ("");

  puts ("main.c: Done.");
  return 0;
}
