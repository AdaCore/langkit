/* Test that custom unit providers defined using the Python bidnings work as
   expected.  */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libfoolang.h"

#include "utils_exc.h"
#include "utils_text.h"

struct unit_buffer {
  const char *filename;
  const char *buffer;
};

#define buffers_count 3
static const struct unit_buffer buffers[buffers_count] = {
  {"m0.txt", "{ a b c }"},
  {"m1.txt", "{ a b c }\n{ a b c }"},
  {"refs.txt", "{ ref m.a ref m.b ref m.c }"}
};

struct custom_unit_provider_data {
  bool first_resolution;
  const char *filename;
  int ple_root_index;
};

static void
cb_destroy (void *data)
{
  free (data);
}

static void
cb_get_unit_location (void *data, foo_text *name, foo_analysis_unit_kind kind,
		      char **filename, int *ple_root_index)
{
  struct custom_unit_provider_data *upd = data;

  if (upd->first_resolution)
    {
      upd->first_resolution = false;
      puts ("!!! cb_get_unit_location");

      printf ("    name=");
      fprint_text (stdout, name, true);
      puts ("");

      printf ("    kind=%i\n", kind);
    }

  *filename = foo_copy_bytes (upd->filename, strlen (upd->filename) + 1);
  abort_on_exception ();

  *ple_root_index = upd->ple_root_index;
}

static foo_analysis_context
create_context (const char *filename, int ple_root_index)
{
  struct custom_unit_provider_data *upd;
  foo_unit_provider up;
  foo_analysis_context ctx;
  int i;

  upd = malloc (sizeof (*upd));
  upd->first_resolution = true;
  upd->filename = filename;
  upd->ple_root_index = ple_root_index;

  up = foo_create_unit_provider (upd, cb_destroy, cb_get_unit_location);
  abort_on_exception ();

  ctx = foo_allocate_analysis_context ();
  abort_on_exception ();

  foo_initialize_analysis_context (
    /* context= */ ctx,
    /* charset= */ NULL,
    /* file_reader= */ NULL,
    /* unit_provider= */ up,
    /* event_handler= */ NULL,
    /* with_trivia= */ 1,
    /* tab_stop= */ 8
  );
  abort_on_exception ();

  for (i = 0; i < buffers_count; ++i)
    {
      foo_get_analysis_unit_from_buffer(
	/* context= */ ctx,
	/* filename= */ buffers[i].filename,
	/* charset= */ NULL,
	/* buffer= */ buffers[i].buffer,
	/* buffer_size= */ strlen(buffers[i].buffer),
	/* rule= */ foo_default_grammar_rule
      );
      abort_on_exception ();
    }

  foo_dec_ref_unit_provider (up);
  abort_on_exception ();
  return ctx;
}

static void
run (const char *filename, int ple_root_index)
{
  foo_analysis_context ctx;
  foo_analysis_unit u;
  foo_node root, block, block_items, ref, resolved;
  unsigned block_i, block_count, ref_i, ref_count;
  foo_text text;
  int success;
  const foo_exception *exc;
  char *exc_name;

  printf("# %s:%i\n\n", filename, ple_root_index);
  ctx = create_context (filename, ple_root_index);

  u = foo_get_analysis_unit_from_file (
    /* context= */ ctx,
    /* filename= */ "refs.txt",
    /* charset= */ NULL,
    /* reparse= */ 0,
    /* rule= */ foo_default_grammar_rule
  );
  abort_on_exception ();

  foo_unit_root (u, &root);
  abort_on_exception ();

  block_count = foo_node_children_count (&root);
  abort_on_exception ();

  for (block_i = 0; block_i < block_count; ++block_i)
    {
      foo_node_child (&root, block_i, &block);
      abort_on_exception ();

      foo_node_child (&block, 0, &block_items);
      abort_on_exception ();

      ref_count = foo_node_children_count (&block_items);
      abort_on_exception ();

      for (ref_i = 0; ref_i < ref_count; ++ref_i)
	{
	  foo_node_child (&block_items, ref_i, &ref);
	  abort_on_exception ();

	  foo_node_text (&ref, &text);
	  abort_on_exception ();

	  success = foo_ref_p_resolve (&ref, &resolved);

	  printf ("  ");
	  fprint_text (stdout, &text, false);
	  printf (": ");

	  if (success)
	    {
	      foo_destroy_text (&text);
	      foo_node_image (&resolved, &text);
	      abort_on_exception ();

	      fprint_text (stdout, &text, false);

	      foo_destroy_text (&text);
	      puts ("");
	    }
	  else
	    {
	      exc = foo_get_last_exception ();
	      exc_name = foo_exception_name (exc->kind);
	      printf ("<%s>\n", exc_name);
	      free (exc_name);

	      foo_destroy_text (&text);
	    }
	}
      puts ("");
    }

  foo_context_decref (ctx);
  abort_on_exception ();
}

int
main (void)
{
  run ("m0.txt", 0);
  run ("m1.txt", 0);
  run ("m1.txt", 1);
  /* File exists, but the PLE root index is out of bounds.  */
  run ("m0.txt", 1);

  return 0;
}
