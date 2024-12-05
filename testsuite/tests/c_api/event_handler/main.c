#include <stdlib.h>
#include <string.h>

#include "libfoolang.h"

#include "utils_exc.h"
#include "utils_text.h"
#include "utils_unit.h"

static void
eh_destroy (void *data)
{
  const char *eh_name = data;
  printf ("%s: eh_destroy\n", eh_name);
  puts ("");
}

static void
eh_unit_requested (void *data, foo_analysis_context ctx, foo_text *name,
		   foo_analysis_unit from, foo_bool found, foo_bool
		   is_not_found_error)
{
  (void) ctx;

  const char *eh_name = data;
  printf ("%s: eh_unit_requested\n", eh_name);

  printf ("  name: ");
  fprint_text (stdout, name, 1);
  puts ("");

  printf ("  from: ");
  fprint_unit (stdout, from);
  puts ("");

  printf ("  found: %i\n", found);
  printf ("  is_not_found_error: %i\n", is_not_found_error);
  puts ("");
}

static void
eh_unit_parsed (void *data, foo_analysis_context ctx, foo_analysis_unit unit,
		foo_bool reparsed)
{
  (void) ctx;

  const char *eh_name = data;
  printf ("%s: eh_unit_requested\n", eh_name);

  printf ("  unit: ");
  fprint_unit (stdout, unit);
  puts ("");

  printf ("  reparsed: %i\n", reparsed);
  puts ("");
}

int
main (void)
{
  const char *buffers[2] = {"example\n", "example example\n"};
  const size_t buffers_size[2] = {strlen (buffers[0]), strlen (buffers[1])};

  char *eh_data = "MyEH";

  uint32_t foo_1_data[] = {'f', 'o', 'o', '_', '1'};
  uint32_t foo_2_data[] = {'f', 'o', 'o', '_', '2'};
  uint32_t foo_3_data[] = {'f', 'o', 'o', '_', '2'};

  foo_text foo_1_text = {foo_1_data, 5, 0};
  foo_text foo_2_text = {foo_2_data, 5, 0};
  foo_text foo_3_text = {foo_3_data, 5, 0};

  foo_symbol_type foo_1_symbol;
  foo_symbol_type foo_2_symbol;
  foo_symbol_type foo_3_symbol;

  foo_event_handler eh;
  foo_analysis_context ctx;
  foo_analysis_unit u;
  foo_node n;

  foo_bool dummy;

  puts ("main.c: Starting...\n");

  /* Create an analysis unit context with our event  handler.  */

  puts ("== create context ==\n");

  eh = foo_create_event_handler (eh_data, eh_destroy, eh_unit_requested,
				 eh_unit_parsed);
  abort_on_exception ();

  ctx = foo_allocate_analysis_context ();
  abort_on_exception ();

  foo_initialize_analysis_context (
    /* context= */ ctx,
    /* charset= */ NULL,
    /* file_reader= */ NULL,
    /* unit_provider= */ NULL,
    /* event_handler= */ eh,
    /* with_trivia= */ 1,
    /* tab_stop= */ 8
  );
  abort_on_exception ();

  /* Trigger the "unit parsed" event twice: once for the initial parsing, and
     once for a reparsing.  */

  puts ("== unit parsed ==\n");

  for (int i = 0; i < 2; ++i)
    {
      u = foo_get_analysis_unit_from_buffer(
	/* context= */ ctx,
	/* filename= */ "main.txt",
	/* charset= */ NULL,
	/* buffer= */ buffers[i],
	/* buffer_size= */ buffers_size[i],
	/* foo_grammar_rule= */ foo_default_grammar_rule
      );
      abort_on_exception ();
    }

  /* Trigger the "unit requested" event with various parameters.  */

  puts ("== unit requested ==\n");

  foo_unit_root (u, &n);
  abort_on_exception ();

  foo_context_symbol (ctx, &foo_1_text, &foo_1_symbol);
  abort_on_exception ();

  foo_context_symbol (ctx, &foo_2_text, &foo_2_symbol);
  abort_on_exception ();

  foo_context_symbol (ctx, &foo_3_text, &foo_3_symbol);
  abort_on_exception ();

  foo_foo_node_p_trigger_unit_requested(
    /* node= */ &n,
    /* name= */ &foo_1_symbol,
    /* found= */ 1,
    /* error= */ 0,
    /* value_p= */ &dummy
  );
  abort_on_exception ();

  foo_foo_node_p_trigger_unit_requested(
    /* node= */ &n,
    /* name= */ &foo_2_symbol,
    /* found= */ 0,
    /* error= */ 0,
    /* value_p= */ &dummy
  );
  abort_on_exception ();

  foo_foo_node_p_trigger_unit_requested(
    /* node= */ &n,
    /* name= */ &foo_3_symbol,
    /* found= */ 0,
    /* error= */ 1,
    /* value_p= */ &dummy
  );
  abort_on_exception ();

  /* Free allocated resources.  */

  foo_context_decref (ctx);
  abort_on_exception ();

  foo_dec_ref_event_handler (eh);
  abort_on_exception ();

  puts ("main.c: Done.");
  return 0;
}
