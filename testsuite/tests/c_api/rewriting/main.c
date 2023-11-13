#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "libfoolang.h"

#include "utils_exc.h"
#include "utils_text.h"
#include "utils_unit.h"

/* Return the analysis unit designated by FILENAME in context CTX. If
   CREATE_URH, also create its unit rewriting context.  */
static foo_analysis_unit
fetch_unit (foo_analysis_context ctx, const char *filename, int create_urh)
{
  foo_analysis_unit u, u_tmp;
  foo_unit_rewriting_handle urh;

  u = foo_get_analysis_unit_from_file (
    /* context= */ ctx,
    /* filename= */ filename,
    /* charset= */ NULL,
    /* reparse= */ 0,
    /* rule= */ FOO_GRAMMAR_RULE_MAIN_RULE_RULE
  );
  abort_on_exception ();

  if (create_urh)
    {
      urh = foo_rewriting_unit_to_handle (u);
      abort_on_exception ();

      u_tmp = foo_rewriting_handle_to_unit (urh);

      if (u != u_tmp)
	{
	  puts ("broken unit rewriting handle <-> unit links");
	  exit (1);
	}
    }

  return u;
}

/* Assuming that UNIT's root node is a list of declaration, return the
   rewriting handle for its INDEX-th declaration (that index is 0-based).  */

static foo_node_rewriting_handle
fetch_decl (foo_unit_rewriting_handle unit, int index)
{
  int i;
  foo_node_rewriting_handle result;

  result = foo_rewriting_unit_root (unit);
  abort_on_exception ();

  result = foo_rewriting_first_child (result);
  abort_on_exception ();

  for (i = 0; i < index; ++i)
    {
      result = foo_rewriting_next_child (result);
      abort_on_exception ();
    }

  return result;
}

/* Print the image of NRH on the standard output.  */

static void
print_nrh (foo_node_rewriting_handle nrh)
{
  foo_text text;

  foo_rewriting_node_image (nrh, &text);
  abort_on_exception ();
  fprint_text (stdout, &text, 0);
  foo_destroy_text (&text);
  abort_on_exception ();
}

static int
unit_compare (const void *left, const void *right)
{
  foo_unit_rewriting_handle left_urh, right_urh;
  foo_analysis_unit left_unit, right_unit;
  char *left_basename;
  char *right_basename;
  int result;

  left_urh = *(foo_analysis_unit *) left;
  right_urh = *(foo_analysis_unit *) right;

  left_unit = foo_rewriting_handle_to_unit (left_urh);
  abort_on_exception ();

  right_unit = foo_rewriting_handle_to_unit (right_urh);
  abort_on_exception ();

  left_basename = unit_basename (left_unit);
  right_basename = unit_basename (right_unit);
  result = strcmp (left_basename, right_basename);
  free (left_basename);
  free (right_basename);
  return result;
}

/* Print the list of units in UNITS, a NULL-terminated array of units.  */

static void
print_units (foo_unit_rewriting_handle *units)
{
  size_t count = 0;
  int i;
  foo_analysis_unit u;

  for (i = 0; units[i] != NULL; ++i)
    count += 1;

  /* Sort units to have deterministic outputs.  */
  qsort (units, count, sizeof (foo_unit_rewriting_handle), &unit_compare);

  printf ("[");
  for (i = 0; units[i] != NULL; ++i)
    {
      if (i > 0)
	printf (", ");

      u = foo_rewriting_handle_to_unit (units[i]);
      abort_on_exception ();

      fprint_unit (stdout, u);
    }
  printf ("]");
}

/* Print the unparsing of the given UNIT.  */

static void
print_unit_unparsing (foo_unit_rewriting_handle unit)
{
  foo_text text;

  foo_rewriting_unit_unparse (unit, &text);
  abort_on_exception ();
  print_indented_buffer ("  |", &text);
  foo_destroy_text (&text);
  abort_on_exception ();
}

/* Print the source buffer of the given UNIT.  */

static void
print_unit_buffer (foo_analysis_unit unit)
{
  foo_token first, last;
  foo_text text;

  foo_unit_first_token (unit, &first);
  abort_on_exception ();
  foo_unit_last_token (unit, &last);
  abort_on_exception ();

  foo_token_range_text (&first, &last, &text);
  abort_on_exception ();

  print_indented_buffer ("  |", &text);
  foo_destroy_text (&text);
  abort_on_exception ();
}

/* Create a clone of DECL, and rewrite its name to NAME.  */

static foo_node_rewriting_handle
renamed_clone (foo_node_rewriting_handle decl, const char *name)
{
  foo_node_rewriting_handle result, id_node;
  foo_text id;

  alloc_text (name, &id);
  result = foo_rewriting_clone (decl);
  abort_on_exception ();
  id_node = foo_rewriting_child (result, foo_member_ref_decl_f_name);
  abort_on_exception ();
  foo_rewriting_set_text (id_node, &id);
  abort_on_exception ();
  free_text (&id);
  return result;
}

int
main (void)
{
  foo_analysis_context ctx, ctx_tmp;
  foo_rewriting_handle rh, rh_tmp;
  foo_analysis_unit u, u_tmp;
  foo_unit_rewriting_handle urh, urh_tmp;
  foo_unit_rewriting_handle *units;
  foo_node entity;
  foo_base_node n, n_tmp;
  foo_node_rewriting_handle nrh, nrh_tmp, nrh_tmp2, nrh_tmp3;
  foo_node_rewriting_handle *nodes;
  foo_node_rewriting_handle nrh_array[3];
  foo_node_kind_enum kind;
  foo_rewriting_apply_result apply_result;
  foo_text text;
  int i, flag, count;

  puts ("main.c: Starting...\n");

  ctx = foo_allocate_analysis_context ();
  abort_on_exception ();

  foo_initialize_analysis_context (
    /* context= */ ctx,
    /* charset= */ NULL,
    /* file_reader= */ NULL,
    /* unit_provider= */ NULL,
    /* event_handler= */ NULL,
    /* with_trivia= */ 1,
    /* tab_stop= */ 8
  );
  abort_on_exception ();

  puts ("Testing dummy rewriting handle lifecycle...");
  rh = foo_rewriting_start_rewriting (ctx);
  abort_on_exception ();

  ctx_tmp = foo_rewriting_handle_to_context (rh);
  abort_on_exception ();

  if (ctx_tmp != ctx)
    {
      puts ("broken rewriting handle -> context link");
      exit (1);
    }

  rh_tmp = foo_rewriting_context_to_handle (ctx);
  abort_on_exception ();

  if (rh_tmp != rh)
    {
      puts ("broken context -> rewriting handle link");
      exit (1);
    }

  printf ("List of rewritten units (empty): ");
  units = foo_rewriting_unit_handles (rh);
  abort_on_exception ();
  print_units (units);
  puts ("");
  free (units);

  printf ("List of rewritten units (non-empty): ");
  fetch_unit (ctx, "s1.txt", 1);
  fetch_unit (ctx, "s2.txt", 1);
  fetch_unit (ctx, "s3.txt", 0);
  units = foo_rewriting_unit_handles (rh);
  abort_on_exception ();
  print_units (units);
  puts ("");
  free (units);

  puts ("Unparse unit:");
  u = fetch_unit (ctx, "s2.txt", 1);
  urh = foo_rewriting_unit_to_handle (u);
  abort_on_exception ();
  print_unit_unparsing (urh);
  puts ("");

  puts ("Unparse node:");

  nrh = foo_rewriting_unit_root (urh);
  abort_on_exception ();
  nrh = foo_rewriting_first_child (nrh);
  abort_on_exception ();
  foo_unit_root (u, &entity);
  abort_on_exception ();
  foo_node_child (&entity, 0, &entity);
  n = entity.node;

  nrh_tmp = foo_rewriting_node_to_handle (n);
  if (nrh_tmp != nrh)
    {
      puts ("broken node -> rewriting handle link");
      exit (1);
    }

  n_tmp = foo_rewriting_handle_to_node (nrh);
  if (n_tmp != n)
    {
      puts ("broken rewriting handle -> node link");
      exit (1);
    }

  rh_tmp = foo_rewriting_node_to_context (nrh);
  if (rh_tmp != rh)
    {
      puts ("broken node -> context link");
      exit (1);
    }

  foo_rewriting_node_unparse (nrh, &text);
  abort_on_exception ();
  print_indented_buffer ("  |", &text);
  foo_destroy_text (&text);
  abort_on_exception ();
  puts ("");

  printf ("Node kind: ");
  nrh = foo_rewriting_unit_root (urh);
  abort_on_exception ();
  kind = foo_rewriting_kind (nrh);
  abort_on_exception ();
  foo_kind_name (kind, &text);
  abort_on_exception ();
  fprint_text (stdout, &text, 0);
  foo_destroy_text (&text);
  abort_on_exception ();
  puts ("");

  printf ("Node image: ");
  print_nrh (nrh);
  puts ("");

  printf ("Node tied: ");
  flag = foo_rewriting_tied (nrh);
  abort_on_exception ();
  puts (flag ? "true" : "false");

  printf ("Children count: ");
  count = foo_rewriting_children_count (nrh);
  abort_on_exception ();
  printf ("%i\n", count);

  puts ("Children:");
  foo_rewriting_children (nrh, &nodes, &count);
  abort_on_exception ();
  for (i = 0; i < count; ++i)
    {
      printf ("  ");
      print_nrh (nodes[i]);
      puts ("");

      nrh = foo_rewriting_parent (nodes[i]);
      abort_on_exception ();
      printf ("  parent: ");
      print_nrh (nodes[i]);
      puts ("");

      nrh = foo_rewriting_child (nodes[i], foo_member_ref_decl_f_name);
      abort_on_exception ();
      printf ("  Decl.f_name: ");
      print_nrh (nodes[i]);
      puts ("");

      foo_rewriting_text (nrh, &text);
      abort_on_exception ();
      printf ("  Text for Decl.f_name: ");
      fprint_text (stdout, &text, 1);
      foo_destroy_text (&text);
      abort_on_exception ();
      puts ("");

      puts ("");
    }
  free (nodes);

  printf ("First child: ");
  nrh = foo_rewriting_unit_root (urh);
  abort_on_exception ();
  nrh = foo_rewriting_first_child (nrh);
  abort_on_exception ();
  print_nrh (nrh);
  puts ("");

  printf ("Next child: ");
  nrh = foo_rewriting_next_child (nrh);
  abort_on_exception ();
  print_nrh (nrh);
  puts ("");

  printf ("Last child: ");
  nrh = foo_rewriting_unit_root (urh);
  abort_on_exception ();
  nrh = foo_rewriting_last_child (nrh);
  abort_on_exception ();
  print_nrh (nrh);
  puts ("");

  printf ("Previous child: ");
  nrh = foo_rewriting_previous_child (nrh);
  abort_on_exception ();
  print_nrh (nrh);
  puts ("");

  puts ("Aborting...");
  foo_rewriting_abort_rewriting (rh);
  abort_on_exception ();

  puts ("OK");
  puts ("");

  puts ("Testing regular rewriting handle lifecycle");
  puts ("");

  puts ("(successful apply)...");
  rh = foo_rewriting_start_rewriting (ctx);
  abort_on_exception ();

  puts ("Swap the roots of s1.txt and s2.txt");
  u = fetch_unit (ctx, "s1.txt", 1);
  u_tmp = fetch_unit (ctx, "s2.txt", 1);
  urh = foo_rewriting_unit_to_handle (u);
  abort_on_exception ();
  urh_tmp = foo_rewriting_unit_to_handle (u_tmp);
  abort_on_exception ();
  nrh = foo_rewriting_unit_root (urh);
  abort_on_exception ();
  nrh_tmp = foo_rewriting_unit_root (urh_tmp);
  abort_on_exception ();
  foo_rewriting_unit_set_root (urh, NULL);
  abort_on_exception ();
  foo_rewriting_unit_set_root (urh_tmp, nrh);
  abort_on_exception ();
  foo_rewriting_unit_set_root (urh, nrh_tmp);
  abort_on_exception ();

  puts ("Swap identifiers for 'a' and 'b' decls in s1.txt [set_child]");
  nrh = fetch_decl (urh, 0);
  nrh_tmp = fetch_decl (urh, 1);
  nrh_tmp2 = foo_rewriting_child (nrh, foo_member_ref_decl_f_name);
  abort_on_exception ();
  nrh_tmp3 = foo_rewriting_child (nrh_tmp, foo_member_ref_decl_f_name);
  abort_on_exception ();
  foo_rewriting_set_child (nrh, foo_member_ref_decl_f_name, NULL);
  abort_on_exception ();
  foo_rewriting_set_child (nrh_tmp, foo_member_ref_decl_f_name, nrh_tmp2);
  abort_on_exception ();
  foo_rewriting_set_child (nrh, foo_member_ref_decl_f_name, nrh_tmp3);
  abort_on_exception ();

  puts ("Swap identifiers for 'c' and 'd' decls in s1.txt [replace]");
  nrh = fetch_decl (urh, 2);
  nrh_tmp = fetch_decl (urh, 3);
  nrh_tmp2 = foo_rewriting_child (nrh, foo_member_ref_decl_f_name);
  abort_on_exception ();
  nrh_tmp3 = foo_rewriting_child (nrh_tmp, foo_member_ref_decl_f_name);
  abort_on_exception ();
  foo_rewriting_replace (nrh_tmp2, NULL);
  abort_on_exception ();
  foo_rewriting_replace (nrh_tmp3, nrh_tmp2);
  abort_on_exception ();
  foo_rewriting_set_child (nrh, foo_member_ref_decl_f_name, nrh_tmp3);
  abort_on_exception ();

  puts ("Rotate identifiers for 'e', 'f' and 'g' decls in s1.txt");
  nrh_array[0] = fetch_decl (urh, 4);
  nrh_array[1] = fetch_decl (urh, 5);
  nrh_array[2] = fetch_decl (urh, 6);
  for (i = 0; i < 3; ++i)
    {
      nrh_array[i] = foo_rewriting_child (nrh_array[i],
					  foo_member_ref_decl_f_name);
      abort_on_exception ();
    }
  foo_rewriting_rotate (&nrh_array[0], 3);
  abort_on_exception ();

  puts ("Set identifier for 'orig_2' to 'renamed' in s2.txt");
  nrh_tmp2 = foo_rewriting_unit_root (urh_tmp);
  abort_on_exception ();
  nrh_tmp3 = foo_rewriting_first_child (nrh_tmp2);
  abort_on_exception ();
  nrh = foo_rewriting_next_child (nrh_tmp3);
  abort_on_exception ();
  nrh = foo_rewriting_child (nrh, foo_member_ref_decl_f_name);
  abort_on_exception ();
  alloc_text ("renamed", &text);
  foo_rewriting_set_text (nrh, &text);
  abort_on_exception ();
  free_text (&text);
  abort_on_exception ();

  puts ("Delete 'orig_3'");
  nrh = foo_rewriting_unit_root (urh_tmp);
  abort_on_exception ();
  nrh = foo_rewriting_last_child (nrh);
  abort_on_exception ();
  foo_rewriting_remove_child (nrh);
  abort_on_exception ();

  puts ("Insert a clone of 'orig_1' called 'first'");
  nrh = foo_rewriting_unit_root (urh_tmp);
  abort_on_exception ();
  nrh_tmp = fetch_decl (urh_tmp, 0);
  nrh_tmp2 = renamed_clone (nrh_tmp, "first");
  foo_rewriting_insert_first (nrh, nrh_tmp2);
  abort_on_exception ();

  puts ("Insert a clone of 'orig_1' called 'before'");
  nrh_tmp2 = renamed_clone (nrh_tmp, "before");
  foo_rewriting_insert_before (nrh_tmp, nrh_tmp2);
  abort_on_exception ();

  puts ("Insert a clone of 'orig_1' called 'after'");
  nrh_tmp2 = renamed_clone (nrh_tmp, "after");
  foo_rewriting_insert_after (nrh_tmp, nrh_tmp2);
  abort_on_exception ();

  puts ("Insert a clone of 'orig_1' called 'last'");
  nrh_tmp2 = renamed_clone (nrh_tmp, "last");
  foo_rewriting_insert_last (nrh, nrh_tmp2);
  abort_on_exception ();

  puts ("Insert a synthetic decl called 'synthetic'");

  nrh_tmp2 = foo_rewriting_create_node (rh, foo_name);
  abort_on_exception ();
  alloc_text ("synthetic", &text);
  foo_rewriting_set_text (nrh_tmp2, &text);
  abort_on_exception ();
  free_text (&text);

  alloc_text ("12", &text);
  nrh_tmp3 = foo_rewriting_create_token_node (rh, foo_literal, &text);
  abort_on_exception ();
  free_text (&text);
  abort_on_exception ();

  nrh_array[0] = nrh_tmp2;
  nrh_array[1] = nrh_tmp3;
  nrh_tmp = foo_rewriting_create_regular_node (rh, foo_var, &nrh_array[0], 2);
  abort_on_exception ();

  foo_rewriting_insert_last (nrh, nrh_tmp);
  abort_on_exception ();

  puts ("Insert a templated decl called 'templated'");

  alloc_text ("templated", &text);
  nrh_array[0] = foo_rewriting_create_token_node (rh, foo_name, &text);
  abort_on_exception ();
  free_text (&text);
  abort_on_exception ();

  alloc_text ("42", &text);
  nrh_array[1] = foo_rewriting_create_token_node (rh, foo_literal, &text);
  abort_on_exception ();
  free_text (&text);
  abort_on_exception ();

  alloc_text ("var {} = {}", &text);
  nrh_tmp = foo_rewriting_create_from_template (
    rh, &text, &nrh_array[0], 2, FOO_GRAMMAR_RULE_VAR_RULE_RULE
  );
  abort_on_exception ();
  free_text (&text);
  foo_rewriting_insert_last (nrh, nrh_tmp);
  abort_on_exception ();

  puts ("Applying...");
  foo_rewriting_apply (rh, &apply_result);
  abort_on_exception ();

  if (!apply_result.success)
    {
      puts ("unexpected apply failure");
      exit (1);
    }

  puts ("Freeing apply result...");
  foo_rewriting_free_apply_result (&apply_result);
  abort_on_exception ();

  puts ("Buffers for s1.txt decls:");
  print_unit_buffer (u);
  puts ("");

  puts ("Buffer for s2.txt:");
  print_unit_buffer (u_tmp);
  puts ("");

  puts ("OK");
  puts ("");

  foo_context_decref (ctx);
  abort_on_exception ();

  puts ("main.c: Done.");
  return 0;
}
