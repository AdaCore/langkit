## vim: filetype=makoada

with Langkit_Support.Internal; use Langkit_Support.Internal;
with Langkit_Support.Internal.Unparsing;
use Langkit_Support.Internal.Unparsing;
with Langkit_Support.Text;     use Langkit_Support.Text;

with ${ada_lib_name}.Generic_Introspection;
use ${ada_lib_name}.Generic_Introspection;

--  This package provides description tables to enable the generic
--  unparsing API in Langkit_Support to work with this Langkit-generated
--  library.

private package ${ada_lib_name}.Unparsers is

   <%
      G = generic_api
      token_families = ctx.lexer.tokens.token_families

      def tok_seq_ref(tok_seq):
         return (
            f"{tok_seq.var_name}'Access"
            if tok_seq.tokens else
            "Empty_Token_Sequence"
         )
   %>

   ## Emit the table to indicate spacing rules between tokens

   Token_Spacings : aliased constant Token_Spacing_Table_Impl := (
   <% spacing_table = ctx.lexer.spacing_table %>
   % for i1, tf1 in enumerate(token_families):
      ${", " if i1 > 0 else ""}${G.token_family_index(tf1)} => (
      % for i2, tf2 in enumerate(token_families):
         ${", " if i2 > 0 else ""}${G.token_family_index(tf2)} => ${(
            spacing_table[tf1][tf2]
         )}
      % endfor
      )
   % endfor
   );

   Token_Newlines : aliased constant Token_Newline_Table_Impl := (
   % for i, t in enumerate(ctx.lexer.sorted_tokens):
      ${", " if i > 0 else ""}${G.token_kind_index(t)} => ${(
         t in ctx.lexer.newline_after
      )}
   % endfor
   );

   ## Emit constants for token unparsers and token sequence unparsers

   % for tok in ctx.unparsers.sorted_token_unparsers:
      % if tok.match_text:
         Text_For_${tok.var_name} : aliased constant Text_Type :=
           ${tok.text_repr};
      % endif
      ${tok.var_name} : aliased constant Token_Unparser_Impl :=
        (${G.token_kind_index(tok.token)},
         ${f"Text_For_{tok.var_name}'Access" if tok.match_text else "null"});
   % endfor

   % for tok_seq in ctx.unparsers.token_sequence_unparsers:
      % if tok_seq:
         ${tok_seq.var_name} : aliased constant Token_Sequence_Impl :=
           (${", ".join(
               f"{i} => {tok.var_name}'Access"
               for i, tok in enumerate(tok_seq.tokens, 1))});
      % endif
   % endfor

   ## Emit constants for lists of field unparsers

   % for node in ctx.astnode_types:
      % if is_regular_node_unparser(node.unparser) \
           and node.unparser.field_unparsers:

         <%
            unparser_list = node.unparser.zip_fields
            field_unparsers = [
               "{} => {}".format(
                  i,
                  "Empty_Field_Unparser"
                  if not f.pre_tokens and not f.post_tokens else
                  f"({tok_seq_ref(f.pre_tokens)},"
                  f" {tok_seq_ref(f.post_tokens)},"
                  f" {f.empty_list_is_absent})"
               )
               for i, (f, _) in enumerate(unparser_list, 1)
            ]
            inter_tokens = [
               f"{i} => {tok_seq_ref(tok_seq)}"
               for i, (_, tok_seq) in enumerate(unparser_list, 1)
            ]
         %>

         ${node.unparser.fields_unparser_var_name}
            : aliased constant Field_Unparser_List_Impl
            := (N               => ${len(unparser_list)},
                Field_Unparsers => (${", ".join(field_unparsers)}),
                Inter_Tokens    => (${", ".join(inter_tokens)}));
      % endif
   % endfor

   ## Emit the unparsing table for nodes themselves

   % for i, node in enumerate(ctx.astnode_types):
      <%
         unparser = node.unparser
         fields = []

         if is_regular_node_unparser(unparser):
            fields += [
               ("Kind", "Regular"),
               ("Pre_Tokens", tok_seq_ref(unparser.pre_tokens)),
               ("Field_Unparsers",
                f"{unparser.fields_unparser_var_name}'Access"),
               ("Post_Tokens", tok_seq_ref(unparser.post_tokens)),
            ]

         elif is_list_node_unparser(unparser):
            fields += [
               ("Kind", "List"),
               ("Separator",
                ("null"
                 if unparser.separator is None else
                 f"{unparser.separator.var_name}'Access")),
            ]

         elif is_token_node_unparser(unparser):
            fields += [("Kind", "Token")]

         else:
            ## This node is synthetic/an error node, so it cannot be
            ## unparsed: provide a dummy entry.
            assert (
               (node.abstract or node.synthetic or node.is_error_node)
               and unparser is None
            ), f"Unexpected unparser for {node.dsl_name}: {unparser}"
      %>

      % if unparser is not None:
         ${unparser.var_name} : aliased constant Node_Unparser_Impl := (
         % for i, (name, value) in enumerate(fields):
            ${"," if i > 0 else ""}
            ${name} => ${value}
         % endfor
         );
      % endif
   % endfor

   ## Finally, emit the map from types to node unparsers

   Node_Unparsers : aliased constant Node_Unparser_Map_Impl := (
      % for i, node in enumerate(ctx.astnode_types):
         ${"," if i > 0 else ""}
         ${G.type_index(node)} => ${(
            "null"
            if node.unparser is None else
            f"{node.unparser.var_name}'Access"
         )}
      % endfor
   );

   Unparsers : aliased constant Unparsers_Impl :=
     (Token_Spacings'Access,
      Token_Newlines'Access,
      Node_Unparsers'Access);

end ${ada_lib_name}.Unparsers;
