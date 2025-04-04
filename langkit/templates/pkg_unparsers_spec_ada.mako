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

   Token_Newlines : aliased constant Token_Newline_Table_Impl :=
   ${ada_block_with_parens(
       [
           f"{G.token_kind_index(t)} => {t in ctx.lexer.newline_after}"
           for t in ctx.lexer.sorted_tokens
       ],
       3,
   )};

   ## Emit constants for token unparsers and token sequence unparsers. Emit
   ## only one Text_Type constant for each text value needed.

   <% tok_text_ids = {} %>
   % for tok in ctx.unparsers.sorted_token_unparsers:
      <%
         text = tok.match_text or ctx.lexer_literals_map[tok]
         text_id = tok_text_ids.get(text)
         if text_id is None:
            emit_text_constant = True
            text_id = f"Text_For_{tok.var_name}"
            tok_text_ids[text] = text_id
         else:
            emit_text_constant = False
      %>
      % if emit_text_constant:
         ${text_id} : aliased constant Text_Type := ${text_repr(text)};
      % endif
      ${tok.var_name} : aliased constant Token_Unparser_Impl :=
        (${G.token_kind_index(tok.token)}, ${text_id}'Access);
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

   % for node in ctx.node_types:
      % if is_regular_node_unparser(node.unparser) \
           and node.unparser.field_unparsers:

         <%
            unparser_list = node.unparser.zip_fields
            field_unparsers = [
               f"{i} => "
               f"({G.member_index(f.field)},"
               f" {tok_seq_ref(f.pre_tokens)},"
               f" {tok_seq_ref(f.post_tokens)},"
               f" {f.empty_list_is_absent})"
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
                Field_Unparsers =>
                ${ada_block_with_parens(field_unparsers, 16)},
                Inter_Tokens    =>
                ${ada_block_with_parens(inter_tokens, 16)});
      % endif
   % endfor

   ## Emit the unparsing table for nodes themselves

   % for i, node in enumerate(ctx.node_types):
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
               ("Sep_Extra", unparser.extra.ada_name),
            ]

         elif is_token_node_unparser(unparser):
            fields += [("Kind", "Token")]

         else:
            ## This node is synthetic/an error node, so it cannot be
            ## unparsed: provide a dummy entry.
            assert (
               (node.abstract or node.synthetic or node.is_error_node)
               and unparser is None
            ), f"Unexpected unparser for {node.lkt_name}: {unparser}"
      %>

      % if unparser is not None:
         ${unparser.var_name} : aliased constant Node_Unparser_Impl :=
         ${ada_block_with_parens(
             [f"{name} => {value}" for name, value in fields], 9
         )};
      % endif
   % endfor

   ## Finally, emit the map from types to node unparsers

   Node_Unparsers : aliased constant Node_Unparser_Map_Impl := (
      % for i, node in enumerate(ctx.node_types):
         ${"," if i > 0 else ""}
         ${G.type_index(node)} => ${(
            "null"
            if node.unparser is None else
            f"{node.unparser.var_name}'Access"
         )}
      % endfor
   );

   ## TODO (eng/toolchain/gnat#600): use the External_Initialization aspect
   ## once it is generally available.

   Default_Config : aliased constant String :=
     ${bytes_repr(ctx.emitter.default_unparsing_config, " " * 5)};

   Unparsers : aliased constant Unparsers_Impl :=
     (Token_Spacings'Access,
      Token_Newlines'Access,
      Node_Unparsers'Access,
      Default_Config'Access);

end ${ada_lib_name}.Unparsers;
