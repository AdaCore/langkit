## vim: filetype=makoada

<%def name="emit_toplevel_row(parser, node_type)">
   <%
   fields = list(reversed(node_type.get_parse_fields()))
   results = []
   for subp in parser.children():
      if subp.discard():
         results.append((subp, None))
      else:
         results.append((subp, fields.pop()))
   %>
   --  In emit_toplevel_row(${parser}, ${node_type}, ${results})
   % for (subp, field) in results:
      % if field:
         declare
            ${field.name} : constant Analysis.Implementation.Abstract_Node :=
               Node.Abstract_Child (${field.index + 1});
         begin
            ${emit_unparser_code(subp, ast_el=str(field.name))}
            pragma Unreferenced (${field.name});
         end;
      % else:
         ${emit_unparser_code(subp)}
      % endif
   % endfor
</%def>

<%def name="emit_unparser_code(parser, node_type=None, ast_el=None)">
   --  In emit_unparser_code(${parser}, ${node_type}, ${ast_el})

   % if not node_type and creates_node(parser):
      if ${ast_el} /= null then
         Unparse_Dispatch (${ast_el}, Preserve_Formatting, Result);
      end if;

   % elif node_type and node_type.is_token_node:
      Append (Result, Node.Abstract_Text);
      Append (Result, " ");

   % elif is_transform(parser):

      % if is_row(parser.parser):
         ${emit_toplevel_row(parser.parser, node_type)}
      % else:
         ${emit_unparser_code(parser.parser)}
      % endif

   % elif is_list(parser):

      for I in 1 .. Node.Abstract_Children_Count loop
         <% assert creates_node (parser.parser) %>

         Unparse_Dispatch
           (Node.Abstract_Child (I), Preserve_Formatting, Result);

         % if parser.sep:
            if I < Node.Abstract_Children_Count then
               ${emit_unparser_code (parser.sep, ast_el=ast_el)}
            end if;
         % endif
      end loop;

   % elif is_opt(parser) and parser._booleanize and node_type:

      % if str(node_type.name).endswith("Present"):
      ${emit_unparser_code(parser.parser, ast_el=ast_el)}
      % endif

   % elif is_opt(parser):

      <% assert ast_el or parser._is_error %>

      % if parser._is_error:
         ${emit_unparser_code(parser.parser, ast_el=ast_el)}
      % else:
         % if parser.get_type().is_list_type:
         if ${ast_el}.Abstract_Children_Count /= 0 then
         % else:
         if ${ast_el} /= null then
         % endif
            ${emit_unparser_code(parser.parser, ast_el=ast_el)}
         end if;
      % endif

   % elif is_extract(parser):

      <% assert (is_row(parser.parser)) %>

      ${emit_unparser_code(parser.parser, ast_el=ast_el)}

   % elif is_tok(parser):

      % if parser.match_text:
         Append (Result, "${parser.match_text}");
      % elif parser.val.matcher:
         Append (Result, "${parser.val.matcher.to_match}");
      % else:
         <% assert node_type and node_type.is_token_node, (
               'Cannot produce unparser for Token parser {}: node_type={}'
               .format(parser, node_type)
            ) %>
      % endif

      Append (Result, " ");

   % elif creates_node(parser):

      if ${ast_el} /= null then
         Unparse_Dispatch (${ast_el}, Preserve_Formatting, Result);
      end if;

   % elif is_null(parser) or is_nobt(parser):
   % elif is_row(parser):

      % for subp in parser.children():
         ${emit_unparser_code(subp, ast_el=ast_el)}
      % endfor

   % elif is_dontskip(parser):
      ${emit_unparser_code(parser.subparser, ast_el=ast_el)}

   %else:
      <% raise NotImplementedError('Parser not handled: {}'.format(parser)) %>
   % endif
</%def>
