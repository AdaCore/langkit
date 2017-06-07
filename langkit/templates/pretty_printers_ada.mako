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
   --  In emit toplevel row
   --  ${parser}
   --  ${node_type}
   --  ${results}
   % for (subp, field) in  results:
      % if field:
      ${emit_parser_pp_code(subp, ast_el="Node.{}".format(field.name))}
      % else:
      ${emit_parser_pp_code(subp)}
      % endif
   % endfor
</%def>

<%def name="emit_parser_pp_code(parser, node_type=None, ast_el=None)">
   --  In emit parser pp code ${parser} ${node_type} ${ast_el}

   % if not node_type and creates_node(parser):
      if ${ast_el} /= null then
         Append (Buffer, ${ast_el}.PP);
      end if;
   % elif is_transform(parser):

      % if is_row(parser.parser):
         ${emit_toplevel_row(parser.parser, node_type)}
      % else:
         ${emit_parser_pp_code(parser.parser)}
      % endif

   % elif is_list(parser):

      for I in 1 .. Length (Node) loop
         <% assert creates_node (parser.parser) %>

         Append (Buffer, Item (Node, I).PP);

         % if parser.sep:
            if I < Length (Node) then
               ${emit_parser_pp_code (parser.sep, ast_el=ast_el)}
            end if;
         % endif
      end loop;

   % elif is_opt(parser) and parser._booleanize and node_type:

      % if str(node_type.name()).endswith("Present"):
      ${emit_parser_pp_code(parser.parser, ast_el=ast_el)}
      % endif

   % elif is_opt(parser):

      <% assert ast_el or parser._is_error %>

      % if parser._is_error:
         ${emit_parser_pp_code(parser.parser, ast_el=ast_el)}
      % else:
         % if parser.get_type().is_list_type:
         if not Is_Empty_List (${ast_el}) then
         % else:
         if ${ast_el} /= null then
         % endif
            ${emit_parser_pp_code(parser.parser, ast_el=ast_el)}
         end if;
      % endif

   % elif is_extract(parser):

      <% assert (is_row(parser.parser)) %>

      ${emit_parser_pp_code(parser.parser, ast_el=ast_el)}

   % elif is_tok(parser):

      % if parser.match_text:
         Append (Buffer, "${parser.match_text}");
      % elif parser.val.matcher:
         Append (Buffer, "${parser.val.matcher.to_match}");
      % elif ast_el != "Node":
         Append
           (Buffer,
            Image (Text (Token (Node, ${ast_el})), With_Quotes => False));
      % else:
         <% assert False %>
      % endif

      Append (Buffer, " ");

   % elif creates_node(parser):

      if ${ast_el} /= null then
         Append (Buffer, ${ast_el}.PP);
      end if;

   % elif is_null(parser):
   % elif is_row(parser):

      % for subp in parser.children():
         ${emit_parser_pp_code(subp, ast_el=ast_el)}
      % endfor

   %else:
      <% raise Exception("Not handled !") %>
   % endif
</%def>


<%def name="pretty_printer(node_type)">
   --  In pretty_printer
   --  ${node_type}
% if node_type.parser:
   declare
      Buffer : Unbounded_String;
   begin
      null;
      ${emit_parser_pp_code(node_type.parser, node_type, "Node")}
      return To_String (Buffer);
   end;
% else:
   return "";
% endif

</%def>
