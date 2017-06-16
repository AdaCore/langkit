## vim: filetype=makoada

<%
is_entity = expr.static_type.is_entity_type
ast_node = expr.static_type.el_type if is_entity else expr.static_type
%>

<%def name="generate_cast(operand_expr)">
   % if is_entity:
      ${expr.result_var.name} := Create
        (El   => ${ast_node.name} (${operand_expr}.El),
         Info => ${operand_expr}.Info);
   % else:
      ${expr.result_var.name} := ${ast_node.name} (${operand_expr});
   % endif
</%def>

${expr.expr.render_pre()}

<%
   operand_expr = expr.expr.render_expr()
   node_expr = operand_expr + ('.El' if is_entity else '')
%>

% if expr.is_downcast:
   ## If we know statically that this is a downcast, then no need to generate
   ## checking code.
   ${generate_cast(operand_expr)}

% else:
   ## Before actually downcasting an access to an AST node, add a type
   ## check so that we raise a Property_Error if it's wrong.
   if ${node_expr} = null
      or else ${node_expr}.all in ${ast_node.value_type_name()}'Class
   then
      ${generate_cast(operand_expr)}
   else
      % if expr.do_raise:
         raise Property_Error with "invalid object cast";
      % else:
         ${expr.result_var.name} := ${expr.static_type.nullexpr()};
      % endif
   end if;
% endif
