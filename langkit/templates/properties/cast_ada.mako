## vim: filetype=makoada

<%
is_entity = expr.type.is_entity_type
%>

<%def name="generate_cast(operand_expr)">
   % if is_entity:
      ${expr.result_var.name} := ${expr.type.constructor_name}
        (Node => ${operand_expr}.Node,
         Info => ${operand_expr}.Info);
   % else:
      ${expr.result_var.name} := ${operand_expr};
   % endif
</%def>

${expr.expr.render_pre()}

<%
   operand_expr = expr.expr.render_expr()
   node_expr = operand_expr + ('.Node' if is_entity else '')
%>

% if expr.check_needed:
   ## Before actually downcasting an access to an AST node, add a type
   ## check so that we raise a Property_Error if it's wrong.
   if ${node_expr} = null
      or else ${node_expr}.Kind in ${expr.dest_node.ada_kind_range_name}
   then
      ${generate_cast(operand_expr)}
   else
      % if expr.do_raise:
         Raise_Property_Exception
           (Self, Property_Error'Identity, "invalid object cast");
      % else:
         ${expr.result_var.name} := ${expr.static_type.nullexpr};
      % endif
   end if;

% else:
   ${generate_cast(operand_expr)}
% endif
