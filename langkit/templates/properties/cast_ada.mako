## vim: filetype=makoada

<%
is_entity = expr.type.is_entity_type
%>

<%def name="generate_cast(operand_expr)">
   % if is_entity:
      ${expr.result_var.name} := ${expr.type.constructor_name}
        (Node => ${expr.dest_node.internal_conversion(
                      expr.input_node,
                      '{}.Node'.format(operand_expr))},
         Info => ${operand_expr}.Info);
   % else:
      ${expr.result_var.name} := ${expr.dest_node.internal_conversion(
                                      expr.input_node,
                                      operand_expr)};
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
         raise Property_Error with "invalid object cast";
      % else:
         ${expr.result_var.name} := ${expr.static_type.nullexpr};
      % endif
   end if;

% else:
   ${generate_cast(operand_expr)}
% endif
