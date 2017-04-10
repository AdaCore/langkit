## vim: filetype=makoada

<%
is_entity = expr.static_type.is_entity_type
ast_node = expr.static_type.el_type if is_entity else expr.static_type
source = str(expr.expr_var.name) + (".El" if is_entity else "")
%>

${expr.expr.render_pre()}
${expr.expr_var.name} := ${expr.expr.render_expr()};

if ${source} = null
     or else
   ${source}.all in ${ast_node.value_type_name()}'Class
then
% if is_entity:
   ${expr.result_var.name} :=
     (El => ${ast_node.name()} (${expr.expr_var.name}.El),
      Md => ${expr.expr_var.name}.Md,
      Parents_Bindings => ${expr.expr_var.name}.Parents_Bindings,
      Is_Null => False);
% else:
   ${expr.result_var.name} := ${ast_node.name()} (${expr.expr_var.name});
% endif
else
   % if expr.do_raise:
   raise Property_Error with "invalid object cast";
   % else:
   ${expr.result_var.name} := ${expr.static_type.nullexpr()};
   % endif
end if;
