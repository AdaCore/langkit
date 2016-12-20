## vim: filetype=makoada

<%
is_env_el = expr.static_type.is_env_element_type
ast_node = expr.static_type.el_type if is_env_el else expr.static_type
source = str(expr.expr_var.name) + (".El" if is_env_el else "")
%>

${expr.expr.render_pre()}
${expr.expr_var.name} := ${expr.expr.render_expr()};

if ${source} = null
     or else
   ${source}.all in ${ast_node.value_type_name()}'Class
then
% if is_env_el:
   ${expr.result_var.name} :=
     (El => ${ast_node.name()} (${expr.expr_var.name}.El),
      Md => ${expr.expr_var.name}.Md,
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
