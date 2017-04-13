## vim: filetype=makoada

<%
is_entity = expr.static_type.is_entity_type
ast_node = expr.static_type.el_type if is_entity else expr.static_type
source = str(expr.expr_var.name) + (".El" if is_entity else "")
%>

${expr.expr.render_pre()}
${expr.expr_var.name} := ${expr.expr.render_expr()};
% if expr.expr.type.is_refcounted():
   Inc_Ref (${expr.expr_var.name});
% endif

if ${source} = null
     or else
   ${source}.all in ${ast_node.value_type_name()}'Class
then
% if is_entity:
   ## We are about to create a new reference to the input expr's env rebinding,
   ## so create a new ownership share for it.
   Inc_Ref (${expr.expr_var.name}.Info.Rebindings);
   ${expr.result_var.name} :=
     (El      => ${ast_node.name()} (${expr.expr_var.name}.El),
      Info    => (MD         => ${expr.expr_var.name}.Info.Md,
                  Rebindings => ${expr.expr_var.name}.Info.Rebindings));
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
