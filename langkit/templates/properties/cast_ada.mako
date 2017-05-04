## vim: filetype=makoada

<%
is_entity = expr.static_type.is_entity_type
ast_node = expr.static_type.el_type if is_entity else expr.static_type
%>

<%def name="generate_cast()">
% if is_entity:
   ${expr.result_var.name} := Create
     (El   => ${ast_node.name()} (${expr.expr_var.name}.El),
      Info => ${expr.expr_var.name}.Info);
% else:
   ${expr.result_var.name} := ${ast_node.name()} (${expr.expr_var.name});
% endif
</%def>

${expr.expr.render_pre()}
${expr.expr_var.name} := ${expr.expr.render_expr()};

% if expr.expr.type.is_refcounted():
   Inc_Ref (${expr.expr_var.name});
% endif

% if expr.is_downcast:
   ## If we know statically that this is a downcast, then no need to generate
   ## checking code.
   ${generate_cast()}
% else:

   <% source = str(expr.expr_var.name) + (".El" if is_entity else "") %>

   if ${source} = null
      or else ${source}.all in ${ast_node.value_type_name()}'Class
   then
      ${generate_cast()}
   else
      % if expr.do_raise:
      raise Property_Error with "invalid object cast";
      % else:
      ${expr.result_var.name} := ${expr.static_type.nullexpr()};
      % endif
   end if;
% endif
