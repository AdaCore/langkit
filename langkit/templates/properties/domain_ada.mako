## vim: filetype=makoada

${expr.domain.render_pre()}
${expr.logic_var_expr.render_pre()}

declare
   Dom : ${expr.domain.type.name} := ${expr.domain.render_expr()};
   A   : Solver.Value_Array (1 .. Length (Dom));
begin
   for J in 0 .. Length (Dom) - 1 loop
      declare
         <%
            element_type = expr.domain.type.element_type
            is_root_type = (element_type.is_root_type
                            if element_type.is_entity_type else
                            element_type.is_root_node)

            node_expr = 'Item.Node' if element_type.is_entity_type else 'Item'
            if not is_root_type:
               node_expr = '{} ({})'.format(T.root_node.name, node_expr)
            info_expr = 'Item.Info' if element_type.is_entity_type else '<>'
         %>
         Item : constant ${element_type.name} := Get (Dom, J);
      begin
         A (J + 1) := (Node => ${node_expr}, Info => ${info_expr});
      end;
   end loop;

   ${expr.result_var.name} := Solver.Create_Domain
     (${expr.logic_var_expr.render_expr()}, A, ${sloc_info_arg});
end;
