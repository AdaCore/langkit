## vim: filetype=makoada

${expr.domain.render_pre()}
${expr.logic_var_expr.render_pre()}

declare
   Dom : ${expr.domain.type.name} := ${expr.domain.render_expr()};
   A   : Bind_Default_Default.Impl.Unify_Left.R_Type_Array (1 .. Length (Dom));
begin
   for J in 0 .. Length (Dom) - 1 loop
      declare
         Item : constant ${expr.domain.type.element_type.name} :=
            Get (Dom, J);
      begin
         A (J + 1) := (
            % if expr.domain.static_type.element_type.is_entity_type:
               (Node => ${root_node_type_name} (Item.Node), Info => Item.Info)
            % else:
               (Node => Item, others => <>)
            % endif
         );
      end;
   end loop;

   ${expr.result_var.name} :=
     Bind_Default_Default.Impl.Member (${expr.logic_var_expr.render_expr()}, A);
end;
