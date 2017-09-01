## vim: filetype=makoada

${expr.domain.render_pre()}
${expr.logic_var_expr.render_pre()}

declare
   Dom : ${expr.domain.type.name} := ${expr.domain.render_expr()};
   A   : Bind_Default_Default.Impl.Unify_Left.R_Type_Array (1 .. Length (Dom));
begin
   for J in 0 .. Length (Dom) - 1 loop
      A (J + 1) := (
         % if expr.domain.static_type.element_type.is_entity_type:
            Get (Dom, J)
         % else:
            (El => Get (Dom, J), others => <>)
         % endif
      );
   end loop;

   ${expr.result_var.name} :=
     Bind_Default_Default.Impl.Member (${expr.logic_var_expr.render_expr()}, A);
end;
