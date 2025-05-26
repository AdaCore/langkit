## vim: filetype=makoada

${expr.domain.render_pre()}
${expr.logic_var_expr.render_pre()}

declare
   Dom : ${expr.domain.type.name} := ${expr.domain.render_expr()};
   A   : Entity_Vars.Value_Array (1 .. Length (Dom));
begin
   for J in 0 .. Length (Dom) - 1 loop
      declare
         Item : constant ${expr.domain.type.element_type.name} :=
           Get (Self, Dom, J);
      begin
         A (J + 1) := (Node => Item.Node, Info => Item.Info);
      end;
   end loop;

   ${expr.result_var.codegen_name} := Solver.Create_Domain
     (${expr.logic_var_expr.render_expr()}, A, ${sloc_info_arg});
end;
