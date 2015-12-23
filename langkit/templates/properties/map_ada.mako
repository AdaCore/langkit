## vim: filetype=makoada

<%
   ind_var = map.induction_var.name
   array_var = map.array_var.name
   vec_var = map.array_var.name + Name('Vec')
%>

${map.collection.render_pre()}

declare
   ${vec_var} : ${map.type.vector()};
begin
   for ${ind_var} of
      % if map.collection.type.is_list_type:
         ${map.collection.render_expr()}.Vec
      % else:
         ${map.collection.render_expr()}.Items
      % endif
   loop
      % if map.filter:
         ${map.filter.render_pre()}
         if ${map.filter.render_expr()} then
      % endif

      ${map.expr.render_pre()}

      % if map.concat:
         for Item_To_Append of
            % if map.expr.type.is_list_type:
               ${map.expr.render_expr()}.Vec
            % else:
               ${map.expr.render_expr()}.Items
            % endif
         loop
            ${vec_var}.Append (Item_To_Append);
         end loop;
      % else:
         ${vec_var}.Append (${map.expr.render_expr()});
      % endif

      % if map.filter:
         end if;
      % endif
   end loop;

   ${array_var} := new ${map.array_var.type.pointed()}
     (Natural (${vec_var}.Length));
   for I in 1 .. ${array_var}.N loop
      ${array_var}.Items (I) := ${vec_var} (I);
   end loop;
end;
