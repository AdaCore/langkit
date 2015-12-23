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

   <%def name="build_loop()">
      ## First, build a vector for all the resulting elements
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

      ## Then convert the vector into the final array type
      ${array_var} := new ${map.array_var.type.pointed()}
        (Natural (${vec_var}.Length));
      for I in 1 .. ${array_var}.N loop
         ${array_var}.Items (I) := ${vec_var} (I);
      end loop;
   </%def>

   % if map.collection.type.is_list_type:
      ## Empty lists are null: handle this pecularity here to make it easier
      ## for property writers.

      if ${map.collection.render_expr()} = null then
         ${array_var} := new ${map.array_var.type.pointed()} (0);
      else
         ${build_loop()}
      end if;

   % else:
      ${build_loop()}
   % endif

end;
