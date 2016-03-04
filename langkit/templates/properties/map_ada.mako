## vim: filetype=makoada

<%
   ind_var = map.induction_var.name
   array_var = map.array_var.name

   vec_var = map.array_var.name + Name('Vec')
   vec_pkg = map.type.pkg_vector()
%>

${map.collection.render_pre()}

declare
   ${vec_var} : ${map.type.vector()};
begin

   <%def name="build_loop_body()">
      % if map.take_while:
      ${map.take_while.render_pre()}
      exit when not (${map.take_while.render_expr()});
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
            ${vec_pkg}.Append (${vec_var}, Item_To_Append);
         end loop;
      % else:
         ${vec_pkg}.Append
           (${vec_var}, ${map.expr.render_expr()});
      % endif
   </%def>

   <%def name="build_loop()">
      % if map.index_var:
         ${map.index_var.name} := 0;
      % endif

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
               ${build_loop_body()}
            end if;
         % else:
            ${build_loop_body()}
         % endif

         % if map.index_var:
            ${map.index_var.name} := ${map.index_var.name} + 1;
         % endif
      end loop;

      ## Then convert the vector into the final array type
      ${array_var} := new ${map.array_var.type.pointed()}
        (Natural (${vec_pkg}.Length (${vec_var})));
      for I in 1 .. ${array_var}.N loop
         ${array_var}.Items (I) := ${vec_pkg}.Get (${vec_var}, I - 1);
      end loop;
      ${vec_pkg}.Destroy (${vec_var});
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
