## vim: filetype=makoada

<%namespace name="scopes" file="scopes_ada.mako" />

<%
   list_element_var = (map.list_element_var.name
                       if map.list_element_var else
                       None)
   element_var = map.element_var.name
   iteration_var = list_element_var or element_var

   array_var = map.result_var.name

   vec_var = map.result_var.name + Name('Vec')
   vec_pkg = map.type.pkg_vector
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
         <% expr = map.expr.render_expr() %>
         for Item_To_Append of
            % if map.expr.type.is_list_type:
               ${expr}.Nodes (1 .. ${expr}.Count)
            % else:
               ${expr}.Items
            % endif
         loop
            % if map.type.element_type().is_refcounted():
               Inc_Ref (Item_To_Append);
            % endif
            ${vec_pkg}.Append (${vec_var}, Item_To_Append);
         end loop;
      % else:
         declare
            Item_To_Append : constant ${map.type.element_type().name} :=
               ${map.expr.render_expr()};
         begin
            % if map.type.element_type().is_refcounted():
               Inc_Ref (Item_To_Append);
            % endif
            ${vec_pkg}.Append (${vec_var}, Item_To_Append);
         end;
      % endif
   </%def>

   <%def name="build_loop()">
      % if map.index_var:
         ${map.index_var.name} := 0;
      % endif

      ## First, build a vector for all the resulting elements
      <% coll_expr = map.collection.render_expr() %>
      for ${iteration_var} of
         % if map.collection.type.is_list_type:
            ${coll_expr}.nodes (1 .. ${coll_expr}.Count)
         % else:
            ${coll_expr}.Items
         % endif
      loop
         ${scopes.start_scope(map.iter_scope)}
         % if map.element_var:
            ${gdb_helper('bind',
                         map.element_var.source_name.lower,
                         iteration_var.camel_with_underscores)}
         % endif
         % if map.index_var:
            ${gdb_helper('bind',
                         map.index_var.source_name.lower,
                         map.index_var.name.camel_with_underscores)}
         % endif

         % if list_element_var:
            ${element_var} :=
               ${map.element_var.type.name} (${list_element_var});
         % endif

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
         ${scopes.finalize_scope(map.iter_scope)}
      end loop;

      ## Then convert the vector into the final array type
      ${array_var} := Create
        (Items_Count => Natural (${vec_pkg}.Length (${vec_var})));
      for I in ${array_var}.Items'Range loop
         ${array_var}.Items (I) := ${vec_pkg}.Get
           (${vec_var},
            I + ${vec_pkg}.Index_Type'First - ${array_var}.Items'First);
      end loop;
      ${vec_pkg}.Destroy (${vec_var});
   </%def>

   % if map.collection.type.is_list_type:
      ## Empty lists are null: handle this pecularity here to make it easier
      ## for property writers.

      if ${map.collection.render_expr()} = null then
         ${array_var} := Create (0);
      else
         ${build_loop()}
      end if;

   % else:
      ${build_loop()}
   % endif

end;
