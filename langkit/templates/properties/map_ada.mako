## vim: filetype=makoada

<%namespace name="collection_expr" file="collection_expr_ada.mako" />
<%namespace name="scopes"          file="scopes_ada.mako" />

<%
   array_var = map.result_var.name

   vec_var = map.result_var.name + Name('Vec')
   vec_pkg = map.type.pkg_vector

   if map.take_while:
      take_while_result = "Take_While_Ok"
      exit_cond = f"not {take_while_result}"
   else:
      take_while_result = None
      exit_cond = None
%>

<%collection_expr:render expr="${map}", exit_cond="${exit_cond}">

   <%def name="before_loop()">
      declare
         ${vec_var} : ${map.type.vector()};
         % if take_while_result:
            ${take_while_result} : Boolean := False;
         % endif
      begin
   </%def>

   <%def name="loop_body()">
      ## Emit the user body for the loop.
      ##
      ## If we have a filter predicate, process only collection items for which
      ## the predicate is true.
      % if map.filter:
         ${map.filter.render_pre()}
         if ${map.filter.render_expr()} then
            ${filtered_loop_body()}
         end if;
      % else:
         ${filtered_loop_body()}
      % endif
   </%def>

   <%def name="filtered_loop_body()">
      ## If we have a "take_while" predicate, stop the whole iteration if it
      ## returns false for the current collection item (see "exit_cond" above).
      % if map.take_while:
         ${map.take_while.render_pre()}
         ${take_while_result} := ${map.take_while.render_expr()};
         if ${take_while_result} then
            ${inner_loop_body()}
         end if;
      % else:
         ${inner_loop_body()}
      % endif
   </%def>

   <%def name="inner_loop_body()">
      ## All predicates accepted this collection item: process it, i.e.
      ## evaluate the "expr" sub-expression and integrate it to the result
      ## according to whether we are doing concatenation mapping or a simple
      ## one.
      ${map.expr.render_pre()}
      <% expr = map.expr.render_expr() %>

      % if map.do_concat:
         for Item_To_Append of
            % if map.expr.type.is_list_type:
               ${expr}.Nodes (1 .. Children_Count (${expr})
            % else:
               ${expr}.Items
            % endif
         loop
            % if map.type.element_type.is_refcounted:
               Inc_Ref (Item_To_Append);
            % endif
            ${vec_pkg}.Append (${vec_var}, Item_To_Append);
         end loop;

      % else:
         declare
            Item_To_Append : constant ${map.type.element_type.name} := ${expr};
         begin
            % if map.type.element_type.is_refcounted:
               Inc_Ref (Item_To_Append);
            % endif
            ${vec_pkg}.Append (${vec_var}, Item_To_Append);
         end;
      % endif
   </%def>

   <%def name="empty_list()">
      ${array_var} := ${map.type.constructor_name} (0);
   </%def>

   <%def name="after_loop()">
      ## The following code continues the "begin" block started in
      ## "before_loop()".

         ## Convert the vector into the final array type
         ${array_var} := ${map.type.constructor_name}
           (Items_Count => Natural (${vec_pkg}.Length (${vec_var})));
         for I in ${array_var}.Items'Range loop
            ${array_var}.Items (I) := ${vec_pkg}.Get
              (${vec_var},
               I + ${vec_pkg}.Index_Type'First - ${array_var}.Items'First);
         end loop;
         ${vec_pkg}.Destroy (${vec_var});

      end;
   </%def>

</%collection_expr:render>
