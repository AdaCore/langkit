## vim: filetype=makoada

<%namespace name="collection_expr" file="collection_expr_ada.mako" />

<%
   set_type = to_set.type
   element_type = set_type.element_type
   result_var = to_set.result_var.codegen_name
%>

<%collection_expr:render expr="${to_set}">

   <%def name="before_loop()">
      ${result_var} := new ${set_type.pointed}'(Ref_Count => 1, Items => <>);
   </%def>

   <%def name="loop_body()">
      ${to_set.inner_expr.render_pre()}
      <% expr = to_set.inner_expr.render_expr() %>

      declare
         Item_To_Append : constant ${element_type.name} := ${expr};
         Dummy_Position : ${set_type.hashed_sets_pkg_name}.Cursor;
         Inserted       : Boolean;
      begin
         ${result_var}.Items.Insert
           (Item_To_Append, Dummy_Position, Inserted);
         % if element_type.is_refcounted:
         if Inserted then
            Inc_Ref (Item_To_Append);
         end if;
         % endif
      end;
   </%def>

   <%def name="empty_list()">
      ${result_var} := ${set_type.nullexpr};
   </%def>

   <%def name="after_loop()">
   </%def>

</%collection_expr:render>


