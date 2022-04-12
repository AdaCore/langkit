## vim: filetype=makoada

<%namespace name="collection_expr" file="collection_expr_ada.mako" />
<%namespace name="scopes"          file="scopes_ada.mako" />

<%
   result_var = find.result_var.name

   # We want to abort the loop as soon as the predicate returns True (element
   # is found).
   exit_cond = find.inner_expr.render_expr()
%>

<%collection_expr:render expr="${find}" exit_cond="${exit_cond}">

   <%def name="before_loop()">
      ${result_var} := ${find.type.nullexpr};
   </%def>

   <%def name="loop_body()">
      ${find.inner_expr.render_pre()}
      if ${find.inner_expr.render_expr()} then
         ${result_var} := ${find.user_element_var.name};
      end if;
   </%def>

   <%def name="empty_list()">
      ${result_var} := ${find.type.nullexpr};
   </%def>

   <%def name="after_loop()">
   </%def>

</%collection_expr:render>
