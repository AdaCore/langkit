## vim: filetype=makoada

<%namespace name="collection_expr" file="collection_expr_ada.mako" />
<%namespace name="scopes"          file="scopes_ada.mako" />

<%
   result_var = quantifier.result_var.codegen_name

   # We want to abort the loop as soon as the predicate returns True for Any
   # quantifiers, and as soon as it returns False for All predicates.
   exit_cond = result_var if quantifier.kind == ANY else f"not {result_var}"
%>

<%collection_expr:render expr="${quantifier}" exit_cond="${exit_cond}">

   <%def name="before_loop()">
      ${result_var} := ${"False" if quantifier.kind == ANY else "True"};
   </%def>

   <%def name="loop_body()">
      ${quantifier.inner_expr.render_pre()}
      ${result_var} := ${quantifier.inner_expr.render_expr()};
   </%def>

   <%def name="empty_list()">
      null;
   </%def>

   <%def name="after_loop()">
   </%def>

</%collection_expr:render>
