## vim: filetype=makoada

<%namespace name="scopes" file="scopes_ada.mako" />

<%
   list_element_var = (quantifier.list_element_var.name
                       if quantifier.list_element_var else
                       None)
   element_var = quantifier.element_var.name
   iteration_var = list_element_var or element_var
   result_var = quantifier.result_var.name
%>

${quantifier.collection.render_pre()}

${result_var} := ${'False' if quantifier.kind == ANY else 'True'};

<%def name="build_loop()">
   % if quantifier.index_var:
      ${quantifier.index_var.name} := 0;
   % endif

   <% coll_expr = quantifier.collection.render_expr() %>
   for ${iteration_var} of
      % if quantifier.collection.type.is_list_type:
         ${coll_expr}.Nodes (1 .. ${coll_expr}.Count)
      % else:
         ${coll_expr}.Items
      % endif
   loop
      ${scopes.start_scope(quantifier.iter_scope)}
      % if list_element_var:
         ${element_var} :=
            ${quantifier.element_var.type.name} (${list_element_var});
      % endif
      ${quantifier.expr.render_pre()}

      ## Depending on the kind of the quantifier, we want to abort as soon as
      ## the predicate holds or as soon as it does not hold.
      % if quantifier.kind == ANY:
         if ${quantifier.expr.render_expr()} then
            ${result_var} := True;
            exit;
         end if;
      % else:
         if not (${quantifier.expr.render_expr()}) then
            ${result_var} := False;
            exit;
         end if;
      % endif

      % if quantifier.index_var:
         ${quantifier.index_var.name} := ${quantifier.index_var.name} + 1;
      % endif

      ${scopes.finalize_scope(quantifier.iter_scope)}
   end loop;
</%def>

% if quantifier.collection.type.is_list_type:
   ## Empty lists are null: handle this pecularity here to make it easier
   ## for property writers.

   if ${quantifier.collection.render_expr()} /= null then
      ${build_loop()}
   end if;

% else:
   ${build_loop()}
% endif
