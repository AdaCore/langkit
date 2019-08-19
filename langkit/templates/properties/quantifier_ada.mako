## vim: filetype=makoada

<%namespace name="scopes" file="scopes_ada.mako" />

<%
   codegen_element_var = quantifier.element_vars[-1][0].name
   user_element_var = quantifier.element_vars[0][0]
   result_var = quantifier.result_var.name
%>

${quantifier.collection.render_pre()}

${result_var} := ${'False' if quantifier.kind == ANY else 'True'};

<%def name="build_loop()">
   % if quantifier.index_var:
      ${quantifier.index_var.name} := 0;
   % endif

   declare
      <%
         coll_expr = quantifier.collection.render_expr()
         coll_type = quantifier.collection.type
      %>
      Collection : constant ${coll_type.name} := ${coll_expr};
   begin
      for ${codegen_element_var} of
         % if quantifier.collection.type.is_list_type:
            Collection.Nodes (1 .. Children_Count (Collection))
         % else:
            Collection.Items
         % endif
      loop
         ## Initialize all element variables
         % for elt_var, init_expr in reversed(quantifier.element_vars):
            % if init_expr:
               ${init_expr.render_pre()}
               ${assign_var(elt_var, init_expr.render_expr())}
            % endif
         % endfor

         ${scopes.start_scope(quantifier.iter_scope)}

         ## Bind user iteration variables
         % if user_element_var.source_name:
            ${gdb_bind_var(user_element_var)}
         % endif
         % if quantifier.index_var:
            ${gdb_bind_var(quantifier.index_var)}
         % endif

         ${quantifier.expr.render_pre()}

         ## Depending on the kind of the quantifier, we want to abort as soon
         ## as the predicate holds or as soon as it does not hold.
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
   end;
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
