## vim: filetype=makoada

## This file provides a "render" Mako template to be used in order to generate
## code that iterate over a "collection" (list node or array).

<%namespace name="scopes" file="scopes_ada.mako" />

## Generate code to iterate on a collection, i.e. a list node or an array
## value.
##
## "expr" must be a CollectionExpression.BaseExpr that computes the iteration
## (e.g. a "map" expression, or a "quantifier" one).
##
## If not None, "exit_cond" must be an Ada expression (as a string) to be used
## as a boolean expression at the end of each iteration: if it is False, the
## iteration continues, and if it is True, the iteration is stopped.
##
## Uses for this template must provide 4 sub-templates:
##
## * "before_loop", to generate code before the loop that implements the
##   iteration.
##
## * "loop_body", to generate code before for an iteration (i.e. inside the
##   loop that implements the iteration).
##
## * "empty_list", to generate code that handles the "empty list" special cases
##   (empty lists are represented as null nodes).
##
## * "after_loop", to generate code after the loop that implements the
##   iteration.
<%def name="render(expr, exit_cond=None)">

   <% loop_body = caller.loop_body %>

   ${expr.collection.render_pre()}

   ${caller.before_loop()}

   <%def name="render_loop()">
      % if expr.index_var:
         ${expr.index_var.name} := 0;
      % endif

      declare
         <%
            coll_expr = expr.collection.render_expr()
            coll_type = expr.collection.type
         %>
         Collection : constant ${coll_type.name} := ${coll_expr};
      begin
         for ${expr.codegen_element_var.name} of
            % if expr.collection.type.is_list_type:
               Collection.Nodes (1 .. Children_Count (Collection))
            % else:
               Collection.Items
            % endif
         loop
            ## Initialize all element variables
            % for v in reversed(expr.iter_vars):
               % if v.init_expr:
                  ${v.init_expr.render_pre()}
                  ${assign_var(v.var, v.init_expr.render_expr())}
               % endif
            % endfor

            ${scopes.start_scope(expr.inner_scope)}

            ## Bind user iteration variables
            % if expr.user_element_var.source_name:
               ${gdb_bind_var(expr.user_element_var)}
            % endif
            % if expr.index_var:
               ${gdb_bind_var(expr.index_var)}
            % endif

            ${loop_body()}

            ${scopes.finalize_scope(expr.inner_scope)}

            ## If we decided after this iteration to exit the loop, we can do
            ## it now that the iteration scope it finalized.
            % if exit_cond:
               exit when ${exit_cond};
            % endif

            ## Prepare the index for the next loop iteration
            % if expr.index_var:
               ${expr.index_var.name} := ${expr.index_var.name} + 1;
            % endif
         end loop;
      end;
   </%def>

   ## Empty lists are null: process null list nodes as such, i.e. run no
   ## iteration instead of trying to dereference the null node. This DSL API is
   ## much more user friendly.
   % if expr.collection.type.is_list_type:
      if ${expr.collection.render_expr()} = null then
         ${caller.empty_list()}
      else
         ${render_loop()}
      end if;
   % else:
      ${render_loop()}
   % endif

   ${caller.after_loop()}

</%def>
