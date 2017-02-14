## vim: filetype=makoada

## This templates assumes that "prefix.render_pre()" was already output above.

% if not ctx.no_property_checks:

   ## Output a null check only if the prefix can be null
   <%
      if prefix.type.is_ptr:
         operand = prefix.render_expr()
      elif prefix.type.is_env_element_type and prefix.type.el_type.is_ptr:
         operand = '{}.El'.format(prefix.render_expr())
      else:
         operand = None
   %>

   % if operand:
      if ${operand} = null then
         raise Property_Error with "dereferencing a null access";
      end if;
   % endif

% endif
