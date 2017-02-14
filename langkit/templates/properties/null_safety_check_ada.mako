## vim: filetype=makoada

## This templates requires two parameters:
##  * prefix: which is the ResolvedExpression on which the null check must be
##    performed. This assumes that "prefix.render_pre()" was already output
##    above.
##  * implicit_deref: whether the null check works on an implicitely
##    dereferenced env element. In this case, the null check is performed on
##    the wrapped element, not on the wrapping one.

% if not ctx.no_property_checks:

   ## Output a null check only if the prefix can be null
   <%
      if prefix.type.is_ptr:
         operand = prefix.render_expr()
      elif implicit_deref:
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
