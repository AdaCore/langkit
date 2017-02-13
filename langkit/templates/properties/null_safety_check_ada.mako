## vim: filetype=makoada

## This templates assumes that "prefix.render_pre()" was already output above.
% if prefix.type.is_ptr:
   if ${prefix.render_expr()} = null then
      raise Property_Error with "dereferencing a null access";
   end if;
% endif
