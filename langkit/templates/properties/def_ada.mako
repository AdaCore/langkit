## vim: filetype=makoada

${"overriding" if property.overriding else ""} function ${property.name}
  (Self : ${Self.type.name()}_Type${"" if property.dispatching else "'Class"})
   return ${property.type.name()}
is
   ${property.vars.render()}
begin
   ${property.constructed_expr.render_pre()}
   return ${property.constructed_expr.render_expr()};
end ${property.name};
