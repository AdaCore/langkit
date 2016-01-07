## vim: filetype=makoada

${"overriding" if property.overriding else ""} function ${property.name}
  (Node :
   access ${Self.type.name()}_Type${"" if property.dispatching else "'Class"})
   return ${property.type.name()}
is
   ## We declare a variable Self, that has the named class wide access type
   ## that we can use to dispatch on other properties and all.
   Self : ${Self.type.name()} := ${Self.type.name()} (Node);

   ${property.vars.render()}
begin
   ${property.constructed_expr.render_pre()}
   return ${property.constructed_expr.render_expr()};
end ${property.name};
