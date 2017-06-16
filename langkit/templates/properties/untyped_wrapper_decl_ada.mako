## vim: filetype=makoada

function ${property.name}
  (E : Entity)
   return ${property.untyped_wrapper_rtype.name}
is
  (${Self.type.name} (E.El).${property.name});
