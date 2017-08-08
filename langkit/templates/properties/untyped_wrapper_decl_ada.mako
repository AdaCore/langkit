## vim: filetype=makoada

function ${property.name}
  (E : Entity)
   return ${property.untyped_wrapper_rtype.name}
is
  (${Self.type.name} (E.El).${property.name}
    ${"(E_Info => E.Info)" if property.uses_entity_info else ""});
