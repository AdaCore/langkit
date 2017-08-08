## vim: filetype=makoada

function ${property.name}
  (E : Entity)
   return ${property.untyped_wrapper_rtype.name}
is
  (${Self.type.name} (E.El).${property.name}
    ${("(E_Info => Shed_Bindings (E.Info, E.El.Node_Env))"
       if property.uses_entity_info else "")});
