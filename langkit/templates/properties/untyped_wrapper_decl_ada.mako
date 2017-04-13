## vim: filetype=makoada

function ${property.name} (E : Entity) return Entity is
  (${Self.type.name()} (E.El).${property.name});
