## vim: filetype=makoada

function ${property.name} (Entity : Env_Element) return Env_Element is
  (${Self.type.name()} (Entity.El).${property.name});
