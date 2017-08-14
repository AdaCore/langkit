## vim: filetype=makoada

function ${property.name}
  (E : Entity
   % for arg in property.natural_arguments:
      ; ${arg.name} : ${arg.type.name}
   % endfor
  ) return ${property.untyped_wrapper_rtype.name};
