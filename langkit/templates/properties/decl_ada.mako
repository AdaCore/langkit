## vim: filetype=makoada

${"overriding" if property.overriding else ""} function ${property.name}
  (${property.self_arg_name} :
   access ${Self.type.name()}_Type${"" if property.dispatching else "'Class"}

   % for arg_name, arg_type, arg_dv in property.arguments:
      ; ${arg_name} : ${arg_type.name()}
      % if arg_dv:
         := ${arg_dv}
      % endif
   % endfor
  )
   return ${property.type.name()}
   % if property.abstract:
      % if property.abstract_runtime_check:
      is (raise Property_Error with "Property not implemented on type")
      % else:
      is abstract
      % endif
   % endif
   ;
${ada_doc(property, 0)}
