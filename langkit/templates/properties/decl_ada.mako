## vim: filetype=makoada

<%namespace name="helpers" file="helpers.mako" />

${"overriding" if property.overriding else ""} function ${property.name}
   ${helpers.argument_list(property, property.dispatching)}
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
