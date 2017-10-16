## vim: filetype=makoada

<%namespace name="helpers" file="helpers.mako" />

## Regular property function

${"overriding" if property.overriding else ""} function ${property.name}
   ${helpers.argument_list(property, property.dispatching)}
   return ${property.type.name}
   % if property.abstract and not property.abstract_runtime_check:
   is abstract
   % endif
   ;
${ada_doc(property, 0)}
