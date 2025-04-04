## vim: filetype=makoada

<%namespace name="helpers" file="helpers.mako" />

## Regular property function

${"overriding" if property.is_overriding else ""}
function ${property.names.codegen}
   ${helpers.argument_list(property, property.dispatching)}
   return ${property.type.name}
   % if property.abstract:
   is abstract
   % endif
   % if property.is_dispatcher:
   with Inline_Always
   % endif
   ;
${ada_doc(property, 0)}
