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

## Wrapper to return convenient Ada arrays

% if property.is_public \
      and not property.overriding \
      and is_array_type(property.type):
   function ${property.name}
     ${helpers.argument_list(property, False)}
     return ${property.type.api_name};
   % if property.type.is_refcounted:
      --  Helper to return a basic Ada array. The array items are still
      --  ref-counted: the caller must dec-ref them when done with them.
   % else:
      --  Helper to return a basic Ada array
   % endif
% endif
