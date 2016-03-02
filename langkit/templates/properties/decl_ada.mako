## vim: filetype=makoada

${"overriding" if property.overriding else ""} function ${property.name}
  (${property.self_arg_name} :
   access ${Self.type.name()}_Type${"" if property.dispatching else "'Class"};
   ${property.env_arg_name} : AST_Envs.Lexical_Env := null)
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
