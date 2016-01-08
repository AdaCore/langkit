## vim: filetype=makoada

${"overriding" if property.overriding else ""} function ${property.name}
  (Node :
   access ${Self.type.name()}_Type${"" if property.dispatching else "'Class"};
   Lex_Env : AST_Envs.Lexical_Env := null)
   return ${property.type.name()} ${"is abstract" if property.abstract else ""};
${ada_doc(property, 0)}
