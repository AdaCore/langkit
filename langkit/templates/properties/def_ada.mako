## vim: filetype=makoada

${"overriding" if property.overriding else ""} function ${property.name}
  (${property.self_arg_name} :
   access ${Self.type.name()}_Type${"" if property.dispatching else "'Class"};
   ${property.env_arg_name} : AST_Envs.Lexical_Env := null)
   return ${property.type.name()}
is
   use type AST_Envs.Lexical_Env;

   pragma Warnings (Off, "is not referenced");
   ## We declare a variable Self, that has the named class wide access type
   ## that we can use to dispatch on other properties and all.
   Self : ${Self.type.name()} := ${Self.type.name()}
     (${property.self_arg_name});

   ## Properties are evaluated in the context of a lexical environment. If none
   ## was passed to the property, we assume that the users want to evaluate it
   ## in the context of the scope of the node.
   Current_Env : AST_Envs.Lexical_Env :=
     (if ${property.env_arg_name} /= null
      then ${property.env_arg_name}
      else Node.Parent_Env);
   pragma Warnings (On, "is not referenced");

   ${property.vars.render()}
begin
   ${property.constructed_expr.render_pre()}
   return ${property.constructed_expr.render_expr()};
end ${property.name};
