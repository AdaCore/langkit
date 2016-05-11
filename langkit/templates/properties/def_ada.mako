## vim: filetype=makoada

% if not property.abstract:
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
      else Node.Self_Env);
   pragma Warnings (On, "is not referenced");

   Property_Result : ${property.type.name()} := ${property.type.nullexpr()};

   ${property.vars.render()}
begin
   ${property.constructed_expr.render_pre()}

   Property_Result := ${property.constructed_expr.render_expr()};
   % if property.type.is_refcounted():
      Inc_Ref (Property_Result);
   % endif

   return Property_Result;

end ${property.name};
% endif
