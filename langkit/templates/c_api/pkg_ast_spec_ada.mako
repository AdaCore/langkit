## vim: filetype=makoada

<%namespace name="array_types" file="array_types_ada.mako" />

with ${_self.ada_api_settings.lib_name}.Analysis.C;
use ${_self.ada_api_settings.lib_name}.Analysis.C;

--  This package defines data types and subprograms to provide the
--  implementation of the exported C API for AST nodes.
--
--  Unless one wants to deal with C code, it is very likely that one needs to
--  use this package. Please refer to the C header if you want to use the C
--  API.

package ${_self.ada_api_settings.lib_name}.AST.C is

   function Wrap (Token : Token_Type) return ${token_type};
   function Unwrap (Token : ${token_type}) return Token_Type;

   ${array_types.decl(get_context().root_grammar_class.array_type())}

   ------------------------------------
   -- Lexical environment primitives --
   ------------------------------------

   ${array_types.decl(LexicalEnvType.array_type())}

   function ${capi.get_name('lexical_env_parent')}
     (Env : ${lexical_env_type})
      return ${lexical_env_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('lexical_env_parent')}";

   function ${capi.get_name('lexical_env_node')}
     (Env : ${lexical_env_type})
      return ${node_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('lexical_env_node')}";

% if env_element_type:
   function ${capi.get_name('lexical_env_get')}
     (Env  : ${lexical_env_type};
      Name : ${text_type})
      return ${_self.env_element.array_type().name().camel_with_underscores}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('lexical_env_get')}";
% endif

end ${_self.ada_api_settings.lib_name}.AST.C;
