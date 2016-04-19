## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="enum_types"    file="enum_types_ada.mako" />

with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;

with ${_self.ada_api_settings.lib_name}.Analysis.C;
use ${_self.ada_api_settings.lib_name}.Analysis.C;

--  This package defines data types and subprograms to provide the
--  implementation of the exported C API for AST nodes.
--
--  Unless one wants to deal with C code, it is very likely that one needs to
--  use this package. Please refer to the C header if you want to use the C
--  API.

package ${_self.ada_api_settings.lib_name}.AST.Types.C is

   % for enum_type in _self.sorted_types(_self.enum_types):
      ${enum_types.spec(enum_type)}
   % endfor

   % for array_type in _self.sorted_types(_self.array_types):
      ${array_types.decl(array_type)}
   % endfor

   % for rec in _self.struct_types:
      type ${rec.c_type(capi).name}_Ptr is access ${rec.name()};
   % endfor

   ------------------------------------
   -- Lexical environment primitives --
   ------------------------------------

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

   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

   --  All these primitives return their result through an OUT parameter. They
   --  return a boolean telling whether the operation was successful (it can
   --  fail if the node does not have the proper type, for instance). When an
   --  AST node is returned, its ref-count is left as-is.

   % for astnode in _self.astnode_types:
       % for field in astnode.fields_with_accessors():
           ${astnode_types.accessor_decl(field)}
       % endfor
   % endfor

end ${_self.ada_api_settings.lib_name}.AST.Types.C;
