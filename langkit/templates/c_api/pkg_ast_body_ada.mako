## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="enum_types"    file="enum_types_ada.mako" />

with ${_self.ada_api_settings.lib_name}.Analysis;
use ${_self.ada_api_settings.lib_name}.Analysis;

package body ${_self.ada_api_settings.lib_name}.AST.C is

   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

   % for astnode in _self.astnode_types:
       % for field in astnode.fields_with_accessors():
           ${astnode_types.accessor_body(field)}
       % endfor
   % endfor

end ${_self.ada_api_settings.lib_name}.AST.C;
