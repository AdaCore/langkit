## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="enum_types"    file="enum_types_ada.mako" />

with ${_self.ada_api_settings.lib_name}.Analysis;
use ${_self.ada_api_settings.lib_name}.Analysis;
with ${_self.ada_api_settings.lib_name}.Analysis_Interfaces;
use ${_self.ada_api_settings.lib_name}.Analysis_Interfaces;

package body ${_self.ada_api_settings.lib_name}.AST.Types.C is

   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

   % for astnode in _self.astnode_types:
       % for field in astnode.fields_with_accessors():
           ${astnode_types.accessor_body(field)}
       % endfor
   % endfor

   % for array_type in _self.sorted_types(_self.array_types):
      % if array_type.element_type().should_emit_array_type:
         ${array_types.body(array_type)}
      % endif
   % endfor

   % for rec in _self.struct_types:
      <%
         type_name = rec.c_type(capi).name
         ptr_name = '{}_Ptr'.format(type_name)
         dec_ref = rec.c_dec_ref(capi)
      %>

      % if rec.is_refcounted():
         procedure ${dec_ref} (R : ${ptr_name}) is
         begin
            Dec_Ref (R.all);
         end ${dec_ref};
      % endif
   % endfor

end ${_self.ada_api_settings.lib_name}.AST.Types.C;
