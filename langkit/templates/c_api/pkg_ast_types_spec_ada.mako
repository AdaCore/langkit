## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="enum_types"    file="enum_types_ada.mako" />

with Ada.Unchecked_Conversion;

with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;

with ${_self.ada_api_settings.lib_name}.Analysis.C;
use ${_self.ada_api_settings.lib_name}.Analysis.C;
with ${_self.ada_api_settings.lib_name}.AST.C;
use ${_self.ada_api_settings.lib_name}.AST.C;

--  This package defines data types and subprograms to provide the
--  implementation of the exported C API for AST nodes subclasses.
--
--  Unless one wants to deal with C code, it is very likely that one needs to
--  use this package. Please refer to the C header if you want to use the C
--  API.

package ${_self.ada_api_settings.lib_name}.AST.Types.C is

   % for enum_type in _self.sorted_types(_self.enum_types):
      ${enum_types.spec(enum_type)}
   % endfor

   % for array_type in _self.sorted_types(_self.array_types):
      % if array_type.element_type().should_emit_array_type:
         ${array_types.decl(array_type)}
      % endif
   % endfor

   % for rec in _self.struct_types:
      <%
         type_name = rec.c_type(capi).name
         ptr_name = '{}_Ptr'.format(type_name)
      %>
      type ${ptr_name} is access ${rec.name()};

      % if rec.is_refcounted():
         procedure ${rec.c_dec_ref(capi)} (R : ${ptr_name});
      % endif
   % endfor

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
