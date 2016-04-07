## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="enum_types"    file="enum_types_ada.mako" />

with ${_self.ada_api_settings.lib_name}.Analysis;
use ${_self.ada_api_settings.lib_name}.Analysis;
with ${_self.ada_api_settings.lib_name}.Analysis_Interfaces;
use ${_self.ada_api_settings.lib_name}.Analysis_Interfaces;

package body ${_self.ada_api_settings.lib_name}.AST.Types.C is

% if env_element_type:
   function ${capi.get_name('lexical_env_get')}
     (Env  : ${lexical_env_type};
      Name : ${text_type})
      return ${_self.env_element.array_type().name()}
   is
      E : constant AST_Envs.Lexical_Env := Unwrap (Env);
   begin
      --  TODO??? The root environment is not tied to any node, so we cannot
      --  get a symbol to look it up. We should probably solve this
      --  automatically creating an anonymous unit to embed the root
      --  environment.
      if E.Node = null then
         raise Property_Error;
      end if;

      declare
         U : constant Analysis_Unit_Interface := E.Node.Unit;
         N : constant Symbol_Type := Unwrap (U, Name);
      begin
         return Create (if N = null
                        then (1 .. 0 => <>)
                        else AST_Envs.Get (E, N));
      end;
   end ${capi.get_name('lexical_env_get')};
% endif

   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

   % for astnode in _self.astnode_types:
       % for field in astnode.fields_with_accessors():
           ${astnode_types.accessor_body(field)}
       % endfor
   % endfor

end ${_self.ada_api_settings.lib_name}.AST.Types.C;
