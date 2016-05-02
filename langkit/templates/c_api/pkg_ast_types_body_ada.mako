## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="enum_types"    file="enum_types_ada.mako" />

with ${_self.ada_api_settings.lib_name}.Analysis;
use ${_self.ada_api_settings.lib_name}.Analysis;
with ${_self.ada_api_settings.lib_name}.Analysis_Interfaces;
use ${_self.ada_api_settings.lib_name}.Analysis_Interfaces;

package body ${_self.ada_api_settings.lib_name}.AST.Types.C is

   ----------
   -- Wrap --
   ----------

   function Wrap (Token : Token_Type) return ${token_type} is
      function Convert is new Ada.Unchecked_Conversion
        (Token_Data_Handler_Access, System.Address);

      D : constant Token_Data_Type := Data (Token);
      K : Token_Kind := D.Kind;
   begin
      return (Token_Data => Convert (Token.TDH),
              Index      => int (Token.Token),
              Kind       => K'Enum_Rep,
              Text       => Wrap (D.Text),
              Sloc_Range => Wrap (D.Sloc_Range));
   end Wrap;

   ------------
   -- Unwrap --
   ------------

   function Unwrap (Token : ${token_type}) return Token_Type is
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Token_Data_Handler_Access);
   begin
      return (TDH   => Convert (Token.Token_Data),
              Token => Token_Index (Token.Index));
   end Unwrap;

   function ${capi.get_name('lexical_env_parent')}
     (Env : ${lexical_env_type})
      return ${lexical_env_type}
   is
      E : constant AST_Envs.Lexical_Env := Unwrap (Env);
   begin
      return Wrap (E.Parent);
   end ${capi.get_name('lexical_env_parent')};

   function ${capi.get_name('lexical_env_node')}
     (Env : ${lexical_env_type})
      return ${node_type}
   is
      E : constant AST_Envs.Lexical_Env := Unwrap (Env);
   begin
      return Wrap (E.Node);
   end ${capi.get_name('lexical_env_node')};

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
