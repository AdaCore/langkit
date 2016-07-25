## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />

with Ada.Unchecked_Conversion;

with Interfaces.C; use Interfaces.C;

package body ${_self.ada_api_settings.lib_name}.AST.C is

   ----------
   -- Wrap --
   ----------

   function Wrap (Token : Token_Type) return ${token_type} is
      function Convert is new Ada.Unchecked_Conversion
        (Token_Data_Handler_Access, System.Address);

   begin
      if Token = No_Token then
         return (Token_Data   => System.Null_Address,
                 Token_Index  => -1,
                 Trivia_Index => -1,
                 others       => <>);
      end if;

      declare
         D : constant Token_Data_Type := Data (Token);
         K : Token_Kind := D.Kind;
      begin
         return (Token_Data   => Convert (Token.TDH),
                 Token_Index  => int (Token.Token),
                 Trivia_Index => int (Token.Trivia),
                 Kind         => K'Enum_Rep,
                 Text         => Wrap (D.Text),
                 Sloc_Range   => Wrap (D.Sloc_Range));
      end;
   end Wrap;

   ------------
   -- Unwrap --
   ------------

   function Unwrap (Token : ${token_type}) return Token_Type is
      use System;

      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Token_Data_Handler_Access);
   begin
      return (if Token.Token_Data = Null_Address
              then No_Token
              else (TDH    => Convert (Token.Token_Data),
                    Token  => Token_Index (Token.Token_Index),
                    Trivia => Token_Index (Token.Trivia_Index)));
   end Unwrap;

   ${array_types.body(T.root_node.array_type())}

   ${array_types.body(LexicalEnvType.array_type())}

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
         raise Property_Error with "LexicalEnv.Get on null LexicalEnv";
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

   procedure ${capi.get_name('lexical_env_dec_ref')}
     (Env : ${lexical_env_type})
   is
      E : AST_Envs.Lexical_Env := Unwrap (Env);
   begin
      Dec_Ref (E);
   end ${capi.get_name('lexical_env_dec_ref')};

end ${_self.ada_api_settings.lib_name}.AST.C;
