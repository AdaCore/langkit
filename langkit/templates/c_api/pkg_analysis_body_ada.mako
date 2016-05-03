## vim: filetype=makoada

<%namespace name="astnode_types" file="astnode_types_ada.mako" />

pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
use Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");

with System.Memory;

with GNATCOLL.Iconv;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Extensions;  use Langkit_Support.Extensions;
with Langkit_Support.Text;        use Langkit_Support.Text;

with ${_self.ada_api_settings.lib_name}.Analysis;
use ${_self.ada_api_settings.lib_name}.Analysis;
with ${_self.ada_api_settings.lib_name}.AST;
use ${_self.ada_api_settings.lib_name}.AST;
with ${_self.ada_api_settings.lib_name}.AST.C;
use ${_self.ada_api_settings.lib_name}.AST.C;
with ${_self.ada_api_settings.lib_name}.Lexer;
use ${_self.ada_api_settings.lib_name}.Lexer;

package body ${_self.ada_api_settings.lib_name}.Analysis.C is

   function Value_Or_Empty (S : chars_ptr) return String
   --  If S is null, return an empty string. Return Value (S) otherwise.
   is (if S = Null_Ptr
       then ""
       else Value (S));

   Last_Exception : ${exception_type}_Ptr := null;

   ----------
   -- Free --
   ----------

   procedure Free (Address : System.Address) is
      procedure C_Free (Address : System.Address)
        with Import        => True,
             Convention    => C,
             External_Name => "free";
   begin
      C_Free (Address);
   end Free;

   -------------------------
   -- Analysis primitives --
   -------------------------

   function ${capi.get_name("create_analysis_context")}
     (Charset : chars_ptr)
      return ${analysis_context_type}
   is
   begin
      Clear_Last_Exception;

      declare
         C : constant String := (if Charset = Null_Ptr
                                 then ""
                                 else Value (Charset));
      begin
         return Wrap (if C'Length = 0
                      then Create
                      else Create (C));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ${analysis_context_type} (System.Null_Address);
   end ${capi.get_name("create_analysis_context")};

   procedure ${capi.get_name("destroy_analysis_context")}
     (Context : ${analysis_context_type})
   is
   begin
      Clear_Last_Exception;

      declare
         C : Analysis_Context := Unwrap (Context);
      begin
         Destroy (C);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ${capi.get_name("destroy_analysis_context")};

   function ${capi.get_name("get_analysis_unit_from_file")}
     (Context           : ${analysis_context_type};
      Filename, Charset : chars_ptr;
      Reparse           : int;
      With_Trivia       : int)
      return ${analysis_unit_type}
   is
   begin
      Clear_Last_Exception;

      declare
         Ctx  : constant Analysis_Context := Unwrap (Context);
         Unit : constant Analysis_Unit := Get_From_File
           (Ctx,
            Value (Filename),
            Value_Or_Empty (Charset),
            Reparse /= 0,
            With_Trivia /= 0);
      begin
         return Wrap (Unit);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ${analysis_unit_type} (System.Null_Address);
   end ${capi.get_name("get_analysis_unit_from_file")};

   function ${capi.get_name("get_analysis_unit_from_buffer")}
     (Context           : ${analysis_context_type};
      Filename, Charset : chars_ptr;
      Buffer            : chars_ptr;
      Buffer_Size       : size_t;
      With_Trivia       : int)
      return ${analysis_unit_type}
   is
   begin
      Clear_Last_Exception;

      declare
         Ctx : constant Analysis_Context := Unwrap (Context);
         Unit : Analysis_Unit;

         Buffer_Str : String (1 .. Positive (Buffer_Size));
         for Buffer_Str'Address use Convert (Buffer);
      begin
         Unit := Get_From_Buffer
           (Ctx,
            Value (Filename),
            Value_Or_Empty (Charset),
            Buffer_Str,
            With_Trivia /= 0);
         return Wrap (Unit);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ${analysis_unit_type} (System.Null_Address);
   end ${capi.get_name("get_analysis_unit_from_buffer")};

   function ${capi.get_name("remove_analysis_unit")}
     (Context  : ${analysis_context_type};
      Filename : chars_ptr) return int
   is
   begin
      Clear_Last_Exception;

      declare
         Ctx : constant Analysis_Context := Unwrap (Context);
      begin
         begin
            Remove (Ctx, Value (Filename));
         exception
            when Constraint_Error =>
               return 0;
         end;
         return 1;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ${capi.get_name("remove_analysis_unit")};

   function ${capi.get_name("unit_root")} (Unit : ${analysis_unit_type})
                                           return ${node_type}
   is
   begin
      Clear_Last_Exception;

      declare
         U : constant Analysis_Unit := Unwrap (Unit);
      begin
         return Wrap (U.AST_Root);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ${node_type} (System.Null_Address);
   end ${capi.get_name("unit_root")};

   procedure ${capi.get_name('unit_first_token')}
     (Unit  : ${analysis_unit_type};
      Token : ${token_type}_Ptr)
   is
      U : constant Analysis_Unit := Unwrap (Unit);
      T : constant Token_Type := First_Token (U);
   begin
      Token.all := Wrap (T);
   end ${capi.get_name('unit_first_token')};

   function ${capi.get_name("unit_diagnostic_count")}
     (Unit : ${analysis_unit_type}) return unsigned
   is
   begin
      Clear_Last_Exception;

      declare
         U : constant Analysis_Unit := Unwrap (Unit);
      begin
         return unsigned (U.Diagnostics.Length);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ${capi.get_name("unit_diagnostic_count")};

   function ${capi.get_name("unit_diagnostic")}
     (Unit         : ${analysis_unit_type};
      N            : unsigned;
      Diagnostic_P : ${diagnostic_type}_Ptr) return int
   is
   begin
      Clear_Last_Exception;

      declare
         U : constant Analysis_Unit := Unwrap (Unit);
      begin
         if N < unsigned (U.Diagnostics.Length) then
            declare
               D_In  : Diagnostic renames U.Diagnostics (Natural (N));
               D_Out : ${diagnostic_type} renames Diagnostic_P.all;
            begin
               D_Out.Sloc_Range := Wrap (D_In.Sloc_Range);
               D_Out.Message := Wrap (D_In.Message);
               return 1;
            end;
         else
            return 0;
         end if;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ${capi.get_name("unit_diagnostic")};

   function ${capi.get_name("unit_incref")}
     (Unit : ${analysis_unit_type}) return ${analysis_unit_type}
   is
   begin
      Clear_Last_Exception;

      declare
         U : constant Analysis_Unit := Unwrap (Unit);
      begin
         Inc_Ref (U);
         return Unit;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ${analysis_unit_type} (System.Null_Address);
   end ${capi.get_name("unit_incref")};

   procedure ${capi.get_name("unit_decref")} (Unit : ${analysis_unit_type})
   is
   begin
      Clear_Last_Exception;

      declare
         U : constant Analysis_Unit := Unwrap (Unit);
      begin
         Dec_Ref (U);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ${capi.get_name("unit_decref")};

   procedure ${capi.get_name("unit_reparse_from_file")}
     (Unit : ${analysis_unit_type}; Charset : chars_ptr)
   is
   begin
      Clear_Last_Exception;

      declare
         U : constant Analysis_Unit := Unwrap (Unit);
      begin
         Reparse (U, Value_Or_Empty (Charset));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ${capi.get_name("unit_reparse_from_file")};

   procedure ${capi.get_name("unit_reparse_from_buffer")}
     (Unit        : ${analysis_unit_type};
      Charset     : chars_ptr;
      Buffer      : chars_ptr;
      Buffer_Size : size_t)
   is
   begin
      Clear_Last_Exception;

      declare
         U : constant Analysis_Unit := Unwrap (Unit);
         Buffer_Str : String (1 .. Positive (Buffer_Size));
         for Buffer_Str'Address use Convert (Buffer);
      begin
         Reparse (U, Value_Or_Empty (Charset), Buffer_Str);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ${capi.get_name("unit_reparse_from_buffer")};

   procedure ${capi.get_name("unit_populate_lexical_env")}
     (Unit : ${analysis_unit_type})
   is
   begin
      Clear_Last_Exception;

      declare
         U : constant Analysis_Unit := Unwrap (Unit);
      begin
         Populate_Lexical_Env (U);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ${capi.get_name("unit_populate_lexical_env")};

   ---------------------------------
   -- General AST node primitives --
   ---------------------------------

   Node_Kind_Names : constant array (Positive range <>) of Text_Access :=
     (new Text_Type'(To_Text ("list"))
      % for astnode in _self.astnode_types:
         % if not astnode.abstract:
            , new Text_Type'(To_Text ("${astnode.name().camel}"))
         % endif
      % endfor
      );

   function ${capi.get_name("node_kind")} (Node : ${node_type})
      return ${node_kind_type}
   is
   begin
      Clear_Last_Exception;

      declare
         N : constant ${root_node_type_name} := Unwrap (Node);
      begin
         return ${node_kind_type} (Kind (N));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ${node_kind_type}'First;
   end ${capi.get_name("node_kind")};

   function ${capi.get_name("kind_name")} (Kind : ${node_kind_type})
                                           return ${text_type}
   is
   begin
      Clear_Last_Exception;

      declare
         Name : Text_Access renames Node_Kind_Names (Natural (Kind));
      begin
         return (Chars => Name.all'Address, Length => Name'Length);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return (System.Null_Address, 0);
   end ${capi.get_name("kind_name")};

   procedure ${capi.get_name("node_sloc_range")}
     (Node         : ${node_type};
      Sloc_Range_P : ${sloc_range_type}_Ptr)
   is
   begin
      Clear_Last_Exception;

      declare
         N : constant ${root_node_type_name} := Unwrap (Node);
      begin
         Sloc_Range_P.all := Wrap (Sloc_Range (N));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end ${capi.get_name("node_sloc_range")};

   function ${capi.get_name("lookup_in_node")}
     (Node : ${node_type};
      Sloc : ${sloc_type}_Ptr) return ${node_type}
   is
   begin
      Clear_Last_Exception;

      declare
         N : constant ${root_node_type_name} := Unwrap (Node);
         S : constant Source_Location := Unwrap (Sloc.all);
      begin
         return Wrap (Lookup (N, S));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ${node_type} (System.Null_Address);
   end ${capi.get_name("lookup_in_node")};

   function ${capi.get_name("node_child_count")} (Node : ${node_type})
                                                  return unsigned
   is
   begin
      Clear_Last_Exception;

      declare
         N : constant ${root_node_type_name} := Unwrap (Node);
      begin
         return unsigned (Child_Count (N));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ${capi.get_name("node_child_count")};

   function ${capi.get_name("node_child")}
     (Node    : ${node_type};
      N       : unsigned;
      Child_P : ${node_type}_Ptr) return int
   is
   begin
      Clear_Last_Exception;

      declare
         Nod    : constant ${root_node_type_name} := Unwrap (Node);
         Result : ${root_node_type_name};
         Exists : Boolean;
      begin
         if N > unsigned (Natural'Last) then
            return 0;
         end if;
         Get_Child (Nod, Natural (N), Exists, Result);
         if Exists then
            Child_P.all := Wrap (Result);
            return 1;
         else
            return 0;
         end if;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ${capi.get_name("node_child")};

   function ${capi.get_name("text_to_locale_string")}
     (Text : ${text_type}) return System.Address
   is
   begin
      Clear_Last_Exception;

      declare
         use GNATCOLL.Iconv;

         Input_Byte_Size : constant size_t := 4 * Text.Length;

         Output_Byte_Size : constant size_t := Input_Byte_Size + 1;
         --  Assuming no encoding will take more than 4 bytes per character, 4
         --  times the size of the input text plus one null byte should be
         --  enough to hold the result. This is a development helper anyway, so
         --  we don't have performance concerns.

         Result : constant System.Address := System.Memory.Alloc
           (System.Memory.size_t (Output_Byte_Size));
         --  Buffer we are going to return to the caller. We use
         --  System.Memory.Alloc so that users can call C's "free" function in
         --  order to free it.

         Input : String (1 .. Natural (Input_Byte_Size));
         for Input'Address use Text.Chars;

         Output : String (1 .. Natural (Output_Byte_Size));
         for Output'Address use Result;

         State                     : Iconv_T;
         Input_Index, Output_Index : Positive := 1;
         Status                    : Iconv_Result;

         From_Code : constant String :=
           (if System."=" (System.Default_Bit_Order, System.Low_Order_First)
            then UTF32LE
            else UTF32BE);

      begin
         --  GNATCOLL.Iconv raises Constraint_Error exceptions for empty
         --  strings, so handle them ourselves.

         if Input_Byte_Size = 0 then
            Output (1) := ASCII.NUL;
         end if;

         --  Encode to the locale. Don't bother with error checking...

         Set_Locale;
         State := Iconv_Open
           (To_Code         => Locale,
            From_Code       => From_Code,
            Transliteration => True,
            Ignore          => True);
         Iconv (State, Input, Input_Index, Output, Output_Index, Status);
         Iconv_Close (State);

         --  Don't forget the trailing NULL character to keep C programs happy
         Output (Output_Index) := ASCII.NUL;

         return Result;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return System.Null_Address;
   end ${capi.get_name("text_to_locale_string")};

   -------------------------
   -- Extensions handling --
   -------------------------

   function ${capi.get_name("register_extension")} (Name : chars_ptr)
      return unsigned
   is
   begin
      Clear_Last_Exception;

      return unsigned (Register_Extension (Value (Name)));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ${capi.get_name("register_extension")};

   function ${capi.get_name("node_extension")}
     (Node   : ${node_type};
      Ext_Id : unsigned;
      Dtor   : ${capi.get_name("node_extension_destructor")})
      return System.Address
   is
   begin
      Clear_Last_Exception;

      declare
         N  : constant ${root_node_type_name} := Unwrap (Node);
         ID : constant Extension_ID := Extension_Id (Ext_Id);
         D  : constant Extension_Destructor := Convert (Dtor);
      begin
         return Get_Extension (N, ID, D).all'Address;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return System.Null_Address;
   end ${capi.get_name("node_extension")};

   ----------
   -- Wrap --
   ----------

   function Wrap (S : Unbounded_Wide_Wide_String) return ${text_type} is
      Chars  : Big_Wide_Wide_String_Access;
      Length : Natural;
   begin
      Get_Wide_Wide_String (S, Chars, Length);
      return (Chars.all'Address, size_t (Length));
   end Wrap;

   ------------------------
   -- Set_Last_Exception --
   ------------------------

   procedure Set_Last_Exception (Exc  : Exception_Occurrence) is
   begin
      --  If it's the first time, allocate room for the exception information

      if Last_Exception = null then
         Last_Exception := new ${exception_type};

      --  If it is not the first time, free memory allocated for the last
      --  exception.

      elsif Last_Exception.Information /= Null_Ptr then
         Free (Last_Exception.Information);
      end if;

      Last_Exception.Information := New_String (Exception_Information (Exc));
   end Set_Last_Exception;

   --------------------------
   -- Clear_Last_Exception --
   --------------------------

   procedure Clear_Last_Exception is
   begin
      if Last_Exception /= null then
         Free (Last_Exception.Information);
      end if;
   end Clear_Last_Exception;

   function ${capi.get_name("get_last_exception")} return ${exception_type}_Ptr
   is
   begin
      if Last_Exception = null
         or else Last_Exception.Information = Null_Ptr
      then
         return null;
      else
         return Last_Exception;
      end if;
   end ${capi.get_name("get_last_exception")};

   function ${capi.get_name('token_kind_name')} (Kind : int) return chars_ptr
   is
      K : Token_Kind;
   begin
      begin
         K := Token_Kind'Enum_Val (Kind);
      exception
         when Exc : Constraint_Error =>
            Set_Last_Exception (Exc);
            return Null_Ptr;
      end;

      return New_String (Token_Kind_Name (K));
   end ${capi.get_name('token_kind_name')};

   procedure ${capi.get_name('token_next')}
     (Token      : ${token_type}_Ptr;
      Next_Token : ${token_type}_Ptr)
   is
      T  : constant Token_Type := Unwrap (Token.all);
      NT : constant Token_Type := Next (T);
   begin
      Next_Token.all := Wrap (NT);
   end ${capi.get_name('token_next')};

   ------------
   -- Unwrap --
   ------------

   function Unwrap
     (Unit : Analysis_Unit_Interface;
      Text : ${text_type})
      return Symbol_Type
   is
      T : Text_Type (1 .. Natural (Text.Length));
      for T'Address use Text.Chars;
   begin
     return Find (Unit.Token_Data.Symbols, T, False);
   end Unwrap;

end ${_self.ada_api_settings.lib_name}.Analysis.C;
