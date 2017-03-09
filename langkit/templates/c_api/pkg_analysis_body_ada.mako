## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="struct_types"  file="struct_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="enum_types"    file="enum_types_ada.mako" />
<%namespace name="exts"          file="../extensions.mako" />

with Ada.Finalization;

pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
use Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");

with System.Memory;
use type System.Address;

with GNATCOLL.Iconv;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Extensions;  use Langkit_Support.Extensions;
with Langkit_Support.Text;        use Langkit_Support.Text;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Lexer;    use ${ada_lib_name}.Lexer;

${exts.include_extension(
   ctx.ext('analysis', 'c_api', 'body_deps')
)}

package body ${ada_lib_name}.Analysis.C is

% if ctx.default_unit_provider:
   type C_Unit_Provider_Type is
      new Ada.Finalization.Controlled
      and Unit_Provider_Interface
   with record
      Data                    : System.Address;
      Destroy_Func            : ${unit_provider_destroy_type};
      Get_Unit_From_Node_Func : ${unit_provider_get_unit_from_node_type};
      Get_Unit_From_Name_Func : ${unit_provider_get_unit_from_name_type};
   end record;

   overriding procedure Finalize (Provider : in out C_Unit_Provider_Type);

   overriding function Get_Unit
     (Provider    : C_Unit_Provider_Type;
      Context     : Analysis_Context;
      Node        : ${root_node_type_name};
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False;
      With_Trivia : Boolean := False) return Analysis_Unit;

   overriding function Get_Unit
     (Provider    : C_Unit_Provider_Type;
      Context     : Analysis_Context;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False;
      With_Trivia : Boolean := False) return Analysis_Unit;
% endif

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
     (Charset            : chars_ptr
      % if ctx.default_unit_provider:
      ; Unit_Provider : ${unit_provider_type}
      % endif
     )
      return ${analysis_context_type}
   is
   begin
      Clear_Last_Exception;

      declare
         C : constant String :=
           (if Charset = Null_Ptr
            then ${string_repr(ctx.default_charset)}
            else Value (Charset));

         % if ctx.default_unit_provider:
         U : Unit_Provider_Access_Cst :=
            Unit_Provider_Access_Cst (Unwrap (Unit_Provider));
         % endif

      begin
         return Wrap (Create (C
            % if ctx.default_unit_provider:
            , U
            % endif
         ));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ${analysis_context_type} (System.Null_Address);
   end;

   function ${capi.get_name('context_incref')}
     (Context : ${analysis_context_type})
      return ${analysis_context_type}
   is
      C : constant Analysis_Context := Unwrap (Context);
   begin
      Inc_Ref (C);
      return Context;
   end;

   procedure ${capi.get_name('context_decref')}
     (Context : ${analysis_context_type})
   is
      C : Analysis_Context := Unwrap (Context);
   begin
      Dec_Ref (C);
   end;

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
   end;

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
   end;

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
   end;

   % if ctx.default_unit_provider:
      function ${capi.get_name("get_analysis_unit_from_provider")}
        (Context     : ${analysis_context_type};
         Name        : ${text_type};
         Kind        : ${unit_kind_type};
         Charset     : chars_ptr;
         Reparse     : int;
         With_Trivia : int)
         return ${analysis_unit_type}
      is
      begin
         Clear_Last_Exception;

         declare
            Text_Name : Text_Type (1 .. Integer (Name.Length))
               with Import  => True,
                    Address => Name.Chars;

            Ctx  : constant Analysis_Context := Unwrap (Context);
            Unit : constant Analysis_Unit := Get_From_Provider
              (Ctx,
               Text_Name,
               Unwrap (Kind),
               Value_Or_Empty (Charset),
               Reparse /= 0,
               With_Trivia /= 0);
         begin
            return Wrap (Unit);
         end;
      exception
         when Invalid_Unit_Name_Error =>
            return ${analysis_unit_type} (System.Null_Address);
         when Exc : others =>
            Set_Last_Exception (Exc);
            return ${analysis_unit_type} (System.Null_Address);
      end;
   % endif

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
   end;

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
   end;

   procedure ${capi.get_name('unit_first_token')}
     (Unit  : ${analysis_unit_type};
      Token : ${token_type}_Ptr)
   is
      U : constant Analysis_Unit := Unwrap (Unit);
      T : constant Token_Type := First_Token (U);
   begin
      Token.all := Wrap (T);
   end;

   procedure ${capi.get_name('unit_last_token')}
     (Unit  : ${analysis_unit_type};
      Token : ${token_type}_Ptr)
   is
      U : constant Analysis_Unit := Unwrap (Unit);
      T : constant Token_Type := Last_Token (U);
   begin
      Token.all := Wrap (T);
   end;

   function ${capi.get_name('unit_token_count')}
     (Unit : ${analysis_unit_type}) return int
   is
      U : constant Analysis_Unit := Unwrap (Unit);
   begin
      return int (Token_Count (U));
   end;

   function ${capi.get_name('unit_trivia_count')}
     (Unit : ${analysis_unit_type}) return int
   is
      U : constant Analysis_Unit := Unwrap (Unit);
   begin
      return int (Trivia_Count (U));
   end;

   function ${capi.get_name('unit_filename')}
     (Unit : ${analysis_unit_type})
      return chars_ptr
   is
      U : constant Analysis_Unit := Unwrap (Unit);
   begin
      return New_String (Get_Filename (U));
   end;

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
   end;

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
               D_In  : Diagnostic renames U.Diagnostics (Natural (N) + 1);
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
   end;

   function ${capi.get_name('node_unit')}
     (Node : ${node_type})
      return ${analysis_unit_type}
   is
   begin
      Clear_Last_Exception;

      declare
         N : constant ${root_node_type_name} := Unwrap (Node);
         U : constant Analysis_Unit := Get_Unit (N);
      begin
         return Wrap (U);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ${analysis_unit_type} (System.Null_Address);
   end;

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
   end;

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
   end;

   function ${capi.get_name('unit_context')}
     (Unit : ${analysis_unit_type})
      return ${analysis_context_type}
   is
      U : constant Analysis_Unit := Unwrap (Unit);
   begin
      return Wrap (U.Context);
   end;

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
   end;

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
   end;

   function ${capi.get_name("unit_populate_lexical_env")}
     (Unit : ${analysis_unit_type})
      return int
   is
   begin
      Clear_Last_Exception;

      declare
         U : constant Analysis_Unit := Unwrap (Unit);
      begin
         Populate_Lexical_Env (U);
      exception
         when Exc : Property_Error =>
            ## If we reach this handler, it means the expression failed at
            ## some point because of a safety check. Tell the user about
            ## it.
            Set_Last_Exception (Exc, Is_Fatal => False);
            return 0;
      end;
      return 1;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   ---------------------------------
   -- General AST node primitives --
   ---------------------------------

   Node_Kind_Names : constant array (${root_node_kind_name}) of Text_Access :=
     (${', '.join('{} => new Text_Type\'(To_Text ("{}"))'.format(
                      cls.ada_kind_name(),
                      cls.name().camel
                  )
                  for cls in ctx.astnode_types
                  if not cls.abstract)});

   function ${capi.get_name("node_kind")} (Node : ${node_type})
      return ${node_kind_type}
   is
   begin
      Clear_Last_Exception;

      declare
         N : constant ${root_node_type_name} := Unwrap (Node);
         K : ${root_node_kind_name} := Kind (N);
      begin
         return ${node_kind_type} (K'Enum_Rep);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ${node_kind_type}'First;
   end;

   function ${capi.get_name("kind_name")} (Kind : ${node_kind_type})
                                           return ${text_type}
   is
   begin
      Clear_Last_Exception;

      declare
         K    : constant ${root_node_kind_name} :=
            ${root_node_kind_name}'Enum_Val (Kind);
         Name : Text_Access renames Node_Kind_Names (K);
      begin
         return (Chars => Name.all'Address, Length => Name'Length,
                 Is_Allocated => 0);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return (System.Null_Address, 0, Is_Allocated => 0);
   end;

   function ${capi.get_name('node_is_ghost')} (Node : ${node_type}) return int
   is
   begin
      Clear_Last_Exception;

      declare
         N : constant ${root_node_type_name} := Unwrap (Node);
      begin
         return Boolean'Pos (N.Is_Ghost);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function ${capi.get_name('node_short_image')} (Node : ${node_type})
                                                  return ${text_type}
   is
   begin
      Clear_Last_Exception;
      declare
         N   : constant ${root_node_type_name} := Unwrap (Node);
         Img : constant Text_Type := N.Short_Image;
      begin
         return Wrap_Alloc (Img);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return (System.Null_Address, 0, 0);
   end;

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
   end;

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
   end;

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
   end;

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
         Get_Child (Nod, Natural (N) + 1, Exists, Result);
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
   end;

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
   end;

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
   end;

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
   end;

   ----------
   -- Wrap --
   ----------

   function Wrap (S : Unbounded_Wide_Wide_String) return ${text_type} is
      Chars  : Big_Wide_Wide_String_Access;
      Length : Natural;
   begin
      Get_Wide_Wide_String (S, Chars, Length);
      return (Chars.all'Address, size_t (Length), 0);
   end Wrap;

   ------------------------
   -- Set_Last_Exception --
   ------------------------

   procedure Set_Last_Exception
     (Exc      : Exception_Occurrence;
      Is_Fatal : Boolean := True)
   is
   begin
      --  If it's the first time, allocate room for the exception information

      if Last_Exception = null then
         Last_Exception := new ${exception_type};

      --  If it is not the first time, free memory allocated for the last
      --  exception.

      elsif Last_Exception.Information /= Null_Ptr then
         Free (Last_Exception.Information);
      end if;

      Last_Exception.Is_Fatal := (if Is_Fatal then 1 else 0);
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
   end;

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
   end;

   procedure ${capi.get_name('token_next')}
     (Token      : ${token_type}_Ptr;
      Next_Token : ${token_type}_Ptr)
   is
      T  : constant Token_Type := Unwrap (Token.all);
      NT : constant Token_Type := Next (T);
   begin
      Next_Token.all := Wrap (NT);
   end;

   function ${capi.get_name('token_is_equivalent')}
     (Left  : ${token_type}_Ptr;
      Right : ${token_type}_Ptr) return ${bool_type}
   is
      L  : constant Token_Type := Unwrap (Left.all);
      R  : constant Token_Type := Unwrap (Right.all);
   begin
      return ${bool_type} (Boolean'Pos (Is_Equivalent (L, R)));
   end;

   procedure ${capi.get_name('token_previous')}
     (Token          : ${token_type}_Ptr;
      Previous_Token : ${token_type}_Ptr)
   is
      T  : constant Token_Type := Unwrap (Token.all);
      PT : constant Token_Type := Previous (T);
   begin
      Previous_Token.all := Wrap (PT);
   end;

   function ${capi.get_name('token_range_text')}
     (First, Last : ${token_type}_Ptr;
      Text        : ${text_type}_Ptr) return int
   is
      FD : constant Token_Data_Type := Data (Unwrap (First.all));
      LD : constant Token_Data_Type := Data (Unwrap (Last.all));
   begin
      if First.Token_Data /= Last.Token_Data then
         return 0;
      end if;
      Text.all := Wrap
        (FD.Source_Buffer,
         Positive (FD.Source_First),
         Natural (LD.Source_Last));
      return 1;
   end;

   ------------
   -- Unwrap --
   ------------

   function Unwrap
     (Unit : Analysis_Unit; Text : ${text_type}) return Symbol_Type
   is
      T : Text_Type (1 .. Natural (Text.Length));
      for T'Address use Text.Chars;
   begin
     return Find (Unit.TDH.Symbols, T, False);
   end Unwrap;

   ----------------
   -- Wrap_Alloc --
   ----------------

   function Wrap_Alloc (S : Text_Type) return ${text_type}
   is
      T : Text_Access := new Text_Type'(S);
   begin
      return ${text_type}'(T.all'Address, T.all'Length, Is_Allocated => 1);
   end Wrap_Alloc;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (S     : Text_Cst_Access;
      First : Positive;
      Last  : Natural) return ${text_type}
   is
      Substring : Text_Type renames S (First .. Last);
   begin
      return (if First > Last
              then (Chars        => System.Null_Address,
                    Length       => 0,
                    Is_Allocated => 0)
              else (Chars        => S (First)'Address,
                    Length       => Substring'Length,
                    Is_Allocated => 0));
   end Wrap;

   procedure ${capi.get_name('destroy_text')} (T : ${text_type}_Ptr) is
      use System;
   begin
      if T.Is_Allocated /= 0 and then T.Chars /= System.Null_Address then
         declare
            TT : Text_Type (1 .. Natural (T.Length));
            for TT'Address use T.Chars;
            TA : Text_Access := TT'Unrestricted_Access;
         begin
            Free (TA);
         end;
         T.Chars := System.Null_Address;
      end if;
   end;

% if ctx.default_unit_provider:
   function ${capi.get_name('create_unit_provider')}
     (Data                    : System.Address;
      Destroy_Func            : ${unit_provider_destroy_type};
      Get_Unit_From_Node_Func : ${unit_provider_get_unit_from_node_type};
      Get_Unit_From_Name_Func : ${unit_provider_get_unit_from_name_type})
      return ${unit_provider_type}
   is
      Result : constant Unit_Provider_Access :=
         new C_Unit_Provider_Type'
           (Ada.Finalization.Controlled with
            Data                    => Data,
            Destroy_Func            => Destroy_Func,
            Get_Unit_From_Node_Func => Get_Unit_From_Node_Func,
            Get_Unit_From_Name_Func => Get_Unit_From_Name_Func);
   begin
      return Wrap (Result);
   end;

   procedure ${capi.get_name('destroy_unit_provider')}
     (Provider : ${unit_provider_type})
   is
      P : Unit_Provider_Access := Unwrap (Provider);
   begin
      Destroy (P);
   end;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Provider : in out C_Unit_Provider_Type)
   is
   begin
      Provider.Destroy_Func (Provider.Data);
   end Finalize;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : C_Unit_Provider_Type;
      Context     : Analysis_Context;
      Node        : ${root_node_type_name};
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False;
      With_Trivia : Boolean := False) return Analysis_Unit
   is
      C_Charset   : chars_ptr := (if Charset'Length = 0
                                  then Null_Ptr
                                  else New_String (Charset));

      C_Result : ${analysis_unit_type} := Provider.Get_Unit_From_Node_Func
        (Provider.Data, Wrap (Context), Wrap (Node), Wrap (Kind), C_Charset,
         Boolean'Pos (Reparse), Boolean'Pos (With_Trivia));
   begin
      Free (C_Charset);
      if C_Result = ${analysis_unit_type} (System.Null_Address) then
         raise Property_Error with "invalid AST node for unit name";
      end if;
      return Unwrap (C_Result);
   end Get_Unit;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : C_Unit_Provider_Type;
      Context     : Analysis_Context;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False;
      With_Trivia : Boolean := False) return Analysis_Unit
   is
      Name_Access : Text_Access := Name'Unrestricted_Access;
      C_Charset   : chars_ptr := (if Charset'Length = 0
                                  then Null_Ptr
                                  else New_String (Charset));

      C_Result    : ${analysis_unit_type} := Provider.Get_Unit_From_Name_Func
        (Provider.Data, Wrap (Context), Wrap (Name_Access), Wrap (Kind),
         C_Charset, Boolean'Pos (Reparse), Boolean'Pos (With_Trivia));
   begin
      Free (C_Charset);
      if C_Result = ${analysis_unit_type} (System.Null_Address) then
         raise Property_Error with "invalid AST node for unit name";
      end if;
      return Unwrap (C_Result);
   end Get_Unit;

   ${exts.include_extension(
      ctx.ext('analysis', 'c_api', 'unit_providers', 'body')
   )}
% endif

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
                 Text         => Wrap
                   (Text_Cst_Access (Token.TDH.Source_Buffer),
                    D.Source_First,
                    D.Source_Last),
                 Sloc_Range   => Wrap (D.Sloc_Range));
      end;
   end Wrap;

   ------------
   -- Unwrap --
   ------------

   function Unwrap (Token : ${token_type}) return Token_Type is
      use System;

      --  The following unchecked conversion makes it possible to restore the
      --  Ada type of token data handler accesses from the C API. All
      --  read/writes for the pointed values are made in Ada through values of
      --  the same access type. Thus, strict aliasing issues should not arise
      --  for these.
      --
      --  See <https://gcc.gnu.org/onlinedocs/gnat_ugn/
      --       Optimization-and-Strict-Aliasing.html>.

      pragma Warnings (Off, "possible aliasing problem for type");
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Token_Data_Handler_Access);
      pragma Warnings (On, "possible aliasing problem for type");

   begin
      return (if Token.Token_Data = Null_Address
              then No_Token
              else (TDH    => Convert (Token.Token_Data),
                    Token  => Token_Index (Token.Token_Index),
                    Trivia => Token_Index (Token.Trivia_Index)));
   end Unwrap;

   ${array_types.body(T.root_node.array_type())}

   ${array_types.body(LexicalEnvType.array_type())}

   ${array_types.body(T.root_node.env_el().array_type())}

   function ${capi.get_name('lexical_env_empty')} return ${lexical_env_type}
   is
   begin
      return Wrap (Empty_Env);
   end;

   function ${capi.get_name('lexical_env_parent')}
     (Env : ${lexical_env_type})
      return ${lexical_env_type}
   is
      E : constant AST_Envs.Lexical_Env := Unwrap (Env);
   begin
      return Wrap (AST_Envs.Get_Env (E.Parent));
   end ${capi.get_name('lexical_env_parent')};

   function ${capi.get_name('lexical_env_node')}
     (Env : ${lexical_env_type})
      return ${node_type}
   is
      E : constant AST_Envs.Lexical_Env := Unwrap (Env);
   begin
      return Wrap (E.Node);
   end ${capi.get_name('lexical_env_node')};

   function ${capi.get_name('lexical_env_get')}
     (Env  : ${lexical_env_type};
      Name : ${text_type})
      return ${T.root_node.env_el().array_type().name()}
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
         U : constant Analysis_Unit := E.Node.Unit;
         N : constant Symbol_Type := Unwrap (U, Name);
      begin
         return Create (if N = null
                        then (1 .. 0 => <>)
                        else AST_Envs.Get (E, N));
      end;
   end ${capi.get_name('lexical_env_get')};

   procedure ${capi.get_name('lexical_env_dec_ref')}
     (Env : ${lexical_env_type})
   is
      E : AST_Envs.Lexical_Env := Unwrap (Env);
   begin
      Dec_Ref (E);
   end ${capi.get_name('lexical_env_dec_ref')};

   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

   % for astnode in ctx.astnode_types:
       % for field in astnode.fields_with_accessors():
           ${astnode_types.accessor_body(field)}
       % endfor
   % endfor

   % for struct_type in ctx.sorted_types(ctx.struct_types):
      ${struct_types.body(struct_type)}
   % endfor

   % for array_type in ctx.sorted_types(ctx.array_types):
      % if array_type.element_type().should_emit_array_type:
         ${array_types.body(array_type)}
      % endif
   % endfor

end ${ada_lib_name}.Analysis.C;
