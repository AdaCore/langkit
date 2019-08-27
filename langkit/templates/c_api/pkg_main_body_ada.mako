## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="struct_types"  file="struct_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="exts"          file="../extensions.mako" />

<% entity_type = root_entity.c_type(capi).name %>

pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
use Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");

with System.Memory;
use type System.Address;

with GNATCOLL.Iconv;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Text;        use Langkit_Support.Text;

with ${ada_lib_name}.Analysis;   use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Converters; use ${ada_lib_name}.Converters;

${exts.with_clauses(with_clauses)}

package body ${ada_lib_name}.Implementation.C is

   --  Avoid hiding from $.Lexer
   subtype Token_Data_Type is Common.Token_Data_Type;

   type C_Unit_Provider is new Unit_Provider_Interface with record
      Data                    : System.Address;
      Destroy_Func            : ${unit_provider_destroy_type};
      Get_Unit_Filename_Func  : ${unit_provider_get_unit_filename_type};
      Get_Unit_From_Name_Func : ${unit_provider_get_unit_from_name_type};
   end record;

   overriding function Get_Unit_Filename
     (Provider : C_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String;

   overriding function Get_Unit
     (Provider : C_Unit_Provider;
      Context  : Analysis_Context'Class;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind;
      Charset  : String := "";
      Reparse  : Boolean := False) return Analysis_Unit'Class;

   overriding procedure Release (Provider : in out C_Unit_Provider);

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
     (Charset       : chars_ptr;
      Unit_Provider : ${unit_provider_type};
      With_Trivia   : int;
      Tab_Stop      : int) return ${analysis_context_type} is
   begin
      Clear_Last_Exception;

      declare
         C : constant String :=
           (if Charset = Null_Ptr
            then ${string_repr(ctx.default_charset)}
            else Value (Charset));

         Provider     : constant Unit_Provider_Reference :=
            (if Unit_Provider = ${unit_provider_type} (System.Null_Address)
             then No_Unit_Provider_Reference
             else Unwrap (Unit_Provider).all);
         Context      : constant Analysis_Context := Create_Context
           (Charset       => C,
            Unit_Provider => Provider,
            With_Trivia   => With_Trivia /= 0,
            Tab_Stop      => Natural (Tab_Stop));
         Internal_Ctx : constant Internal_Context := Unwrap_Context (Context);

      begin
         --  Create a new ownership share for the result since the one Context
         --  owns will disappear once we return.
         Inc_Ref (Internal_Ctx);
         return Internal_Ctx;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   function ${capi.get_name('context_incref')}
     (Context : ${analysis_context_type}) return ${analysis_context_type} is
   begin
      Inc_Ref (Context);
      return Context;
   end;

   procedure ${capi.get_name('context_decref')}
     (Context : ${analysis_context_type})
   is
      Context_Var : Internal_Context := Context;
   begin
      Dec_Ref (Context_Var);
   end;

   function ${capi.get_name('context_symbol')}
     (Context : ${analysis_context_type};
      Text    : access ${text_type};
      Symbol  : access ${symbol_type}) return int
   is
      Raw_Text : Text_Type (1 .. Natural (Text.Length))
         with Import, Address => Text.Chars;
   begin
      Clear_Last_Exception;
      Symbol.all := Wrap_Symbol (Lookup_Symbol (Context, Raw_Text));
      return 1;
   exception
      when Invalid_Symbol_Error =>
         return 0;
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   procedure ${capi.get_name("context_discard_errors_in_populate_lexical_env")}
     (Context : ${analysis_context_type};
      Discard : int) is
   begin
      Clear_Last_Exception;
      Discard_Errors_In_Populate_Lexical_Env (Context, Discard /= 0);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function ${capi.get_name("get_analysis_unit_from_file")}
     (Context           : ${analysis_context_type};
      Filename, Charset : chars_ptr;
      Reparse           : int;
      Rule              : ${grammar_rule_type}) return ${analysis_unit_type} is
   begin
      Clear_Last_Exception;

      return Get_From_File
        (Context,
         Value (Filename),
         Value_Or_Empty (Charset),
         Reparse /= 0,
         Rule);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   function ${capi.get_name("get_analysis_unit_from_buffer")}
     (Context           : ${analysis_context_type};
      Filename, Charset : chars_ptr;
      Buffer            : chars_ptr;
      Buffer_Size       : size_t;
      Rule              : ${grammar_rule_type}) return ${analysis_unit_type} is
   begin
      Clear_Last_Exception;

      declare
         Buffer_Str : String (1 .. Positive (Buffer_Size))
            with Import, Address => Convert (Buffer);
      begin
         return Get_From_Buffer
           (Context,
            Value (Filename),
            Value_Or_Empty (Charset),
            Buffer_Str,
            Rule);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   % if ctx.default_unit_provider:
   function ${capi.get_name("get_analysis_unit_from_provider")}
     (Context : ${analysis_context_type};
      Name    : ${text_type};
      Kind    : ${unit_kind_type};
      Charset : chars_ptr;
      Reparse : int) return ${analysis_unit_type} is
   begin
      Clear_Last_Exception;

      declare
         Text_Name : Text_Type (1 .. Integer (Name.Length))
            with Import, Address => Name.Chars;
      begin
         return Get_From_Provider
           (Context,
            Text_Name,
            Kind,
            Value_Or_Empty (Charset),
            Reparse /= 0);
      end;
   exception
      when Invalid_Unit_Name_Error =>
         return null;
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;
   % endif

   procedure ${capi.get_name('unit_root')}
     (Unit     : ${analysis_unit_type};
      Result_P : ${entity_type}_Ptr) is
   begin
      Clear_Last_Exception;

      Result_P.all := (Unit.AST_Root, ${T.entity_info.nullexpr});
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure ${capi.get_name('unit_first_token')}
     (Unit  : ${analysis_unit_type};
      Token : access ${token_type}) is
   begin
      Clear_Last_Exception;

      declare
         T : constant Token_Reference := First_Token (Unit);
      begin
         Token.all := Wrap (T);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure ${capi.get_name('unit_last_token')}
     (Unit  : ${analysis_unit_type};
      Token : access ${token_type}) is
   begin
      Clear_Last_Exception;

      declare
         T : constant Token_Reference := Last_Token (Unit);
      begin
         Token.all := Wrap (T);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function ${capi.get_name('unit_token_count')}
     (Unit : ${analysis_unit_type}) return int is
   begin
      Clear_Last_Exception;

      return int (Token_Count (Unit));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return -1;
   end;

   function ${capi.get_name('unit_trivia_count')}
     (Unit : ${analysis_unit_type}) return int is
   begin
      Clear_Last_Exception;

      return int (Trivia_Count (Unit));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return -1;
   end;

   procedure ${capi.get_name('unit_lookup_token')}
     (Unit   : ${analysis_unit_type};
      Sloc   : access ${sloc_type};
      Result : access ${token_type}) is
   begin
      Clear_Last_Exception;

      declare
         S   : constant Source_Location := Unwrap (Sloc.all);
         Tok : constant Token_Reference := Lookup_Token (Unit, S);
      begin
         Result.all := Wrap (Tok);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function ${capi.get_name('unit_filename')}
     (Unit : ${analysis_unit_type}) return chars_ptr is
   begin
      Clear_Last_Exception;

      return New_String (Get_Filename (Unit));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return Null_Ptr;
   end;

   function ${capi.get_name("unit_diagnostic_count")}
     (Unit : ${analysis_unit_type}) return unsigned is
   begin
      Clear_Last_Exception;

      return unsigned (Unit.Diagnostics.Length);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function ${capi.get_name("unit_diagnostic")}
     (Unit         : ${analysis_unit_type};
      N            : unsigned;
      Diagnostic_P : access ${diagnostic_type}) return int
   is
   begin
      Clear_Last_Exception;

      if N < unsigned (Unit.Diagnostics.Length) then
         declare
            D_In  : Diagnostic renames Unit.Diagnostics (Natural (N) + 1);
            D_Out : ${diagnostic_type} renames Diagnostic_P.all;
         begin
            D_Out.Sloc_Range := Wrap (D_In.Sloc_Range);
            D_Out.Message := Wrap (D_In.Message);
            return 1;
         end;
      else
         return 0;
      end if;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function ${capi.get_name('unit_context')}
     (Unit : ${analysis_unit_type}) return ${analysis_context_type} is
   begin
      Clear_Last_Exception;
      return Unit.Context;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   procedure ${capi.get_name("unit_reparse_from_file")}
     (Unit : ${analysis_unit_type}; Charset : chars_ptr) is
   begin
      Clear_Last_Exception;

      Reparse (Unit, Value_Or_Empty (Charset));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure ${capi.get_name("unit_reparse_from_buffer")}
     (Unit        : ${analysis_unit_type};
      Charset     : chars_ptr;
      Buffer      : chars_ptr;
      Buffer_Size : size_t) is
   begin
      Clear_Last_Exception;

      declare
         Buffer_Str : String (1 .. Positive (Buffer_Size))
            with Import, Address => Convert (Buffer);
      begin
         Reparse (Unit, Value_Or_Empty (Charset), Buffer_Str);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function ${capi.get_name("unit_populate_lexical_env")}
     (Unit : ${analysis_unit_type}) return int is
   begin
      Clear_Last_Exception;
      Populate_Lexical_Env (Unit);
      return 1;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   ---------------------------------
   -- General AST node primitives --
   ---------------------------------

   Node_Kind_Names : constant array (${T.node_kind}) of Text_Access :=
     (${', '.join('{} => new Text_Type\'(To_Text ("{}"))'.format(
                      cls.ada_kind_name,
                      cls.kwless_raw_name.camel
                  )
                  for cls in ctx.astnode_types
                  if not cls.abstract)});

   function ${capi.get_name('node_kind')}
     (Node : ${entity_type}_Ptr) return ${node_kind_type} is
   begin
      Clear_Last_Exception;

      declare
         K : constant ${T.node_kind} := Node.Node.Kind;
      begin
         return ${node_kind_type} (K'Enum_Rep);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ${node_kind_type}'First;
   end;

   procedure ${capi.get_name('kind_name')}
     (Kind : ${node_kind_type}; Result : access ${text_type}) is
   begin
      Clear_Last_Exception;

      declare
         K    : constant ${T.node_kind} := ${T.node_kind}'Enum_Val (Kind);
         Name : Text_Access renames Node_Kind_Names (K);
      begin
         Result.all := (Chars        => Name.all'Address,
                        Length       => Name'Length,
                        Is_Allocated => 0);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function ${capi.get_name('node_unit')}
     (Node : ${entity_type}_Ptr) return ${analysis_unit_type} is
   begin
      Clear_Last_Exception;
      return Node.Node.Unit;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   function ${capi.get_name('is_token_node')}
     (Node : ${entity_type}_Ptr) return int is
   begin
      Clear_Last_Exception;
      return Boolean'Pos (Is_Token_Node (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function ${capi.get_name('is_synthetic')}
     (Node : ${entity_type}_Ptr) return int is
   begin
      Clear_Last_Exception;
      return Boolean'Pos (Is_Synthetic (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   procedure ${capi.get_name('node_short_image')}
     (Node : ${entity_type}_Ptr; Result : access ${text_type}) is
   begin
      Clear_Last_Exception;
      declare
         Img : constant Text_Type := Short_Text_Image (Node.Node);
      begin
         Result.all := Wrap_Alloc (Img);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure ${capi.get_name('node_text')}
     (Node : ${entity_type}_Ptr;
      Text : access ${text_type}) is
   begin
      Clear_Last_Exception;
      Text.all := Wrap_Alloc (Implementation.Text (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure ${capi.get_name('node_sloc_range')}
     (Node         : ${entity_type}_Ptr;
      Sloc_Range_P : access ${sloc_range_type}) is
   begin
      Clear_Last_Exception;

      Sloc_Range_P.all := Wrap (Sloc_Range (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure ${capi.get_name('lookup_in_node')}
     (Node   : ${entity_type}_Ptr;
      Sloc   : ${sloc_type};
      Result : ${entity_type}_Ptr) is
   begin
      Clear_Last_Exception;

      declare
         S : constant Source_Location := Unwrap (Sloc);
      begin
         Result.all := (Lookup (Node.Node, S), Node.Info);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function ${capi.get_name("node_children_count")}
     (Node : ${entity_type}_Ptr) return unsigned is
   begin
      Clear_Last_Exception;
      return unsigned (Children_Count (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function ${capi.get_name('node_child')}
     (Node    : ${entity_type}_Ptr;
      N       : unsigned;
      Child_P : ${entity_type}_Ptr) return int is
   begin
      Clear_Last_Exception;

      declare
         Result : ${T.root_node.name};
         Exists : Boolean;
      begin
         if N > unsigned (Natural'Last) then
            return 0;
         end if;
         Get_Child (Node.Node, Natural (N) + 1, Exists, Result);
         if Exists then
            Child_P.all := (Result, Node.Info);
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
     (Text : ${text_type}) return System.Address is
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

   procedure Set_Last_Exception (Exc : Exception_Occurrence) is
   begin
      --  If it's the first time, allocate room for the exception information

      if Last_Exception = null then
         Last_Exception := new ${exception_type};

      --  If it is not the first time, free memory allocated for the last
      --  exception.

      elsif Last_Exception.Information /= Null_Ptr then
         Free (Last_Exception.Information);
      end if;

      --  Get the kind corresponding to Exc

      declare
         Id : constant Exception_Id := Exception_Identity (Exc);
      begin
         % for i, (_, exc) in enumerate(ctx.sorted_exception_types):
         ${'elsif' if i > 0 else 'if'} Id = ${exc}'Identity then
            Last_Exception.Kind := ${ctx.exception_kind_name(exc)};
         % endfor
         else
            Last_Exception.Kind := ${
               ctx.exception_kind_name(
                  ctx.exception_types['langkit.native_exception']
               )
            };
         end if;
      end;

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
     (Token      : ${token_type};
      Next_Token : access ${token_type})
   is
   begin
      Clear_Last_Exception;
      declare
         T  : constant Token_Reference := Unwrap (Token);
         NT : constant Token_Reference := Next (T);
      begin
         Next_Token.all := Wrap (NT);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure ${capi.get_name('token_previous')}
     (Token          : ${token_type};
      Previous_Token : access ${token_type})
   is
   begin
      Clear_Last_Exception;
      declare
         T  : constant Token_Reference := Unwrap (Token);
         PT : constant Token_Reference := Previous (T);
      begin
         Previous_Token.all := Wrap (PT);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function ${capi.get_name('token_range_text')}
     (First, Last : ${token_type};
      Text        : access ${text_type}) return int
   is
   begin
      Clear_Last_Exception;
      declare
         FD : constant Token_Data_Type := Data (Unwrap (First));
         LD : constant Token_Data_Type := Data (Unwrap (Last));

         First_Source_Buffer, Last_Source_Buffer : Text_Cst_Access;
         First_Index, Ignored_First              : Positive;
         Last_Index, Ignored_Last                : Natural;
      begin
         Extract_Token_Text
           (FD, First_Source_Buffer, First_Index, Ignored_Last);
         Extract_Token_Text
           (LD, Last_Source_Buffer, Ignored_First, Last_Index);
         if First_Source_Buffer /= Last_Source_Buffer then
            return 0;
         end if;
         Text.all := Wrap (First_Source_Buffer, First_Index, Last_Index);
         return 1;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function ${capi.get_name('token_is_equivalent')}
     (Left  : ${token_type};
      Right : ${token_type}) return ${bool_type}
   is
   begin
      Clear_Last_Exception;
         declare
         L  : constant Token_Reference := Unwrap (Left);
         R  : constant Token_Reference := Unwrap (Right);
      begin
         return ${bool_type} (Boolean'Pos (Is_Equivalent (L, R)));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   procedure ${capi.get_name('entity_image')}
     (Ent : ${entity_type}_Ptr; Result : access ${text_type}) is
   begin
      Clear_Last_Exception;
      declare
         Img : constant Text_Type := Text_Image (Ent.all);
      begin
         Result.all := Wrap_Alloc (Img);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   ----------------
   -- Wrap_Alloc --
   ----------------

   function Wrap_Alloc (S : Text_Type) return ${text_type} is
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

   procedure ${capi.get_name('destroy_text')} (T : access ${text_type}) is
   begin
      Clear_Last_Exception;
      declare
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
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure ${capi.get_name('symbol_text')}
     (Symbol : access ${symbol_type}; Text : access ${text_type}) is
   begin
      Clear_Last_Exception;
      declare
         Sym    : constant Symbol_Type := Unwrap_Symbol (Symbol.all);
         Result : constant Text_Type :=
           (if Sym = null then "" else Image (Sym));
      begin
         Text.all := Wrap_Alloc (Result);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function ${capi.get_name("create_big_integer")}
     (Text : access ${text_type}) return ${big_integer_type} is
   begin
      Clear_Last_Exception;
      declare
         T      : Text_Type (1 .. Natural (Text.Length))
            with Import, Address => Text.Chars;
         Image  : constant String := Langkit_Support.Text.Image (T);
         Result : constant Big_Integer_Type := Create_Big_Integer (Image);
      begin
         return Wrap_Big_Integer (Result);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ${big_integer_type} (System.Null_Address);
   end ${capi.get_name("create_big_integer")};

   procedure ${capi.get_name("big_integer_text")}
     (Bigint : ${big_integer_type}; Text : access ${text_type}) is
   begin
      Clear_Last_Exception;
      declare
         BI    : constant Big_Integer_Type := Unwrap_Big_Integer (Bigint);
         Image : constant String := BI.Value.Image;
      begin
         Text.all := Wrap_Alloc (To_Text (Image));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure ${capi.get_name("big_integer_decref")}
     (Bigint : ${big_integer_type}) is
   begin
      Clear_Last_Exception;
      declare
         BI : Big_Integer_Type := Unwrap_Big_Integer (Bigint);
      begin
         Dec_Ref (BI);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   --------------------------------------
   -- Create_Unit_Provider_C_Reference --
   --------------------------------------

   function Create_Unit_Provider_C_Reference
     (Provider : Unit_Provider_Interface'Class) return ${unit_provider_type}
   is
      Result : constant Unit_Provider_Access :=
         new Unit_Provider_Reference'
           (Create_Unit_Provider_Reference (Provider));
   begin
      return Wrap (Result);
   end Create_Unit_Provider_C_Reference;

   function ${capi.get_name('create_unit_provider')}
     (Data                    : System.Address;
      Destroy_Func            : ${unit_provider_destroy_type};
      Get_Unit_Filename_Func  : ${unit_provider_get_unit_filename_type};
      Get_Unit_From_Name_Func : ${unit_provider_get_unit_from_name_type})
      return ${unit_provider_type} is
   begin
      Clear_Last_Exception;
      return Create_Unit_Provider_C_Reference
        (C_Unit_Provider'
           (Data                    => Data,
            Destroy_Func            => Destroy_Func,
            Get_Unit_Filename_Func  => Get_Unit_Filename_Func,
            Get_Unit_From_Name_Func => Get_Unit_From_Name_Func));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return ${unit_provider_type} (System.Null_Address);
   end;

   procedure ${capi.get_name('destroy_unit_provider')}
     (Provider : ${unit_provider_type}) is
   begin
      Clear_Last_Exception;
      declare
         P : Unit_Provider_Access := Unwrap (Provider);
      begin
         Free (P);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : C_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String
   is
      Name_Access : constant Text_Cst_Access := Name'Unrestricted_Access;

      C_Result : chars_ptr := Provider.Get_Unit_Filename_Func
        (Provider.Data, Wrap (Name_Access), Kind);
   begin
      if C_Result = Null_Ptr then
         raise Property_Error with "invalid AST node for unit name";
      else
         declare
            Result : constant String := Value (C_Result);
         begin
            Free (C_Result);
            return Result;
         end;
      end if;
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider : C_Unit_Provider;
      Context  : Analysis_Context'Class;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind;
      Charset  : String := "";
      Reparse  : Boolean := False) return Analysis_Unit'Class
   is
      Ctx         : constant ${analysis_context_type} :=
         Unwrap_Context (Context);
      Name_Access : constant Text_Cst_Access := Name'Unrestricted_Access;
      C_Charset   : chars_ptr := (if Charset'Length = 0
                                  then Null_Ptr
                                  else New_String (Charset));

      C_Result : ${analysis_unit_type} := Provider.Get_Unit_From_Name_Func
        (Provider.Data, Ctx, Wrap (Name_Access), Kind,
         C_Charset, Boolean'Pos (Reparse));
   begin
      Free (C_Charset);
      if C_Result = null then
         raise Property_Error with "invalid AST node for unit name";
      end if;
      return Wrap_Unit (C_Result);
   end Get_Unit;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Provider : in out C_Unit_Provider) is
   begin
      Provider.Destroy_Func (Provider.Data);
   end Release;

   ${exts.include_extension(
      ctx.ext('analysis', 'c_api', 'unit_providers', 'body')
   )}

   ----------
   -- Wrap --
   ----------

   function Wrap (Token : Token_Reference) return ${token_type} is
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
         K : constant Token_Kind := Kind (D);

         Index : constant Token_Or_Trivia_Index := Get_Token_Index (Token);

         Source_Buffer : Text_Cst_Access;
         First         : Positive;
         Last          : Natural;
      begin
         Extract_Token_Text (D, Source_Buffer, First, Last);
         return (Token_Data   => Convert (Get_Token_TDH (Token)),
                 Token_Index  => int (Index.Token),
                 Trivia_Index => int (Index.Trivia),
                 Kind         => K'Enum_Rep,
                 Text         => Wrap (Source_Buffer, First, Last),
                 Sloc_Range   => Wrap (Sloc_Range (D)));
      end;
   end Wrap;

   ------------
   -- Unwrap --
   ------------

   function Unwrap (Token : ${token_type}) return Token_Reference is
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
              else Wrap_Token_Reference
                     (Convert (Token.Token_Data),
                      (Token  => Token_Index (Token.Token_Index),
                       Trivia => Token_Index (Token.Trivia_Index))));
   end Unwrap;

   ${array_types.body(T.root_node.array)}

   ${array_types.body(T.root_node.entity.array)}

   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

   % for astnode in ctx.astnode_types:
       % for field in astnode.fields_with_accessors():
           ${astnode_types.accessor_body(field)}
       % endfor
   % endfor

   % for struct_type in ctx.struct_types:
      % if struct_type.exposed and struct_type.emit_c_type:
         ${struct_types.body(struct_type)}
      % endif
   % endfor

   % for array_type in ctx.array_types:
      % if array_type.element_type.should_emit_array_type and \
            array_type.exposed and \
            array_type.emit_c_type:
         ${array_types.body(array_type)}
      % endif
   % endfor

end ${ada_lib_name}.Implementation.C;
