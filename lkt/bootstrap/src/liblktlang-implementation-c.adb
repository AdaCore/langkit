








with Ada.Exceptions.Traceback;
with Ada.Finalization;
pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
use Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");

with System.Memory;
use type System.Address;

with GNAT.Traceback.Symbolic;

with GNATCOLL.Iconv;

with Liblktlang_Support.Diagnostics; use Liblktlang_Support.Diagnostics;
with Liblktlang_Support.Text;        use Liblktlang_Support.Text;

with Liblktlang.Private_Converters;
use Liblktlang.Private_Converters;


          with Liblktlang_Support.Errors;
          with Liblktlang.Implementation.Extensions;
            use Liblktlang.Implementation.Extensions;


package body Liblktlang.Implementation.C is

   --  Avoid hiding from $.Lexer
   subtype Token_Data_Type is Common.Token_Data_Type;

   --------------------
   -- Event handlers --
   --------------------

   type C_Event_Handler is limited new
      Ada.Finalization.Limited_Controlled
      and Internal_Event_Handler
   with record
      Ref_Count           : Natural;
      Data                : System.Address;
      Destroy_Func        : lkt_event_handler_destroy_callback;
      Unit_Requested_Func : lkt_event_handler_unit_requested_callback;
      Unit_Parsed_Func    : lkt_event_handler_unit_parsed_callback;
   end record;

   overriding procedure Finalize (Self : in out C_Event_Handler);
   overriding procedure Inc_Ref (Self : in out C_Event_Handler);
   overriding function Dec_Ref (Self : in out C_Event_Handler) return Boolean;

   overriding procedure Unit_Requested_Callback
     (Self               : in out C_Event_Handler;
      Context            : Internal_Context;
      Name               : Text_Type;
      From               : Internal_Unit;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean);

   overriding procedure Unit_Parsed_Callback
     (Self     : in out C_Event_Handler;
      Context  : Internal_Context;
      Unit     : Internal_Unit;
      Reparsed : Boolean);

   ------------------
   -- File readers --
   ------------------

   type C_File_Reader is limited new
      Ada.Finalization.Limited_Controlled
      and Internal_File_Reader
   with record
      Ref_Count    : Natural;
      Data         : System.Address;
      Destroy_Func : lkt_file_reader_destroy_callback;
      Read_Func    : lkt_file_reader_read_callback;
   end record;

   type C_File_Reader_Access is access all C_File_Reader;

   overriding procedure Finalize (Self : in out C_File_Reader);
   overriding procedure Inc_Ref (Self : in out C_File_Reader);
   overriding function Dec_Ref (Self : in out C_File_Reader) return Boolean;
   overriding procedure Read
     (Self        : C_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);

   function Value_Or_Empty (S : chars_ptr) return String
   --  If S is null, return an empty string. Return Value (S) otherwise.
   is (if S = Null_Ptr
       then ""
       else Value (S));

   Last_Stack_Trace : lkt_stack_trace := null;
   Last_Exception   : lkt_exception_Ptr := null;

   -----------------------------
   -- UTF transcoding helpers --
   -----------------------------

   type U32_Array is array (size_t range <>) of Unsigned_32;
   type U8_Array is array (size_t range <>) of Unsigned_8;
   function "+" is new Ada.Unchecked_Conversion (System.Address, chars_ptr);
      function "+" is new Ada.Unchecked_Conversion (chars_ptr, System.Address);

   function Codepoint_UTF8_Size (Codepoint : Unsigned_32) return size_t
   is (case Codepoint is
       when 0 .. 16#7f#                => 1,
       when 16#80# .. 16#07ff#         => 2,
       when 16#0800# .. 16#ffff#       => 3,
       when 16#01_0000# .. 16#10_ffff# => 4,
       when others                     => raise Program_Error);
   --  Return the number of bytes necessary to encode the given codepoint in
   --  UTF8.

   procedure UTF32_To_UTF8
     (UTF32 : U32_Array; Bytes : out chars_ptr; Length : out size_t);
   --  Allocate an UTF-8 buffer and transcode the given UTF-32 buffer into it.
   --  But the allocated buffer in ``Bytes`` and its size in ``Length``.

   function UTF8_Codepoints_Count (UTF8 : U8_Array) return Natural;
   --  Return the number of codepoints in the given UTF-8 buffer

   procedure UTF8_To_UTF32 (UTF8 : U8_Array; UTF32 : out U32_Array);
   --  Assuming that UTF8 contains UTF32'Size codepoints, transcode UTF8 into
   --  UTF32.

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

   function lkt_allocate_analysis_context
     return lkt_analysis_context is
   begin
      Clear_Last_Exception;
      begin
         return Allocate_Context;
      exception
         when Exc : others =>
            Set_Last_Exception (Exc);
            return null;
      end;
   end;

   procedure lkt_initialize_analysis_context
     (Context       : lkt_analysis_context;
      Charset       : chars_ptr;
      File_Reader   : lkt_file_reader;
      Unit_Provider : lkt_unit_provider;
      Event_Handler : lkt_event_handler;
      With_Trivia   : int;
      Tab_Stop      : int) is
   begin
      Clear_Last_Exception;

      declare
         C : constant String :=
           (if Charset = Null_Ptr
            then "utf-8"
            else Value (Charset));
      begin
         Initialize_Context
            (Context       => Context,
             Charset       => C,
             File_Reader   => Unwrap_Private_File_Reader (File_Reader),
             Unit_Provider => Unwrap_Private_Provider (Unit_Provider),
             Event_Handler => Unwrap_Private_Event_Handler (Event_Handler),
             With_Trivia   => With_Trivia /= 0,
             Tab_Stop      => Natural (Tab_Stop));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function lkt_context_incref
     (Context : lkt_analysis_context) return lkt_analysis_context is
   begin
      Clear_Last_Exception;
      Inc_Ref (Context);
      return Context;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   procedure lkt_context_decref
     (Context : lkt_analysis_context)
   is
      Context_Var : Internal_Context := Context;
   begin
      Clear_Last_Exception;
      Dec_Ref (Context_Var);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function lkt_context_symbol
     (Context : lkt_analysis_context;
      Text    : access lkt_text;
      Symbol  : access lkt_symbol_type) return int
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

   procedure lkt_context_discard_errors_in_populate_lexical_env
     (Context : lkt_analysis_context;
      Discard : int) is
   begin
      Clear_Last_Exception;
      Discard_Errors_In_Populate_Lexical_Env (Context, Discard /= 0);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function lkt_get_analysis_unit_from_file
     (Context           : lkt_analysis_context;
      Filename, Charset : chars_ptr;
      Reparse           : int;
      Rule              : lkt_grammar_rule) return lkt_analysis_unit is
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

   function lkt_get_analysis_unit_from_buffer
     (Context           : lkt_analysis_context;
      Filename, Charset : chars_ptr;
      Buffer            : chars_ptr;
      Buffer_Size       : size_t;
      Rule              : lkt_grammar_rule) return lkt_analysis_unit is
   begin
      Clear_Last_Exception;

      declare
         Buffer_Str : String (1 .. Natural (Buffer_Size))
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

   function lkt_get_analysis_unit_from_provider
     (Context : lkt_analysis_context;
      Name    : lkt_text;
      Kind    : lkt_analysis_unit_kind;
      Charset : chars_ptr;
      Reparse : int) return lkt_analysis_unit is
   begin
      Clear_Last_Exception;

      declare
         Text_Name : Text_Type (1 .. Natural (Name.Length))
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

   procedure lkt_unit_root
     (Unit     : lkt_analysis_unit;
      Result_P : lkt_node_Ptr) is
   begin
      Clear_Last_Exception;

      Result_P.all := (Unit.Ast_Root, No_Entity_Info);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure lkt_unit_first_token
     (Unit  : lkt_analysis_unit;
      Token : access lkt_token) is
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

   procedure lkt_unit_last_token
     (Unit  : lkt_analysis_unit;
      Token : access lkt_token) is
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

   function lkt_unit_token_count
     (Unit : lkt_analysis_unit) return int is
   begin
      Clear_Last_Exception;

      return int (Token_Count (Unit));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return -1;
   end;

   function lkt_unit_trivia_count
     (Unit : lkt_analysis_unit) return int is
   begin
      Clear_Last_Exception;

      return int (Trivia_Count (Unit));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return -1;
   end;

   procedure lkt_unit_lookup_token
     (Unit   : lkt_analysis_unit;
      Sloc   : access lkt_source_location;
      Result : access lkt_token) is
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

   procedure lkt_unit_dump_lexical_env
     (Unit : lkt_analysis_unit) is
   begin
      Clear_Last_Exception;
      Dump_Lexical_Env (Unit);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function lkt_unit_filename
     (Unit : lkt_analysis_unit) return chars_ptr is
   begin
      Clear_Last_Exception;

      return New_String (Get_Filename (Unit));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return Null_Ptr;
   end;

   function lkt_unit_diagnostic_count
     (Unit : lkt_analysis_unit) return unsigned is
   begin
      Clear_Last_Exception;

      return unsigned (Unit.Diagnostics.Length);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function lkt_unit_diagnostic
     (Unit         : lkt_analysis_unit;
      N            : unsigned;
      Diagnostic_P : access lkt_diagnostic) return int
   is
   begin
      Clear_Last_Exception;

      if N < unsigned (Unit.Diagnostics.Length) then
         declare
            D_In  : Diagnostic renames Unit.Diagnostics (Natural (N) + 1);
            D_Out : lkt_diagnostic renames Diagnostic_P.all;
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

   function lkt_unit_context
     (Unit : lkt_analysis_unit) return lkt_analysis_context is
   begin
      Clear_Last_Exception;
      return Unit.Context;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   procedure lkt_unit_reparse_from_file
     (Unit : lkt_analysis_unit; Charset : chars_ptr) is
   begin
      Clear_Last_Exception;

      Reparse (Unit, Value_Or_Empty (Charset));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure lkt_unit_reparse_from_buffer
     (Unit        : lkt_analysis_unit;
      Charset     : chars_ptr;
      Buffer      : chars_ptr;
      Buffer_Size : size_t) is
   begin
      Clear_Last_Exception;

      declare
         Buffer_Str : String (1 .. Natural (Buffer_Size))
            with Import, Address => Convert (Buffer);
      begin
         Reparse (Unit, Value_Or_Empty (Charset), Buffer_Str);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function lkt_unit_populate_lexical_env
     (Unit : lkt_analysis_unit
   ) return int is
   begin
      Clear_Last_Exception;
      Populate_Lexical_Env
        (Unit, 1);
      return 1;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   ---------------------------------
   -- General AST node primitives --
   ---------------------------------

   Node_Kind_Names : constant array (Lkt_Node_Kind_Type) of Text_Access :=
     (Lkt_Argument => new Text_Type'(To_Text ("Argument")),
      Lkt_Lexer_Case_Rule_Cond_Alt => new Text_Type'(To_Text ("LexerCaseRuleCondAlt")),
      Lkt_Lexer_Case_Rule_Default_Alt => new Text_Type'(To_Text ("LexerCaseRuleDefaultAlt")),
      Lkt_Block_String_Line => new Text_Type'(To_Text ("BlockStringLine")),
      Lkt_Class_Qualifier_Absent => new Text_Type'(To_Text ("ClassQualifierAbsent")),
      Lkt_Class_Qualifier_Present => new Text_Type'(To_Text ("ClassQualifierPresent")),
      Lkt_Grammar_Rule_Decl => new Text_Type'(To_Text ("GrammarRuleDecl")),
      Lkt_Synthetic_Lexer_Decl => new Text_Type'(To_Text ("SyntheticLexerDecl")),
      Lkt_Node_Decl => new Text_Type'(To_Text ("NodeDecl")),
      Lkt_Self_Decl => new Text_Type'(To_Text ("SelfDecl")),
      Lkt_Enum_Lit_Decl => new Text_Type'(To_Text ("EnumLitDecl")),
      Lkt_Field_Decl => new Text_Type'(To_Text ("FieldDecl")),
      Lkt_Fun_Param_Decl => new Text_Type'(To_Text ("FunParamDecl")),
      Lkt_Lambda_Param_Decl => new Text_Type'(To_Text ("LambdaParamDecl")),
      Lkt_Dyn_Var_Decl => new Text_Type'(To_Text ("DynVarDecl")),
      Lkt_Match_Val_Decl => new Text_Type'(To_Text ("MatchValDecl")),
      Lkt_Val_Decl => new Text_Type'(To_Text ("ValDecl")),
      Lkt_Fun_Decl => new Text_Type'(To_Text ("FunDecl")),
      Lkt_Env_Spec_Decl => new Text_Type'(To_Text ("EnvSpecDecl")),
      Lkt_Generic_Decl => new Text_Type'(To_Text ("GenericDecl")),
      Lkt_Grammar_Decl => new Text_Type'(To_Text ("GrammarDecl")),
      Lkt_Lexer_Decl => new Text_Type'(To_Text ("LexerDecl")),
      Lkt_Lexer_Family_Decl => new Text_Type'(To_Text ("LexerFamilyDecl")),
      Lkt_Synth_Fun_Decl => new Text_Type'(To_Text ("SynthFunDecl")),
      Lkt_Synth_Param_Decl => new Text_Type'(To_Text ("SynthParamDecl")),
      Lkt_Any_Type_Decl => new Text_Type'(To_Text ("AnyTypeDecl")),
      Lkt_Enum_Class_Alt_Decl => new Text_Type'(To_Text ("EnumClassAltDecl")),
      Lkt_Function_Type => new Text_Type'(To_Text ("FunctionType")),
      Lkt_Generic_Param_Type_Decl => new Text_Type'(To_Text ("GenericParamTypeDecl")),
      Lkt_Class_Decl => new Text_Type'(To_Text ("ClassDecl")),
      Lkt_Enum_Class_Decl => new Text_Type'(To_Text ("EnumClassDecl")),
      Lkt_Enum_Type_Decl => new Text_Type'(To_Text ("EnumTypeDecl")),
      Lkt_Struct_Decl => new Text_Type'(To_Text ("StructDecl")),
      Lkt_Trait_Decl => new Text_Type'(To_Text ("TraitDecl")),
      Lkt_Decl_Annotation => new Text_Type'(To_Text ("DeclAnnotation")),
      Lkt_Decl_Annotation_Args => new Text_Type'(To_Text ("DeclAnnotationArgs")),
      Lkt_Dyn_Env_Wrapper => new Text_Type'(To_Text ("DynEnvWrapper")),
      Lkt_Elsif_Branch => new Text_Type'(To_Text ("ElsifBranch")),
      Lkt_Enum_Class_Case => new Text_Type'(To_Text ("EnumClassCase")),
      Lkt_Excludes_Null_Absent => new Text_Type'(To_Text ("ExcludesNullAbsent")),
      Lkt_Excludes_Null_Present => new Text_Type'(To_Text ("ExcludesNullPresent")),
      Lkt_Any_Of => new Text_Type'(To_Text ("AnyOf")),
      Lkt_Array_Literal => new Text_Type'(To_Text ("ArrayLiteral")),
      Lkt_Call_Expr => new Text_Type'(To_Text ("CallExpr")),
      Lkt_Logic_Predicate => new Text_Type'(To_Text ("LogicPredicate")),
      Lkt_Logic_Propagate_Call => new Text_Type'(To_Text ("LogicPropagateCall")),
      Lkt_Bin_Op => new Text_Type'(To_Text ("BinOp")),
      Lkt_Block_Expr => new Text_Type'(To_Text ("BlockExpr")),
      Lkt_Cast_Expr => new Text_Type'(To_Text ("CastExpr")),
      Lkt_Dot_Expr => new Text_Type'(To_Text ("DotExpr")),
      Lkt_Error_On_Null => new Text_Type'(To_Text ("ErrorOnNull")),
      Lkt_Generic_Instantiation => new Text_Type'(To_Text ("GenericInstantiation")),
      Lkt_Grammar_Cut => new Text_Type'(To_Text ("GrammarCut")),
      Lkt_Grammar_Discard => new Text_Type'(To_Text ("GrammarDiscard")),
      Lkt_Grammar_Dont_Skip => new Text_Type'(To_Text ("GrammarDontSkip")),
      Lkt_Grammar_List => new Text_Type'(To_Text ("GrammarList")),
      Lkt_Grammar_Null => new Text_Type'(To_Text ("GrammarNull")),
      Lkt_Grammar_Opt => new Text_Type'(To_Text ("GrammarOpt")),
      Lkt_Grammar_Opt_Error => new Text_Type'(To_Text ("GrammarOptError")),
      Lkt_Grammar_Opt_Error_Group => new Text_Type'(To_Text ("GrammarOptErrorGroup")),
      Lkt_Grammar_Opt_Group => new Text_Type'(To_Text ("GrammarOptGroup")),
      Lkt_Grammar_Or_Expr => new Text_Type'(To_Text ("GrammarOrExpr")),
      Lkt_Grammar_Pick => new Text_Type'(To_Text ("GrammarPick")),
      Lkt_Grammar_Implicit_Pick => new Text_Type'(To_Text ("GrammarImplicitPick")),
      Lkt_Grammar_Predicate => new Text_Type'(To_Text ("GrammarPredicate")),
      Lkt_Grammar_Rule_Ref => new Text_Type'(To_Text ("GrammarRuleRef")),
      Lkt_Grammar_Skip => new Text_Type'(To_Text ("GrammarSkip")),
      Lkt_Grammar_Stop_Cut => new Text_Type'(To_Text ("GrammarStopCut")),
      Lkt_Parse_Node_Expr => new Text_Type'(To_Text ("ParseNodeExpr")),
      Lkt_Token_Lit => new Text_Type'(To_Text ("TokenLit")),
      Lkt_Token_No_Case_Lit => new Text_Type'(To_Text ("TokenNoCaseLit")),
      Lkt_Token_Pattern_Concat => new Text_Type'(To_Text ("TokenPatternConcat")),
      Lkt_Token_Pattern_Lit => new Text_Type'(To_Text ("TokenPatternLit")),
      Lkt_Token_Ref => new Text_Type'(To_Text ("TokenRef")),
      Lkt_Id => new Text_Type'(To_Text ("Id")),
      Lkt_Def_Id => new Text_Type'(To_Text ("DefId")),
      Lkt_Module_Ref_Id => new Text_Type'(To_Text ("ModuleRefId")),
      Lkt_Ref_Id => new Text_Type'(To_Text ("RefId")),
      Lkt_If_Expr => new Text_Type'(To_Text ("IfExpr")),
      Lkt_Isa => new Text_Type'(To_Text ("Isa")),
      Lkt_Keep_Expr => new Text_Type'(To_Text ("KeepExpr")),
      Lkt_Lambda_Expr => new Text_Type'(To_Text ("LambdaExpr")),
      Lkt_Big_Num_Lit => new Text_Type'(To_Text ("BigNumLit")),
      Lkt_Char_Lit => new Text_Type'(To_Text ("CharLit")),
      Lkt_Null_Lit => new Text_Type'(To_Text ("NullLit")),
      Lkt_Num_Lit => new Text_Type'(To_Text ("NumLit")),
      Lkt_Block_String_Lit => new Text_Type'(To_Text ("BlockStringLit")),
      Lkt_Single_Line_String_Lit => new Text_Type'(To_Text ("SingleLineStringLit")),
      Lkt_Pattern_Single_Line_String_Lit => new Text_Type'(To_Text ("PatternSingleLineStringLit")),
      Lkt_Logic_Assign => new Text_Type'(To_Text ("LogicAssign")),
      Lkt_Logic_Expr => new Text_Type'(To_Text ("LogicExpr")),
      Lkt_Logic_Propagate => new Text_Type'(To_Text ("LogicPropagate")),
      Lkt_Logic_Unify => new Text_Type'(To_Text ("LogicUnify")),
      Lkt_Match_Expr => new Text_Type'(To_Text ("MatchExpr")),
      Lkt_Not_Expr => new Text_Type'(To_Text ("NotExpr")),
      Lkt_Paren_Expr => new Text_Type'(To_Text ("ParenExpr")),
      Lkt_Raise_Expr => new Text_Type'(To_Text ("RaiseExpr")),
      Lkt_Subscript_Expr => new Text_Type'(To_Text ("SubscriptExpr")),
      Lkt_Try_Expr => new Text_Type'(To_Text ("TryExpr")),
      Lkt_Un_Op => new Text_Type'(To_Text ("UnOp")),
      Lkt_Full_Decl => new Text_Type'(To_Text ("FullDecl")),
      Lkt_Grammar_List_Sep => new Text_Type'(To_Text ("GrammarListSep")),
      Lkt_Import => new Text_Type'(To_Text ("Import")),
      Lkt_Langkit_Root => new Text_Type'(To_Text ("LangkitRoot")),
      Lkt_Lexer_Case_Rule => new Text_Type'(To_Text ("LexerCaseRule")),
      Lkt_Lexer_Case_Rule_Send => new Text_Type'(To_Text ("LexerCaseRuleSend")),
      Lkt_List_Kind_One => new Text_Type'(To_Text ("ListKindOne")),
      Lkt_List_Kind_Zero => new Text_Type'(To_Text ("ListKindZero")),
      Lkt_Argument_List => new Text_Type'(To_Text ("ArgumentList")),
      Lkt_Base_Lexer_Case_Rule_Alt_List => new Text_Type'(To_Text ("BaseLexerCaseRuleAltList")),
      Lkt_Block_String_Line_List => new Text_Type'(To_Text ("BlockStringLineList")),
      Lkt_Call_Expr_List => new Text_Type'(To_Text ("CallExprList")),
      Lkt_Decl_Annotation_List => new Text_Type'(To_Text ("DeclAnnotationList")),
      Lkt_Elsif_Branch_List => new Text_Type'(To_Text ("ElsifBranchList")),
      Lkt_Enum_Class_Alt_Decl_List => new Text_Type'(To_Text ("EnumClassAltDeclList")),
      Lkt_Enum_Class_Case_List => new Text_Type'(To_Text ("EnumClassCaseList")),
      Lkt_Enum_Lit_Decl_List => new Text_Type'(To_Text ("EnumLitDeclList")),
      Lkt_Expr_List => new Text_Type'(To_Text ("ExprList")),
      Lkt_Any_Of_List => new Text_Type'(To_Text ("AnyOfList")),
      Lkt_Full_Decl_List => new Text_Type'(To_Text ("FullDeclList")),
      Lkt_Decl_Block => new Text_Type'(To_Text ("DeclBlock")),
      Lkt_Generic_Param_Decl_List => new Text_Type'(To_Text ("GenericParamDeclList")),
      Lkt_Fun_Param_Decl_List => new Text_Type'(To_Text ("FunParamDeclList")),
      Lkt_Grammar_Expr_List => new Text_Type'(To_Text ("GrammarExprList")),
      Lkt_Grammar_Expr_List_List => new Text_Type'(To_Text ("GrammarExprListList")),
      Lkt_Import_List => new Text_Type'(To_Text ("ImportList")),
      Lkt_Lambda_Param_Decl_List => new Text_Type'(To_Text ("LambdaParamDeclList")),
      Lkt_Lkt_Node_List => new Text_Type'(To_Text ("LktNodeList")),
      Lkt_Block_Decl_List => new Text_Type'(To_Text ("BlockDeclList")),
      Lkt_Match_Branch_List => new Text_Type'(To_Text ("MatchBranchList")),
      Lkt_Ref_Id_List => new Text_Type'(To_Text ("RefIdList")),
      Lkt_Type_Ref_List => new Text_Type'(To_Text ("TypeRefList")),
      Lkt_Isa_List => new Text_Type'(To_Text ("IsaList")),
      Lkt_Match_Branch => new Text_Type'(To_Text ("MatchBranch")),
      Lkt_Null_Cond_Qualifier_Absent => new Text_Type'(To_Text ("NullCondQualifierAbsent")),
      Lkt_Null_Cond_Qualifier_Present => new Text_Type'(To_Text ("NullCondQualifierPresent")),
      Lkt_Op_Amp => new Text_Type'(To_Text ("OpAmp")),
      Lkt_Op_And => new Text_Type'(To_Text ("OpAnd")),
      Lkt_Op_Div => new Text_Type'(To_Text ("OpDiv")),
      Lkt_Op_Eq => new Text_Type'(To_Text ("OpEq")),
      Lkt_Op_Gt => new Text_Type'(To_Text ("OpGt")),
      Lkt_Op_Gte => new Text_Type'(To_Text ("OpGte")),
      Lkt_Op_Logic_And => new Text_Type'(To_Text ("OpLogicAnd")),
      Lkt_Op_Logic_Or => new Text_Type'(To_Text ("OpLogicOr")),
      Lkt_Op_Lt => new Text_Type'(To_Text ("OpLt")),
      Lkt_Op_Lte => new Text_Type'(To_Text ("OpLte")),
      Lkt_Op_Minus => new Text_Type'(To_Text ("OpMinus")),
      Lkt_Op_Mult => new Text_Type'(To_Text ("OpMult")),
      Lkt_Op_Ne => new Text_Type'(To_Text ("OpNe")),
      Lkt_Op_Or => new Text_Type'(To_Text ("OpOr")),
      Lkt_Op_Or_Int => new Text_Type'(To_Text ("OpOrInt")),
      Lkt_Op_Plus => new Text_Type'(To_Text ("OpPlus")),
      Lkt_Default_List_Type_Ref => new Text_Type'(To_Text ("DefaultListTypeRef")),
      Lkt_Function_Type_Ref => new Text_Type'(To_Text ("FunctionTypeRef")),
      Lkt_Generic_Type_Ref => new Text_Type'(To_Text ("GenericTypeRef")),
      Lkt_Simple_Type_Ref => new Text_Type'(To_Text ("SimpleTypeRef")),
      Lkt_Var_Bind => new Text_Type'(To_Text ("VarBind")));

   function lkt_node_kind
     (Node : lkt_node_Ptr) return lkt_node_kind_enum is
   begin
      Clear_Last_Exception;

      declare
         K : constant Lkt_Node_Kind_Type := Node.Node.Kind;
      begin
         return lkt_node_kind_enum (K'Enum_Rep);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return lkt_node_kind_enum'First;
   end;

   procedure lkt_kind_name
     (Kind : lkt_node_kind_enum; Result : access lkt_text) is
   begin
      Clear_Last_Exception;

      declare
         K    : constant Lkt_Node_Kind_Type := Lkt_Node_Kind_Type'Enum_Val (Kind);
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

   function lkt_node_unit
     (Node : lkt_node_Ptr) return lkt_analysis_unit is
   begin
      Clear_Last_Exception;
      return Node.Node.Unit;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   procedure lkt_create_bare_entity
     (Node   : lkt_base_node;
      Entity : access lkt_node)
   is
   begin
      Clear_Last_Exception;
      Entity.all := (Node => Unwrap (Node), Info => No_Entity_Info);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function lkt_is_equivalent
     (L, R : lkt_node_Ptr) return lkt_bool
   is
   begin
      Clear_Last_Exception;
      return lkt_bool (Boolean'Pos (Compare_Entity (L.all, R.all)));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function lkt_hash
     (Node : lkt_node_Ptr) return uint32_t
   is
   begin
      Clear_Last_Exception;
      return uint32_t (Hash_Entity (Node.all));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function lkt_is_token_node
     (Node : lkt_node_Ptr) return int is
   begin
      Clear_Last_Exception;
      return Boolean'Pos (Is_Token_Node (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function lkt_is_synthetic
     (Node : lkt_node_Ptr) return int is
   begin
      Clear_Last_Exception;
      return Boolean'Pos (Is_Synthetic (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   procedure lkt_node_image
     (Node : lkt_node_Ptr; Result : access lkt_text) is
   begin
      Clear_Last_Exception;
      declare
         Img : constant Text_Type := Text_Image (Node.all);
      begin
         Result.all := Wrap_Alloc (Img);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure lkt_node_text
     (Node : lkt_node_Ptr;
      Text : access lkt_text) is
   begin
      Clear_Last_Exception;
      Text.all := Wrap_Alloc (Implementation.Text (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure lkt_node_sloc_range
     (Node         : lkt_node_Ptr;
      Sloc_Range_P : access lkt_source_location_range) is
   begin
      Clear_Last_Exception;

      Sloc_Range_P.all := Wrap (Sloc_Range (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure lkt_lookup_in_node
     (Node   : lkt_node_Ptr;
      Sloc   : lkt_source_location;
      Result : lkt_node_Ptr) is
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

   function lkt_node_children_count
     (Node : lkt_node_Ptr) return unsigned is
   begin
      Clear_Last_Exception;
      return unsigned (Children_Count (Node.Node));
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function lkt_node_child
     (Node    : lkt_node_Ptr;
      N       : unsigned;
      Child_P : lkt_node_Ptr) return int is
   begin
      Clear_Last_Exception;

      declare
         Result : Bare_Lkt_Node;
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

   function lkt_text_to_locale_string
     (Text : lkt_text) return System.Address is
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

   -------------------
   -- UTF32_To_UTF8 --
   -------------------

   procedure UTF32_To_UTF8
     (UTF32 : U32_Array; Bytes : out chars_ptr; Length : out size_t)
   is
      Output : System.Address;
   begin
      --  Compute the size of the UTF-8 string and allocate it, using
      --  System.Memory.Alloc so that users can call C's "free" function to
      --  deallocate it.

      Length := 0;
      for C of UTF32 loop
         Length := Length + Codepoint_UTF8_Size (C);
      end loop;
      Output := System.Memory.Alloc (System.Memory.size_t (Length));
      Bytes := +Output;

      --  Do the conversion

      declare
         B    : U8_Array (1 .. Length) with Import, Address => Output;
         Next : size_t := B'First;
      begin
         for C of UTF32 loop
            case C is
               when 0 .. 16#7f# =>
                  B (Next) := Unsigned_8 (C);
                  Next := Next + 1;

               when 16#80# .. 16#07ff# =>
                  B (Next) :=
                    16#c0# or Unsigned_8 (Shift_Right (C and 16#07c0#, 6));
                  B (Next + 1) :=
                    16#80# or Unsigned_8 (C and 16#003f#);
                  Next := Next + 2;

               when 16#0800# .. 16#ffff# =>
                  B (Next) :=
                    16#e0# or Unsigned_8 (Shift_Right (C and 16#f000#, 12));
                  B (Next + 1) :=
                    16#80# or Unsigned_8 (Shift_Right (C and 16#0fc0#, 6));
                  B (Next + 2) :=
                    16#80# or Unsigned_8 (C and 16#003f#);
                  Next := Next + 3;

               when 16#01_0000# .. 16#10_ffff# =>
                  B (Next) :=
                    16#f0# or Unsigned_8 (Shift_Right (C and 16#1c_0000#, 18));
                  B (Next + 1) :=
                    16#80# or Unsigned_8 (Shift_Right (C and 16#03_f000#, 12));
                  B (Next + 2) :=
                    16#80# or Unsigned_8 (Shift_Right (C and 16#00_0fc0#, 6));
                  B (Next + 3) :=
                    16#80# or Unsigned_8 (C and 16#00_003f#);
                  Next := Next + 4;

               when others =>
                  raise Program_Error;
            end case;
         end loop;
      end;
   end UTF32_To_UTF8;

   ---------------------------
   -- UTF8_Codepoints_Count --
   ---------------------------

   function UTF8_Codepoints_Count (UTF8 : U8_Array) return Natural is
   begin
      return Result : Natural := 0 do
         for Byte of UTF8 loop
            if (Byte and 16#c0#) /= 16#80# then
               Result := Result + 1;
            end if;
         end loop;
      end return;
   end UTF8_Codepoints_Count;

   -------------------
   -- UTF8_To_UTF32 --
   -------------------

   procedure UTF8_To_UTF32 (UTF8 : U8_Array; UTF32 : out U32_Array) is
      Next : size_t := UTF8'First;
   begin
      for C of UTF32 loop
         case UTF8 (Next) and 16#f8# is
            when 16#00# .. 16#78# =>
               C := Unsigned_32 (UTF8 (Next));
               Next := Next + 1;

            when 16#c0# .. 16#d8# =>
               C :=
                 Shift_Left (Unsigned_32 (UTF8 (Next) and 16#1f#), 6)
                 or Unsigned_32 (UTF8 (Next + 1) and 16#3f#);
               Next := Next + 2;

            when 16#e0# .. 16#e8# =>
               C :=
                 Shift_Left (Unsigned_32 (UTF8 (Next) and 16#0f#), 12)
                 or Shift_Left (Unsigned_32 (UTF8 (Next + 1) and 16#3f#), 6)
                 or Unsigned_32 (UTF8 (Next + 2) and 16#3f#);
               Next := Next + 3;

            when 16#f0# =>
               C :=
                 Shift_Left (Unsigned_32 (UTF8 (Next) and 16#07#), 18)
                 or Shift_Left (Unsigned_32 (UTF8 (Next + 1) and 16#3f#), 12)
                 or Shift_Left (Unsigned_32 (UTF8 (Next + 2) and 16#3f#), 6)
                 or Unsigned_32 (UTF8 (Next + 3) and 16#3f#);
               Next := Next + 4;

            when others =>
               raise Program_Error;
         end case;
      end loop;
   end UTF8_To_UTF32;

   procedure lkt_text_to_utf8
     (Text   : lkt_text;
      Bytes  : out chars_ptr;
      Length : out size_t) is
   begin
      Clear_Last_Exception;

      declare
         UTF32 : constant U32_Array (1 .. Text.Length)
           with Import, Address => Text.Chars;
      begin
         UTF32_To_UTF8 (UTF32, Bytes, Length);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure lkt_text_from_utf8
     (Bytes  : chars_ptr;
      Length : size_t;
      Text   : out lkt_text)
   is
      B : U8_Array (1 .. Length) with Import, Address => +Bytes;

      --  Allocate a buffer with just enough room to transcode Bytes

      Text_Length : Natural := 0;
      Text_Alloc  : Text_Access;
   begin
      Clear_Last_Exception;

      Text_Length := UTF8_Codepoints_Count (B);
      Text_Alloc := new Text_Type (1 .. Text_Length);
      Text :=
        (Chars        => Text_Alloc.all'Address,
         Length       => size_t (Text_Length),
         Is_Allocated => 1);

      --  Do the conversion

      declare
         T : U32_Array (1 .. size_t (Text_Length))
           with Import, Address => Text_Alloc.all'Address;
      begin
         UTF8_To_UTF32 (B, T);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure lkt_char_to_utf8
     (Char   : Unsigned_32;
      Bytes  : out chars_ptr;
      Length : out size_t) is
   begin
      Clear_Last_Exception;

      UTF32_To_UTF8 ((1 => Char), Bytes, Length);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure lkt_char_from_utf8
     (Bytes  : chars_ptr;
      Length : size_t;
      Char   : out Unsigned_32)
   is
      B : U8_Array (1 .. Length) with Import, Address => +Bytes;
      T : U32_Array (1 .. 1);
   begin
      Clear_Last_Exception;

      --  Callers are supposed to provide a 1 codepoint sized buffer

      if UTF8_Codepoints_Count (B) /= 1 then
         raise Program_Error;
      end if;

      UTF8_To_UTF32 (B, T);
      Char := T (1);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure lkt_string_to_utf8
     (Str    : lkt_string_type;
      Bytes  : out chars_ptr;
      Length : out size_t) is
   begin
      Clear_Last_Exception;

      declare
         UTF32 : constant U32_Array (1 .. size_t (Str.Length))
           with Import, Address => Str.Content'Address;
      begin
         UTF32_To_UTF8 (UTF32, Bytes, Length);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure lkt_string_from_utf8
     (Bytes  : chars_ptr;
      Length : size_t;
      Str    : out lkt_string_type)
   is
      B : U8_Array (1 .. Length) with Import, Address => +Bytes;

      --  Allocate a buffer with just enough room to transcode Bytes

      String_Length : Natural := 0;
   begin
      Clear_Last_Exception;

      String_Length := UTF8_Codepoints_Count (B);
      Str := new String_Record (String_Length);
      Str.Ref_Count := 1;

      --  Do the conversion

      declare
         T : U32_Array (1 .. size_t (String_Length))
           with Import, Address => Str.Content'Address;
      begin
         UTF8_To_UTF32 (B, T);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   ----------
   -- Wrap --
   ----------

   function Wrap (S : Unbounded_Wide_Wide_String) return lkt_text is
      Chars  : Big_Wide_Wide_String_Access;
      Length : Natural;
   begin
      Get_Wide_Wide_String (S, Chars, Length);
      return (Chars.all'Address, size_t (Length), 0);
   end Wrap;

   function lkt_stack_trace_size
     (Stack_Trace : lkt_stack_trace) return int is
   begin
      return int (Stack_Trace.Size);
   end;

   function lkt_stack_trace_element
     (Stack_Trace : lkt_stack_trace; Index : int) return System.Address
   is
   begin
      return Stack_Trace.Items (Natural (Index) + 1);
   end;

   function lkt_create_stack_trace
     (Size : int; Elements : System.Address) return lkt_stack_trace
   is
      S      : constant Natural := Natural (Size);
      Result : constant lkt_stack_trace := new Stack_Trace_Record (S);
      E      : GNAT.Traceback.Tracebacks_Array (1 .. S)
        with Import, Address => Elements;
   begin
      Result.Size := S;
      Result.Items := E;
      return Result;
   end;

   procedure lkt_destroy_stack_trace
     (Stack_Trace : lkt_stack_trace)
   is
      ST : lkt_stack_trace := Stack_Trace;
   begin
      Free (ST);
   end;

   function lkt_symbolize_stack_trace
     (Stack_Trace : lkt_stack_trace) return chars_ptr
   is
      Result : constant String :=
        GNAT.Traceback.Symbolic.Symbolic_Traceback_No_Hex
          (Stack_Trace.Items (1 .. Stack_Trace.Size));
   begin
      return New_String (Result);
   end;

   ------------------------
   -- Set_Last_Exception --
   ------------------------

   procedure Set_Last_Exception (Exc : Exception_Occurrence) is
   begin
      Set_Last_Exception
        (Exception_Identity (Exc),
         Exception_Message (Exc),
         Ada.Exceptions.Traceback.Tracebacks (Exc));
   end Set_Last_Exception;

   ------------------------
   -- Set_Last_Exception --
   ------------------------

   procedure Set_Last_Exception
     (Id          : Exception_Id;
      Message     : String;
      Stack_Trace : GNAT.Traceback.Tracebacks_Array) is
   begin
      --  If it's the first time, allocate room for the exception information

      if Last_Exception = null then
         Last_Exception := new lkt_exception;

      --  If it is not the first time, free memory allocated for the last
      --  exception.

      else
         if Last_Exception.Information /= Null_Ptr then
            Free (Last_Exception.Information);
         end if;
      end if;

      --  Allocate a big enough stack trace buffer if needed

      if Last_Stack_Trace = null
         or else Last_Stack_Trace.Capacity < Stack_Trace'Length
      then
         Free (Last_Stack_Trace);
         Last_Stack_Trace := new Stack_Trace_Record (Stack_Trace'Length);
         Last_Exception.Stack_Trace := Last_Stack_Trace;
      end if;

      --  Get the kind corresponding to Exc

      if Id = Liblktlang_Support.Errors.File_Read_Error'Identity then
         Last_Exception.Kind := Exception_File_Read_Error;
      elsif Id = Liblktlang_Support.Errors.Introspection.Bad_Type_Error'Identity then
         Last_Exception.Kind := Exception_Bad_Type_Error;
      elsif Id = Liblktlang_Support.Errors.Introspection.Out_Of_Bounds_Error'Identity then
         Last_Exception.Kind := Exception_Out_Of_Bounds_Error;
      elsif Id = Liblktlang_Support.Errors.Invalid_Input'Identity then
         Last_Exception.Kind := Exception_Invalid_Input;
      elsif Id = Liblktlang_Support.Errors.Invalid_Symbol_Error'Identity then
         Last_Exception.Kind := Exception_Invalid_Symbol_Error;
      elsif Id = Liblktlang_Support.Errors.Invalid_Unit_Name_Error'Identity then
         Last_Exception.Kind := Exception_Invalid_Unit_Name_Error;
      elsif Id = Liblktlang_Support.Errors.Native_Exception'Identity then
         Last_Exception.Kind := Exception_Native_Exception;
      elsif Id = Liblktlang_Support.Errors.Precondition_Failure'Identity then
         Last_Exception.Kind := Exception_Precondition_Failure;
      elsif Id = Liblktlang_Support.Errors.Property_Error'Identity then
         Last_Exception.Kind := Exception_Property_Error;
      elsif Id = Liblktlang_Support.Errors.Rewriting.Template_Args_Error'Identity then
         Last_Exception.Kind := Exception_Template_Args_Error;
      elsif Id = Liblktlang_Support.Errors.Rewriting.Template_Format_Error'Identity then
         Last_Exception.Kind := Exception_Template_Format_Error;
      elsif Id = Liblktlang_Support.Errors.Rewriting.Template_Instantiation_Error'Identity then
         Last_Exception.Kind := Exception_Template_Instantiation_Error;
      elsif Id = Liblktlang_Support.Errors.Stale_Reference_Error'Identity then
         Last_Exception.Kind := Exception_Stale_Reference_Error;
      elsif Id = Liblktlang_Support.Errors.Syntax_Error'Identity then
         Last_Exception.Kind := Exception_Syntax_Error;
      elsif Id = Liblktlang_Support.Errors.Unknown_Charset'Identity then
         Last_Exception.Kind := Exception_Unknown_Charset;
      elsif Id = Liblktlang_Support.Errors.Unparsing.Malformed_Tree_Error'Identity then
         Last_Exception.Kind := Exception_Malformed_Tree_Error;
      else
         Last_Exception.Kind := Exception_Native_Exception;
      end if;

      --  Unconditionally set the exception message

      Last_Exception.Information := New_String (Message);

      --  Set the exception stack trace

      Last_Stack_Trace.Size := Stack_Trace'Length;
      Last_Stack_Trace.Items (1 .. Last_Stack_Trace.Size) := Stack_Trace;
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

   function lkt_get_last_exception return lkt_exception_Ptr
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

   function lkt_exception_name
     (Kind : lkt_exception_kind) return chars_ptr is
   begin
      return New_String (Kind'Image);
   end;

   function lkt_token_get_kind
     (Token : lkt_token) return int is
   begin
      Clear_Last_Exception;
      declare
         T : constant Token_Reference := Unwrap (Token);
         D : constant Token_Data_Type := Data (T);
      begin
         return Kind (D)'Enum_Rep;
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   function lkt_token_kind_name (Kind : int) return chars_ptr
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

   procedure lkt_token_sloc_range
     (Token : lkt_token; Result : access lkt_source_location_range) is
   begin
      Clear_Last_Exception;
      declare
         T : constant Token_Reference := Unwrap (Token);
         D : constant Token_Data_Type := Data (T);
      begin
         Result.all := Wrap (Sloc_Range (D));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure lkt_token_next
     (Token      : lkt_token;
      Next_Token : access lkt_token)
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

   procedure lkt_token_previous
     (Token          : lkt_token;
      Previous_Token : access lkt_token)
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

   function lkt_token_range_text
     (First, Last : lkt_token;
      Text        : access lkt_text) return int
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

   function lkt_token_is_equivalent
     (Left  : lkt_token;
      Right : lkt_token) return lkt_bool
   is
   begin
      Clear_Last_Exception;
         declare
         L  : constant Token_Reference := Unwrap (Left);
         R  : constant Token_Reference := Unwrap (Right);
      begin
         return lkt_bool (Boolean'Pos (Is_Equivalent (L, R)));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end;

   ----------------
   -- Wrap_Alloc --
   ----------------

   function Wrap_Alloc (S : Text_Type) return lkt_text is
      T : Text_Access := new Text_Type'(S);
   begin
      return lkt_text'(T.all'Address, T.all'Length, Is_Allocated => 1);
   end Wrap_Alloc;

   ----------------
   -- Wrap_Alloc --
   ----------------

   function Wrap_Alloc (S : Unbounded_Wide_Wide_String) return lkt_text is
      Chars     : Big_Wide_Wide_String_Access;
      Length    : Natural;
      Allocated : Text_Access;
   begin
      Get_Wide_Wide_String (S, Chars, Length);
      Allocated := new Text_Type (1 .. Length);
      Allocated.all := Chars (1 .. Length);
      return (Allocated.all'Address, Allocated.all'Length, 1);
   end Wrap_Alloc;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (S     : Text_Cst_Access;
      First : Positive;
      Last  : Natural) return lkt_text
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

   procedure lkt_destroy_text (T : access lkt_text) is
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

   procedure lkt_symbol_text
     (Symbol : access lkt_symbol_type; Text : access lkt_text) is
   begin
      Clear_Last_Exception;
      declare
         Sym    : constant Symbol_Type := Unwrap_Symbol (Symbol.all);
         Result : constant Text_Type :=
           (if Sym = No_Symbol then "" else Image (Sym));
      begin
         Text.all := Wrap_Alloc (Result);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function lkt_create_big_integer
     (Text : access lkt_text) return lkt_big_integer is
   begin
      Clear_Last_Exception;
      declare
         T      : Text_Type (1 .. Natural (Text.Length))
            with Import, Address => Text.Chars;
         Image  : constant String := Liblktlang_Support.Text.Image (T);
         Result : constant Big_Integer_Type := Create_Big_Integer (Image);
      begin
         return Wrap_Big_Integer (Result);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return lkt_big_integer (System.Null_Address);
   end lkt_create_big_integer;

   procedure lkt_big_integer_text
     (Bigint : lkt_big_integer; Text : access lkt_text) is
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

   procedure lkt_big_integer_decref
     (Bigint : lkt_big_integer) is
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

   procedure lkt_get_versions
     (Version, Build_Date : access chars_ptr)
   is
   begin
      Clear_Last_Exception;
      Version.all := New_String (Liblktlang.Version);
      Build_Date.all := New_String (Liblktlang.Build_Date);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function lkt_create_string
     (Content : System.Address; Length : int) return lkt_string_type
   is
      Value : Text_Type (1 .. Integer (Length))
        with Import, Address => Content;
   begin
      Clear_Last_Exception;
      return Create_String (Value);
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return null;
   end;

   procedure lkt_string_dec_ref (Self : lkt_string_type) is
   begin
      Clear_Last_Exception;
      declare
         Self_Var : String_Type := Self;
      begin
         Dec_Ref (Self_Var);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   procedure lkt_dec_ref_unit_provider
     (Provider : lkt_unit_provider) is
   begin
      Clear_Last_Exception;
      declare
         P : Internal_Unit_Provider_Access :=
            Unwrap_Private_Provider (Provider);
      begin
         Dec_Ref (P);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   function lkt_create_event_handler
     (Data                : System.Address;
      Destroy_Func        : lkt_event_handler_destroy_callback;
      Unit_Requested_Func : lkt_event_handler_unit_requested_callback;
      Unit_Parsed_Func    : lkt_event_handler_unit_parsed_callback)
      return lkt_event_handler is
   begin
      Clear_Last_Exception;
      declare
         Result : constant Internal_Event_Handler_Access :=
           new C_Event_Handler'
             (Ada.Finalization.Limited_Controlled with
              Ref_Count           => 1,
              Data                => Data,
              Destroy_Func        => Destroy_Func,
              Unit_Requested_Func => Unit_Requested_Func,
              Unit_Parsed_Func    => Unit_Parsed_Func);
      begin
         return Wrap_Private_Event_Handler (Result);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return lkt_event_handler (System.Null_Address);
   end;

   procedure lkt_dec_ref_event_handler
     (Handler : lkt_event_handler) is
   begin
      Clear_Last_Exception;
      declare
         P : Internal_Event_Handler_Access :=
            Unwrap_Private_Event_Handler (Handler);
      begin
         Dec_Ref (P);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out C_File_Reader) is
   begin
      Self.Destroy_Func (Self.Data);
   end Finalize;

   -------------
   -- Inc_Ref --
   -------------

   overriding procedure Inc_Ref (Self : in out C_File_Reader) is
   begin
      Self.Ref_Count := Self.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   overriding function Dec_Ref (Self : in out C_File_Reader) return Boolean is
   begin
      Self.Ref_Count := Self.Ref_Count - 1;
      if Self.Ref_Count = 0 then
         return True;
      else
         return False;
      end if;
   end Dec_Ref;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self        : C_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      C_Filename : chars_ptr := New_String (Filename);
      C_Charset  : chars_ptr := New_String (Charset);
      C_Read_BOM : constant int := (if Read_BOM then 1 else 0);

      C_Contents   : aliased lkt_text;
      C_Diagnostic : aliased lkt_diagnostic :=
        (Sloc_Range => <>,
         Message    => (Chars        => Null_Address,
                        Length       => 0,
                        Is_Allocated => 0));
   begin
      Self.Read_Func.all
        (Self.Data, C_Filename, C_Charset, C_Read_BOM, C_Contents'Access,
         C_Diagnostic'Access);

      if C_Diagnostic.Message.Chars = Null_Address then

         --  If there is a diagnostic (an error), there is no content to return

         declare
            Message : Text_Type (1 .. Natural (C_Diagnostic.Message.Length))
               with Import,
                    Convention => Ada,
                    Address    => C_Diagnostic.Message.Chars;
         begin
            Append (Diagnostics,
                    Unwrap (C_Diagnostic.Sloc_Range),
                    Message);
         end;

      else
         --  Otherwise, create a copy of the buffer

         declare
            Buffer : Text_Type (1 .. Natural (C_Contents.Length))
               with Import, Convention => Ada, Address => C_Contents.Chars;
         begin
            Contents.Buffer := new Text_Type (Buffer'Range);
            Contents.First := Buffer'First;
            Contents.Last := Buffer'Last;
            Contents.Buffer.all := Buffer;
         end;
      end if;

      Free (C_Filename);
      Free (C_Charset);
   end Read;

   function lkt_create_file_reader
     (Data         : System.Address;
      Destroy_Func : lkt_file_reader_destroy_callback;
      Read_Func    : lkt_file_reader_read_callback) return lkt_file_reader
   is
   begin
      Clear_Last_Exception;
      declare
         Result : constant C_File_Reader_Access := new C_File_Reader'
           (Ada.Finalization.Limited_Controlled with
            Ref_Count    => 1,
            Data         => Data,
            Destroy_Func => Destroy_Func,
            Read_Func    => Read_Func);
      begin
         return Wrap_Private_File_Reader (Internal_File_Reader_Access (Result));
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return lkt_file_reader (System.Null_Address);
   end;

   procedure lkt_dec_ref_file_reader
     (File_Reader : lkt_file_reader) is
   begin
      Clear_Last_Exception;
      declare
         P : Internal_File_Reader_Access :=
            Unwrap_Private_File_Reader (File_Reader);
      begin
         Dec_Ref (P);
      end;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
   end;

   


   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out C_Event_Handler) is
   begin
      if Self.Destroy_Func /= null then
          Self.Destroy_Func (Self.Data);
      end if;
   end Finalize;

   -------------
   -- Inc_Ref --
   -------------

   overriding procedure Inc_Ref (Self : in out C_Event_Handler) is
   begin
      Self.Ref_Count := Self.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   overriding function Dec_Ref (Self : in out C_Event_Handler) return Boolean
   is
   begin
      Self.Ref_Count := Self.Ref_Count - 1;
      if Self.Ref_Count = 0 then
         return True;
      else
         return False;
      end if;
   end Dec_Ref;

   -----------------------------
   -- Unit_Requested_Callback --
   -----------------------------

   overriding procedure Unit_Requested_Callback
     (Self               : in out C_Event_Handler;
      Context            : Internal_Context;
      Name               : Text_Type;
      From               : Internal_Unit;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean)
   is
      Name_Access : constant Text_Cst_Access := Name'Unrestricted_Access;
      C_Name      : aliased constant lkt_text := Wrap (Name_Access);
   begin
      Self.Unit_Requested_Func
        (Self.Data,
         Context,
         C_Name'Access,
         From,
         (if Found then 1 else 0),
         (if Is_Not_Found_Error then 1 else 0));
   end Unit_Requested_Callback;

   --------------------------
   -- Unit_Parsed_Callback --
   --------------------------

   overriding procedure Unit_Parsed_Callback
     (Self     : in out C_Event_Handler;
      Context  : Internal_Context;
      Unit     : Internal_Unit;
      Reparsed : Boolean)
   is
   begin
      Self.Unit_Parsed_Func
        (Self.Data, Context, Unit, (if Reparsed then 1 else 0));
   end Unit_Parsed_Callback;

   


   ----------
   -- Wrap --
   ----------

   function Wrap (Token : Token_Reference) return lkt_token is
   begin
      if Token = No_Token then
         return (Token_Data   => null,
                 Token_Index  => -1,
                 Trivia_Index => -1,
                 others       => <>);
      end if;

      declare
         Index : constant Token_Or_Trivia_Index := Get_Token_Index (Token);
      begin
         return (Context         => Get_Token_Context (Token),
                 Token_Data      => Get_Token_TDH (Token),
                 Token_Index     => int (Index.Token),
                 Trivia_Index    => int (Index.Trivia));
      end;
   end Wrap;

   ------------
   -- Unwrap --
   ------------

   function Unwrap (Token : lkt_token) return Token_Reference is
   begin
      return (if Token.Token_Data = null
              then No_Token
              else Wrap_Token_Reference
                     (Token.Context,
                      Token.Token_Data,
                      (Token  => Token_Index (Token.Token_Index),
                       Trivia => Token_Index (Token.Trivia_Index))));
   end Unwrap;

   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

           

   

   
   

   function lkt_lkt_node_parent
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity;
         begin
            Result := Liblktlang.Implementation.Parent
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_parent;


           

   

   
   

   function lkt_lkt_node_parents
     (Node : lkt_node_Ptr;

         With_Self :
            
            lkt_bool;

      Value_P : access lkt_node_array) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
         
         Unwrapped_With_Self : constant Boolean :=
               With_Self /= 0
         ;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Array_Access;
         begin
            Result := Liblktlang.Implementation.Parents
              (Unwrapped_Node,
               With_Self => Unwrapped_With_Self,
               E_Info => Node.Info);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_parents;


           

   

   
   

   function lkt_lkt_node_children
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node_array) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Array_Access;
         begin
            Result := Liblktlang.Implementation.Children
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_children;


           

   

   
   

   function lkt_lkt_node_token_start
     (Node : lkt_node_Ptr;


      Value_P : access lkt_token) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Token_Reference;
         begin
            Result := Liblktlang.Implementation.Token_Start
              (Unwrapped_Node);

            Value_P.all :=
                   Wrap (Result)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_token_start;


           

   

   
   

   function lkt_lkt_node_token_end
     (Node : lkt_node_Ptr;


      Value_P : access lkt_token) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Token_Reference;
         begin
            Result := Liblktlang.Implementation.Token_End
              (Unwrapped_Node);

            Value_P.all :=
                   Wrap (Result)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_token_end;


           

   

   
   

   function lkt_lkt_node_child_index
     (Node : lkt_node_Ptr;


      Value_P : access int) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Integer;
         begin
            Result := Liblktlang.Implementation.Child_Index
              (Unwrapped_Node);

            Value_P.all :=
                   int (Result)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_child_index;


           

   

   
   

   function lkt_lkt_node_previous_sibling
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity;
         begin
            Result := Liblktlang.Implementation.Previous_Sibling
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_previous_sibling;


           

   

   
   

   function lkt_lkt_node_next_sibling
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity;
         begin
            Result := Liblktlang.Implementation.Next_Sibling
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_next_sibling;


           

   

   
   

   function lkt_lkt_node_unit
     (Node : lkt_node_Ptr;


      Value_P : access lkt_analysis_unit) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Unit;
         begin
            Result := Liblktlang.Implementation.Unit
              (Unwrapped_Node);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_unit;


           

   

   
   

   function lkt_lkt_node_is_ghost
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Boolean;
         begin
            Result := Liblktlang.Implementation.Is_Ghost
              (Unwrapped_Node);

            Value_P.all :=
                   lkt_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_is_ghost;


           

   

   
   

   function lkt_lkt_node_full_sloc_image
     (Node : lkt_node_Ptr;


      Value_P : access lkt_string_type) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : String_Type;
         begin
            Result := Liblktlang.Implementation.Full_Sloc_Image
              (Unwrapped_Node);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_full_sloc_image;


           

   

   
   

   function lkt_lkt_node_completion_item_kind_to_int
     (Node : lkt_node_Ptr;

         Kind :
            
            lkt_completion_item_kind;

      Value_P : access int) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
         
         Unwrapped_Kind : constant Completion_Item_Kind :=
               Kind
         ;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Integer;
         begin
            Result := Liblktlang.Implementation.Completion_Item_Kind_To_Int
              (Unwrapped_Node,
               Kind => Unwrapped_Kind);

            Value_P.all :=
                   int (Result)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_completion_item_kind_to_int;


           

   

   
   

   function lkt_lkt_node_p_set_solver_debug_mode
     (Node : lkt_node_Ptr;

         Enable :
            
            lkt_bool;

      Value_P : access lkt_bool) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
         
         Unwrapped_Enable : constant Boolean :=
               Enable /= 0
         ;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Boolean;
         begin
            Result := Liblktlang.Implementation.Extensions.Lkt_Node_P_Set_Solver_Debug_Mode
              (Unwrapped_Node,
               Enable => Unwrapped_Enable);

            Value_P.all :=
                   lkt_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_set_solver_debug_mode;


           

   

   
   

   function lkt_lkt_node_p_basic_trait_gen
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Generic_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Basic_Trait_Gen
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_basic_trait_gen;


           

   

   
   

   function lkt_lkt_node_p_basic_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Trait_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Basic_Trait
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_basic_trait;


           

   

   
   

   function lkt_lkt_node_p_node_gen_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Generic_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Node_Gen_Trait
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_node_gen_trait;


           

   

   
   

   function lkt_lkt_node_p_node_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Trait_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Node_Trait
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_node_trait;


           

   

   
   

   function lkt_lkt_node_p_indexable_gen_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Generic_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Indexable_Gen_Trait
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_indexable_gen_trait;


           

   

   
   

   function lkt_lkt_node_p_indexable_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Trait_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Indexable_Trait
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_indexable_trait;


           

   

   
   

   function lkt_lkt_node_p_token_node_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Named_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Token_Node_Trait
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_token_node_trait;


           

   

   
   

   function lkt_lkt_node_p_error_node_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Named_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Error_Node_Trait
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_error_node_trait;


           

   

   
   

   function lkt_lkt_node_p_char_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Named_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Char_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_char_type;


           

   

   
   

   function lkt_lkt_node_p_int_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Named_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Int_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_int_type;


           

   

   
   

   function lkt_lkt_node_p_bool_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Named_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Bool_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_bool_type;


           

   

   
   

   function lkt_lkt_node_p_bigint_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Named_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Bigint_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_bigint_type;


           

   

   
   

   function lkt_lkt_node_p_string_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Named_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_String_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_string_type;


           

   

   
   

   function lkt_lkt_node_p_symbol_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Named_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Symbol_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_symbol_type;


           

   

   
   

   function lkt_lkt_node_p_property_error_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Named_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Property_Error_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_property_error_type;


           

   

   
   

   function lkt_lkt_node_p_regexp_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Named_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Regexp_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_regexp_type;


           

   

   
   

   function lkt_lkt_node_p_entity_gen_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Generic_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Entity_Gen_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_entity_gen_type;


           

   

   
   

   function lkt_lkt_node_p_entity_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Named_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Entity_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_entity_type;


           

   

   
   

   function lkt_lkt_node_p_logicvar_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Named_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Logicvar_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_logicvar_type;


           

   

   
   

   function lkt_lkt_node_p_equation_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Named_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Equation_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_equation_type;


           

   

   
   

   function lkt_lkt_node_p_array_gen_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Generic_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Array_Gen_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_array_gen_type;


           

   

   
   

   function lkt_lkt_node_p_array_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Named_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Array_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_array_type;


           

   

   
   

   function lkt_lkt_node_p_astlist_gen_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Generic_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Astlist_Gen_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_astlist_gen_type;


           

   

   
   

   function lkt_lkt_node_p_astlist_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Named_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Astlist_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_astlist_type;


           

   

   
   

   function lkt_lkt_node_p_node_builder_gen_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Generic_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Node_Builder_Gen_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_node_builder_gen_type;


           

   

   
   

   function lkt_lkt_node_p_node_builder_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Named_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Node_Builder_Type
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_node_builder_type;


           

   

   
   

   function lkt_lkt_node_p_iterator_gen_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Generic_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Iterator_Gen_Trait
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_iterator_gen_trait;


           

   

   
   

   function lkt_lkt_node_p_iterator_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Trait_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Iterator_Trait
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_iterator_trait;


           

   

   
   

   function lkt_lkt_node_p_analysis_unit_gen_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Generic_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Analysis_Unit_Gen_Trait
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_analysis_unit_gen_trait;


           

   

   
   

   function lkt_lkt_node_p_analysis_unit_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Entity_Trait_Decl;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Analysis_Unit_Trait
              (Unwrapped_Node);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_analysis_unit_trait;


           

   

   
   

   function lkt_lkt_node_p_topmost_invalid_decl
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Bare_Lkt_Node;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Topmost_Invalid_Decl
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_topmost_invalid_decl;


           

   

   
   

   function lkt_lkt_node_p_nameres_diagnostics
     (Node : lkt_node_Ptr;


      Value_P : access lkt_internal_solver_diagnostic_array) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Solver_Diagnostic_Array_Access;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Nameres_Diagnostics
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_nameres_diagnostics;


           

   

   
   

   function lkt_lkt_node_p_solve_enclosing_context
     (Node : lkt_node_Ptr;


      Value_P : access lkt_internal_solver_result) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Internal_Solver_Result;
         begin
            Result := Liblktlang.Implementation.Lkt_Node_P_Solve_Enclosing_Context
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_solve_enclosing_context;


           

   

   
   

   function lkt_lkt_node_p_xref_entry_point
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;



         declare
            

            Result : Boolean;
         begin
            Result := Liblktlang.Implementation.Dispatcher_Lkt_Node_P_Xref_Entry_Point
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                   lkt_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;


   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lkt_node_p_xref_entry_point;


           

   

   
   

   function lkt_argument_f_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Argument_Range then

         declare
            

            Result : Bare_Ref_Id;
         begin
            Result := Argument_F_Name
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_argument_f_name;


           

   

   
   

   function lkt_argument_f_value
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Argument_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Argument_F_Value
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_argument_f_value;


           

   

   
   

   function lkt_base_lexer_case_rule_alt_f_send
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Base_Lexer_Case_Rule_Alt then

         declare
            

            Result : Bare_Lexer_Case_Rule_Send;
         begin
            Result := Base_Lexer_Case_Rule_Alt_F_Send
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_base_lexer_case_rule_alt_f_send;


           

   

   
   

   function lkt_lexer_case_rule_cond_alt_f_cond_exprs
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Lexer_Case_Rule_Cond_Alt_Range then

         declare
            

            Result : Bare_Ref_Id_List;
         begin
            Result := Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lexer_case_rule_cond_alt_f_cond_exprs;


           

   

   
   

   function lkt_class_qualifier_p_as_bool
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Class_Qualifier then

         declare
            

            Result : Boolean;
         begin
            Result := Liblktlang.Implementation.Dispatcher_Class_Qualifier_P_As_Bool
              (Unwrapped_Node);

            Value_P.all :=
                   lkt_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_class_qualifier_p_as_bool;


           

   

   
   

   function lkt_decl_f_syn_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Decl then

         declare
            

            Result : Bare_Def_Id;
         begin
            Result := Decl_F_Syn_Name
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_decl_f_syn_name;


           

   

   
   

   function lkt_decl_p_custom_image
     (Node : lkt_node_Ptr;


      Value_P : access lkt_string_type) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Decl then

         declare
            

            Result : String_Type;
         begin
            Result := Liblktlang.Implementation.Extensions.Decl_P_Custom_Image
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_decl_p_custom_image;


           

   

   
   

   function lkt_decl_p_decl_type_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_string_type) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Decl then

         declare
            

            Result : String_Type;
         begin
            Result := Liblktlang.Implementation.Dispatcher_Decl_P_Decl_Type_Name
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_decl_p_decl_type_name;


           

   

   
   

   function lkt_decl_p_as_bare_decl
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Decl then

         declare
            

            Result : Internal_Entity_Decl;
         begin
            Result := Liblktlang.Implementation.Decl_P_As_Bare_Decl
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_decl_p_as_bare_decl;


           

   

   
   

   function lkt_decl_p_get_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Decl then

         declare
            

            Result : Internal_Entity_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Decl_P_Get_Type
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_decl_p_get_type;


           

   

   
   

   function lkt_decl_p_get_cast_type
     (Node : lkt_node_Ptr;

         Cast_To :
            access constant
            lkt_node;

      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
         
         Unwrapped_Cast_To : constant Internal_Entity_Type_Decl :=
               (if Cast_To.all.Node = null
                then No_Entity_Type_Decl
                else (Cast_To.all.Node, Cast_To.all.Info))
         ;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Decl then

         declare
            

            Result : Internal_Entity_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Decl_P_Get_Cast_Type
              (Unwrapped_Node,
               Cast_To => Unwrapped_Cast_To,
               E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_decl_p_get_cast_type;


           

   

   
   

   function lkt_decl_p_get_keep_type
     (Node : lkt_node_Ptr;

         Keep_Type :
            access constant
            lkt_node;

      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
         
         Unwrapped_Keep_Type : constant Internal_Entity_Type_Decl :=
               (if Keep_Type.all.Node = null
                then No_Entity_Type_Decl
                else (Keep_Type.all.Node, Keep_Type.all.Info))
         ;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Decl then

         declare
            

            Result : Internal_Entity_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Decl_P_Get_Keep_Type
              (Unwrapped_Node,
               Keep_Type => Unwrapped_Keep_Type,
               E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_decl_p_get_keep_type;


           

   

   
   

   function lkt_decl_p_get_suffix_type
     (Node : lkt_node_Ptr;

         Prefix_Type :
            access constant
            lkt_node;

      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
         
         Unwrapped_Prefix_Type : constant Internal_Entity_Type_Decl :=
               (if Prefix_Type.all.Node = null
                then No_Entity_Type_Decl
                else (Prefix_Type.all.Node, Prefix_Type.all.Info))
         ;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Decl then

         declare
            

            Result : Internal_Entity_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Decl_P_Get_Suffix_Type
              (Unwrapped_Node,
               Prefix_Type => Unwrapped_Prefix_Type,
               E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_decl_p_get_suffix_type;


           

   

   
   

   function lkt_decl_p_is_generic
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Decl then

         declare
            

            Result : Boolean;
         begin
            Result := Liblktlang.Implementation.Decl_P_Is_Generic
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                   lkt_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_decl_p_is_generic;


           

   

   
   

   function lkt_decl_p_return_type_is_instantiated
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Decl then

         declare
            

            Result : Boolean;
         begin
            Result := Liblktlang.Implementation.Decl_P_Return_Type_Is_Instantiated
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                   lkt_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_decl_p_return_type_is_instantiated;


           

   

   
   

   function lkt_decl_p_is_instantiated
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Decl then

         declare
            

            Result : Boolean;
         begin
            Result := Liblktlang.Implementation.Decl_P_Is_Instantiated
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                   lkt_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_decl_p_is_instantiated;


           

   

   
   

   function lkt_decl_p_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_symbol_type) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Decl then

         declare
            

            Result : Symbol_Type;
         begin
            Result := Liblktlang.Implementation.Dispatcher_Decl_P_Name
              (Unwrapped_Node);

            Value_P.all :=
                   Wrap_Symbol (Result)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_decl_p_name;


           

   

   
   

   function lkt_decl_p_full_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_string_type) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Decl then

         declare
            

            Result : String_Type;
         begin
            Result := Liblktlang.Implementation.Dispatcher_Decl_P_Full_Name
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_decl_p_full_name;


           

   

   
   

   function lkt_base_grammar_rule_decl_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Base_Grammar_Rule_Decl then

         declare
            

            Result : Bare_Grammar_Expr;
         begin
            Result := Base_Grammar_Rule_Decl_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_base_grammar_rule_decl_f_expr;


           

   

   
   

   function lkt_explicitly_typed_decl_f_decl_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Explicitly_Typed_Decl then

         declare
            

            Result : Bare_Type_Ref;
         begin
            Result := Explicitly_Typed_Decl_F_Decl_Type
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_explicitly_typed_decl_f_decl_type;


           

   

   
   

   function lkt_component_decl_f_default_val
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Component_Decl then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Component_Decl_F_Default_Val
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_component_decl_f_default_val;


           

   

   
   

   function lkt_field_decl_f_trait_ref
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Field_Decl_Range then

         declare
            

            Result : Bare_Dot_Expr;
         begin
            Result := Field_Decl_F_Trait_Ref
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_field_decl_f_trait_ref;


           

   

   
   

   function lkt_fun_param_decl_f_decl_annotations
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Fun_Param_Decl_Range then

         declare
            

            Result : Bare_Decl_Annotation_List;
         begin
            Result := Fun_Param_Decl_F_Decl_Annotations
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_fun_param_decl_f_decl_annotations;


           

   

   
   

   function lkt_val_decl_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Val_Decl_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Val_Decl_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_val_decl_f_expr;


           

   

   
   

   function lkt_fun_decl_f_params
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Fun_Decl_Range then

         declare
            

            Result : Bare_Fun_Param_Decl_List;
         begin
            Result := Fun_Decl_F_Params
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_fun_decl_f_params;


           

   

   
   

   function lkt_fun_decl_f_return_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Fun_Decl_Range then

         declare
            

            Result : Bare_Type_Ref;
         begin
            Result := Fun_Decl_F_Return_Type
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_fun_decl_f_return_type;


           

   

   
   

   function lkt_fun_decl_f_trait_ref
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Fun_Decl_Range then

         declare
            

            Result : Bare_Dot_Expr;
         begin
            Result := Fun_Decl_F_Trait_Ref
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_fun_decl_f_trait_ref;


           

   

   
   

   function lkt_fun_decl_f_body
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Fun_Decl_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Fun_Decl_F_Body
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_fun_decl_f_body;


           

   

   
   

   function lkt_fun_decl_p_is_dynamic_combiner
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Fun_Decl_Range then

         declare
            

            Result : Boolean;
         begin
            Result := Liblktlang.Implementation.Fun_Decl_P_Is_Dynamic_Combiner
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                   lkt_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_fun_decl_p_is_dynamic_combiner;


           

   

   
   

   function lkt_env_spec_decl_f_actions
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Env_Spec_Decl_Range then

         declare
            

            Result : Bare_Call_Expr_List;
         begin
            Result := Env_Spec_Decl_F_Actions
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_env_spec_decl_f_actions;


           

   

   
   

   function lkt_generic_decl_f_generic_param_decls
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Generic_Decl_Range then

         declare
            

            Result : Bare_Generic_Param_Decl_List;
         begin
            Result := Generic_Decl_F_Generic_Param_Decls
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_generic_decl_f_generic_param_decls;


           

   

   
   

   function lkt_generic_decl_f_decl
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Generic_Decl_Range then

         declare
            

            Result : Bare_Decl;
         begin
            Result := Generic_Decl_F_Decl
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_generic_decl_f_decl;


           

   

   
   

   function lkt_grammar_decl_f_rules
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_Decl_Range then

         declare
            

            Result : Bare_Full_Decl_List;
         begin
            Result := Grammar_Decl_F_Rules
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_decl_f_rules;


           

   

   
   

   function lkt_lexer_decl_f_rules
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Lexer_Decl_Range then

         declare
            

            Result : Bare_Lkt_Node_List;
         begin
            Result := Lexer_Decl_F_Rules
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lexer_decl_f_rules;


           

   

   
   

   function lkt_lexer_family_decl_f_rules
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Lexer_Family_Decl_Range then

         declare
            

            Result : Bare_Full_Decl_List;
         begin
            Result := Lexer_Family_Decl_F_Rules
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lexer_family_decl_f_rules;


           

   

   
   

   function lkt_type_decl_f_traits
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Type_Decl then

         declare
            

            Result : Bare_Type_Ref_List;
         begin
            Result := Type_Decl_F_Traits
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_type_decl_f_traits;


           

   

   
   

   function lkt_type_decl_f_syn_base_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Type_Decl then

         declare
            

            Result : Bare_Type_Ref;
         begin
            Result := Type_Decl_F_Syn_Base_Type
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_type_decl_f_syn_base_type;


           

   

   
   

   function lkt_type_decl_p_base_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Type_Decl then

         declare
            

            Result : Internal_Entity_Type_Ref;
         begin
            Result := Liblktlang.Implementation.Type_Decl_P_Base_Type
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_type_decl_p_base_type;


           

   

   
   

   function lkt_type_decl_p_base_type_if_entity
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Type_Decl then

         declare
            

            Result : Internal_Entity_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Type_Decl_P_Base_Type_If_Entity
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_type_decl_p_base_type_if_entity;


           

   

   
   

   function lkt_generic_param_type_decl_f_has_class
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Generic_Param_Type_Decl_Range then

         declare
            

            Result : Bare_Class_Qualifier;
         begin
            Result := Generic_Param_Type_Decl_F_Has_Class
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_generic_param_type_decl_f_has_class;


           

   

   
   

   function lkt_named_type_decl_f_decls
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Named_Type_Decl then

         declare
            

            Result : Bare_Decl_Block;
         begin
            Result := Named_Type_Decl_F_Decls
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_named_type_decl_f_decls;


           

   

   
   

   function lkt_enum_class_decl_f_branches
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Enum_Class_Decl_Range then

         declare
            

            Result : Bare_Enum_Class_Case_List;
         begin
            Result := Enum_Class_Decl_F_Branches
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_enum_class_decl_f_branches;


           

   

   
   

   function lkt_enum_type_decl_f_literals
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Enum_Type_Decl_Range then

         declare
            

            Result : Bare_Enum_Lit_Decl_List;
         begin
            Result := Enum_Type_Decl_F_Literals
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_enum_type_decl_f_literals;


           

   

   
   

   function lkt_decl_annotation_f_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Decl_Annotation_Range then

         declare
            

            Result : Bare_Id;
         begin
            Result := Decl_Annotation_F_Name
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_decl_annotation_f_name;


           

   

   
   

   function lkt_decl_annotation_f_args
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Decl_Annotation_Range then

         declare
            

            Result : Bare_Decl_Annotation_Args;
         begin
            Result := Decl_Annotation_F_Args
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_decl_annotation_f_args;


           

   

   
   

   function lkt_decl_annotation_args_f_args
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Decl_Annotation_Args_Range then

         declare
            

            Result : Bare_Argument_List;
         begin
            Result := Decl_Annotation_Args_F_Args
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_decl_annotation_args_f_args;


           

   

   
   

   function lkt_elsif_branch_f_cond_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Elsif_Branch_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Elsif_Branch_F_Cond_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_elsif_branch_f_cond_expr;


           

   

   
   

   function lkt_elsif_branch_f_then_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Elsif_Branch_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Elsif_Branch_F_Then_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_elsif_branch_f_then_expr;


           

   

   
   

   function lkt_enum_class_case_f_decls
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Enum_Class_Case_Range then

         declare
            

            Result : Bare_Enum_Class_Alt_Decl_List;
         begin
            Result := Enum_Class_Case_F_Decls
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_enum_class_case_f_decls;


           

   

   
   

   function lkt_excludes_null_p_as_bool
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Excludes_Null then

         declare
            

            Result : Boolean;
         begin
            Result := Liblktlang.Implementation.Dispatcher_Excludes_Null_P_As_Bool
              (Unwrapped_Node);

            Value_P.all :=
                   lkt_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_excludes_null_p_as_bool;


           

   

   
   

   function lkt_expr_p_get_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Expr then

         declare
            

            Result : Internal_Entity_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Expr_P_Get_Type
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_expr_p_get_type;


           

   

   
   

   function lkt_expr_p_get_generic_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Expr then

         declare
            

            Result : Internal_Entity_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Expr_P_Get_Generic_Type
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_expr_p_get_generic_type;


           

   

   
   

   function lkt_expr_p_get_expected_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Expr then

         declare
            

            Result : Internal_Entity_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Expr_P_Get_Expected_Type
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_expr_p_get_expected_type;


           

   

   
   

   function lkt_expr_p_referenced_decl
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Expr then

         declare
            

            Result : Internal_Entity_Decl;
         begin
            Result := Liblktlang.Implementation.Dispatcher_Expr_P_Referenced_Decl
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_expr_p_referenced_decl;


           

   

   
   

   function lkt_any_of_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Any_Of_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Any_Of_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_any_of_f_expr;


           

   

   
   

   function lkt_any_of_f_values
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Any_Of_Range then

         declare
            

            Result : Bare_Any_Of_List;
         begin
            Result := Any_Of_F_Values
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_any_of_f_values;


           

   

   
   

   function lkt_array_literal_f_exprs
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Array_Literal_Range then

         declare
            

            Result : Bare_Expr_List;
         begin
            Result := Array_Literal_F_Exprs
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_array_literal_f_exprs;


           

   

   
   

   function lkt_array_literal_f_element_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Array_Literal_Range then

         declare
            

            Result : Bare_Type_Ref;
         begin
            Result := Array_Literal_F_Element_Type
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_array_literal_f_element_type;


           

   

   
   

   function lkt_base_call_expr_f_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Base_Call_Expr then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Base_Call_Expr_F_Name
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_base_call_expr_f_name;


           

   

   
   

   function lkt_base_call_expr_f_args
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Base_Call_Expr then

         declare
            

            Result : Bare_Argument_List;
         begin
            Result := Base_Call_Expr_F_Args
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_base_call_expr_f_args;


           

   

   
   

   function lkt_bin_op_f_left
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Bin_Op_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Bin_Op_F_Left
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_bin_op_f_left;


           

   

   
   

   function lkt_bin_op_f_op
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Bin_Op_Range then

         declare
            

            Result : Bare_Op;
         begin
            Result := Bin_Op_F_Op
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_bin_op_f_op;


           

   

   
   

   function lkt_bin_op_f_right
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Bin_Op_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Bin_Op_F_Right
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_bin_op_f_right;


           

   

   
   

   function lkt_block_expr_f_val_defs
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Block_Expr_Range then

         declare
            

            Result : Bare_Block_Decl_List;
         begin
            Result := Block_Expr_F_Val_Defs
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_block_expr_f_val_defs;


           

   

   
   

   function lkt_block_expr_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Block_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Block_Expr_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_block_expr_f_expr;


           

   

   
   

   function lkt_cast_expr_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Cast_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Cast_Expr_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_cast_expr_f_expr;


           

   

   
   

   function lkt_cast_expr_f_excludes_null
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Cast_Expr_Range then

         declare
            

            Result : Bare_Excludes_Null;
         begin
            Result := Cast_Expr_F_Excludes_Null
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_cast_expr_f_excludes_null;


           

   

   
   

   function lkt_cast_expr_f_dest_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Cast_Expr_Range then

         declare
            

            Result : Bare_Type_Ref;
         begin
            Result := Cast_Expr_F_Dest_Type
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_cast_expr_f_dest_type;


           

   

   
   

   function lkt_dot_expr_f_prefix
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Dot_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Dot_Expr_F_Prefix
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_dot_expr_f_prefix;


           

   

   
   

   function lkt_dot_expr_f_null_cond
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Dot_Expr_Range then

         declare
            

            Result : Bare_Null_Cond_Qualifier;
         begin
            Result := Dot_Expr_F_Null_Cond
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_dot_expr_f_null_cond;


           

   

   
   

   function lkt_dot_expr_f_suffix
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Dot_Expr_Range then

         declare
            

            Result : Bare_Ref_Id;
         begin
            Result := Dot_Expr_F_Suffix
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_dot_expr_f_suffix;


           

   

   
   

   function lkt_error_on_null_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Error_On_Null_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Error_On_Null_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_error_on_null_f_expr;


           

   

   
   

   function lkt_generic_instantiation_f_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Generic_Instantiation_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Generic_Instantiation_F_Name
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_generic_instantiation_f_name;


           

   

   
   

   function lkt_generic_instantiation_f_args
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Generic_Instantiation_Range then

         declare
            

            Result : Bare_Type_Ref_List;
         begin
            Result := Generic_Instantiation_F_Args
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_generic_instantiation_f_args;


           

   

   
   

   function lkt_grammar_discard_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_Discard_Range then

         declare
            

            Result : Bare_Grammar_Expr;
         begin
            Result := Grammar_Discard_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_discard_f_expr;


           

   

   
   

   function lkt_grammar_dont_skip_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_Dont_Skip_Range then

         declare
            

            Result : Bare_Grammar_Expr;
         begin
            Result := Grammar_Dont_Skip_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_dont_skip_f_expr;


           

   

   
   

   function lkt_grammar_dont_skip_f_dont_skip
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_Dont_Skip_Range then

         declare
            

            Result : Bare_Grammar_Expr;
         begin
            Result := Grammar_Dont_Skip_F_Dont_Skip
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_dont_skip_f_dont_skip;


           

   

   
   

   function lkt_grammar_list_f_list_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_List_Range then

         declare
            

            Result : Bare_Type_Ref;
         begin
            Result := Grammar_List_F_List_Type
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_list_f_list_type;


           

   

   
   

   function lkt_grammar_list_f_kind
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_List_Range then

         declare
            

            Result : Bare_List_Kind;
         begin
            Result := Grammar_List_F_Kind
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_list_f_kind;


           

   

   
   

   function lkt_grammar_list_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_List_Range then

         declare
            

            Result : Bare_Grammar_Expr;
         begin
            Result := Grammar_List_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_list_f_expr;


           

   

   
   

   function lkt_grammar_list_f_sep
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_List_Range then

         declare
            

            Result : Bare_Grammar_List_Sep;
         begin
            Result := Grammar_List_F_Sep
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_list_f_sep;


           

   

   
   

   function lkt_grammar_null_f_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_Null_Range then

         declare
            

            Result : Bare_Type_Ref;
         begin
            Result := Grammar_Null_F_Name
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_null_f_name;


           

   

   
   

   function lkt_grammar_opt_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_Opt_Range then

         declare
            

            Result : Bare_Grammar_Expr;
         begin
            Result := Grammar_Opt_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_opt_f_expr;


           

   

   
   

   function lkt_grammar_opt_error_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_Opt_Error_Range then

         declare
            

            Result : Bare_Grammar_Expr;
         begin
            Result := Grammar_Opt_Error_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_opt_error_f_expr;


           

   

   
   

   function lkt_grammar_opt_error_group_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_Opt_Error_Group_Range then

         declare
            

            Result : Bare_Grammar_Expr_List;
         begin
            Result := Grammar_Opt_Error_Group_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_opt_error_group_f_expr;


           

   

   
   

   function lkt_grammar_opt_group_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_Opt_Group_Range then

         declare
            

            Result : Bare_Grammar_Expr_List;
         begin
            Result := Grammar_Opt_Group_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_opt_group_f_expr;


           

   

   
   

   function lkt_grammar_or_expr_f_sub_exprs
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_Or_Expr_Range then

         declare
            

            Result : Bare_Grammar_Expr_List_List;
         begin
            Result := Grammar_Or_Expr_F_Sub_Exprs
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_or_expr_f_sub_exprs;


           

   

   
   

   function lkt_grammar_pick_f_exprs
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_Pick_Range then

         declare
            

            Result : Bare_Grammar_Expr_List;
         begin
            Result := Grammar_Pick_F_Exprs
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_pick_f_exprs;


           

   

   
   

   function lkt_grammar_predicate_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_Predicate_Range then

         declare
            

            Result : Bare_Grammar_Expr;
         begin
            Result := Grammar_Predicate_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_predicate_f_expr;


           

   

   
   

   function lkt_grammar_predicate_f_prop_ref
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_Predicate_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Grammar_Predicate_F_Prop_Ref
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_predicate_f_prop_ref;


           

   

   
   

   function lkt_grammar_rule_ref_f_node_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_Rule_Ref_Range then

         declare
            

            Result : Bare_Ref_Id;
         begin
            Result := Grammar_Rule_Ref_F_Node_Name
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_rule_ref_f_node_name;


           

   

   
   

   function lkt_grammar_skip_f_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_Skip_Range then

         declare
            

            Result : Bare_Type_Ref;
         begin
            Result := Grammar_Skip_F_Name
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_skip_f_name;


           

   

   
   

   function lkt_grammar_stop_cut_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_Stop_Cut_Range then

         declare
            

            Result : Bare_Grammar_Expr;
         begin
            Result := Grammar_Stop_Cut_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_stop_cut_f_expr;


           

   

   
   

   function lkt_parse_node_expr_f_node_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Parse_Node_Expr_Range then

         declare
            

            Result : Bare_Type_Ref;
         begin
            Result := Parse_Node_Expr_F_Node_Name
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_parse_node_expr_f_node_name;


           

   

   
   

   function lkt_parse_node_expr_f_sub_exprs
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Parse_Node_Expr_Range then

         declare
            

            Result : Bare_Grammar_Expr_List;
         begin
            Result := Parse_Node_Expr_F_Sub_Exprs
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_parse_node_expr_f_sub_exprs;


           

   

   
   

   function lkt_token_lit_p_denoted_value
     (Node : lkt_node_Ptr;


      Value_P : access lkt_internal_decoded_string_value) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Token_Lit_Range then

         declare
            

            Result : Internal_Decoded_String_Value;
         begin
            Result := Liblktlang.Implementation.Extensions.Token_Lit_P_Denoted_Value
              (Unwrapped_Node);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_token_lit_p_denoted_value;


           

   

   
   

   function lkt_token_no_case_lit_f_lit
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Token_No_Case_Lit_Range then

         declare
            

            Result : Bare_Token_Lit;
         begin
            Result := Token_No_Case_Lit_F_Lit
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_token_no_case_lit_f_lit;


           

   

   
   

   function lkt_token_pattern_concat_f_left
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Token_Pattern_Concat_Range then

         declare
            

            Result : Bare_Grammar_Expr;
         begin
            Result := Token_Pattern_Concat_F_Left
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_token_pattern_concat_f_left;


           

   

   
   

   function lkt_token_pattern_concat_f_right
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Token_Pattern_Concat_Range then

         declare
            

            Result : Bare_Token_Pattern_Lit;
         begin
            Result := Token_Pattern_Concat_F_Right
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_token_pattern_concat_f_right;


           

   

   
   

   function lkt_token_pattern_lit_p_denoted_value
     (Node : lkt_node_Ptr;


      Value_P : access lkt_internal_decoded_string_value) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Token_Pattern_Lit_Range then

         declare
            

            Result : Internal_Decoded_String_Value;
         begin
            Result := Liblktlang.Implementation.Extensions.Token_Pattern_Lit_P_Denoted_Value
              (Unwrapped_Node);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_token_pattern_lit_p_denoted_value;


           

   

   
   

   function lkt_token_ref_f_token_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Token_Ref_Range then

         declare
            

            Result : Bare_Ref_Id;
         begin
            Result := Token_Ref_F_Token_Name
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_token_ref_f_token_name;


           

   

   
   

   function lkt_token_ref_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Token_Ref_Range then

         declare
            

            Result : Bare_Token_Lit;
         begin
            Result := Token_Ref_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_token_ref_f_expr;


           

   

   
   

   function lkt_id_p_custom_image
     (Node : lkt_node_Ptr;


      Value_P : access lkt_string_type) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Id_Range then

         declare
            

            Result : String_Type;
         begin
            Result := Liblktlang.Implementation.Extensions.Id_P_Custom_Image
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_id_p_custom_image;


           

   

   
   

   function lkt_if_expr_f_cond_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_If_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := If_Expr_F_Cond_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_if_expr_f_cond_expr;


           

   

   
   

   function lkt_if_expr_f_then_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_If_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := If_Expr_F_Then_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_if_expr_f_then_expr;


           

   

   
   

   function lkt_if_expr_f_alternatives
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_If_Expr_Range then

         declare
            

            Result : Bare_Elsif_Branch_List;
         begin
            Result := If_Expr_F_Alternatives
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_if_expr_f_alternatives;


           

   

   
   

   function lkt_if_expr_f_else_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_If_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := If_Expr_F_Else_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_if_expr_f_else_expr;


           

   

   
   

   function lkt_isa_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Isa_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Isa_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_isa_f_expr;


           

   

   
   

   function lkt_isa_f_dest_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Isa_Range then

         declare
            

            Result : Bare_Isa_List;
         begin
            Result := Isa_F_Dest_Type
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_isa_f_dest_type;


           

   

   
   

   function lkt_keep_expr_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Keep_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Keep_Expr_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_keep_expr_f_expr;


           

   

   
   

   function lkt_keep_expr_f_null_cond
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Keep_Expr_Range then

         declare
            

            Result : Bare_Null_Cond_Qualifier;
         begin
            Result := Keep_Expr_F_Null_Cond
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_keep_expr_f_null_cond;


           

   

   
   

   function lkt_keep_expr_f_keep_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Keep_Expr_Range then

         declare
            

            Result : Bare_Type_Ref;
         begin
            Result := Keep_Expr_F_Keep_Type
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_keep_expr_f_keep_type;


           

   

   
   

   function lkt_lambda_expr_f_params
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Lambda_Expr_Range then

         declare
            

            Result : Bare_Lambda_Param_Decl_List;
         begin
            Result := Lambda_Expr_F_Params
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lambda_expr_f_params;


           

   

   
   

   function lkt_lambda_expr_f_return_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Lambda_Expr_Range then

         declare
            

            Result : Bare_Type_Ref;
         begin
            Result := Lambda_Expr_F_Return_Type
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lambda_expr_f_return_type;


           

   

   
   

   function lkt_lambda_expr_f_body
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Lambda_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Lambda_Expr_F_Body
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lambda_expr_f_body;


           

   

   
   

   function lkt_char_lit_p_denoted_value
     (Node : lkt_node_Ptr;


      Value_P : access lkt_internal_decoded_char_value) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Char_Lit_Range then

         declare
            

            Result : Internal_Decoded_Char_Value;
         begin
            Result := Liblktlang.Implementation.Extensions.Char_Lit_P_Denoted_Value
              (Unwrapped_Node);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_char_lit_p_denoted_value;


           

   

   
   

   function lkt_null_lit_f_dest_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Null_Lit_Range then

         declare
            

            Result : Bare_Type_Ref;
         begin
            Result := Null_Lit_F_Dest_Type
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_null_lit_f_dest_type;


           

   

   
   

   function lkt_string_lit_p_denoted_value
     (Node : lkt_node_Ptr;


      Value_P : access lkt_internal_decoded_string_value) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_String_Lit then

         declare
            

            Result : Internal_Decoded_String_Value;
         begin
            Result := Liblktlang.Implementation.Dispatcher_String_Lit_P_Denoted_Value
              (Unwrapped_Node);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_string_lit_p_denoted_value;


           

   

   
   

   function lkt_string_lit_p_is_prefixed_string
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_String_Lit then

         declare
            

            Result : Boolean;
         begin
            Result := Liblktlang.Implementation.Dispatcher_String_Lit_P_Is_Prefixed_String
              (Unwrapped_Node);

            Value_P.all :=
                   lkt_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_string_lit_p_is_prefixed_string;


           

   

   
   

   function lkt_string_lit_p_prefix
     (Node : lkt_node_Ptr;


      Value_P : access uint32_t) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_String_Lit then

         declare
            

            Result : Character_Type;
         begin
            Result := Liblktlang.Implementation.Dispatcher_String_Lit_P_Prefix
              (Unwrapped_Node);

            Value_P.all :=
                   Character_Type'Pos (Result)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_string_lit_p_prefix;


           

   

   
   

   function lkt_string_lit_p_is_regexp_literal
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_String_Lit then

         declare
            

            Result : Boolean;
         begin
            Result := Liblktlang.Implementation.String_Lit_P_Is_Regexp_Literal
              (Unwrapped_Node);

            Value_P.all :=
                   lkt_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_string_lit_p_is_regexp_literal;


           

   

   
   

   function lkt_block_string_lit_f_lines
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Block_String_Lit_Range then

         declare
            

            Result : Bare_Block_String_Line_List;
         begin
            Result := Block_String_Lit_F_Lines
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_block_string_lit_f_lines;


           

   

   
   

   function lkt_logic_assign_f_dest_var
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Logic_Assign_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Logic_Assign_F_Dest_Var
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_logic_assign_f_dest_var;


           

   

   
   

   function lkt_logic_assign_f_value
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Logic_Assign_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Logic_Assign_F_Value
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_logic_assign_f_value;


           

   

   
   

   function lkt_logic_expr_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Logic_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Logic_Expr_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_logic_expr_f_expr;


           

   

   
   

   function lkt_logic_propagate_f_dest_var
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Logic_Propagate_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Logic_Propagate_F_Dest_Var
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_logic_propagate_f_dest_var;


           

   

   
   

   function lkt_logic_propagate_f_call
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Logic_Propagate_Range then

         declare
            

            Result : Bare_Logic_Propagate_Call;
         begin
            Result := Logic_Propagate_F_Call
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_logic_propagate_f_call;


           

   

   
   

   function lkt_logic_unify_f_lhs
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Logic_Unify_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Logic_Unify_F_Lhs
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_logic_unify_f_lhs;


           

   

   
   

   function lkt_logic_unify_f_rhs
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Logic_Unify_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Logic_Unify_F_Rhs
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_logic_unify_f_rhs;


           

   

   
   

   function lkt_match_expr_f_match_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Match_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Match_Expr_F_Match_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_match_expr_f_match_expr;


           

   

   
   

   function lkt_match_expr_f_branches
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Match_Expr_Range then

         declare
            

            Result : Bare_Match_Branch_List;
         begin
            Result := Match_Expr_F_Branches
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_match_expr_f_branches;


           

   

   
   

   function lkt_not_expr_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Not_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Not_Expr_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_not_expr_f_expr;


           

   

   
   

   function lkt_paren_expr_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Paren_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Paren_Expr_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_paren_expr_f_expr;


           

   

   
   

   function lkt_raise_expr_f_dest_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Raise_Expr_Range then

         declare
            

            Result : Bare_Type_Ref;
         begin
            Result := Raise_Expr_F_Dest_Type
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_raise_expr_f_dest_type;


           

   

   
   

   function lkt_raise_expr_f_except_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Raise_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Raise_Expr_F_Except_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_raise_expr_f_except_expr;


           

   

   
   

   function lkt_subscript_expr_f_prefix
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Subscript_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Subscript_Expr_F_Prefix
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_subscript_expr_f_prefix;


           

   

   
   

   function lkt_subscript_expr_f_null_cond
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Subscript_Expr_Range then

         declare
            

            Result : Bare_Null_Cond_Qualifier;
         begin
            Result := Subscript_Expr_F_Null_Cond
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_subscript_expr_f_null_cond;


           

   

   
   

   function lkt_subscript_expr_f_index
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Subscript_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Subscript_Expr_F_Index
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_subscript_expr_f_index;


           

   

   
   

   function lkt_try_expr_f_try_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Try_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Try_Expr_F_Try_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_try_expr_f_try_expr;


           

   

   
   

   function lkt_try_expr_f_or_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Try_Expr_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Try_Expr_F_Or_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_try_expr_f_or_expr;


           

   

   
   

   function lkt_un_op_f_op
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Un_Op_Range then

         declare
            

            Result : Bare_Op;
         begin
            Result := Un_Op_F_Op
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_un_op_f_op;


           

   

   
   

   function lkt_un_op_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Un_Op_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Un_Op_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_un_op_f_expr;


           

   

   
   

   function lkt_full_decl_f_doc
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Full_Decl_Range then

         declare
            

            Result : Bare_String_Lit;
         begin
            Result := Full_Decl_F_Doc
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_full_decl_f_doc;


           

   

   
   

   function lkt_full_decl_f_decl_annotations
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Full_Decl_Range then

         declare
            

            Result : Bare_Decl_Annotation_List;
         begin
            Result := Full_Decl_F_Decl_Annotations
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_full_decl_f_decl_annotations;


           

   

   
   

   function lkt_full_decl_f_decl
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Full_Decl_Range then

         declare
            

            Result : Bare_Decl;
         begin
            Result := Full_Decl_F_Decl
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_full_decl_f_decl;


           

   

   
   

   function lkt_full_decl_p_has_annotation
     (Node : lkt_node_Ptr;

         Name :
            access constant
            lkt_symbol_type;

      Value_P : access lkt_bool) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
         
         Unwrapped_Name : constant Symbol_Type :=
               Unwrap_Symbol (Name.all)
         ;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Full_Decl_Range then

         declare
            

            Result : Boolean;
         begin
            Result := Liblktlang.Implementation.Full_Decl_P_Has_Annotation
              (Unwrapped_Node,
               Name => Unwrapped_Name);

            Value_P.all :=
                   lkt_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_full_decl_p_has_annotation;


           

   

   
   

   function lkt_grammar_list_sep_f_token
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_List_Sep_Range then

         declare
            

            Result : Bare_Grammar_Expr;
         begin
            Result := Grammar_List_Sep_F_Token
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_list_sep_f_token;


           

   

   
   

   function lkt_grammar_list_sep_f_extra
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Grammar_List_Sep_Range then

         declare
            

            Result : Bare_Id;
         begin
            Result := Grammar_List_Sep_F_Extra
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_grammar_list_sep_f_extra;


           

   

   
   

   function lkt_import_f_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Import_Range then

         declare
            

            Result : Bare_Module_Ref_Id;
         begin
            Result := Import_F_Name
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_import_f_name;


           

   

   
   

   function lkt_import_p_referenced_unit
     (Node : lkt_node_Ptr;


      Value_P : access lkt_analysis_unit) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Import_Range then

         declare
            

            Result : Internal_Unit;
         begin
            Result := Liblktlang.Implementation.Import_P_Referenced_Unit
              (Unwrapped_Node);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_import_p_referenced_unit;


           

   

   
   

   function lkt_langkit_root_f_imports
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Langkit_Root_Range then

         declare
            

            Result : Bare_Import_List;
         begin
            Result := Langkit_Root_F_Imports
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_langkit_root_f_imports;


           

   

   
   

   function lkt_langkit_root_f_decls
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Langkit_Root_Range then

         declare
            

            Result : Bare_Full_Decl_List;
         begin
            Result := Langkit_Root_F_Decls
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_langkit_root_f_decls;


           

   

   
   

   function lkt_langkit_root_p_fetch_prelude
     (Node : lkt_node_Ptr;


      Value_P : access lkt_analysis_unit) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Langkit_Root_Range then

         declare
            

            Result : Internal_Unit;
         begin
            Result := Liblktlang.Implementation.Extensions.Langkit_Root_P_Fetch_Prelude
              (Unwrapped_Node);

            Value_P.all :=
                   Result
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_langkit_root_p_fetch_prelude;


           

   

   
   

   function lkt_lexer_case_rule_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Lexer_Case_Rule_Range then

         declare
            

            Result : Bare_Grammar_Expr;
         begin
            Result := Lexer_Case_Rule_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lexer_case_rule_f_expr;


           

   

   
   

   function lkt_lexer_case_rule_f_alts
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Lexer_Case_Rule_Range then

         declare
            

            Result : Bare_Base_Lexer_Case_Rule_Alt_List;
         begin
            Result := Lexer_Case_Rule_F_Alts
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lexer_case_rule_f_alts;


           

   

   
   

   function lkt_lexer_case_rule_send_f_sent
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Lexer_Case_Rule_Send_Range then

         declare
            

            Result : Bare_Ref_Id;
         begin
            Result := Lexer_Case_Rule_Send_F_Sent
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lexer_case_rule_send_f_sent;


           

   

   
   

   function lkt_lexer_case_rule_send_f_match_size
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Lexer_Case_Rule_Send_Range then

         declare
            

            Result : Bare_Num_Lit;
         begin
            Result := Lexer_Case_Rule_Send_F_Match_Size
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_lexer_case_rule_send_f_match_size;


           

   

   
   

   function lkt_match_branch_f_decl
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Match_Branch_Range then

         declare
            

            Result : Bare_Match_Val_Decl;
         begin
            Result := Match_Branch_F_Decl
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_match_branch_f_decl;


           

   

   
   

   function lkt_match_branch_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Match_Branch_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Match_Branch_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_match_branch_f_expr;


           

   

   
   

   function lkt_null_cond_qualifier_p_as_bool
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Null_Cond_Qualifier then

         declare
            

            Result : Boolean;
         begin
            Result := Liblktlang.Implementation.Dispatcher_Null_Cond_Qualifier_P_As_Bool
              (Unwrapped_Node);

            Value_P.all :=
                   lkt_bool (Boolean'Pos (Result))
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_null_cond_qualifier_p_as_bool;


           

   

   
   

   function lkt_type_ref_p_referenced_decl
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Type_Ref then

         declare
            

            Result : Internal_Entity_Type_Decl;
         begin
            Result := Liblktlang.Implementation.Type_Ref_P_Referenced_Decl
              (Unwrapped_Node,
               E_Info => Node.Info);

            Value_P.all :=
                  (Result.Node, Result.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_type_ref_p_referenced_decl;


           

   

   
   

   function lkt_function_type_ref_f_param_types
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Function_Type_Ref_Range then

         declare
            

            Result : Bare_Type_Ref_List;
         begin
            Result := Function_Type_Ref_F_Param_Types
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_function_type_ref_f_param_types;


           

   

   
   

   function lkt_function_type_ref_f_return_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Function_Type_Ref_Range then

         declare
            

            Result : Bare_Type_Ref;
         begin
            Result := Function_Type_Ref_F_Return_Type
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_function_type_ref_f_return_type;


           

   

   
   

   function lkt_generic_type_ref_f_type_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Generic_Type_Ref_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Generic_Type_Ref_F_Type_Name
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_generic_type_ref_f_type_name;


           

   

   
   

   function lkt_generic_type_ref_f_args
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Generic_Type_Ref_Range then

         declare
            

            Result : Bare_Type_Ref_List;
         begin
            Result := Generic_Type_Ref_F_Args
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_generic_type_ref_f_args;


           

   

   
   

   function lkt_simple_type_ref_f_type_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Simple_Type_Ref_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Simple_Type_Ref_F_Type_Name
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_simple_type_ref_f_type_name;


           

   

   
   

   function lkt_var_bind_f_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Var_Bind_Range then

         declare
            

            Result : Bare_Ref_Id;
         begin
            Result := Var_Bind_F_Name
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_var_bind_f_name;


           

   

   
   

   function lkt_var_bind_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

   is
      Unwrapped_Node : constant Bare_Lkt_Node := Node.Node;
   begin
      Clear_Last_Exception;


      if Unwrapped_Node.Kind in Lkt_Var_Bind_Range then

         declare
            

            Result : Bare_Expr;
         begin
            Result := Var_Bind_F_Expr
              (Unwrapped_Node);

            Value_P.all :=
                   (Result, Node.Info)
            ;

            return 1;
         exception
            when Exc : Property_Error =>
               Set_Last_Exception (Exc);
               return 0;
         end;

      else
         return 0;
      end if;

   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end lkt_var_bind_f_expr;



         



procedure lkt_internal_decoded_char_value_inc_ref (R : lkt_internal_decoded_char_value_Ptr) is
begin
   Clear_Last_Exception;
   Inc_Ref (R.all);
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end lkt_internal_decoded_char_value_inc_ref;

procedure lkt_internal_decoded_char_value_dec_ref (R : lkt_internal_decoded_char_value_Ptr) is
begin
   Clear_Last_Exception;
   Dec_Ref (R.all);
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end lkt_internal_decoded_char_value_dec_ref;


         



procedure lkt_internal_decoded_string_value_inc_ref (R : lkt_internal_decoded_string_value_Ptr) is
begin
   Clear_Last_Exception;
   Inc_Ref (R.all);
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end lkt_internal_decoded_string_value_inc_ref;

procedure lkt_internal_decoded_string_value_dec_ref (R : lkt_internal_decoded_string_value_Ptr) is
begin
   Clear_Last_Exception;
   Dec_Ref (R.all);
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end lkt_internal_decoded_string_value_dec_ref;


         





         





         



procedure lkt_internal_solver_diagnostic_inc_ref (R : lkt_internal_solver_diagnostic_Ptr) is
begin
   Clear_Last_Exception;
   Inc_Ref (R.all);
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end lkt_internal_solver_diagnostic_inc_ref;

procedure lkt_internal_solver_diagnostic_dec_ref (R : lkt_internal_solver_diagnostic_Ptr) is
begin
   Clear_Last_Exception;
   Dec_Ref (R.all);
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end lkt_internal_solver_diagnostic_dec_ref;


         



procedure lkt_internal_solver_result_inc_ref (R : lkt_internal_solver_result_Ptr) is
begin
   Clear_Last_Exception;
   Inc_Ref (R.all);
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end lkt_internal_solver_result_inc_ref;

procedure lkt_internal_solver_result_dec_ref (R : lkt_internal_solver_result_Ptr) is
begin
   Clear_Last_Exception;
   Dec_Ref (R.all);
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end lkt_internal_solver_result_dec_ref;



         



function lkt_node_array_create (Length : int) return Internal_Entity_Array_Access is
begin
   Clear_Last_Exception;
   return Create_Internal_Entity_Array (Natural (Length));
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
      return null;
end lkt_node_array_create;

procedure lkt_node_array_inc_ref (A : Internal_Entity_Array_Access) is
begin
   Clear_Last_Exception;
   Inc_Ref (A);
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end;

procedure lkt_node_array_dec_ref (A : Internal_Entity_Array_Access) is
begin
   Clear_Last_Exception;
   declare
      A_Var : Internal_Entity_Array_Access := A;
   begin
      Dec_Ref (A_Var);
   end;
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end;


         



function lkt_internal_logic_context_array_create (Length : int) return Internal_Logic_Context_Array_Access is
begin
   Clear_Last_Exception;
   return Create_Internal_Logic_Context_Array (Natural (Length));
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
      return null;
end lkt_internal_logic_context_array_create;

procedure lkt_internal_logic_context_array_inc_ref (A : Internal_Logic_Context_Array_Access) is
begin
   Clear_Last_Exception;
   Inc_Ref (A);
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end;

procedure lkt_internal_logic_context_array_dec_ref (A : Internal_Logic_Context_Array_Access) is
begin
   Clear_Last_Exception;
   declare
      A_Var : Internal_Logic_Context_Array_Access := A;
   begin
      Dec_Ref (A_Var);
   end;
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end;


         



function lkt_internal_solver_diagnostic_array_create (Length : int) return Internal_Solver_Diagnostic_Array_Access is
begin
   Clear_Last_Exception;
   return Create_Internal_Solver_Diagnostic_Array (Natural (Length));
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
      return null;
end lkt_internal_solver_diagnostic_array_create;

procedure lkt_internal_solver_diagnostic_array_inc_ref (A : Internal_Solver_Diagnostic_Array_Access) is
begin
   Clear_Last_Exception;
   Inc_Ref (A);
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end;

procedure lkt_internal_solver_diagnostic_array_dec_ref (A : Internal_Solver_Diagnostic_Array_Access) is
begin
   Clear_Last_Exception;
   declare
      A_Var : Internal_Solver_Diagnostic_Array_Access := A;
   begin
      Dec_Ref (A_Var);
   end;
exception
   when Exc : others =>
      Set_Last_Exception (Exc);
end;




end Liblktlang.Implementation.C;
