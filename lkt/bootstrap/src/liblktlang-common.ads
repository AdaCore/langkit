


--  To facilitate use from a -gnatX project, since we don't use the [] syntax
pragma Warnings (Off, "obsolescent");

with GNATCOLL.GMP.Integers;

with Liblktlang_Support.Errors;
private with Liblktlang_Support.Internal.Analysis;
with Liblktlang_Support.Symbols; use Liblktlang_Support.Symbols;
with Liblktlang_Support.Token_Data_Handlers;
use Liblktlang_Support.Token_Data_Handlers;
with Liblktlang_Support.Types;   use Liblktlang_Support.Types;


--  This package provides types and functions used in the whole Liblktlang
--  package tree.

package Liblktlang.Common is

   use Support.Slocs, Support.Text;

   subtype Big_Integer is GNATCOLL.GMP.Integers.Big_Integer;
   --  Shortcut for ``GNATCOLL.GMP.Integers.Big_Integer``

   

   Default_Charset : constant String :=
     "utf-8";
   --  Default charset to use when creating analysis contexts

   ----------------
   -- Exceptions --
   ----------------

   File_Read_Error : exception renames Liblktlang_Support.Errors.File_Read_Error;
   --  Subprograms may raise this when they cannot open a source file. Note
   --  that this does *not* concern analysis unit getters, which create
   --  diagnostic vectors for such errors.

   Invalid_Input : exception renames Liblktlang_Support.Errors.Invalid_Input;
   --  Raised by lexing functions (``Liblktlang.Lexer``) when the input
   --  contains an invalid byte sequence.

   Invalid_Symbol_Error : exception renames Liblktlang_Support.Errors.Invalid_Symbol_Error;
   --  Exception raise when an invalid symbol is passed to a subprogram.

   Invalid_Unit_Name_Error : exception renames Liblktlang_Support.Errors.Invalid_Unit_Name_Error;
   --  Raised when an invalid unit name is provided.

   Native_Exception : exception renames Liblktlang_Support.Errors.Native_Exception;
   --  Exception raised in language bindings when the underlying C API reports
   --  an unexpected error that occurred in the library.
   --
   --  This kind of exception is raised for internal errors: they should never
   --  happen in normal situations and if they are raised at some point, it
   --  means the library state is potentially corrupted.
   --
   --  Nevertheless, the library does its best not to crash the program,
   --  materializing internal errors using this kind of exception.

   Precondition_Failure : exception renames Liblktlang_Support.Errors.Precondition_Failure;
   --  Exception raised when an API is called while its preconditions are not
   --  satisfied.

   Property_Error : exception renames Liblktlang_Support.Errors.Property_Error;
   --  Exception that is raised when an error occurs while evaluating any
   --  function whose name starts with ``P_``. This is the only exceptions that
   --  such functions can raise.

   Stale_Reference_Error : exception renames Liblktlang_Support.Errors.Stale_Reference_Error;
   --  Exception raised while trying to access data that was deallocated. This
   --  happens when one tries to use a node whose unit has been reparsed, for
   --  instance.

   Syntax_Error : exception renames Liblktlang_Support.Errors.Syntax_Error;
   --  Subprograms may raise this when they try to parse invalid syntax. Note
   --  that this does *not* concern analysis unit getters, which create
   --  diagnostic vectors for such errors.

   Unknown_Charset : exception renames Liblktlang_Support.Errors.Unknown_Charset;
   --  Raised by lexing functions (``Liblktlang.Lexer``) when the input charset
   --  is not supported.

   -------------------
   -- Introspection --
   -------------------

   Bad_Type_Error : exception renames Liblktlang_Support.Errors.Introspection.Bad_Type_Error;
   --  Raised when introspection functions (``Liblktlang.Introspection``) are
   --  provided mismatching types/values.

   Out_Of_Bounds_Error : exception renames Liblktlang_Support.Errors.Introspection.Out_Of_Bounds_Error;
   --  Raised when introspection functions (``Liblktlang.Introspection``) are
   --  passed an out of bounds index.

   ---------------
   -- Rewriting --
   ---------------

   Template_Args_Error : exception renames Liblktlang_Support.Errors.Rewriting.Template_Args_Error;
   --  Exception raised when the provided arguments for a template don't match
   --  what the template expects.

   Template_Format_Error : exception renames Liblktlang_Support.Errors.Rewriting.Template_Format_Error;
   --  Exception raised when a template has an invalid syntax, such as badly
   --  formatted placeholders.

   Template_Instantiation_Error : exception renames Liblktlang_Support.Errors.Rewriting.Template_Instantiation_Error;
   --  Exception raised when the instantiation of a template cannot be parsed.

   ---------------
   -- Unparsing --
   ---------------

   Malformed_Tree_Error : exception renames Liblktlang_Support.Errors.Unparsing.Malformed_Tree_Error;
   --  Raised when unparsing functions working on rewritten trees
   --  (``Liblktlang.Rewriting``) are called on malformed trees.


   ----------------------------
   -- Misc enumeration types --
   ----------------------------

      type Analysis_Unit_Kind is
        (Unit_Specification,
         Unit_Body)
      with Convention => C;
      --  Specify a kind of analysis unit. Specification units provide an
      --  interface to the outer world while body units provide an
      --  implementation for the corresponding interface.

      function Trace_Image (Self : Analysis_Unit_Kind) return String
      is (Self'Image);

      type Completion_Item_Kind is
        (Text_Kind,
         Method_Kind,
         Function_Kind,
         Constructor_Kind,
         Field_Kind,
         Variable_Kind,
         Class_Kind,
         Interface_Kind,
         Module_Kind,
         Property_Kind,
         Unit_Kind,
         Value_Kind,
         Enum_Kind,
         Keyword_Kind,
         Snippet_Kind,
         Color_Kind,
         File_Kind,
         Reference_Kind,
         Folder_Kind,
         Enum_Member_Kind,
         Constant_Kind,
         Struct_Kind,
         Event_Kind,
         Operator_Kind,
         Type_Parameter_Kind)
      with Convention => C;
      --  Type of completion item. Refer to the official LSP specification.

      function Trace_Image (Self : Completion_Item_Kind) return String
      is (Self'Image);

      type Designated_Env_Kind is
        (None,
         Current_Env,
         Named_Env,
         Direct_Env)
      with Convention => C;
      --  Discriminant for DesignatedEnv structures.

      function Trace_Image (Self : Designated_Env_Kind) return String
      is (Self'Image);

      type Grammar_Rule is
        (Main_Rule_Rule,
         Id_Rule,
         Ref_Id_Rule,
         Type_Ref_Id_Rule,
         Def_Id_Rule,
         Doc_Rule,
         Import_Stmt_Rule,
         Imports_Rule,
         Lexer_Decl_Rule,
         Grammar_Decl_Rule,
         Grammar_Rule_Rule,
         Lexer_Rule_Rule,
         Lexer_Family_Decl_Rule,
         Lexer_Case_Rule_Rule,
         Lexer_Case_Alt_Rule,
         Lexer_Case_Send_Rule,
         Grammar_Primary_Rule,
         Grammar_Expr_Rule,
         Grammar_Pick_Rule,
         Grammar_Implicit_Pick_Rule,
         Grammar_Opt_Rule,
         Grammar_Opt_Error_Rule,
         Grammar_Cut_Rule,
         Grammar_Stopcut_Rule,
         Grammar_Or_Expr_Rule,
         Grammar_Discard_Expr_Rule,
         Token_Literal_Rule,
         Token_No_Case_Literal_Rule,
         Token_Pattern_Rule,
         Token_Pattern_Literal_Rule,
         Parse_Node_Expr_Rule,
         Grammar_Rule_Ref_Rule,
         Grammar_List_Expr_Rule,
         Grammar_List_Sep_Rule,
         Grammar_Skip_Rule,
         Grammar_Null_Rule,
         Grammar_Token_Rule,
         Type_Decl_Rule,
         Generic_Decl_Rule,
         Generic_Param_Type_Rule,
         Enum_Lit_Decl_Rule,
         Fun_Decl_Rule,
         Lambda_Param_Decl_Rule,
         Fun_Param_Decl_Rule,
         Fun_Param_List_Rule,
         Lambda_Param_List_Rule,
         Field_Decl_Rule,
         Bare_Decl_Rule,
         Decl_Rule,
         Type_Member_Ref_Rule,
         Type_Expr_Rule,
         Type_Ref_Rule,
         Type_List_Rule,
         Decls_Rule,
         Decl_Block_Rule,
         Val_Decl_Rule,
         Dynvar_Decl_Rule,
         Var_Bind_Rule,
         Env_Spec_Action_Rule,
         Env_Spec_Decl_Rule,
         Block_Rule,
         Pattern_Rule,
         Fil_Pattern_Rule,
         Value_Pattern_Rule,
         Regex_Pattern_Rule,
         Bool_Pattern_Rule,
         Ellipsis_Pattern_Rule,
         Integer_Pattern_Rule,
         List_Pattern_Rule,
         Tuple_Pattern_Rule,
         Pattern_Arg_Rule,
         Selector_Call_Rule,
         Expr_Rule,
         Rel_Rule,
         Eq_Rule,
         Arith_1_Rule,
         Arith_2_Rule,
         Arith_3_Rule,
         Isa_Or_Primary_Rule,
         Logic_Propagate_Call_Rule,
         Primary_Rule,
         Match_Expr_Rule,
         Num_Lit_Rule,
         Big_Num_Lit_Rule,
         String_Lit_Rule,
         Block_String_Lit_Rule,
         Char_Lit_Rule,
         If_Expr_Rule,
         Raise_Expr_Rule,
         Try_Expr_Rule,
         Array_Literal_Rule,
         Callable_Ref_Rule,
         Null_Cond_Qual_Rule,
         Basic_Expr_Rule,
         Term_Rule,
         Basic_Name_Rule,
         Lambda_Expr_Rule,
         Null_Lit_Rule,
         Argument_Rule,
         Args_Rule,
         Decl_Annotation_Args_Rule,
         Decl_Annotation_Rule)
      with Convention => C;
      --  Gramar rule to use for parsing.

      function Trace_Image (Self : Grammar_Rule) return String
      is (Self'Image);

      type Lookup_Kind is
        (Recursive,
         Flat,
         Minimal)
      with Convention => C;
      

      function Trace_Image (Self : Lookup_Kind) return String
      is (Self'Image);


   -----------
   -- Nodes --
   -----------

   type Lkt_Node_Kind_Type is
     (Lkt_Argument,
      Lkt_Lexer_Case_Rule_Cond_Alt,
      Lkt_Lexer_Case_Rule_Default_Alt,
      Lkt_Match_Branch,
      Lkt_Pattern_Match_Branch,
      Lkt_Block_String_Line,
      Lkt_Class_Qualifier_Absent,
      Lkt_Class_Qualifier_Present,
      Lkt_Grammar_Rule_Decl,
      Lkt_Synthetic_Lexer_Decl,
      Lkt_Node_Decl,
      Lkt_Self_Decl,
      Lkt_Binding_Val_Decl,
      Lkt_Enum_Lit_Decl,
      Lkt_Field_Decl,
      Lkt_Fun_Param_Decl,
      Lkt_Lambda_Param_Decl,
      Lkt_Dyn_Var_Decl,
      Lkt_Match_Val_Decl,
      Lkt_Val_Decl,
      Lkt_Fun_Decl,
      Lkt_Env_Spec_Decl,
      Lkt_Generic_Decl,
      Lkt_Grammar_Decl,
      Lkt_Lexer_Decl,
      Lkt_Lexer_Family_Decl,
      Lkt_Synth_Fun_Decl,
      Lkt_Synth_Param_Decl,
      Lkt_Any_Type_Decl,
      Lkt_Enum_Class_Alt_Decl,
      Lkt_Function_Type,
      Lkt_Generic_Param_Type_Decl,
      Lkt_Class_Decl,
      Lkt_Enum_Class_Decl,
      Lkt_Enum_Type_Decl,
      Lkt_Struct_Decl,
      Lkt_Trait_Decl,
      Lkt_Decl_Annotation,
      Lkt_Decl_Annotation_Args,
      Lkt_Dyn_Env_Wrapper,
      Lkt_Elsif_Branch,
      Lkt_Enum_Class_Case,
      Lkt_Excludes_Null_Absent,
      Lkt_Excludes_Null_Present,
      Lkt_Any_Of,
      Lkt_Array_Literal,
      Lkt_Call_Expr,
      Lkt_Logic_Predicate,
      Lkt_Logic_Propagate_Call,
      Lkt_Bin_Op,
      Lkt_Block_Expr,
      Lkt_Cast_Expr,
      Lkt_Dot_Expr,
      Lkt_Error_On_Null,
      Lkt_Generic_Instantiation,
      Lkt_Grammar_Cut,
      Lkt_Grammar_Discard,
      Lkt_Grammar_Dont_Skip,
      Lkt_Grammar_List,
      Lkt_Grammar_Null,
      Lkt_Grammar_Opt,
      Lkt_Grammar_Opt_Error,
      Lkt_Grammar_Opt_Error_Group,
      Lkt_Grammar_Opt_Group,
      Lkt_Grammar_Or_Expr,
      Lkt_Grammar_Pick,
      Lkt_Grammar_Implicit_Pick,
      Lkt_Grammar_Predicate,
      Lkt_Grammar_Rule_Ref,
      Lkt_Grammar_Skip,
      Lkt_Grammar_Stop_Cut,
      Lkt_Parse_Node_Expr,
      Lkt_Token_Lit,
      Lkt_Token_No_Case_Lit,
      Lkt_Token_Pattern_Concat,
      Lkt_Token_Pattern_Lit,
      Lkt_Token_Ref,
      Lkt_Id,
      Lkt_Def_Id,
      Lkt_Module_Ref_Id,
      Lkt_Ref_Id,
      Lkt_If_Expr,
      Lkt_Isa,
      Lkt_Keep_Expr,
      Lkt_Lambda_Expr,
      Lkt_Big_Num_Lit,
      Lkt_Char_Lit,
      Lkt_Null_Lit,
      Lkt_Num_Lit,
      Lkt_Block_String_Lit,
      Lkt_Single_Line_String_Lit,
      Lkt_Pattern_Single_Line_String_Lit,
      Lkt_Logic_Assign,
      Lkt_Logic_Expr,
      Lkt_Logic_Propagate,
      Lkt_Logic_Unify,
      Lkt_Match_Expr,
      Lkt_Not_Expr,
      Lkt_Paren_Expr,
      Lkt_Raise_Expr,
      Lkt_Subscript_Expr,
      Lkt_Try_Expr,
      Lkt_Un_Op,
      Lkt_Full_Decl,
      Lkt_Grammar_List_Sep,
      Lkt_Import,
      Lkt_Langkit_Root,
      Lkt_Lexer_Case_Rule,
      Lkt_Lexer_Case_Rule_Send,
      Lkt_List_Kind_One,
      Lkt_List_Kind_Zero,
      Lkt_Argument_List,
      Lkt_Base_Lexer_Case_Rule_Alt_List,
      Lkt_Base_Match_Branch_List,
      Lkt_Block_String_Line_List,
      Lkt_Call_Expr_List,
      Lkt_Decl_Annotation_List,
      Lkt_Elsif_Branch_List,
      Lkt_Enum_Class_Alt_Decl_List,
      Lkt_Enum_Class_Case_List,
      Lkt_Enum_Lit_Decl_List,
      Lkt_Expr_List,
      Lkt_Any_Of_List,
      Lkt_Full_Decl_List,
      Lkt_Decl_Block,
      Lkt_Generic_Param_Decl_List,
      Lkt_Fun_Param_Decl_List,
      Lkt_Grammar_Expr_List,
      Lkt_Grammar_Expr_List_List,
      Lkt_Import_List,
      Lkt_Lambda_Param_Decl_List,
      Lkt_Lkt_Node_List,
      Lkt_Block_Decl_List,
      Lkt_Pattern_Detail_List,
      Lkt_Pattern_List,
      Lkt_Ref_Id_List,
      Lkt_Type_Ref_List,
      Lkt_Synthetic_Type_Ref_List,
      Lkt_Null_Cond_Qualifier_Absent,
      Lkt_Null_Cond_Qualifier_Present,
      Lkt_Op_Amp,
      Lkt_Op_And,
      Lkt_Op_Div,
      Lkt_Op_Eq,
      Lkt_Op_Gt,
      Lkt_Op_Gte,
      Lkt_Op_Logic_And,
      Lkt_Op_Logic_Or,
      Lkt_Op_Lt,
      Lkt_Op_Lte,
      Lkt_Op_Minus,
      Lkt_Op_Mult,
      Lkt_Op_Ne,
      Lkt_Op_Or,
      Lkt_Op_Or_Int,
      Lkt_Op_Plus,
      Lkt_Any_Type_Pattern,
      Lkt_Binding_Pattern,
      Lkt_Bool_Pattern_False,
      Lkt_Bool_Pattern_True,
      Lkt_Ellipsis_Pattern,
      Lkt_Extended_Pattern,
      Lkt_Filtered_Pattern,
      Lkt_Integer_Pattern,
      Lkt_List_Pattern,
      Lkt_Not_Pattern,
      Lkt_Null_Pattern,
      Lkt_Or_Pattern,
      Lkt_Paren_Pattern,
      Lkt_Regex_Pattern,
      Lkt_Tuple_Pattern,
      Lkt_Type_Pattern,
      Lkt_Field_Pattern_Detail,
      Lkt_Property_Pattern_Detail,
      Lkt_Selector_Pattern_Detail,
      Lkt_Selector_Call,
      Lkt_Default_List_Type_Ref,
      Lkt_Function_Type_Ref,
      Lkt_Generic_Type_Ref,
      Lkt_Simple_Type_Ref,
      Lkt_Var_Bind);
   --  Type for concrete nodes

   for Lkt_Node_Kind_Type use
     (Lkt_Argument => 1,
      Lkt_Lexer_Case_Rule_Cond_Alt => 2,
      Lkt_Lexer_Case_Rule_Default_Alt => 3,
      Lkt_Match_Branch => 4,
      Lkt_Pattern_Match_Branch => 5,
      Lkt_Block_String_Line => 6,
      Lkt_Class_Qualifier_Absent => 7,
      Lkt_Class_Qualifier_Present => 8,
      Lkt_Grammar_Rule_Decl => 9,
      Lkt_Synthetic_Lexer_Decl => 10,
      Lkt_Node_Decl => 11,
      Lkt_Self_Decl => 12,
      Lkt_Binding_Val_Decl => 13,
      Lkt_Enum_Lit_Decl => 14,
      Lkt_Field_Decl => 15,
      Lkt_Fun_Param_Decl => 16,
      Lkt_Lambda_Param_Decl => 17,
      Lkt_Dyn_Var_Decl => 18,
      Lkt_Match_Val_Decl => 19,
      Lkt_Val_Decl => 20,
      Lkt_Fun_Decl => 21,
      Lkt_Env_Spec_Decl => 22,
      Lkt_Generic_Decl => 23,
      Lkt_Grammar_Decl => 24,
      Lkt_Lexer_Decl => 25,
      Lkt_Lexer_Family_Decl => 26,
      Lkt_Synth_Fun_Decl => 27,
      Lkt_Synth_Param_Decl => 28,
      Lkt_Any_Type_Decl => 29,
      Lkt_Enum_Class_Alt_Decl => 30,
      Lkt_Function_Type => 31,
      Lkt_Generic_Param_Type_Decl => 32,
      Lkt_Class_Decl => 33,
      Lkt_Enum_Class_Decl => 34,
      Lkt_Enum_Type_Decl => 35,
      Lkt_Struct_Decl => 36,
      Lkt_Trait_Decl => 37,
      Lkt_Decl_Annotation => 38,
      Lkt_Decl_Annotation_Args => 39,
      Lkt_Dyn_Env_Wrapper => 40,
      Lkt_Elsif_Branch => 41,
      Lkt_Enum_Class_Case => 42,
      Lkt_Excludes_Null_Absent => 43,
      Lkt_Excludes_Null_Present => 44,
      Lkt_Any_Of => 45,
      Lkt_Array_Literal => 46,
      Lkt_Call_Expr => 47,
      Lkt_Logic_Predicate => 48,
      Lkt_Logic_Propagate_Call => 49,
      Lkt_Bin_Op => 50,
      Lkt_Block_Expr => 51,
      Lkt_Cast_Expr => 52,
      Lkt_Dot_Expr => 53,
      Lkt_Error_On_Null => 54,
      Lkt_Generic_Instantiation => 55,
      Lkt_Grammar_Cut => 56,
      Lkt_Grammar_Discard => 57,
      Lkt_Grammar_Dont_Skip => 58,
      Lkt_Grammar_List => 59,
      Lkt_Grammar_Null => 60,
      Lkt_Grammar_Opt => 61,
      Lkt_Grammar_Opt_Error => 62,
      Lkt_Grammar_Opt_Error_Group => 63,
      Lkt_Grammar_Opt_Group => 64,
      Lkt_Grammar_Or_Expr => 65,
      Lkt_Grammar_Pick => 66,
      Lkt_Grammar_Implicit_Pick => 67,
      Lkt_Grammar_Predicate => 68,
      Lkt_Grammar_Rule_Ref => 69,
      Lkt_Grammar_Skip => 70,
      Lkt_Grammar_Stop_Cut => 71,
      Lkt_Parse_Node_Expr => 72,
      Lkt_Token_Lit => 73,
      Lkt_Token_No_Case_Lit => 74,
      Lkt_Token_Pattern_Concat => 75,
      Lkt_Token_Pattern_Lit => 76,
      Lkt_Token_Ref => 77,
      Lkt_Id => 78,
      Lkt_Def_Id => 79,
      Lkt_Module_Ref_Id => 80,
      Lkt_Ref_Id => 81,
      Lkt_If_Expr => 82,
      Lkt_Isa => 83,
      Lkt_Keep_Expr => 84,
      Lkt_Lambda_Expr => 85,
      Lkt_Big_Num_Lit => 86,
      Lkt_Char_Lit => 87,
      Lkt_Null_Lit => 88,
      Lkt_Num_Lit => 89,
      Lkt_Block_String_Lit => 90,
      Lkt_Single_Line_String_Lit => 91,
      Lkt_Pattern_Single_Line_String_Lit => 92,
      Lkt_Logic_Assign => 93,
      Lkt_Logic_Expr => 94,
      Lkt_Logic_Propagate => 95,
      Lkt_Logic_Unify => 96,
      Lkt_Match_Expr => 97,
      Lkt_Not_Expr => 98,
      Lkt_Paren_Expr => 99,
      Lkt_Raise_Expr => 100,
      Lkt_Subscript_Expr => 101,
      Lkt_Try_Expr => 102,
      Lkt_Un_Op => 103,
      Lkt_Full_Decl => 104,
      Lkt_Grammar_List_Sep => 105,
      Lkt_Import => 106,
      Lkt_Langkit_Root => 107,
      Lkt_Lexer_Case_Rule => 108,
      Lkt_Lexer_Case_Rule_Send => 109,
      Lkt_List_Kind_One => 110,
      Lkt_List_Kind_Zero => 111,
      Lkt_Argument_List => 112,
      Lkt_Base_Lexer_Case_Rule_Alt_List => 113,
      Lkt_Base_Match_Branch_List => 114,
      Lkt_Block_String_Line_List => 115,
      Lkt_Call_Expr_List => 116,
      Lkt_Decl_Annotation_List => 117,
      Lkt_Elsif_Branch_List => 118,
      Lkt_Enum_Class_Alt_Decl_List => 119,
      Lkt_Enum_Class_Case_List => 120,
      Lkt_Enum_Lit_Decl_List => 121,
      Lkt_Expr_List => 122,
      Lkt_Any_Of_List => 123,
      Lkt_Full_Decl_List => 124,
      Lkt_Decl_Block => 125,
      Lkt_Generic_Param_Decl_List => 126,
      Lkt_Fun_Param_Decl_List => 127,
      Lkt_Grammar_Expr_List => 128,
      Lkt_Grammar_Expr_List_List => 129,
      Lkt_Import_List => 130,
      Lkt_Lambda_Param_Decl_List => 131,
      Lkt_Lkt_Node_List => 132,
      Lkt_Block_Decl_List => 133,
      Lkt_Pattern_Detail_List => 134,
      Lkt_Pattern_List => 135,
      Lkt_Ref_Id_List => 136,
      Lkt_Type_Ref_List => 137,
      Lkt_Synthetic_Type_Ref_List => 138,
      Lkt_Null_Cond_Qualifier_Absent => 139,
      Lkt_Null_Cond_Qualifier_Present => 140,
      Lkt_Op_Amp => 141,
      Lkt_Op_And => 142,
      Lkt_Op_Div => 143,
      Lkt_Op_Eq => 144,
      Lkt_Op_Gt => 145,
      Lkt_Op_Gte => 146,
      Lkt_Op_Logic_And => 147,
      Lkt_Op_Logic_Or => 148,
      Lkt_Op_Lt => 149,
      Lkt_Op_Lte => 150,
      Lkt_Op_Minus => 151,
      Lkt_Op_Mult => 152,
      Lkt_Op_Ne => 153,
      Lkt_Op_Or => 154,
      Lkt_Op_Or_Int => 155,
      Lkt_Op_Plus => 156,
      Lkt_Any_Type_Pattern => 157,
      Lkt_Binding_Pattern => 158,
      Lkt_Bool_Pattern_False => 159,
      Lkt_Bool_Pattern_True => 160,
      Lkt_Ellipsis_Pattern => 161,
      Lkt_Extended_Pattern => 162,
      Lkt_Filtered_Pattern => 163,
      Lkt_Integer_Pattern => 164,
      Lkt_List_Pattern => 165,
      Lkt_Not_Pattern => 166,
      Lkt_Null_Pattern => 167,
      Lkt_Or_Pattern => 168,
      Lkt_Paren_Pattern => 169,
      Lkt_Regex_Pattern => 170,
      Lkt_Tuple_Pattern => 171,
      Lkt_Type_Pattern => 172,
      Lkt_Field_Pattern_Detail => 173,
      Lkt_Property_Pattern_Detail => 174,
      Lkt_Selector_Pattern_Detail => 175,
      Lkt_Selector_Call => 176,
      Lkt_Default_List_Type_Ref => 177,
      Lkt_Function_Type_Ref => 178,
      Lkt_Generic_Type_Ref => 179,
      Lkt_Simple_Type_Ref => 180,
      Lkt_Var_Bind => 181);

      subtype Lkt_Lkt_Node is Lkt_Node_Kind_Type
            range Lkt_Argument .. Lkt_Var_Bind;
      --% no-document: True
      subtype Lkt_Argument_Range is Lkt_Node_Kind_Type
            range Lkt_Argument .. Lkt_Argument;
      --% no-document: True
      subtype Lkt_Base_Lexer_Case_Rule_Alt is Lkt_Node_Kind_Type
            range Lkt_Lexer_Case_Rule_Cond_Alt .. Lkt_Lexer_Case_Rule_Default_Alt;
      --% no-document: True
      subtype Lkt_Lexer_Case_Rule_Cond_Alt_Range is Lkt_Node_Kind_Type
            range Lkt_Lexer_Case_Rule_Cond_Alt .. Lkt_Lexer_Case_Rule_Cond_Alt;
      --% no-document: True
      subtype Lkt_Lexer_Case_Rule_Default_Alt_Range is Lkt_Node_Kind_Type
            range Lkt_Lexer_Case_Rule_Default_Alt .. Lkt_Lexer_Case_Rule_Default_Alt;
      --% no-document: True
      subtype Lkt_Base_Match_Branch is Lkt_Node_Kind_Type
            range Lkt_Match_Branch .. Lkt_Pattern_Match_Branch;
      --% no-document: True
      subtype Lkt_Match_Branch_Range is Lkt_Node_Kind_Type
            range Lkt_Match_Branch .. Lkt_Match_Branch;
      --% no-document: True
      subtype Lkt_Pattern_Match_Branch_Range is Lkt_Node_Kind_Type
            range Lkt_Pattern_Match_Branch .. Lkt_Pattern_Match_Branch;
      --% no-document: True
      subtype Lkt_Block_String_Line_Range is Lkt_Node_Kind_Type
            range Lkt_Block_String_Line .. Lkt_Block_String_Line;
      --% no-document: True
      subtype Lkt_Class_Qualifier is Lkt_Node_Kind_Type
            range Lkt_Class_Qualifier_Absent .. Lkt_Class_Qualifier_Present;
      --% no-document: True
      subtype Lkt_Class_Qualifier_Absent_Range is Lkt_Node_Kind_Type
            range Lkt_Class_Qualifier_Absent .. Lkt_Class_Qualifier_Absent;
      --% no-document: True
      subtype Lkt_Class_Qualifier_Present_Range is Lkt_Node_Kind_Type
            range Lkt_Class_Qualifier_Present .. Lkt_Class_Qualifier_Present;
      --% no-document: True
      subtype Lkt_Decl is Lkt_Node_Kind_Type
            range Lkt_Grammar_Rule_Decl .. Lkt_Trait_Decl;
      --% no-document: True
      subtype Lkt_Base_Grammar_Rule_Decl is Lkt_Node_Kind_Type
            range Lkt_Grammar_Rule_Decl .. Lkt_Synthetic_Lexer_Decl;
      --% no-document: True
      subtype Lkt_Grammar_Rule_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Rule_Decl .. Lkt_Grammar_Rule_Decl;
      --% no-document: True
      subtype Lkt_Synthetic_Lexer_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Synthetic_Lexer_Decl .. Lkt_Synthetic_Lexer_Decl;
      --% no-document: True
      subtype Lkt_Base_Val_Decl is Lkt_Node_Kind_Type
            range Lkt_Node_Decl .. Lkt_Fun_Decl;
      --% no-document: True
      subtype Lkt_Node_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Node_Decl .. Lkt_Node_Decl;
      --% no-document: True
      subtype Lkt_Self_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Self_Decl .. Lkt_Self_Decl;
      --% no-document: True
      subtype Lkt_User_Val_Decl is Lkt_Node_Kind_Type
            range Lkt_Binding_Val_Decl .. Lkt_Fun_Decl;
      --% no-document: True
      subtype Lkt_Binding_Val_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Binding_Val_Decl .. Lkt_Binding_Val_Decl;
      --% no-document: True
      subtype Lkt_Enum_Lit_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Enum_Lit_Decl .. Lkt_Enum_Lit_Decl;
      --% no-document: True
      subtype Lkt_Explicitly_Typed_Decl is Lkt_Node_Kind_Type
            range Lkt_Field_Decl .. Lkt_Val_Decl;
      --% no-document: True
      subtype Lkt_Component_Decl is Lkt_Node_Kind_Type
            range Lkt_Field_Decl .. Lkt_Lambda_Param_Decl;
      --% no-document: True
      subtype Lkt_Field_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Field_Decl .. Lkt_Field_Decl;
      --% no-document: True
      subtype Lkt_Fun_Param_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Fun_Param_Decl .. Lkt_Fun_Param_Decl;
      --% no-document: True
      subtype Lkt_Lambda_Param_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Lambda_Param_Decl .. Lkt_Lambda_Param_Decl;
      --% no-document: True
      subtype Lkt_Dyn_Var_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Dyn_Var_Decl .. Lkt_Dyn_Var_Decl;
      --% no-document: True
      subtype Lkt_Match_Val_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Match_Val_Decl .. Lkt_Match_Val_Decl;
      --% no-document: True
      subtype Lkt_Val_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Val_Decl .. Lkt_Val_Decl;
      --% no-document: True
      subtype Lkt_Fun_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Fun_Decl .. Lkt_Fun_Decl;
      --% no-document: True
      subtype Lkt_Env_Spec_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Env_Spec_Decl .. Lkt_Env_Spec_Decl;
      --% no-document: True
      subtype Lkt_Generic_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Generic_Decl .. Lkt_Generic_Decl;
      --% no-document: True
      subtype Lkt_Grammar_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Decl .. Lkt_Grammar_Decl;
      --% no-document: True
      subtype Lkt_Lexer_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Lexer_Decl .. Lkt_Lexer_Decl;
      --% no-document: True
      subtype Lkt_Lexer_Family_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Lexer_Family_Decl .. Lkt_Lexer_Family_Decl;
      --% no-document: True
      subtype Lkt_Synth_Fun_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Synth_Fun_Decl .. Lkt_Synth_Fun_Decl;
      --% no-document: True
      subtype Lkt_Synth_Param_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Synth_Param_Decl .. Lkt_Synth_Param_Decl;
      --% no-document: True
      subtype Lkt_Type_Decl is Lkt_Node_Kind_Type
            range Lkt_Any_Type_Decl .. Lkt_Trait_Decl;
      --% no-document: True
      subtype Lkt_Any_Type_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Any_Type_Decl .. Lkt_Any_Type_Decl;
      --% no-document: True
      subtype Lkt_Enum_Class_Alt_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Enum_Class_Alt_Decl .. Lkt_Enum_Class_Alt_Decl;
      --% no-document: True
      subtype Lkt_Function_Type_Range is Lkt_Node_Kind_Type
            range Lkt_Function_Type .. Lkt_Function_Type;
      --% no-document: True
      subtype Lkt_Generic_Param_Type_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Generic_Param_Type_Decl .. Lkt_Generic_Param_Type_Decl;
      --% no-document: True
      subtype Lkt_Named_Type_Decl is Lkt_Node_Kind_Type
            range Lkt_Class_Decl .. Lkt_Trait_Decl;
      --% no-document: True
      subtype Lkt_Basic_Class_Decl is Lkt_Node_Kind_Type
            range Lkt_Class_Decl .. Lkt_Enum_Class_Decl;
      --% no-document: True
      subtype Lkt_Class_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Class_Decl .. Lkt_Class_Decl;
      --% no-document: True
      subtype Lkt_Enum_Class_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Enum_Class_Decl .. Lkt_Enum_Class_Decl;
      --% no-document: True
      subtype Lkt_Enum_Type_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Enum_Type_Decl .. Lkt_Enum_Type_Decl;
      --% no-document: True
      subtype Lkt_Struct_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Struct_Decl .. Lkt_Struct_Decl;
      --% no-document: True
      subtype Lkt_Trait_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Trait_Decl .. Lkt_Trait_Decl;
      --% no-document: True
      subtype Lkt_Decl_Annotation_Range is Lkt_Node_Kind_Type
            range Lkt_Decl_Annotation .. Lkt_Decl_Annotation;
      --% no-document: True
      subtype Lkt_Decl_Annotation_Args_Range is Lkt_Node_Kind_Type
            range Lkt_Decl_Annotation_Args .. Lkt_Decl_Annotation_Args;
      --% no-document: True
      subtype Lkt_Dyn_Env_Wrapper_Range is Lkt_Node_Kind_Type
            range Lkt_Dyn_Env_Wrapper .. Lkt_Dyn_Env_Wrapper;
      --% no-document: True
      subtype Lkt_Elsif_Branch_Range is Lkt_Node_Kind_Type
            range Lkt_Elsif_Branch .. Lkt_Elsif_Branch;
      --% no-document: True
      subtype Lkt_Enum_Class_Case_Range is Lkt_Node_Kind_Type
            range Lkt_Enum_Class_Case .. Lkt_Enum_Class_Case;
      --% no-document: True
      subtype Lkt_Excludes_Null is Lkt_Node_Kind_Type
            range Lkt_Excludes_Null_Absent .. Lkt_Excludes_Null_Present;
      --% no-document: True
      subtype Lkt_Excludes_Null_Absent_Range is Lkt_Node_Kind_Type
            range Lkt_Excludes_Null_Absent .. Lkt_Excludes_Null_Absent;
      --% no-document: True
      subtype Lkt_Excludes_Null_Present_Range is Lkt_Node_Kind_Type
            range Lkt_Excludes_Null_Present .. Lkt_Excludes_Null_Present;
      --% no-document: True
      subtype Lkt_Expr is Lkt_Node_Kind_Type
            range Lkt_Any_Of .. Lkt_Un_Op;
      --% no-document: True
      subtype Lkt_Any_Of_Range is Lkt_Node_Kind_Type
            range Lkt_Any_Of .. Lkt_Any_Of;
      --% no-document: True
      subtype Lkt_Array_Literal_Range is Lkt_Node_Kind_Type
            range Lkt_Array_Literal .. Lkt_Array_Literal;
      --% no-document: True
      subtype Lkt_Base_Call_Expr is Lkt_Node_Kind_Type
            range Lkt_Call_Expr .. Lkt_Logic_Propagate_Call;
      --% no-document: True
      subtype Lkt_Call_Expr_Range is Lkt_Node_Kind_Type
            range Lkt_Call_Expr .. Lkt_Call_Expr;
      --% no-document: True
      subtype Lkt_Logic_Call_Expr is Lkt_Node_Kind_Type
            range Lkt_Logic_Predicate .. Lkt_Logic_Propagate_Call;
      --% no-document: True
      subtype Lkt_Logic_Predicate_Range is Lkt_Node_Kind_Type
            range Lkt_Logic_Predicate .. Lkt_Logic_Predicate;
      --% no-document: True
      subtype Lkt_Logic_Propagate_Call_Range is Lkt_Node_Kind_Type
            range Lkt_Logic_Propagate_Call .. Lkt_Logic_Propagate_Call;
      --% no-document: True
      subtype Lkt_Bin_Op_Range is Lkt_Node_Kind_Type
            range Lkt_Bin_Op .. Lkt_Bin_Op;
      --% no-document: True
      subtype Lkt_Block_Expr_Range is Lkt_Node_Kind_Type
            range Lkt_Block_Expr .. Lkt_Block_Expr;
      --% no-document: True
      subtype Lkt_Cast_Expr_Range is Lkt_Node_Kind_Type
            range Lkt_Cast_Expr .. Lkt_Cast_Expr;
      --% no-document: True
      subtype Lkt_Dot_Expr_Range is Lkt_Node_Kind_Type
            range Lkt_Dot_Expr .. Lkt_Dot_Expr;
      --% no-document: True
      subtype Lkt_Error_On_Null_Range is Lkt_Node_Kind_Type
            range Lkt_Error_On_Null .. Lkt_Error_On_Null;
      --% no-document: True
      subtype Lkt_Generic_Instantiation_Range is Lkt_Node_Kind_Type
            range Lkt_Generic_Instantiation .. Lkt_Generic_Instantiation;
      --% no-document: True
      subtype Lkt_Grammar_Expr is Lkt_Node_Kind_Type
            range Lkt_Grammar_Cut .. Lkt_Token_Ref;
      --% no-document: True
      subtype Lkt_Grammar_Cut_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Cut .. Lkt_Grammar_Cut;
      --% no-document: True
      subtype Lkt_Grammar_Discard_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Discard .. Lkt_Grammar_Discard;
      --% no-document: True
      subtype Lkt_Grammar_Dont_Skip_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Dont_Skip .. Lkt_Grammar_Dont_Skip;
      --% no-document: True
      subtype Lkt_Grammar_List_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_List .. Lkt_Grammar_List;
      --% no-document: True
      subtype Lkt_Grammar_Null_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Null .. Lkt_Grammar_Null;
      --% no-document: True
      subtype Lkt_Grammar_Opt_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Opt .. Lkt_Grammar_Opt;
      --% no-document: True
      subtype Lkt_Grammar_Opt_Error_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Opt_Error .. Lkt_Grammar_Opt_Error;
      --% no-document: True
      subtype Lkt_Grammar_Opt_Error_Group_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Opt_Error_Group .. Lkt_Grammar_Opt_Error_Group;
      --% no-document: True
      subtype Lkt_Grammar_Opt_Group_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Opt_Group .. Lkt_Grammar_Opt_Group;
      --% no-document: True
      subtype Lkt_Grammar_Or_Expr_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Or_Expr .. Lkt_Grammar_Or_Expr;
      --% no-document: True
      subtype Lkt_Grammar_Pick_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Pick .. Lkt_Grammar_Implicit_Pick;
      --% no-document: True
      subtype Lkt_Grammar_Implicit_Pick_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Implicit_Pick .. Lkt_Grammar_Implicit_Pick;
      --% no-document: True
      subtype Lkt_Grammar_Predicate_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Predicate .. Lkt_Grammar_Predicate;
      --% no-document: True
      subtype Lkt_Grammar_Rule_Ref_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Rule_Ref .. Lkt_Grammar_Rule_Ref;
      --% no-document: True
      subtype Lkt_Grammar_Skip_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Skip .. Lkt_Grammar_Skip;
      --% no-document: True
      subtype Lkt_Grammar_Stop_Cut_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Stop_Cut .. Lkt_Grammar_Stop_Cut;
      --% no-document: True
      subtype Lkt_Parse_Node_Expr_Range is Lkt_Node_Kind_Type
            range Lkt_Parse_Node_Expr .. Lkt_Parse_Node_Expr;
      --% no-document: True
      subtype Lkt_Token_Lit_Range is Lkt_Node_Kind_Type
            range Lkt_Token_Lit .. Lkt_Token_Lit;
      --% no-document: True
      subtype Lkt_Token_No_Case_Lit_Range is Lkt_Node_Kind_Type
            range Lkt_Token_No_Case_Lit .. Lkt_Token_No_Case_Lit;
      --% no-document: True
      subtype Lkt_Token_Pattern_Concat_Range is Lkt_Node_Kind_Type
            range Lkt_Token_Pattern_Concat .. Lkt_Token_Pattern_Concat;
      --% no-document: True
      subtype Lkt_Token_Pattern_Lit_Range is Lkt_Node_Kind_Type
            range Lkt_Token_Pattern_Lit .. Lkt_Token_Pattern_Lit;
      --% no-document: True
      subtype Lkt_Token_Ref_Range is Lkt_Node_Kind_Type
            range Lkt_Token_Ref .. Lkt_Token_Ref;
      --% no-document: True
      subtype Lkt_Id_Range is Lkt_Node_Kind_Type
            range Lkt_Id .. Lkt_Ref_Id;
      --% no-document: True
      subtype Lkt_Def_Id_Range is Lkt_Node_Kind_Type
            range Lkt_Def_Id .. Lkt_Def_Id;
      --% no-document: True
      subtype Lkt_Module_Ref_Id_Range is Lkt_Node_Kind_Type
            range Lkt_Module_Ref_Id .. Lkt_Module_Ref_Id;
      --% no-document: True
      subtype Lkt_Ref_Id_Range is Lkt_Node_Kind_Type
            range Lkt_Ref_Id .. Lkt_Ref_Id;
      --% no-document: True
      subtype Lkt_If_Expr_Range is Lkt_Node_Kind_Type
            range Lkt_If_Expr .. Lkt_If_Expr;
      --% no-document: True
      subtype Lkt_Isa_Range is Lkt_Node_Kind_Type
            range Lkt_Isa .. Lkt_Isa;
      --% no-document: True
      subtype Lkt_Keep_Expr_Range is Lkt_Node_Kind_Type
            range Lkt_Keep_Expr .. Lkt_Keep_Expr;
      --% no-document: True
      subtype Lkt_Lambda_Expr_Range is Lkt_Node_Kind_Type
            range Lkt_Lambda_Expr .. Lkt_Lambda_Expr;
      --% no-document: True
      subtype Lkt_Lit is Lkt_Node_Kind_Type
            range Lkt_Big_Num_Lit .. Lkt_Pattern_Single_Line_String_Lit;
      --% no-document: True
      subtype Lkt_Big_Num_Lit_Range is Lkt_Node_Kind_Type
            range Lkt_Big_Num_Lit .. Lkt_Big_Num_Lit;
      --% no-document: True
      subtype Lkt_Char_Lit_Range is Lkt_Node_Kind_Type
            range Lkt_Char_Lit .. Lkt_Char_Lit;
      --% no-document: True
      subtype Lkt_Null_Lit_Range is Lkt_Node_Kind_Type
            range Lkt_Null_Lit .. Lkt_Null_Lit;
      --% no-document: True
      subtype Lkt_Num_Lit_Range is Lkt_Node_Kind_Type
            range Lkt_Num_Lit .. Lkt_Num_Lit;
      --% no-document: True
      subtype Lkt_String_Lit is Lkt_Node_Kind_Type
            range Lkt_Block_String_Lit .. Lkt_Pattern_Single_Line_String_Lit;
      --% no-document: True
      subtype Lkt_Block_String_Lit_Range is Lkt_Node_Kind_Type
            range Lkt_Block_String_Lit .. Lkt_Block_String_Lit;
      --% no-document: True
      subtype Lkt_Single_Line_String_Lit_Range is Lkt_Node_Kind_Type
            range Lkt_Single_Line_String_Lit .. Lkt_Pattern_Single_Line_String_Lit;
      --% no-document: True
      subtype Lkt_Pattern_Single_Line_String_Lit_Range is Lkt_Node_Kind_Type
            range Lkt_Pattern_Single_Line_String_Lit .. Lkt_Pattern_Single_Line_String_Lit;
      --% no-document: True
      subtype Lkt_Logic_Assign_Range is Lkt_Node_Kind_Type
            range Lkt_Logic_Assign .. Lkt_Logic_Assign;
      --% no-document: True
      subtype Lkt_Logic_Expr_Range is Lkt_Node_Kind_Type
            range Lkt_Logic_Expr .. Lkt_Logic_Expr;
      --% no-document: True
      subtype Lkt_Logic_Propagate_Range is Lkt_Node_Kind_Type
            range Lkt_Logic_Propagate .. Lkt_Logic_Propagate;
      --% no-document: True
      subtype Lkt_Logic_Unify_Range is Lkt_Node_Kind_Type
            range Lkt_Logic_Unify .. Lkt_Logic_Unify;
      --% no-document: True
      subtype Lkt_Match_Expr_Range is Lkt_Node_Kind_Type
            range Lkt_Match_Expr .. Lkt_Match_Expr;
      --% no-document: True
      subtype Lkt_Not_Expr_Range is Lkt_Node_Kind_Type
            range Lkt_Not_Expr .. Lkt_Not_Expr;
      --% no-document: True
      subtype Lkt_Paren_Expr_Range is Lkt_Node_Kind_Type
            range Lkt_Paren_Expr .. Lkt_Paren_Expr;
      --% no-document: True
      subtype Lkt_Raise_Expr_Range is Lkt_Node_Kind_Type
            range Lkt_Raise_Expr .. Lkt_Raise_Expr;
      --% no-document: True
      subtype Lkt_Subscript_Expr_Range is Lkt_Node_Kind_Type
            range Lkt_Subscript_Expr .. Lkt_Subscript_Expr;
      --% no-document: True
      subtype Lkt_Try_Expr_Range is Lkt_Node_Kind_Type
            range Lkt_Try_Expr .. Lkt_Try_Expr;
      --% no-document: True
      subtype Lkt_Un_Op_Range is Lkt_Node_Kind_Type
            range Lkt_Un_Op .. Lkt_Un_Op;
      --% no-document: True
      subtype Lkt_Full_Decl_Range is Lkt_Node_Kind_Type
            range Lkt_Full_Decl .. Lkt_Full_Decl;
      --% no-document: True
      subtype Lkt_Grammar_List_Sep_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_List_Sep .. Lkt_Grammar_List_Sep;
      --% no-document: True
      subtype Lkt_Import_Range is Lkt_Node_Kind_Type
            range Lkt_Import .. Lkt_Import;
      --% no-document: True
      subtype Lkt_Langkit_Root_Range is Lkt_Node_Kind_Type
            range Lkt_Langkit_Root .. Lkt_Langkit_Root;
      --% no-document: True
      subtype Lkt_Lexer_Case_Rule_Range is Lkt_Node_Kind_Type
            range Lkt_Lexer_Case_Rule .. Lkt_Lexer_Case_Rule;
      --% no-document: True
      subtype Lkt_Lexer_Case_Rule_Send_Range is Lkt_Node_Kind_Type
            range Lkt_Lexer_Case_Rule_Send .. Lkt_Lexer_Case_Rule_Send;
      --% no-document: True
      subtype Lkt_List_Kind is Lkt_Node_Kind_Type
            range Lkt_List_Kind_One .. Lkt_List_Kind_Zero;
      --% no-document: True
      subtype Lkt_List_Kind_One_Range is Lkt_Node_Kind_Type
            range Lkt_List_Kind_One .. Lkt_List_Kind_One;
      --% no-document: True
      subtype Lkt_List_Kind_Zero_Range is Lkt_Node_Kind_Type
            range Lkt_List_Kind_Zero .. Lkt_List_Kind_Zero;
      --% no-document: True
      subtype Lkt_Lkt_Node_Base_List is Lkt_Node_Kind_Type
            range Lkt_Argument_List .. Lkt_Synthetic_Type_Ref_List;
      --% no-document: True
      subtype Lkt_Argument_List_Range is Lkt_Node_Kind_Type
            range Lkt_Argument_List .. Lkt_Argument_List;
      --% no-document: True
      subtype Lkt_Base_Lexer_Case_Rule_Alt_List_Range is Lkt_Node_Kind_Type
            range Lkt_Base_Lexer_Case_Rule_Alt_List .. Lkt_Base_Lexer_Case_Rule_Alt_List;
      --% no-document: True
      subtype Lkt_Base_Match_Branch_List_Range is Lkt_Node_Kind_Type
            range Lkt_Base_Match_Branch_List .. Lkt_Base_Match_Branch_List;
      --% no-document: True
      subtype Lkt_Block_String_Line_List_Range is Lkt_Node_Kind_Type
            range Lkt_Block_String_Line_List .. Lkt_Block_String_Line_List;
      --% no-document: True
      subtype Lkt_Call_Expr_List_Range is Lkt_Node_Kind_Type
            range Lkt_Call_Expr_List .. Lkt_Call_Expr_List;
      --% no-document: True
      subtype Lkt_Decl_Annotation_List_Range is Lkt_Node_Kind_Type
            range Lkt_Decl_Annotation_List .. Lkt_Decl_Annotation_List;
      --% no-document: True
      subtype Lkt_Elsif_Branch_List_Range is Lkt_Node_Kind_Type
            range Lkt_Elsif_Branch_List .. Lkt_Elsif_Branch_List;
      --% no-document: True
      subtype Lkt_Enum_Class_Alt_Decl_List_Range is Lkt_Node_Kind_Type
            range Lkt_Enum_Class_Alt_Decl_List .. Lkt_Enum_Class_Alt_Decl_List;
      --% no-document: True
      subtype Lkt_Enum_Class_Case_List_Range is Lkt_Node_Kind_Type
            range Lkt_Enum_Class_Case_List .. Lkt_Enum_Class_Case_List;
      --% no-document: True
      subtype Lkt_Enum_Lit_Decl_List_Range is Lkt_Node_Kind_Type
            range Lkt_Enum_Lit_Decl_List .. Lkt_Enum_Lit_Decl_List;
      --% no-document: True
      subtype Lkt_Expr_List_Range is Lkt_Node_Kind_Type
            range Lkt_Expr_List .. Lkt_Any_Of_List;
      --% no-document: True
      subtype Lkt_Any_Of_List_Range is Lkt_Node_Kind_Type
            range Lkt_Any_Of_List .. Lkt_Any_Of_List;
      --% no-document: True
      subtype Lkt_Full_Decl_List_Range is Lkt_Node_Kind_Type
            range Lkt_Full_Decl_List .. Lkt_Generic_Param_Decl_List;
      --% no-document: True
      subtype Lkt_Decl_Block_Range is Lkt_Node_Kind_Type
            range Lkt_Decl_Block .. Lkt_Decl_Block;
      --% no-document: True
      subtype Lkt_Generic_Param_Decl_List_Range is Lkt_Node_Kind_Type
            range Lkt_Generic_Param_Decl_List .. Lkt_Generic_Param_Decl_List;
      --% no-document: True
      subtype Lkt_Fun_Param_Decl_List_Range is Lkt_Node_Kind_Type
            range Lkt_Fun_Param_Decl_List .. Lkt_Fun_Param_Decl_List;
      --% no-document: True
      subtype Lkt_Grammar_Expr_List_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Expr_List .. Lkt_Grammar_Expr_List;
      --% no-document: True
      subtype Lkt_Grammar_Expr_List_List_Range is Lkt_Node_Kind_Type
            range Lkt_Grammar_Expr_List_List .. Lkt_Grammar_Expr_List_List;
      --% no-document: True
      subtype Lkt_Import_List_Range is Lkt_Node_Kind_Type
            range Lkt_Import_List .. Lkt_Import_List;
      --% no-document: True
      subtype Lkt_Lambda_Param_Decl_List_Range is Lkt_Node_Kind_Type
            range Lkt_Lambda_Param_Decl_List .. Lkt_Lambda_Param_Decl_List;
      --% no-document: True
      subtype Lkt_Lkt_Node_List_Range is Lkt_Node_Kind_Type
            range Lkt_Lkt_Node_List .. Lkt_Block_Decl_List;
      --% no-document: True
      subtype Lkt_Block_Decl_List_Range is Lkt_Node_Kind_Type
            range Lkt_Block_Decl_List .. Lkt_Block_Decl_List;
      --% no-document: True
      subtype Lkt_Pattern_Detail_List_Range is Lkt_Node_Kind_Type
            range Lkt_Pattern_Detail_List .. Lkt_Pattern_Detail_List;
      --% no-document: True
      subtype Lkt_Pattern_List_Range is Lkt_Node_Kind_Type
            range Lkt_Pattern_List .. Lkt_Pattern_List;
      --% no-document: True
      subtype Lkt_Ref_Id_List_Range is Lkt_Node_Kind_Type
            range Lkt_Ref_Id_List .. Lkt_Ref_Id_List;
      --% no-document: True
      subtype Lkt_Type_Ref_List_Range is Lkt_Node_Kind_Type
            range Lkt_Type_Ref_List .. Lkt_Synthetic_Type_Ref_List;
      --% no-document: True
      subtype Lkt_Synthetic_Type_Ref_List_Range is Lkt_Node_Kind_Type
            range Lkt_Synthetic_Type_Ref_List .. Lkt_Synthetic_Type_Ref_List;
      --% no-document: True
      subtype Lkt_Null_Cond_Qualifier is Lkt_Node_Kind_Type
            range Lkt_Null_Cond_Qualifier_Absent .. Lkt_Null_Cond_Qualifier_Present;
      --% no-document: True
      subtype Lkt_Null_Cond_Qualifier_Absent_Range is Lkt_Node_Kind_Type
            range Lkt_Null_Cond_Qualifier_Absent .. Lkt_Null_Cond_Qualifier_Absent;
      --% no-document: True
      subtype Lkt_Null_Cond_Qualifier_Present_Range is Lkt_Node_Kind_Type
            range Lkt_Null_Cond_Qualifier_Present .. Lkt_Null_Cond_Qualifier_Present;
      --% no-document: True
      subtype Lkt_Op is Lkt_Node_Kind_Type
            range Lkt_Op_Amp .. Lkt_Op_Plus;
      --% no-document: True
      subtype Lkt_Op_Amp_Range is Lkt_Node_Kind_Type
            range Lkt_Op_Amp .. Lkt_Op_Amp;
      --% no-document: True
      subtype Lkt_Op_And_Range is Lkt_Node_Kind_Type
            range Lkt_Op_And .. Lkt_Op_And;
      --% no-document: True
      subtype Lkt_Op_Div_Range is Lkt_Node_Kind_Type
            range Lkt_Op_Div .. Lkt_Op_Div;
      --% no-document: True
      subtype Lkt_Op_Eq_Range is Lkt_Node_Kind_Type
            range Lkt_Op_Eq .. Lkt_Op_Eq;
      --% no-document: True
      subtype Lkt_Op_Gt_Range is Lkt_Node_Kind_Type
            range Lkt_Op_Gt .. Lkt_Op_Gt;
      --% no-document: True
      subtype Lkt_Op_Gte_Range is Lkt_Node_Kind_Type
            range Lkt_Op_Gte .. Lkt_Op_Gte;
      --% no-document: True
      subtype Lkt_Op_Logic_And_Range is Lkt_Node_Kind_Type
            range Lkt_Op_Logic_And .. Lkt_Op_Logic_And;
      --% no-document: True
      subtype Lkt_Op_Logic_Or_Range is Lkt_Node_Kind_Type
            range Lkt_Op_Logic_Or .. Lkt_Op_Logic_Or;
      --% no-document: True
      subtype Lkt_Op_Lt_Range is Lkt_Node_Kind_Type
            range Lkt_Op_Lt .. Lkt_Op_Lt;
      --% no-document: True
      subtype Lkt_Op_Lte_Range is Lkt_Node_Kind_Type
            range Lkt_Op_Lte .. Lkt_Op_Lte;
      --% no-document: True
      subtype Lkt_Op_Minus_Range is Lkt_Node_Kind_Type
            range Lkt_Op_Minus .. Lkt_Op_Minus;
      --% no-document: True
      subtype Lkt_Op_Mult_Range is Lkt_Node_Kind_Type
            range Lkt_Op_Mult .. Lkt_Op_Mult;
      --% no-document: True
      subtype Lkt_Op_Ne_Range is Lkt_Node_Kind_Type
            range Lkt_Op_Ne .. Lkt_Op_Ne;
      --% no-document: True
      subtype Lkt_Op_Or_Range is Lkt_Node_Kind_Type
            range Lkt_Op_Or .. Lkt_Op_Or;
      --% no-document: True
      subtype Lkt_Op_Or_Int_Range is Lkt_Node_Kind_Type
            range Lkt_Op_Or_Int .. Lkt_Op_Or_Int;
      --% no-document: True
      subtype Lkt_Op_Plus_Range is Lkt_Node_Kind_Type
            range Lkt_Op_Plus .. Lkt_Op_Plus;
      --% no-document: True
      subtype Lkt_Pattern is Lkt_Node_Kind_Type
            range Lkt_Any_Type_Pattern .. Lkt_Type_Pattern;
      --% no-document: True
      subtype Lkt_Any_Type_Pattern_Range is Lkt_Node_Kind_Type
            range Lkt_Any_Type_Pattern .. Lkt_Any_Type_Pattern;
      --% no-document: True
      subtype Lkt_Binding_Pattern_Range is Lkt_Node_Kind_Type
            range Lkt_Binding_Pattern .. Lkt_Binding_Pattern;
      --% no-document: True
      subtype Lkt_Bool_Pattern is Lkt_Node_Kind_Type
            range Lkt_Bool_Pattern_False .. Lkt_Bool_Pattern_True;
      --% no-document: True
      subtype Lkt_Bool_Pattern_False_Range is Lkt_Node_Kind_Type
            range Lkt_Bool_Pattern_False .. Lkt_Bool_Pattern_False;
      --% no-document: True
      subtype Lkt_Bool_Pattern_True_Range is Lkt_Node_Kind_Type
            range Lkt_Bool_Pattern_True .. Lkt_Bool_Pattern_True;
      --% no-document: True
      subtype Lkt_Ellipsis_Pattern_Range is Lkt_Node_Kind_Type
            range Lkt_Ellipsis_Pattern .. Lkt_Ellipsis_Pattern;
      --% no-document: True
      subtype Lkt_Extended_Pattern_Range is Lkt_Node_Kind_Type
            range Lkt_Extended_Pattern .. Lkt_Extended_Pattern;
      --% no-document: True
      subtype Lkt_Filtered_Pattern_Range is Lkt_Node_Kind_Type
            range Lkt_Filtered_Pattern .. Lkt_Filtered_Pattern;
      --% no-document: True
      subtype Lkt_Integer_Pattern_Range is Lkt_Node_Kind_Type
            range Lkt_Integer_Pattern .. Lkt_Integer_Pattern;
      --% no-document: True
      subtype Lkt_List_Pattern_Range is Lkt_Node_Kind_Type
            range Lkt_List_Pattern .. Lkt_List_Pattern;
      --% no-document: True
      subtype Lkt_Not_Pattern_Range is Lkt_Node_Kind_Type
            range Lkt_Not_Pattern .. Lkt_Not_Pattern;
      --% no-document: True
      subtype Lkt_Null_Pattern_Range is Lkt_Node_Kind_Type
            range Lkt_Null_Pattern .. Lkt_Null_Pattern;
      --% no-document: True
      subtype Lkt_Or_Pattern_Range is Lkt_Node_Kind_Type
            range Lkt_Or_Pattern .. Lkt_Or_Pattern;
      --% no-document: True
      subtype Lkt_Paren_Pattern_Range is Lkt_Node_Kind_Type
            range Lkt_Paren_Pattern .. Lkt_Paren_Pattern;
      --% no-document: True
      subtype Lkt_Regex_Pattern_Range is Lkt_Node_Kind_Type
            range Lkt_Regex_Pattern .. Lkt_Regex_Pattern;
      --% no-document: True
      subtype Lkt_Tuple_Pattern_Range is Lkt_Node_Kind_Type
            range Lkt_Tuple_Pattern .. Lkt_Tuple_Pattern;
      --% no-document: True
      subtype Lkt_Type_Pattern_Range is Lkt_Node_Kind_Type
            range Lkt_Type_Pattern .. Lkt_Type_Pattern;
      --% no-document: True
      subtype Lkt_Pattern_Detail is Lkt_Node_Kind_Type
            range Lkt_Field_Pattern_Detail .. Lkt_Selector_Pattern_Detail;
      --% no-document: True
      subtype Lkt_Field_Pattern_Detail_Range is Lkt_Node_Kind_Type
            range Lkt_Field_Pattern_Detail .. Lkt_Field_Pattern_Detail;
      --% no-document: True
      subtype Lkt_Property_Pattern_Detail_Range is Lkt_Node_Kind_Type
            range Lkt_Property_Pattern_Detail .. Lkt_Property_Pattern_Detail;
      --% no-document: True
      subtype Lkt_Selector_Pattern_Detail_Range is Lkt_Node_Kind_Type
            range Lkt_Selector_Pattern_Detail .. Lkt_Selector_Pattern_Detail;
      --% no-document: True
      subtype Lkt_Selector_Call_Range is Lkt_Node_Kind_Type
            range Lkt_Selector_Call .. Lkt_Selector_Call;
      --% no-document: True
      subtype Lkt_Type_Ref is Lkt_Node_Kind_Type
            range Lkt_Default_List_Type_Ref .. Lkt_Simple_Type_Ref;
      --% no-document: True
      subtype Lkt_Default_List_Type_Ref_Range is Lkt_Node_Kind_Type
            range Lkt_Default_List_Type_Ref .. Lkt_Default_List_Type_Ref;
      --% no-document: True
      subtype Lkt_Function_Type_Ref_Range is Lkt_Node_Kind_Type
            range Lkt_Function_Type_Ref .. Lkt_Function_Type_Ref;
      --% no-document: True
      subtype Lkt_Generic_Type_Ref_Range is Lkt_Node_Kind_Type
            range Lkt_Generic_Type_Ref .. Lkt_Generic_Type_Ref;
      --% no-document: True
      subtype Lkt_Simple_Type_Ref_Range is Lkt_Node_Kind_Type
            range Lkt_Simple_Type_Ref .. Lkt_Simple_Type_Ref;
      --% no-document: True
      subtype Lkt_Var_Bind_Range is Lkt_Node_Kind_Type
            range Lkt_Var_Bind .. Lkt_Var_Bind;
      --% no-document: True

   subtype Synthetic_Nodes is Lkt_Node_Kind_Type
      with Static_Predicate =>
         Synthetic_Nodes in
         Lkt_Synthetic_Lexer_Decl
         | Lkt_Node_Decl
         | Lkt_Self_Decl
         | Lkt_Synth_Fun_Decl
         | Lkt_Synth_Param_Decl
         | Lkt_Any_Type_Decl
         | Lkt_Function_Type
         | Lkt_Dyn_Env_Wrapper
         | Lkt_Synthetic_Type_Ref_List
   ;
   --  Set of nodes that are synthetic.
      --
      --  Parsers cannot create synthetic nodes, so these correspond to no
      --  source text. These nodes are created dynamically for convenience
      --  during semantic analysis.

   Default_Grammar_Rule : constant Grammar_Rule := Main_Rule_Rule;
   --  Default grammar rule to use when parsing analysis units

   ------------
   -- Tokens --
   ------------

   type Token_Kind is
     (Lkt_Amp,
      Lkt_And_Kw,
      Lkt_At,
      Lkt_Big_Number,
      Lkt_Bind_Kw,
      Lkt_Block_String_Line,
      Lkt_Case_Kw,
      Lkt_Char,
      Lkt_Class_Kw,
      Lkt_Colon,
      Lkt_Comb,
      Lkt_Comma,
      Lkt_Comment,
      Lkt_Discard_Kw,
      Lkt_Div,
      Lkt_Doc_Comment,
      Lkt_Dot,
      Lkt_Dyn_Var_Kw,
      Lkt_E_Q,
      Lkt_Elif_Kw,
      Lkt_Ellipsis,
      Lkt_Else_Kw,
      Lkt_Enum_Kw,
      Lkt_Equal,
      Lkt_Excl_Mark,
      Lkt_Fat_Right_Arrow,
      Lkt_Fun_Kw,
      Lkt_G_T,
      Lkt_G_T_E,
      Lkt_Generic_Kw,
      Lkt_Grammar_Kw,
      Lkt_Identifier,
      Lkt_If_Kw,
      Lkt_Implements_Kw,
      Lkt_Import_Kw,
      Lkt_In_Kw,
      Lkt_Int_Mark,
      Lkt_Is_Kw,
      Lkt_L_Brace,
      Lkt_L_Brack,
      Lkt_L_Par,
      Lkt_L_T,
      Lkt_L_T_E,
      Lkt_Left_Arrow,
      Lkt_Lexer_Kw,
      Lkt_Lexing_Failure,
      Lkt_Match_Kw,
      Lkt_Minus,
      Lkt_N_E,
      Lkt_Not_Kw,
      Lkt_Null_Kw,
      Lkt_Number,
      Lkt_Or_Kw,
      Lkt_P_String,
      Lkt_Percent,
      Lkt_Pipe,
      Lkt_Plus,
      Lkt_Private_Kw,
      Lkt_Public_Kw,
      Lkt_R_Brace,
      Lkt_R_Brack,
      Lkt_R_Par,
      Lkt_Raise_Kw,
      Lkt_Right_Arrow,
      Lkt_Semicolon,
      Lkt_String,
      Lkt_Struct_Kw,
      Lkt_Termination,
      Lkt_Then_Kw,
      Lkt_Times,
      Lkt_Trait_Kw,
      Lkt_Try_Kw,
      Lkt_Two_Sided_Arrow,
      Lkt_Val_Kw,
      Lkt_When_Kw,
      Lkt_Whitespace);
   --  Kind of token: indentifier, string literal, ...

   type Token_Family is
     (Alphanumericals,
      Default_Family);
   --  Groups of token kinds, to make the processing of some groups of token
   --  uniform.


   Token_Kind_To_Family : array (Token_Kind) of Token_Family :=
     (Lkt_Amp => Default_Family,
      Lkt_And_Kw => Alphanumericals,
      Lkt_At => Default_Family,
      Lkt_Big_Number => Alphanumericals,
      Lkt_Bind_Kw => Alphanumericals,
      Lkt_Block_String_Line => Default_Family,
      Lkt_Case_Kw => Alphanumericals,
      Lkt_Char => Default_Family,
      Lkt_Class_Kw => Alphanumericals,
      Lkt_Colon => Default_Family,
      Lkt_Comb => Default_Family,
      Lkt_Comma => Default_Family,
      Lkt_Comment => Default_Family,
      Lkt_Discard_Kw => Alphanumericals,
      Lkt_Div => Default_Family,
      Lkt_Doc_Comment => Default_Family,
      Lkt_Dot => Default_Family,
      Lkt_Dyn_Var_Kw => Alphanumericals,
      Lkt_E_Q => Default_Family,
      Lkt_Elif_Kw => Alphanumericals,
      Lkt_Ellipsis => Default_Family,
      Lkt_Else_Kw => Alphanumericals,
      Lkt_Enum_Kw => Alphanumericals,
      Lkt_Equal => Default_Family,
      Lkt_Excl_Mark => Default_Family,
      Lkt_Fat_Right_Arrow => Default_Family,
      Lkt_Fun_Kw => Alphanumericals,
      Lkt_G_T => Default_Family,
      Lkt_G_T_E => Default_Family,
      Lkt_Generic_Kw => Alphanumericals,
      Lkt_Grammar_Kw => Alphanumericals,
      Lkt_Identifier => Alphanumericals,
      Lkt_If_Kw => Alphanumericals,
      Lkt_Implements_Kw => Alphanumericals,
      Lkt_Import_Kw => Alphanumericals,
      Lkt_In_Kw => Alphanumericals,
      Lkt_Int_Mark => Default_Family,
      Lkt_Is_Kw => Alphanumericals,
      Lkt_L_Brace => Default_Family,
      Lkt_L_Brack => Default_Family,
      Lkt_L_Par => Default_Family,
      Lkt_L_T => Default_Family,
      Lkt_L_T_E => Default_Family,
      Lkt_Left_Arrow => Default_Family,
      Lkt_Lexer_Kw => Alphanumericals,
      Lkt_Lexing_Failure => Default_Family,
      Lkt_Match_Kw => Alphanumericals,
      Lkt_Minus => Default_Family,
      Lkt_N_E => Default_Family,
      Lkt_Not_Kw => Alphanumericals,
      Lkt_Null_Kw => Alphanumericals,
      Lkt_Number => Alphanumericals,
      Lkt_Or_Kw => Alphanumericals,
      Lkt_P_String => Alphanumericals,
      Lkt_Percent => Default_Family,
      Lkt_Pipe => Default_Family,
      Lkt_Plus => Default_Family,
      Lkt_Private_Kw => Alphanumericals,
      Lkt_Public_Kw => Alphanumericals,
      Lkt_R_Brace => Default_Family,
      Lkt_R_Brack => Default_Family,
      Lkt_R_Par => Default_Family,
      Lkt_Raise_Kw => Alphanumericals,
      Lkt_Right_Arrow => Default_Family,
      Lkt_Semicolon => Default_Family,
      Lkt_String => Default_Family,
      Lkt_Struct_Kw => Alphanumericals,
      Lkt_Termination => Default_Family,
      Lkt_Then_Kw => Alphanumericals,
      Lkt_Times => Default_Family,
      Lkt_Trait_Kw => Alphanumericals,
      Lkt_Try_Kw => Alphanumericals,
      Lkt_Two_Sided_Arrow => Default_Family,
      Lkt_Val_Kw => Alphanumericals,
      Lkt_When_Kw => Alphanumericals,
      Lkt_Whitespace => Default_Family);
   --  Associate a token family to all token kinds
   --
   --% document-value: False

   function Token_Kind_Name (Token_Id : Token_Kind) return String;
   --  Return a human-readable name for a token kind.

   function Token_Kind_Literal (Token_Id : Token_Kind) return Text_Type;
   --  Return the canonical literal corresponding to this token kind, or an
   --  empty string if this token has no literal.

   function Token_Error_Image (Token_Id : Token_Kind) return String;
   --  Return a string representation of ``Token_Id`` that is suitable in error
   --  messages.

   function To_Token_Kind (Raw : Raw_Token_Kind) return Token_Kind
      with Inline;
   function From_Token_Kind (Kind : Token_Kind) return Raw_Token_Kind
      with Inline;

   function Is_Token_Node (Kind : Lkt_Node_Kind_Type) return Boolean;
   --  Return whether Kind corresponds to a token node

   function Is_List_Node (Kind : Lkt_Node_Kind_Type) return Boolean;
   --  Return whether Kind corresponds to a list node

   function Is_Error_Node (Kind : Lkt_Node_Kind_Type) return Boolean;
   --  Return whether Kind corresponds to an error node

   type Visit_Status is (Into, Over, Stop);
   --  Helper type to control the node traversal process. See the
   --  ``Liblktlang.Analysis.Traverse`` function.

   -----------------------
   -- Lexical utilities --
   -----------------------

   type Token_Reference is private;
   --  Reference to a token in an analysis unit.

   No_Token : constant Token_Reference;

   type Token_Data_Type is private;

   function "<" (Left, Right : Token_Reference) return Boolean;
   --  Assuming ``Left`` and ``Right`` belong to the same analysis unit, return
   --  whether ``Left`` came before ``Right`` in the source file.

   function Next
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference;
   --  Return a reference to the next token in the corresponding analysis unit.

   function Previous
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference;
   --  Return a reference to the previous token in the corresponding analysis
   --  unit.

   function Data (Token : Token_Reference) return Token_Data_Type;
   --  Return the data associated to ``Token``

   function Is_Equivalent (L, R : Token_Reference) return Boolean;
   --  Return whether ``L`` and ``R`` are structurally equivalent tokens. This
   --  means that their position in the stream won't be taken into account,
   --  only the kind and text of the token.

   function Image (Token : Token_Reference) return String;
   --  Debug helper: return a human-readable text to represent a token

   function Text (Token : Token_Reference) return Text_Type;
   --  Return the text of the token as ``Text_Type``

   function Text (First, Last : Token_Reference) return Text_Type;
   --  Compute the source buffer slice corresponding to the text that spans
   --  between the ``First`` and ``Last`` tokens (both included). This yields
   --  an empty slice if ``Last`` actually appears before ``First``.
   --
   --  This raises a ``Constraint_Error`` if ``First`` and ``Last`` don't
   --  belong to the same analysis unit.

   function Get_Symbol (Token : Token_Reference) return Symbol_Type;
   --  Assuming that ``Token`` refers to a token that contains a symbol, return
   --  the corresponding symbol.

   function Kind (Token_Data : Token_Data_Type) return Token_Kind;
   --  Kind for this token.

   function Is_Trivia (Token : Token_Reference) return Boolean;
   --  Return whether this token is a trivia. If it's not, it's a regular
   --  token.

   function Is_Trivia (Token_Data : Token_Data_Type) return Boolean;
   --  Return whether this token is a trivia. If it's not, it's a regular
   --  token.

   function Index (Token : Token_Reference) return Token_Index;
   --  One-based index for this token/trivia. Tokens and trivias get their own
   --  index space.

   function Index (Token_Data : Token_Data_Type) return Token_Index;
   --  One-based index for this token/trivia. Tokens and trivias get their own
   --  index space.

   function Sloc_Range
     (Token_Data : Token_Data_Type) return Source_Location_Range;
   --  Source location range for this token. Note that the end bound is
   --  exclusive.

   function Origin_Filename (Token : Token_Reference) return String;
   --  Return the name of the file whose content was scanned to create Token.
   --  Return an empty string if the source comes from a memory buffer instead
   --  of a file.

   function Origin_Charset (Token : Token_Reference) return String;
   --  Return the charset used to decode the source that was scanned to create
   --  Token. Return an empty string if the source was already decoded during
   --  the scan.

   function Convert
     (TDH      : Token_Data_Handler;
      Token    : Token_Reference;
      Raw_Data : Stored_Token_Data) return Token_Data_Type;
   --  Turn data from ``TDH`` and ``Raw_Data`` into a user-ready token data
   --  record.

   type Child_Or_Trivia is (Child, Trivia);
   --  Discriminator for the ``Child_Record`` type

   function Raw_Data (T : Token_Reference) return Stored_Token_Data;
   --  Return the raw token data for ``T``

   function Token_Node_Kind (Kind : Lkt_Node_Kind_Type) return Token_Kind
      with Pre => Is_Token_Node (Kind);
   --  Return the token kind corresponding to the given token node kind

   


private

   type Token_Safety_Net is record
      Context         : Liblktlang_Support.Internal.Analysis.Internal_Context;
      Context_Version : Version_Number;
      --  Analysis context and version number at the time this safety net was
      --  produced.
      --
      --  TODO: it is not possible to refer to
      --  $.Implementation.Internal_Context from this spec (otherwise we get a
      --  circular dependency). For now, use the generic pointer from
      --  Liblktlang_Support (hack), but in the future the Token_Reference type
      --  (and this this safety net type) will go to the generic API, so we
      --  will get rid of this hack.

      TDH_Version : Version_Number;
      --  Version of the token data handler at the time this safety net was
      --  produced.
   end record;
   --  Information to embed in public APIs with token references, used to check
   --  before using the references that they are not stale.

   No_Token_Safety_Net : constant Token_Safety_Net :=
     (Liblktlang_Support.Internal.Analysis.No_Internal_Context, 0, 0);

   type Token_Reference is record
      TDH : Token_Data_Handler_Access;
      --  Token data handler that owns this token

      Index : Token_Or_Trivia_Index;
      --  Identifier for the trivia or the token this refers to

      Safety_Net : Token_Safety_Net;
   end record;

   procedure Check_Safety_Net (Self : Token_Reference);
   --  If ``Self`` is a stale token reference, raise a
   --  ``Stale_Reference_Error`` error.

   No_Token : constant Token_Reference :=
     (null, No_Token_Or_Trivia_Index, No_Token_Safety_Net);

   type Token_Data_Type is record
      Kind : Token_Kind;
      --  See documentation for the Kind accessor

      Is_Trivia : Boolean;
      --  See documentation for the Is_Trivia accessor

      Index : Token_Index;
      --  See documentation for the Index accessor

      Source_Buffer : Text_Cst_Access;
      --  Text for the original source file

      Source_First : Positive;
      Source_Last  : Natural;
      --  Bounds in Source_Buffer for the text corresponding to this token

      Sloc_Range : Source_Location_Range;
      --  See documenation for the Sloc_Range accessor
   end record;

end Liblktlang.Common;
