
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with System;

with GNATCOLL.Iconv;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Liblktlang_Support.Generic_API; use Liblktlang_Support.Generic_API;
with Liblktlang_Support.Generic_API.Analysis;
use Liblktlang_Support.Generic_API.Analysis;
with Liblktlang_Support.Internal.Analysis;
with Liblktlang_Support.Internal.Conversions;

with Liblktlang.Generic_API;
with Liblktlang.Implementation; use Liblktlang.Implementation;
with Liblktlang.Lexer_Implementation;
use Liblktlang.Lexer_Implementation;
with Liblktlang.Private_Converters;


package body Liblktlang.Common is

   Is_Token_Node_Kind : constant array (Lkt_Node_Kind_Type) of Boolean :=
     (Lkt_Argument => False,
      Lkt_Import => False,
      Lkt_Import_All_From => False,
      Lkt_Import_From => False,
      Lkt_Error_Lexer_Case_Rule_Alt => False,
      Lkt_Lexer_Case_Rule_Cond_Alt => False,
      Lkt_Lexer_Case_Rule_Default_Alt => False,
      Lkt_Match_Branch => False,
      Lkt_Pattern_Match_Branch => False,
      Lkt_Block_Expr_Clause => False,
      Lkt_Block_String_Line => True,
      Lkt_Class_Qualifier_Absent => False,
      Lkt_Class_Qualifier_Present => False,
      Lkt_Grammar_Rule_Decl => False,
      Lkt_Synthetic_Lexer_Decl => False,
      Lkt_Node_Decl => False,
      Lkt_Self_Decl => False,
      Lkt_Binding_Val_Decl => False,
      Lkt_Enum_Lit_Decl => False,
      Lkt_Field_Decl => False,
      Lkt_Fun_Param_Decl => False,
      Lkt_Lambda_Param_Decl => False,
      Lkt_Dyn_Var_Decl => False,
      Lkt_Match_Val_Decl => False,
      Lkt_Val_Decl => False,
      Lkt_Fun_Decl => False,
      Lkt_Env_Spec_Decl => False,
      Lkt_Error_Decl => False,
      Lkt_Generic_Decl => False,
      Lkt_Grammar_Decl => False,
      Lkt_Lexer_Decl => False,
      Lkt_Lexer_Family_Decl => False,
      Lkt_Synth_Fun_Decl => False,
      Lkt_Synth_Param_Decl => False,
      Lkt_Any_Type_Decl => False,
      Lkt_Enum_Class_Alt_Decl => False,
      Lkt_Function_Type => False,
      Lkt_Generic_Param_Type_Decl => False,
      Lkt_Class_Decl => False,
      Lkt_Enum_Class_Decl => False,
      Lkt_Enum_Type_Decl => False,
      Lkt_Struct_Decl => False,
      Lkt_Trait_Decl => False,
      Lkt_Decl_Annotation => False,
      Lkt_Decl_Annotation_Args => False,
      Lkt_Dyn_Env_Wrapper => False,
      Lkt_Elsif_Branch => False,
      Lkt_Enum_Class_Case => False,
      Lkt_Excludes_Null_Absent => False,
      Lkt_Excludes_Null_Present => False,
      Lkt_Any_Of => False,
      Lkt_Array_Literal => False,
      Lkt_Call_Expr => False,
      Lkt_Logic_Predicate => False,
      Lkt_Logic_Propagate_Call => False,
      Lkt_Bin_Op => False,
      Lkt_Block_Expr => False,
      Lkt_Cast_Expr => False,
      Lkt_Dot_Expr => False,
      Lkt_Error_On_Null => False,
      Lkt_Generic_Instantiation => False,
      Lkt_Error_Grammar_Expr => False,
      Lkt_Grammar_Cut => False,
      Lkt_Grammar_Discard => False,
      Lkt_Grammar_Dont_Skip => False,
      Lkt_Grammar_List => False,
      Lkt_Grammar_Null => False,
      Lkt_Grammar_Opt => False,
      Lkt_Grammar_Opt_Error => False,
      Lkt_Grammar_Opt_Error_Group => False,
      Lkt_Grammar_Opt_Group => False,
      Lkt_Grammar_Or_Expr => False,
      Lkt_Grammar_Pick => False,
      Lkt_Grammar_Implicit_Pick => False,
      Lkt_Grammar_Predicate => False,
      Lkt_Grammar_Rule_Ref => False,
      Lkt_Grammar_Skip => False,
      Lkt_Grammar_Stop_Cut => False,
      Lkt_Parse_Node_Expr => False,
      Lkt_Token_Lit => True,
      Lkt_Token_No_Case_Lit => False,
      Lkt_Token_Pattern_Concat => False,
      Lkt_Token_Pattern_Lit => True,
      Lkt_Token_Ref => False,
      Lkt_Id => True,
      Lkt_Def_Id => True,
      Lkt_Imported_Id => True,
      Lkt_Module_Id => True,
      Lkt_Ref_Id => True,
      Lkt_If_Expr => False,
      Lkt_Isa => False,
      Lkt_Keep_Expr => False,
      Lkt_Lambda_Expr => False,
      Lkt_Big_Num_Lit => True,
      Lkt_Char_Lit => True,
      Lkt_Null_Lit => False,
      Lkt_Num_Lit => True,
      Lkt_Block_String_Lit => False,
      Lkt_Module_Doc_String_Lit => False,
      Lkt_Single_Line_String_Lit => True,
      Lkt_Pattern_Single_Line_String_Lit => True,
      Lkt_Logic_Assign => False,
      Lkt_Logic_Expr => False,
      Lkt_Logic_Propagate => False,
      Lkt_Logic_Unify => False,
      Lkt_Match_Expr => False,
      Lkt_Not_Expr => False,
      Lkt_Paren_Expr => False,
      Lkt_Query => False,
      Lkt_Raise_Expr => False,
      Lkt_Subscript_Expr => False,
      Lkt_Try_Expr => False,
      Lkt_Un_Op => False,
      Lkt_Full_Decl => False,
      Lkt_Grammar_List_Sep => False,
      Lkt_Imported_Name => False,
      Lkt_Langkit_Root => False,
      Lkt_Lexer_Case_Rule => False,
      Lkt_Lexer_Case_Rule_Send => False,
      Lkt_List_Kind_One => False,
      Lkt_List_Kind_Zero => False,
      Lkt_Argument_List => False,
      Lkt_Base_Import_List => False,
      Lkt_Base_Lexer_Case_Rule_Alt_List => False,
      Lkt_Base_Match_Branch_List => False,
      Lkt_Block_String_Line_List => False,
      Lkt_Call_Expr_List => False,
      Lkt_Decl_Annotation_List => False,
      Lkt_Elsif_Branch_List => False,
      Lkt_Enum_Class_Alt_Decl_List => False,
      Lkt_Enum_Class_Case_List => False,
      Lkt_Enum_Lit_Decl_List => False,
      Lkt_Expr_List => False,
      Lkt_Any_Of_List => False,
      Lkt_Full_Decl_List => False,
      Lkt_Decl_Block => False,
      Lkt_Generic_Param_Decl_List => False,
      Lkt_Fun_Param_Decl_List => False,
      Lkt_Grammar_Expr_List => False,
      Lkt_Grammar_Expr_List_List => False,
      Lkt_Imported_Name_List => False,
      Lkt_Lambda_Param_Decl_List => False,
      Lkt_Lkt_Node_List => False,
      Lkt_Module_Doc_String_Line_List => False,
      Lkt_Pattern_Detail_List => False,
      Lkt_Pattern_List => False,
      Lkt_Ref_Id_List => False,
      Lkt_Type_Ref_List => False,
      Lkt_Synthetic_Type_Ref_List => False,
      Lkt_Module_Doc_String_Line => True,
      Lkt_Null_Cond_Qualifier_Absent => False,
      Lkt_Null_Cond_Qualifier_Present => False,
      Lkt_Op_Amp => False,
      Lkt_Op_And => False,
      Lkt_Op_Div => False,
      Lkt_Op_Eq => False,
      Lkt_Op_Gt => False,
      Lkt_Op_Gte => False,
      Lkt_Op_Logic_And => False,
      Lkt_Op_Logic_Or => False,
      Lkt_Op_Lt => False,
      Lkt_Op_Lte => False,
      Lkt_Op_Minus => False,
      Lkt_Op_Mult => False,
      Lkt_Op_Ne => False,
      Lkt_Op_Or => False,
      Lkt_Op_Or_Int => False,
      Lkt_Op_Plus => False,
      Lkt_Op_Stream_Concat => False,
      Lkt_Op_Stream_Cons => False,
      Lkt_Any_Type_Pattern => False,
      Lkt_Binding_Pattern => False,
      Lkt_Bool_Pattern_False => False,
      Lkt_Bool_Pattern_True => False,
      Lkt_Ellipsis_Pattern => False,
      Lkt_Extended_Pattern => False,
      Lkt_Filtered_Pattern => False,
      Lkt_Integer_Pattern => True,
      Lkt_List_Pattern => False,
      Lkt_Not_Pattern => False,
      Lkt_Null_Pattern => False,
      Lkt_Or_Pattern => False,
      Lkt_Paren_Pattern => False,
      Lkt_Regex_Pattern => True,
      Lkt_Tuple_Pattern => False,
      Lkt_Type_Pattern => False,
      Lkt_Field_Pattern_Detail => False,
      Lkt_Property_Pattern_Detail => False,
      Lkt_Selector_Pattern_Detail => False,
      Lkt_Selector_Call => False,
      Lkt_Default_List_Type_Ref => True,
      Lkt_Function_Type_Ref => False,
      Lkt_Generic_Type_Ref => False,
      Lkt_Simple_Type_Ref => False,
      Lkt_Var_Bind => False);
   --  For each node kind, return whether it is a node that contains only a
   --  single token.

   Is_Error_Node_Kind : constant array (Lkt_Node_Kind_Type) of Boolean :=
     (Lkt_Argument => False,
      Lkt_Import => False,
      Lkt_Import_All_From => False,
      Lkt_Import_From => False,
      Lkt_Error_Lexer_Case_Rule_Alt => True,
      Lkt_Lexer_Case_Rule_Cond_Alt => False,
      Lkt_Lexer_Case_Rule_Default_Alt => False,
      Lkt_Match_Branch => False,
      Lkt_Pattern_Match_Branch => False,
      Lkt_Block_Expr_Clause => False,
      Lkt_Block_String_Line => False,
      Lkt_Class_Qualifier_Absent => False,
      Lkt_Class_Qualifier_Present => False,
      Lkt_Grammar_Rule_Decl => False,
      Lkt_Synthetic_Lexer_Decl => False,
      Lkt_Node_Decl => False,
      Lkt_Self_Decl => False,
      Lkt_Binding_Val_Decl => False,
      Lkt_Enum_Lit_Decl => False,
      Lkt_Field_Decl => False,
      Lkt_Fun_Param_Decl => False,
      Lkt_Lambda_Param_Decl => False,
      Lkt_Dyn_Var_Decl => False,
      Lkt_Match_Val_Decl => False,
      Lkt_Val_Decl => False,
      Lkt_Fun_Decl => False,
      Lkt_Env_Spec_Decl => False,
      Lkt_Error_Decl => True,
      Lkt_Generic_Decl => False,
      Lkt_Grammar_Decl => False,
      Lkt_Lexer_Decl => False,
      Lkt_Lexer_Family_Decl => False,
      Lkt_Synth_Fun_Decl => False,
      Lkt_Synth_Param_Decl => False,
      Lkt_Any_Type_Decl => False,
      Lkt_Enum_Class_Alt_Decl => False,
      Lkt_Function_Type => False,
      Lkt_Generic_Param_Type_Decl => False,
      Lkt_Class_Decl => False,
      Lkt_Enum_Class_Decl => False,
      Lkt_Enum_Type_Decl => False,
      Lkt_Struct_Decl => False,
      Lkt_Trait_Decl => False,
      Lkt_Decl_Annotation => False,
      Lkt_Decl_Annotation_Args => False,
      Lkt_Dyn_Env_Wrapper => False,
      Lkt_Elsif_Branch => False,
      Lkt_Enum_Class_Case => False,
      Lkt_Excludes_Null_Absent => False,
      Lkt_Excludes_Null_Present => False,
      Lkt_Any_Of => False,
      Lkt_Array_Literal => False,
      Lkt_Call_Expr => False,
      Lkt_Logic_Predicate => False,
      Lkt_Logic_Propagate_Call => False,
      Lkt_Bin_Op => False,
      Lkt_Block_Expr => False,
      Lkt_Cast_Expr => False,
      Lkt_Dot_Expr => False,
      Lkt_Error_On_Null => False,
      Lkt_Generic_Instantiation => False,
      Lkt_Error_Grammar_Expr => True,
      Lkt_Grammar_Cut => False,
      Lkt_Grammar_Discard => False,
      Lkt_Grammar_Dont_Skip => False,
      Lkt_Grammar_List => False,
      Lkt_Grammar_Null => False,
      Lkt_Grammar_Opt => False,
      Lkt_Grammar_Opt_Error => False,
      Lkt_Grammar_Opt_Error_Group => False,
      Lkt_Grammar_Opt_Group => False,
      Lkt_Grammar_Or_Expr => False,
      Lkt_Grammar_Pick => False,
      Lkt_Grammar_Implicit_Pick => False,
      Lkt_Grammar_Predicate => False,
      Lkt_Grammar_Rule_Ref => False,
      Lkt_Grammar_Skip => False,
      Lkt_Grammar_Stop_Cut => False,
      Lkt_Parse_Node_Expr => False,
      Lkt_Token_Lit => False,
      Lkt_Token_No_Case_Lit => False,
      Lkt_Token_Pattern_Concat => False,
      Lkt_Token_Pattern_Lit => False,
      Lkt_Token_Ref => False,
      Lkt_Id => False,
      Lkt_Def_Id => False,
      Lkt_Imported_Id => False,
      Lkt_Module_Id => False,
      Lkt_Ref_Id => False,
      Lkt_If_Expr => False,
      Lkt_Isa => False,
      Lkt_Keep_Expr => False,
      Lkt_Lambda_Expr => False,
      Lkt_Big_Num_Lit => False,
      Lkt_Char_Lit => False,
      Lkt_Null_Lit => False,
      Lkt_Num_Lit => False,
      Lkt_Block_String_Lit => False,
      Lkt_Module_Doc_String_Lit => False,
      Lkt_Single_Line_String_Lit => False,
      Lkt_Pattern_Single_Line_String_Lit => False,
      Lkt_Logic_Assign => False,
      Lkt_Logic_Expr => False,
      Lkt_Logic_Propagate => False,
      Lkt_Logic_Unify => False,
      Lkt_Match_Expr => False,
      Lkt_Not_Expr => False,
      Lkt_Paren_Expr => False,
      Lkt_Query => False,
      Lkt_Raise_Expr => False,
      Lkt_Subscript_Expr => False,
      Lkt_Try_Expr => False,
      Lkt_Un_Op => False,
      Lkt_Full_Decl => False,
      Lkt_Grammar_List_Sep => False,
      Lkt_Imported_Name => False,
      Lkt_Langkit_Root => False,
      Lkt_Lexer_Case_Rule => False,
      Lkt_Lexer_Case_Rule_Send => False,
      Lkt_List_Kind_One => False,
      Lkt_List_Kind_Zero => False,
      Lkt_Argument_List => False,
      Lkt_Base_Import_List => False,
      Lkt_Base_Lexer_Case_Rule_Alt_List => False,
      Lkt_Base_Match_Branch_List => False,
      Lkt_Block_String_Line_List => False,
      Lkt_Call_Expr_List => False,
      Lkt_Decl_Annotation_List => False,
      Lkt_Elsif_Branch_List => False,
      Lkt_Enum_Class_Alt_Decl_List => False,
      Lkt_Enum_Class_Case_List => False,
      Lkt_Enum_Lit_Decl_List => False,
      Lkt_Expr_List => False,
      Lkt_Any_Of_List => False,
      Lkt_Full_Decl_List => False,
      Lkt_Decl_Block => False,
      Lkt_Generic_Param_Decl_List => False,
      Lkt_Fun_Param_Decl_List => False,
      Lkt_Grammar_Expr_List => False,
      Lkt_Grammar_Expr_List_List => False,
      Lkt_Imported_Name_List => False,
      Lkt_Lambda_Param_Decl_List => False,
      Lkt_Lkt_Node_List => False,
      Lkt_Module_Doc_String_Line_List => False,
      Lkt_Pattern_Detail_List => False,
      Lkt_Pattern_List => False,
      Lkt_Ref_Id_List => False,
      Lkt_Type_Ref_List => False,
      Lkt_Synthetic_Type_Ref_List => False,
      Lkt_Module_Doc_String_Line => False,
      Lkt_Null_Cond_Qualifier_Absent => False,
      Lkt_Null_Cond_Qualifier_Present => False,
      Lkt_Op_Amp => False,
      Lkt_Op_And => False,
      Lkt_Op_Div => False,
      Lkt_Op_Eq => False,
      Lkt_Op_Gt => False,
      Lkt_Op_Gte => False,
      Lkt_Op_Logic_And => False,
      Lkt_Op_Logic_Or => False,
      Lkt_Op_Lt => False,
      Lkt_Op_Lte => False,
      Lkt_Op_Minus => False,
      Lkt_Op_Mult => False,
      Lkt_Op_Ne => False,
      Lkt_Op_Or => False,
      Lkt_Op_Or_Int => False,
      Lkt_Op_Plus => False,
      Lkt_Op_Stream_Concat => False,
      Lkt_Op_Stream_Cons => False,
      Lkt_Any_Type_Pattern => False,
      Lkt_Binding_Pattern => False,
      Lkt_Bool_Pattern_False => False,
      Lkt_Bool_Pattern_True => False,
      Lkt_Ellipsis_Pattern => False,
      Lkt_Extended_Pattern => False,
      Lkt_Filtered_Pattern => False,
      Lkt_Integer_Pattern => False,
      Lkt_List_Pattern => False,
      Lkt_Not_Pattern => False,
      Lkt_Null_Pattern => False,
      Lkt_Or_Pattern => False,
      Lkt_Paren_Pattern => False,
      Lkt_Regex_Pattern => False,
      Lkt_Tuple_Pattern => False,
      Lkt_Type_Pattern => False,
      Lkt_Field_Pattern_Detail => False,
      Lkt_Property_Pattern_Detail => False,
      Lkt_Selector_Pattern_Detail => False,
      Lkt_Selector_Call => False,
      Lkt_Default_List_Type_Ref => False,
      Lkt_Function_Type_Ref => False,
      Lkt_Generic_Type_Ref => False,
      Lkt_Simple_Type_Ref => False,
      Lkt_Var_Bind => False);
   --  For each node kind, return whether it is an error node

   function Wrap_Token_Reference
     (Context : Internal_Context;
      TDH     : Token_Data_Handler_Access;
      Index   : Token_Or_Trivia_Index) return Token_Reference;
   function Get_Token_Context (Token : Token_Reference) return Internal_Context;
   function Get_Token_Unit (Token : Token_Reference) return Internal_Unit;
   function Get_Token_TDH
     (Token : Token_Reference) return Token_Data_Handler_Access;
   function Get_Token_Index
     (Token : Token_Reference) return Token_Or_Trivia_Index;
   procedure Extract_Token_Text
     (Token         : Token_Data_Type;
      Source_Buffer : out Text_Cst_Access;
      First         : out Positive;
      Last          : out Natural);
   --  Implementations for converters soft-links

   function From_Generic (Token : Lk_Token) return Common.Token_Reference
     with Export, External_Name => "Liblktlang__from_generic_token";
   function To_Generic (Token : Common.Token_Reference) return Lk_Token
     with Export, External_Name => "Liblktlang__to_generic_token";
   --  Implementation for converters hard-links in Private_Converters

   function "+" is new Ada.Unchecked_Conversion
     (Liblktlang_Support.Internal.Analysis.Internal_Context, Internal_Context);
   function "+" is new Ada.Unchecked_Conversion
     (Internal_Context, Liblktlang_Support.Internal.Analysis.Internal_Context);

   function Rewrap_Token
     (Origin : Token_Reference;
      Index  : Token_Or_Trivia_Index) return Token_Reference;
   --  Create a token reference for ``Index`` using the token data handler
   --  reference from ``Origin``.

   Token_Kind_To_Literals : constant array (Token_Kind) of Text_Access := (
   

         Lkt_Excl_Mark => new Text_Type'("!"),
         
         Lkt_Semicolon => new Text_Type'(";"),
         
         Lkt_Colon => new Text_Type'(":"),
         
         Lkt_Double_Colon => new Text_Type'("::"),
         
         Lkt_Triple_Colon => new Text_Type'(":::"),
         
         Lkt_Int_Mark => new Text_Type'("?"),
         
         Lkt_Div => new Text_Type'("/"),
         
         Lkt_Times => new Text_Type'("*"),
         
         Lkt_Plus => new Text_Type'("+"),
         
         Lkt_Amp => new Text_Type'("&"),
         
         Lkt_Minus => new Text_Type'("-"),
         
         Lkt_Pipe => new Text_Type'("|"),
         
         Lkt_Two_Sided_Arrow => new Text_Type'("<->"),
         
         Lkt_Left_Arrow => new Text_Type'("<-"),
         
         Lkt_Right_Arrow => new Text_Type'("->"),
         
         Lkt_Dot => new Text_Type'("."),
         
         Lkt_L_Par => new Text_Type'("("),
         
         Lkt_R_Par => new Text_Type'(")"),
         
         Lkt_L_Brack => new Text_Type'("["),
         
         Lkt_R_Brack => new Text_Type'("]"),
         
         Lkt_L_Brace => new Text_Type'("{"),
         
         Lkt_R_Brace => new Text_Type'("}"),
         
         Lkt_Comb => new Text_Type'("|>"),
         
         Lkt_Comma => new Text_Type'(","),
         
         Lkt_Ellipsis => new Text_Type'("..."),
         
         Lkt_At => new Text_Type'("@"),
         
         Lkt_Fat_Right_Arrow => new Text_Type'("=>"),
         
         Lkt_Equal => new Text_Type'("="),
         
         Lkt_E_Q => new Text_Type'("=="),
         
         Lkt_N_E => new Text_Type'("!="),
         
         Lkt_L_T_E => new Text_Type'("<="),
         
         Lkt_G_T_E => new Text_Type'(">="),
         
         Lkt_L_T => new Text_Type'("<"),
         
         Lkt_G_T => new Text_Type'(">"),
         
         Lkt_Percent => new Text_Type'("%"),
         
         Lkt_Lexer_Kw => new Text_Type'("lexer"),
         
         Lkt_Grammar_Kw => new Text_Type'("grammar"),
         
         Lkt_Class_Kw => new Text_Type'("class"),
         
         Lkt_Struct_Kw => new Text_Type'("struct"),
         
         Lkt_Fun_Kw => new Text_Type'("fun"),
         
         Lkt_Public_Kw => new Text_Type'("public"),
         
         Lkt_Private_Kw => new Text_Type'("private"),
         
         Lkt_Null_Kw => new Text_Type'("null"),
         
         Lkt_Is_Kw => new Text_Type'("is"),
         
         Lkt_In_Kw => new Text_Type'("in"),
         
         Lkt_Val_Kw => new Text_Type'("val"),
         
         Lkt_When_Kw => new Text_Type'("when"),
         
         Lkt_If_Kw => new Text_Type'("if"),
         
         Lkt_Elif_Kw => new Text_Type'("elif"),
         
         Lkt_Else_Kw => new Text_Type'("else"),
         
         Lkt_Then_Kw => new Text_Type'("then"),
         
         Lkt_And_Kw => new Text_Type'("and"),
         
         Lkt_Or_Kw => new Text_Type'("or"),
         
         Lkt_Not_Kw => new Text_Type'("not"),
         
         Lkt_Bind_Kw => new Text_Type'("bind"),
         
         Lkt_Match_Kw => new Text_Type'("match"),
         
         Lkt_Case_Kw => new Text_Type'("case"),
         
         Lkt_From_Kw => new Text_Type'("from"),
         
         Lkt_Select_Kw => new Text_Type'("select"),
         
         Lkt_Raise_Kw => new Text_Type'("raise"),
         
         Lkt_Try_Kw => new Text_Type'("try"),
         
         Lkt_Enum_Kw => new Text_Type'("enum"),
         
         Lkt_Generic_Kw => new Text_Type'("generic"),
         
         Lkt_Discard_Kw => new Text_Type'("discard"),
         
         Lkt_Import_Kw => new Text_Type'("import"),
         
         Lkt_Implements_Kw => new Text_Type'("implements"),
         
         Lkt_Trait_Kw => new Text_Type'("trait"),
         
         Lkt_Dyn_Var_Kw => new Text_Type'("dynvar"),
         
      others => new Text_Type'("")
   );

   Token_Kind_Names : constant array (Token_Kind) of String_Access := (
          Lkt_Whitespace =>
             new String'("Whitespace")
              ,
          Lkt_Excl_Mark =>
             new String'("Excl_Mark")
              ,
          Lkt_Semicolon =>
             new String'("Semicolon")
              ,
          Lkt_Colon =>
             new String'("Colon")
              ,
          Lkt_Double_Colon =>
             new String'("Double_Colon")
              ,
          Lkt_Triple_Colon =>
             new String'("Triple_Colon")
              ,
          Lkt_Int_Mark =>
             new String'("Int_Mark")
              ,
          Lkt_Div =>
             new String'("Div")
              ,
          Lkt_Times =>
             new String'("Times")
              ,
          Lkt_Plus =>
             new String'("Plus")
              ,
          Lkt_Amp =>
             new String'("Amp")
              ,
          Lkt_Minus =>
             new String'("Minus")
              ,
          Lkt_Pipe =>
             new String'("Pipe")
              ,
          Lkt_Two_Sided_Arrow =>
             new String'("Two_Sided_Arrow")
              ,
          Lkt_Left_Arrow =>
             new String'("Left_Arrow")
              ,
          Lkt_Right_Arrow =>
             new String'("Right_Arrow")
              ,
          Lkt_Dot =>
             new String'("Dot")
              ,
          Lkt_L_Par =>
             new String'("L_Par")
              ,
          Lkt_R_Par =>
             new String'("R_Par")
              ,
          Lkt_L_Brack =>
             new String'("L_Brack")
              ,
          Lkt_R_Brack =>
             new String'("R_Brack")
              ,
          Lkt_L_Brace =>
             new String'("L_Brace")
              ,
          Lkt_R_Brace =>
             new String'("R_Brace")
              ,
          Lkt_Comb =>
             new String'("Comb")
              ,
          Lkt_Comma =>
             new String'("Comma")
              ,
          Lkt_Ellipsis =>
             new String'("Ellipsis")
              ,
          Lkt_At =>
             new String'("At")
              ,
          Lkt_Fat_Right_Arrow =>
             new String'("Fat_Right_Arrow")
              ,
          Lkt_Equal =>
             new String'("Equal")
              ,
          Lkt_E_Q =>
             new String'("E_Q")
              ,
          Lkt_N_E =>
             new String'("N_E")
              ,
          Lkt_L_T_E =>
             new String'("L_T_E")
              ,
          Lkt_G_T_E =>
             new String'("G_T_E")
              ,
          Lkt_L_T =>
             new String'("L_T")
              ,
          Lkt_G_T =>
             new String'("G_T")
              ,
          Lkt_Percent =>
             new String'("Percent")
              ,
          Lkt_Lexer_Kw =>
             new String'("Lexer_Kw")
              ,
          Lkt_Grammar_Kw =>
             new String'("Grammar_Kw")
              ,
          Lkt_Class_Kw =>
             new String'("Class_Kw")
              ,
          Lkt_Struct_Kw =>
             new String'("Struct_Kw")
              ,
          Lkt_Fun_Kw =>
             new String'("Fun_Kw")
              ,
          Lkt_Public_Kw =>
             new String'("Public_Kw")
              ,
          Lkt_Private_Kw =>
             new String'("Private_Kw")
              ,
          Lkt_Null_Kw =>
             new String'("Null_Kw")
              ,
          Lkt_Is_Kw =>
             new String'("Is_Kw")
              ,
          Lkt_In_Kw =>
             new String'("In_Kw")
              ,
          Lkt_Val_Kw =>
             new String'("Val_Kw")
              ,
          Lkt_When_Kw =>
             new String'("When_Kw")
              ,
          Lkt_If_Kw =>
             new String'("If_Kw")
              ,
          Lkt_Elif_Kw =>
             new String'("Elif_Kw")
              ,
          Lkt_Else_Kw =>
             new String'("Else_Kw")
              ,
          Lkt_Then_Kw =>
             new String'("Then_Kw")
              ,
          Lkt_And_Kw =>
             new String'("And_Kw")
              ,
          Lkt_Or_Kw =>
             new String'("Or_Kw")
              ,
          Lkt_Not_Kw =>
             new String'("Not_Kw")
              ,
          Lkt_Bind_Kw =>
             new String'("Bind_Kw")
              ,
          Lkt_Match_Kw =>
             new String'("Match_Kw")
              ,
          Lkt_Case_Kw =>
             new String'("Case_Kw")
              ,
          Lkt_From_Kw =>
             new String'("From_Kw")
              ,
          Lkt_Select_Kw =>
             new String'("Select_Kw")
              ,
          Lkt_Raise_Kw =>
             new String'("Raise_Kw")
              ,
          Lkt_Try_Kw =>
             new String'("Try_Kw")
              ,
          Lkt_Enum_Kw =>
             new String'("Enum_Kw")
              ,
          Lkt_Generic_Kw =>
             new String'("Generic_Kw")
              ,
          Lkt_Discard_Kw =>
             new String'("Discard_Kw")
              ,
          Lkt_Import_Kw =>
             new String'("Import_Kw")
              ,
          Lkt_Implements_Kw =>
             new String'("Implements_Kw")
              ,
          Lkt_Trait_Kw =>
             new String'("Trait_Kw")
              ,
          Lkt_Dyn_Var_Kw =>
             new String'("Dyn_Var_Kw")
              ,
          Lkt_Identifier =>
             new String'("Identifier")
              ,
          Lkt_Number =>
             new String'("Number")
              ,
          Lkt_Big_Number =>
             new String'("Big_Number")
              ,
          Lkt_String =>
             new String'("String")
              ,
          Lkt_P_String =>
             new String'("P_String")
              ,
          Lkt_Char =>
             new String'("Char")
              ,
          Lkt_Module_Doc_String_Line =>
             new String'("Module_Doc_String_Line")
              ,
          Lkt_Block_String_Line =>
             new String'("Block_String_Line")
              ,
          Lkt_Comment =>
             new String'("Comment")
              ,
          Lkt_Termination =>
             new String'("Termination")
              ,
          Lkt_Lexing_Failure =>
             new String'("Lexing_Failure")
   );

   ---------------------
   -- Token_Kind_Name --
   ---------------------

   function Token_Kind_Name (Token_Id : Token_Kind) return String is
     (Token_Kind_Names (Token_Id).all);

   ------------------------
   -- Token_Kind_Literal --
   ------------------------

   function Token_Kind_Literal (Token_Id : Token_Kind) return Text_Type is
     (Token_Kind_To_Literals (Token_Id).all);

   -----------------------
   -- Token_Error_Image --
   -----------------------

   function Token_Error_Image (Token_Id : Token_Kind) return String is
      Literal : constant Text_Type := Token_Kind_Literal (Token_Id);
   begin
      return (if Literal /= ""
              then "'" & Image (Literal) & "'"
              else Token_Kind_Name (Token_Id));
   end Token_Error_Image;

   function To_Token_Kind (Raw : Raw_Token_Kind) return Token_Kind
   is (Token_Kind'Val (Raw));

   function From_Token_Kind (Kind : Token_Kind) return Raw_Token_Kind
   is (Token_Kind'Pos (Kind));

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Kind : Lkt_Node_Kind_Type) return Boolean is
   begin
      return Is_Token_Node_Kind (Kind);
   end Is_Token_Node;

   -------------------
   -- Is_Error_Node --
   -------------------

   function Is_Error_Node (Kind : Lkt_Node_Kind_Type) return Boolean is
   begin
      return Is_Error_Node_Kind (Kind);
   end Is_Error_Node;

   ------------------
   -- Is_List_Node --
   ------------------

   function Is_List_Node (Kind : Lkt_Node_Kind_Type) return Boolean is
   begin
         return Kind in Lkt_Lkt_Node_Base_List;
   end Is_List_Node;

   ------------------
   -- Rewrap_Token --
   ------------------

   function Rewrap_Token
     (Origin : Token_Reference;
      Index  : Token_Or_Trivia_Index) return Token_Reference is
   begin
      return (if Index = No_Token_Or_Trivia_Index
              then No_Token
              else (Origin.TDH, Index, Origin.Safety_Net));
   end Rewrap_Token;

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Self : Token_Reference) is
      SN  : Token_Safety_Net renames Self.Safety_Net;
      Ctx : constant Internal_Context := +SN.Context;
   begin
      if Self.TDH /= null
         and then (Ctx.Serial_Number /= SN.Context_Version
                   or else Self.TDH.Version /= SN.TDH_Version)
      then
         raise Stale_Reference_Error;
      end if;
   end Check_Safety_Net;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Token_Reference) return Boolean is
      pragma Assert (Left.TDH = Right.TDH);
   begin
      Check_Safety_Net (Left);
      Check_Safety_Net (Right);
      if Left.Index.Token < Right.Index.Token then
         return True;

      elsif Left.Index.Token = Right.Index.Token then
         return Left.Index.Trivia < Right.Index.Trivia;

      else
         return False;
      end if;
   end "<";

   ----------
   -- Next --
   ----------

   function Next
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference is
   begin
      Check_Safety_Net (Token);
      return (if Token.TDH = null
              then No_Token
              else Rewrap_Token (Token,
                                 Next (Token.Index, Token.TDH.all,
                                       Exclude_Trivia)));
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference is
   begin
      Check_Safety_Net (Token);
      return (if Token.TDH = null
              then No_Token
              else Rewrap_Token (Token,
                                 Previous (Token.Index, Token.TDH.all,
                                           Exclude_Trivia)));
   end Previous;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol (Token : Token_Reference) return Symbol_Type is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return Get_Symbol (Token.Index, Token.TDH.all);
   end Get_Symbol;

   ----------
   -- Data --
   ----------

   function Data (Token : Token_Reference) return Token_Data_Type is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return Convert (Token.TDH.all, Token, Raw_Data (Token));
   end Data;

   ----------
   -- Text --
   ----------

   function Text (Token : Token_Reference) return Text_Type is
      RD : constant Stored_Token_Data := Raw_Data (Token);
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return Token.TDH.Source_Buffer (RD.Source_First .. RD.Source_Last);
   end Text;

   ----------
   -- Text --
   ----------

   function Text (First, Last : Token_Reference) return Text_Type is
      FD, LD : Token_Data_Type;
   begin
      Check_Safety_Net (First);
      Check_Safety_Net (Last);
      if First.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      if First.TDH /= Last.TDH then
         raise Precondition_Failure with
            "token arguments must belong to the same source";
      end if;
      FD := Data (First);
      LD := Data (Last);
      return FD.Source_Buffer.all (FD.Source_First .. LD.Source_Last);
   end Text;

   ----------
   -- Kind --
   ----------

   function Kind (Token_Data : Token_Data_Type) return Token_Kind is
   begin
      return Token_Data.Kind;
   end Kind;

   ---------------
   -- Is_Trivia --
   ---------------

   function Is_Trivia (Token : Token_Reference) return Boolean is
   begin
      Check_Safety_Net (Token);
      return Token.Index.Trivia /= No_Token_Index;
   end Is_Trivia;

   ---------------
   -- Is_Trivia --
   ---------------

   function Is_Trivia (Token_Data : Token_Data_Type) return Boolean is
   begin
      return Token_Data.Is_Trivia;
   end Is_Trivia;

   -----------
   -- Index --
   -----------

   function Index (Token : Token_Reference) return Token_Index is
   begin
      Check_Safety_Net (Token);
      return (if Token.Index.Trivia = No_Token_Index
              then Token.Index.Token
              else Token.Index.Trivia);
   end Index;

   -----------
   -- Index --
   -----------

   function Index (Token_Data : Token_Data_Type) return Token_Index is
   begin
      return Token_Data.Index;
   end Index;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Token_Data : Token_Data_Type) return Source_Location_Range
   is
   begin
      return Token_Data.Sloc_Range;
   end Sloc_Range;

   ---------------------
   -- Origin_Filename --
   ---------------------

   function Origin_Filename (Token : Token_Reference) return String is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return +Token.TDH.Filename.Full_Name;
   end Origin_Filename;

   --------------------
   -- Origin_Charset --
   --------------------

   function Origin_Charset (Token : Token_Reference) return String is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return To_String (Token.TDH.Charset);
   end Origin_Charset;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent (L, R : Token_Reference) return Boolean is
      DL : constant Stored_Token_Data := Raw_Data (L);
      DR : constant Stored_Token_Data := Raw_Data (R);
   begin
      --  Two tokens with different kinds are never equivalent

      if DL.Kind /= DR.Kind then
         return False;
      end if;

      --  Depending on the token kind involved, the equivalence considers
      --  different token attributes: just the kind, the symbol or the actual
      --  token text.

      
      case To_Token_Kind (DL.Kind) is
            when Lkt_Amp | Lkt_And_Kw | Lkt_At | Lkt_Bind_Kw | Lkt_Case_Kw | Lkt_Class_Kw | Lkt_Colon | Lkt_Comb | Lkt_Comma | Lkt_Discard_Kw | Lkt_Div | Lkt_Dot | Lkt_Double_Colon | Lkt_Dyn_Var_Kw | Lkt_E_Q | Lkt_Elif_Kw | Lkt_Ellipsis | Lkt_Else_Kw | Lkt_Enum_Kw | Lkt_Equal | Lkt_Excl_Mark | Lkt_Fat_Right_Arrow | Lkt_From_Kw | Lkt_Fun_Kw | Lkt_G_T | Lkt_G_T_E | Lkt_Generic_Kw | Lkt_Grammar_Kw | Lkt_If_Kw | Lkt_Implements_Kw | Lkt_Import_Kw | Lkt_In_Kw | Lkt_Int_Mark | Lkt_Is_Kw | Lkt_L_Brace | Lkt_L_Brack | Lkt_L_Par | Lkt_L_T | Lkt_L_T_E | Lkt_Left_Arrow | Lkt_Lexer_Kw | Lkt_Lexing_Failure | Lkt_Match_Kw | Lkt_Minus | Lkt_N_E | Lkt_Not_Kw | Lkt_Null_Kw | Lkt_Or_Kw | Lkt_Percent | Lkt_Pipe | Lkt_Plus | Lkt_Private_Kw | Lkt_Public_Kw | Lkt_R_Brace | Lkt_R_Brack | Lkt_R_Par | Lkt_Raise_Kw | Lkt_Right_Arrow | Lkt_Select_Kw | Lkt_Semicolon | Lkt_Struct_Kw | Lkt_Termination | Lkt_Then_Kw | Lkt_Times | Lkt_Trait_Kw | Lkt_Triple_Colon | Lkt_Try_Kw | Lkt_Two_Sided_Arrow | Lkt_Val_Kw | Lkt_When_Kw =>
               return True;

            when Lkt_Identifier =>

               --  Comparing the symbol reference itself is invalid when L and
               --  R belong to two different contexts (the two symbol
               --  references designate symbols in different symbol tables):
               --  compare the texts behind symbols for such cases.

               return (if L.TDH.Symbols = R.TDH.Symbols
                       then DL.Symbol = DR.Symbol
                       else Get (L.TDH.Symbols, DL.Symbol).all
                            = Get (R.TDH.Symbols, DR.Symbol).all);

            when Lkt_Big_Number | Lkt_Block_String_Line | Lkt_Char | Lkt_Comment | Lkt_Module_Doc_String_Line | Lkt_Number | Lkt_P_String | Lkt_String | Lkt_Whitespace =>
               declare
                  TL : Text_Type renames
                    L.TDH.Source_Buffer (DL.Source_First .. DL.Source_Last);
                  TR : Text_Type renames
                    R.TDH.Source_Buffer (DR.Source_First .. DR.Source_Last);
               begin
                  return TL = TR;
               end;
      end case;
   end Is_Equivalent;

   -----------
   -- Image --
   -----------

   function Image (Token : Token_Reference) return String is
      D : constant Token_Data_Type := Data (Token);
   begin
      return ("<Token Kind=" & Token_Kind_Name (D.Kind) &
              " Text=" & Image (Text (Token), With_Quotes => True) & ">");
   end Image;

   --------------
   -- Raw_Data --
   --------------

   function Raw_Data (T : Token_Reference) return Stored_Token_Data is
   begin
      Check_Safety_Net (T);
      if T.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return
        (if T.Index.Trivia = No_Token_Index
         then Token_Vectors.Get (T.TDH.Tokens, Natural (T.Index.Token))
         else Trivia_Vectors.Get (T.TDH.Trivias, Natural (T.Index.Trivia)).T);
   end Raw_Data;

   -------------
   -- Convert --
   -------------

   function Convert
     (TDH      : Token_Data_Handler;
      Token    : Token_Reference;
      Raw_Data : Stored_Token_Data) return Token_Data_Type is
   begin
      Check_Safety_Net (Token);
      return (Kind          => To_Token_Kind (Raw_Data.Kind),
              Is_Trivia     => Token.Index.Trivia /= No_Token_Index,
              Index         => (if Token.Index.Trivia = No_Token_Index
                                then Token.Index.Token
                                else Token.Index.Trivia),
              Source_Buffer => Text_Cst_Access (TDH.Source_Buffer),
              Source_First  => Raw_Data.Source_First,
              Source_Last   => Raw_Data.Source_Last,
              Sloc_Range    => Sloc_Range (TDH, Raw_Data));
   end Convert;

   ------------------
   -- From_Generic --
   ------------------

   function From_Generic (Token : Lk_Token) return Common.Token_Reference is
      use Liblktlang_Support.Internal.Conversions;
      Id         : Any_Language_Id;
      Data       : Liblktlang_Support.Internal.Analysis.Internal_Token;
      Safety_Net : Liblktlang_Support.Internal.Analysis.Token_Safety_Net;
   begin
      Unwrap_Token (Token, Id, Data, Safety_Net);
      pragma Assert (Id = Generic_API.Self_Id);
      return (Data.TDH,
              Data.Index,
              (Safety_Net.Context,
               Safety_Net.Context_Version,
               Safety_Net.TDH_Version));
   end From_Generic;

   ----------------
   -- To_Generic --
   ----------------

   function To_Generic (Token : Common.Token_Reference) return Lk_Token is
      use Liblktlang_Support.Internal.Conversions;
   begin
      return Wrap_Token
        (Generic_API.Self_Id,
         (Token.TDH, Token.Index),
         (Token.Safety_Net.Context,
          Token.Safety_Net.Context_Version,
          Token.Safety_Net.TDH_Version));
   end To_Generic;

   --------------------------
   -- Wrap_Token_Reference --
   --------------------------

   function Wrap_Token_Reference
     (Context : Internal_Context;
      TDH     : Token_Data_Handler_Access;
      Index   : Token_Or_Trivia_Index) return Token_Reference is
   begin
      if Index = No_Token_Or_Trivia_Index then
         return No_Token;
      end if;

      declare
         SN : constant Token_Safety_Net :=
           (Context         => +Context,
            Context_Version => Context.Serial_Number,
            TDH_Version     => TDH.Version);
      begin
        return (TDH, Index, SN);
      end;
   end Wrap_Token_Reference;

   --------------------
   -- Get_Token_Unit --
   --------------------

   function Get_Token_Unit (Token : Token_Reference) return Internal_Unit is
      function "+" is new Ada.Unchecked_Conversion
        (System.Address, Internal_Unit);
   begin
      if Token = No_Token then
         raise Precondition_Failure with "null token argument";
      end if;
      Check_Safety_Net (Token);
      return +Token.TDH.Owner;
   end Get_Token_Unit;

   -----------------------
   -- Get_Token_Context --
   -----------------------

   function Get_Token_Context
     (Token : Token_Reference) return Internal_Context is
   begin
      return +Token.Safety_Net.Context;
   end Get_Token_Context;

   -------------------
   -- Get_Token_TDH --
   -------------------

   function Get_Token_TDH
     (Token : Token_Reference) return Token_Data_Handler_Access is
   begin
      return Token.TDH;
   end Get_Token_TDH;

   ---------------------
   -- Get_Token_Index --
   ---------------------

   function Get_Token_Index
     (Token : Token_Reference) return Token_Or_Trivia_Index is
   begin
      return Token.Index;
   end Get_Token_Index;

   ------------------------
   -- Extract_Token_Text --
   ------------------------

   procedure Extract_Token_Text
     (Token         : Token_Data_Type;
      Source_Buffer : out Text_Cst_Access;
      First         : out Positive;
      Last          : out Natural) is
   begin
      Source_Buffer := Token.Source_Buffer;
      First := Token.Source_First;
      Last := Token.Source_Last;
   end Extract_Token_Text;

   ---------------------
   -- Token_Node_Kind --
   ---------------------

   function Token_Node_Kind (Kind : Lkt_Node_Kind_Type) return Token_Kind is
      
   begin
         case Kind is
               when Lkt_Block_String_Line =>
                  return Lkt_Block_String_Line;
               when Lkt_Token_Lit =>
                  return Lkt_String;
               when Lkt_Token_Pattern_Lit =>
                  return Lkt_P_String;
               when Lkt_Id =>
                  return Lkt_Identifier;
               when Lkt_Def_Id =>
                  return Lkt_Identifier;
               when Lkt_Imported_Id =>
                  return Lkt_Identifier;
               when Lkt_Module_Id =>
                  return Lkt_Identifier;
               when Lkt_Ref_Id =>
                  return Lkt_Identifier;
               when Lkt_Big_Num_Lit =>
                  return Lkt_Big_Number;
               when Lkt_Char_Lit =>
                  return Lkt_Char;
               when Lkt_Num_Lit =>
                  return Lkt_Number;
               when Lkt_Single_Line_String_Lit =>
                  return Lkt_String;
               when Lkt_Pattern_Single_Line_String_Lit =>
                  return Lkt_P_String;
               when Lkt_Module_Doc_String_Line =>
                  return Lkt_Module_Doc_String_Line;
               when Lkt_Integer_Pattern =>
                  return Lkt_Number;
               when Lkt_Regex_Pattern =>
                  return Lkt_String;
               when Lkt_Default_List_Type_Ref =>
                  return Lkt_Identifier;

            when others =>
               --  Kind is not a token node, and thus the precondition does not
               --  hold.
               return (raise Program_Error);
         end case;

   end Token_Node_Kind;


begin
   --  Check that we actually have full Libiconv support: as nothing works
   --  without it, we explicitly check support here instead of letting
   --  user-unfriendly errors happen during lexing.

   if not GNATCOLL.Iconv.Has_Iconv then
      raise Program_Error with "Libiconv is not available";
   end if;


   Private_Converters.Wrap_Token_Reference := Wrap_Token_Reference'Access;
   Private_Converters.Get_Token_Context := Get_Token_Context'Access;
   Private_Converters.Get_Token_Unit := Get_Token_Unit'Access;
   Private_Converters.Get_Token_TDH := Get_Token_TDH'Access;
   Private_Converters.Get_Token_Index := Get_Token_Index'Access;
   Private_Converters.Extract_Token_Text := Extract_Token_Text'Access;
end Liblktlang.Common;
