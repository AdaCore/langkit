
with Ada.Unchecked_Deallocation;

with Liblktlang_Support.Generic_API; use Liblktlang_Support.Generic_API;
pragma Warnings (Off, "referenced");
with Liblktlang_Support.Generic_API.Analysis;
use Liblktlang_Support.Generic_API.Analysis;
pragma Warnings (On, "referenced");
with Liblktlang_Support.Generic_API.Introspection;
use Liblktlang_Support.Generic_API.Introspection;
with Liblktlang_Support.Internal.Introspection;
use Liblktlang_Support.Internal.Introspection;
with Liblktlang_Support.Text;        use Liblktlang_Support.Text;

with Liblktlang.Analysis; use Liblktlang.Analysis;
with Liblktlang.Common;   use Liblktlang.Common;

--  This package provides description tables to enable the generic
--  introspection API in Liblktlang_Support to work with this Langkit-generated
--  library.

private package Liblktlang.Generic_Introspection is

   

   --------------------------
   -- Type index constants --
   --------------------------

      Type_Index_For_Analysis_Unit : constant Type_Index := 1;
      Type_Index_For_Big_Int : constant Type_Index := 2;
      Type_Index_For_Bool : constant Type_Index := 3;
      Type_Index_For_Char : constant Type_Index := 4;
      Type_Index_For_Int : constant Type_Index := 5;
      Type_Index_For_Source_Location : constant Type_Index := 6;
      Type_Index_For_Source_Location_Range : constant Type_Index := 7;
      Type_Index_For_String : constant Type_Index := 8;
      Type_Index_For_Token : constant Type_Index := 9;
      Type_Index_For_Symbol : constant Type_Index := 10;
      Type_Index_For_Analysis_Unit_Kind : constant Type_Index := 11;
      Type_Index_For_Completion_Item_Kind : constant Type_Index := 12;
      Type_Index_For_Designated_Env_Kind : constant Type_Index := 13;
      Type_Index_For_Grammar_Rule : constant Type_Index := 14;
      Type_Index_For_Lookup_Kind : constant Type_Index := 15;
      Type_Index_For_Complete_Item_Array : constant Type_Index := 16;
      Type_Index_For_Lkt_Node_Array : constant Type_Index := 17;
      Type_Index_For_Def_Id_Array : constant Type_Index := 18;
      Type_Index_For_Fun_Decl_Array : constant Type_Index := 19;
      Type_Index_For_Logic_Context_Array : constant Type_Index := 20;
      Type_Index_For_Ref_Result_Array : constant Type_Index := 21;
      Type_Index_For_Solver_Diagnostic_Array : constant Type_Index := 22;
      Type_Index_For_Analysis_Unit_Array : constant Type_Index := 23;
      Type_Index_For_Complete_Item : constant Type_Index := 24;
      Type_Index_For_Decoded_Char_Value : constant Type_Index := 25;
      Type_Index_For_Decoded_String_Value : constant Type_Index := 26;
      Type_Index_For_Logic_Context : constant Type_Index := 27;
      Type_Index_For_Ref_Result : constant Type_Index := 28;
      Type_Index_For_Solver_Diagnostic : constant Type_Index := 29;
      Type_Index_For_Solver_Result : constant Type_Index := 30;
      Type_Index_For_Lkt_Node : constant Type_Index := 31;
      Type_Index_For_Argument : constant Type_Index := 32;
      Type_Index_For_Base_Import : constant Type_Index := 33;
      Type_Index_For_Import : constant Type_Index := 34;
      Type_Index_For_Import_All_From : constant Type_Index := 35;
      Type_Index_For_Import_From : constant Type_Index := 36;
      Type_Index_For_Base_Lexer_Case_Rule_Alt : constant Type_Index := 37;
      Type_Index_For_Error_Lexer_Case_Rule_Alt : constant Type_Index := 38;
      Type_Index_For_Lexer_Case_Rule_Cond_Alt : constant Type_Index := 39;
      Type_Index_For_Lexer_Case_Rule_Default_Alt : constant Type_Index := 40;
      Type_Index_For_Base_Match_Branch : constant Type_Index := 41;
      Type_Index_For_Match_Branch : constant Type_Index := 42;
      Type_Index_For_Pattern_Match_Branch : constant Type_Index := 43;
      Type_Index_For_Block_Expr_Clause : constant Type_Index := 44;
      Type_Index_For_Block_String_Line : constant Type_Index := 45;
      Type_Index_For_Class_Qualifier : constant Type_Index := 46;
      Type_Index_For_Class_Qualifier_Absent : constant Type_Index := 47;
      Type_Index_For_Class_Qualifier_Present : constant Type_Index := 48;
      Type_Index_For_Decl : constant Type_Index := 49;
      Type_Index_For_Base_Grammar_Rule_Decl : constant Type_Index := 50;
      Type_Index_For_Grammar_Rule_Decl : constant Type_Index := 51;
      Type_Index_For_Synthetic_Lexer_Decl : constant Type_Index := 52;
      Type_Index_For_Base_Val_Decl : constant Type_Index := 53;
      Type_Index_For_Node_Decl : constant Type_Index := 54;
      Type_Index_For_Self_Decl : constant Type_Index := 55;
      Type_Index_For_User_Val_Decl : constant Type_Index := 56;
      Type_Index_For_Binding_Val_Decl : constant Type_Index := 57;
      Type_Index_For_Enum_Lit_Decl : constant Type_Index := 58;
      Type_Index_For_Explicitly_Typed_Decl : constant Type_Index := 59;
      Type_Index_For_Component_Decl : constant Type_Index := 60;
      Type_Index_For_Field_Decl : constant Type_Index := 61;
      Type_Index_For_Fun_Param_Decl : constant Type_Index := 62;
      Type_Index_For_Lambda_Param_Decl : constant Type_Index := 63;
      Type_Index_For_Dyn_Var_Decl : constant Type_Index := 64;
      Type_Index_For_Match_Val_Decl : constant Type_Index := 65;
      Type_Index_For_Val_Decl : constant Type_Index := 66;
      Type_Index_For_Fun_Decl : constant Type_Index := 67;
      Type_Index_For_Env_Spec_Decl : constant Type_Index := 68;
      Type_Index_For_Error_Decl : constant Type_Index := 69;
      Type_Index_For_Generic_Decl : constant Type_Index := 70;
      Type_Index_For_Grammar_Decl : constant Type_Index := 71;
      Type_Index_For_Langkit_Root : constant Type_Index := 72;
      Type_Index_For_Lexer_Decl : constant Type_Index := 73;
      Type_Index_For_Lexer_Family_Decl : constant Type_Index := 74;
      Type_Index_For_Synth_Fun_Decl : constant Type_Index := 75;
      Type_Index_For_Synth_Param_Decl : constant Type_Index := 76;
      Type_Index_For_Type_Decl : constant Type_Index := 77;
      Type_Index_For_Any_Type_Decl : constant Type_Index := 78;
      Type_Index_For_Enum_Class_Alt_Decl : constant Type_Index := 79;
      Type_Index_For_Function_Type : constant Type_Index := 80;
      Type_Index_For_Generic_Param_Type_Decl : constant Type_Index := 81;
      Type_Index_For_Named_Type_Decl : constant Type_Index := 82;
      Type_Index_For_Basic_Class_Decl : constant Type_Index := 83;
      Type_Index_For_Class_Decl : constant Type_Index := 84;
      Type_Index_For_Enum_Class_Decl : constant Type_Index := 85;
      Type_Index_For_Enum_Type_Decl : constant Type_Index := 86;
      Type_Index_For_Struct_Decl : constant Type_Index := 87;
      Type_Index_For_Trait_Decl : constant Type_Index := 88;
      Type_Index_For_Decl_Annotation : constant Type_Index := 89;
      Type_Index_For_Decl_Annotation_Args : constant Type_Index := 90;
      Type_Index_For_Dyn_Env_Wrapper : constant Type_Index := 91;
      Type_Index_For_Elsif_Branch : constant Type_Index := 92;
      Type_Index_For_Enum_Class_Case : constant Type_Index := 93;
      Type_Index_For_Excludes_Null : constant Type_Index := 94;
      Type_Index_For_Excludes_Null_Absent : constant Type_Index := 95;
      Type_Index_For_Excludes_Null_Present : constant Type_Index := 96;
      Type_Index_For_Expr : constant Type_Index := 97;
      Type_Index_For_Any_Of : constant Type_Index := 98;
      Type_Index_For_Array_Literal : constant Type_Index := 99;
      Type_Index_For_Base_Call_Expr : constant Type_Index := 100;
      Type_Index_For_Call_Expr : constant Type_Index := 101;
      Type_Index_For_Logic_Call_Expr : constant Type_Index := 102;
      Type_Index_For_Logic_Predicate : constant Type_Index := 103;
      Type_Index_For_Logic_Propagate_Call : constant Type_Index := 104;
      Type_Index_For_Bin_Op : constant Type_Index := 105;
      Type_Index_For_Block_Expr : constant Type_Index := 106;
      Type_Index_For_Cast_Expr : constant Type_Index := 107;
      Type_Index_For_Dot_Expr : constant Type_Index := 108;
      Type_Index_For_Error_On_Null : constant Type_Index := 109;
      Type_Index_For_Generic_Instantiation : constant Type_Index := 110;
      Type_Index_For_Grammar_Expr : constant Type_Index := 111;
      Type_Index_For_Error_Grammar_Expr : constant Type_Index := 112;
      Type_Index_For_Grammar_Cut : constant Type_Index := 113;
      Type_Index_For_Grammar_Discard : constant Type_Index := 114;
      Type_Index_For_Grammar_Dont_Skip : constant Type_Index := 115;
      Type_Index_For_Grammar_List : constant Type_Index := 116;
      Type_Index_For_Grammar_Null : constant Type_Index := 117;
      Type_Index_For_Grammar_Opt : constant Type_Index := 118;
      Type_Index_For_Grammar_Opt_Error : constant Type_Index := 119;
      Type_Index_For_Grammar_Opt_Error_Group : constant Type_Index := 120;
      Type_Index_For_Grammar_Opt_Group : constant Type_Index := 121;
      Type_Index_For_Grammar_Or_Expr : constant Type_Index := 122;
      Type_Index_For_Grammar_Pick : constant Type_Index := 123;
      Type_Index_For_Grammar_Implicit_Pick : constant Type_Index := 124;
      Type_Index_For_Grammar_Predicate : constant Type_Index := 125;
      Type_Index_For_Grammar_Rule_Ref : constant Type_Index := 126;
      Type_Index_For_Grammar_Skip : constant Type_Index := 127;
      Type_Index_For_Grammar_Stop_Cut : constant Type_Index := 128;
      Type_Index_For_Parse_Node_Expr : constant Type_Index := 129;
      Type_Index_For_Token_Lit : constant Type_Index := 130;
      Type_Index_For_Token_No_Case_Lit : constant Type_Index := 131;
      Type_Index_For_Token_Pattern_Concat : constant Type_Index := 132;
      Type_Index_For_Token_Pattern_Lit : constant Type_Index := 133;
      Type_Index_For_Token_Ref : constant Type_Index := 134;
      Type_Index_For_Id : constant Type_Index := 135;
      Type_Index_For_Def_Id : constant Type_Index := 136;
      Type_Index_For_Imported_Id : constant Type_Index := 137;
      Type_Index_For_Module_Id : constant Type_Index := 138;
      Type_Index_For_Ref_Id : constant Type_Index := 139;
      Type_Index_For_If_Expr : constant Type_Index := 140;
      Type_Index_For_Isa : constant Type_Index := 141;
      Type_Index_For_Keep_Expr : constant Type_Index := 142;
      Type_Index_For_Lambda_Expr : constant Type_Index := 143;
      Type_Index_For_Lit : constant Type_Index := 144;
      Type_Index_For_Big_Num_Lit : constant Type_Index := 145;
      Type_Index_For_Char_Lit : constant Type_Index := 146;
      Type_Index_For_Null_Lit : constant Type_Index := 147;
      Type_Index_For_Num_Lit : constant Type_Index := 148;
      Type_Index_For_String_Lit : constant Type_Index := 149;
      Type_Index_For_Block_String_Lit : constant Type_Index := 150;
      Type_Index_For_Module_Doc_String_Lit : constant Type_Index := 151;
      Type_Index_For_Single_Line_String_Lit : constant Type_Index := 152;
      Type_Index_For_Pattern_Single_Line_String_Lit : constant Type_Index := 153;
      Type_Index_For_Logic_Assign : constant Type_Index := 154;
      Type_Index_For_Logic_Expr : constant Type_Index := 155;
      Type_Index_For_Logic_Propagate : constant Type_Index := 156;
      Type_Index_For_Logic_Unify : constant Type_Index := 157;
      Type_Index_For_Match_Expr : constant Type_Index := 158;
      Type_Index_For_Not_Expr : constant Type_Index := 159;
      Type_Index_For_Paren_Expr : constant Type_Index := 160;
      Type_Index_For_Query : constant Type_Index := 161;
      Type_Index_For_Raise_Expr : constant Type_Index := 162;
      Type_Index_For_Subscript_Expr : constant Type_Index := 163;
      Type_Index_For_Try_Expr : constant Type_Index := 164;
      Type_Index_For_Un_Op : constant Type_Index := 165;
      Type_Index_For_Full_Decl : constant Type_Index := 166;
      Type_Index_For_Grammar_List_Sep : constant Type_Index := 167;
      Type_Index_For_Imported_Name : constant Type_Index := 168;
      Type_Index_For_Lexer_Case_Rule : constant Type_Index := 169;
      Type_Index_For_Lexer_Case_Rule_Send : constant Type_Index := 170;
      Type_Index_For_List_Kind : constant Type_Index := 171;
      Type_Index_For_List_Kind_One : constant Type_Index := 172;
      Type_Index_For_List_Kind_Zero : constant Type_Index := 173;
      Type_Index_For_Lkt_Node_Base_List : constant Type_Index := 174;
      Type_Index_For_Argument_List : constant Type_Index := 175;
      Type_Index_For_Base_Import_List : constant Type_Index := 176;
      Type_Index_For_Base_Lexer_Case_Rule_Alt_List : constant Type_Index := 177;
      Type_Index_For_Base_Match_Branch_List : constant Type_Index := 178;
      Type_Index_For_Block_String_Line_List : constant Type_Index := 179;
      Type_Index_For_Call_Expr_List : constant Type_Index := 180;
      Type_Index_For_Decl_Annotation_List : constant Type_Index := 181;
      Type_Index_For_Elsif_Branch_List : constant Type_Index := 182;
      Type_Index_For_Enum_Class_Alt_Decl_List : constant Type_Index := 183;
      Type_Index_For_Enum_Class_Case_List : constant Type_Index := 184;
      Type_Index_For_Enum_Lit_Decl_List : constant Type_Index := 185;
      Type_Index_For_Expr_List : constant Type_Index := 186;
      Type_Index_For_Any_Of_List : constant Type_Index := 187;
      Type_Index_For_Full_Decl_List : constant Type_Index := 188;
      Type_Index_For_Decl_Block : constant Type_Index := 189;
      Type_Index_For_Generic_Param_Decl_List : constant Type_Index := 190;
      Type_Index_For_Fun_Param_Decl_List : constant Type_Index := 191;
      Type_Index_For_Grammar_Expr_List : constant Type_Index := 192;
      Type_Index_For_Grammar_Expr_List_List : constant Type_Index := 193;
      Type_Index_For_Imported_Name_List : constant Type_Index := 194;
      Type_Index_For_Lambda_Param_Decl_List : constant Type_Index := 195;
      Type_Index_For_Lkt_Node_List : constant Type_Index := 196;
      Type_Index_For_Module_Doc_String_Line_List : constant Type_Index := 197;
      Type_Index_For_Pattern_Detail_List : constant Type_Index := 198;
      Type_Index_For_Pattern_List : constant Type_Index := 199;
      Type_Index_For_Ref_Id_List : constant Type_Index := 200;
      Type_Index_For_Type_Ref_List : constant Type_Index := 201;
      Type_Index_For_Synthetic_Type_Ref_List : constant Type_Index := 202;
      Type_Index_For_Module_Doc_String_Line : constant Type_Index := 203;
      Type_Index_For_Null_Cond_Qualifier : constant Type_Index := 204;
      Type_Index_For_Null_Cond_Qualifier_Absent : constant Type_Index := 205;
      Type_Index_For_Null_Cond_Qualifier_Present : constant Type_Index := 206;
      Type_Index_For_Op : constant Type_Index := 207;
      Type_Index_For_Op_Amp : constant Type_Index := 208;
      Type_Index_For_Op_And : constant Type_Index := 209;
      Type_Index_For_Op_Div : constant Type_Index := 210;
      Type_Index_For_Op_Eq : constant Type_Index := 211;
      Type_Index_For_Op_Gt : constant Type_Index := 212;
      Type_Index_For_Op_Gte : constant Type_Index := 213;
      Type_Index_For_Op_Logic_And : constant Type_Index := 214;
      Type_Index_For_Op_Logic_Or : constant Type_Index := 215;
      Type_Index_For_Op_Lt : constant Type_Index := 216;
      Type_Index_For_Op_Lte : constant Type_Index := 217;
      Type_Index_For_Op_Minus : constant Type_Index := 218;
      Type_Index_For_Op_Mult : constant Type_Index := 219;
      Type_Index_For_Op_Ne : constant Type_Index := 220;
      Type_Index_For_Op_Or : constant Type_Index := 221;
      Type_Index_For_Op_Or_Int : constant Type_Index := 222;
      Type_Index_For_Op_Plus : constant Type_Index := 223;
      Type_Index_For_Op_Stream_Concat : constant Type_Index := 224;
      Type_Index_For_Op_Stream_Cons : constant Type_Index := 225;
      Type_Index_For_Pattern : constant Type_Index := 226;
      Type_Index_For_Any_Type_Pattern : constant Type_Index := 227;
      Type_Index_For_Bool_Pattern : constant Type_Index := 228;
      Type_Index_For_Bool_Pattern_False : constant Type_Index := 229;
      Type_Index_For_Bool_Pattern_True : constant Type_Index := 230;
      Type_Index_For_Complex_Pattern : constant Type_Index := 231;
      Type_Index_For_Renaming_Complex_Pattern : constant Type_Index := 232;
      Type_Index_For_Ellipsis_Pattern : constant Type_Index := 233;
      Type_Index_For_Integer_Pattern : constant Type_Index := 234;
      Type_Index_For_List_Pattern : constant Type_Index := 235;
      Type_Index_For_Not_Pattern : constant Type_Index := 236;
      Type_Index_For_Null_Pattern : constant Type_Index := 237;
      Type_Index_For_Or_Pattern : constant Type_Index := 238;
      Type_Index_For_Paren_Pattern : constant Type_Index := 239;
      Type_Index_For_Regex_Pattern : constant Type_Index := 240;
      Type_Index_For_Type_Pattern : constant Type_Index := 241;
      Type_Index_For_Pattern_Detail : constant Type_Index := 242;
      Type_Index_For_Field_Pattern_Detail : constant Type_Index := 243;
      Type_Index_For_Property_Pattern_Detail : constant Type_Index := 244;
      Type_Index_For_Type_Ref : constant Type_Index := 245;
      Type_Index_For_Default_List_Type_Ref : constant Type_Index := 246;
      Type_Index_For_Function_Type_Ref : constant Type_Index := 247;
      Type_Index_For_Generic_Type_Ref : constant Type_Index := 248;
      Type_Index_For_Simple_Type_Ref : constant Type_Index := 249;
      Type_Index_For_Var_Bind : constant Type_Index := 250;

   ----------------------------
   -- Member index constants --
   ----------------------------

      Member_Index_For_Complete_Item_Declaration : constant Struct_Member_Index := 1;
      Member_Index_For_Decoded_Char_Value_Value : constant Struct_Member_Index := 2;
      Member_Index_For_Decoded_Char_Value_Has_Error : constant Struct_Member_Index := 3;
      Member_Index_For_Decoded_Char_Value_Error_Sloc : constant Struct_Member_Index := 4;
      Member_Index_For_Decoded_Char_Value_Error_Message : constant Struct_Member_Index := 5;
      Member_Index_For_Decoded_String_Value_Value : constant Struct_Member_Index := 6;
      Member_Index_For_Decoded_String_Value_Has_Error : constant Struct_Member_Index := 7;
      Member_Index_For_Decoded_String_Value_Error_Sloc : constant Struct_Member_Index := 8;
      Member_Index_For_Decoded_String_Value_Error_Message : constant Struct_Member_Index := 9;
      Member_Index_For_Logic_Context_Ref_Node : constant Struct_Member_Index := 10;
      Member_Index_For_Logic_Context_Decl_Node : constant Struct_Member_Index := 11;
      Member_Index_For_Ref_Result_Ref : constant Struct_Member_Index := 12;
      Member_Index_For_Solver_Diagnostic_Message_Template : constant Struct_Member_Index := 13;
      Member_Index_For_Solver_Diagnostic_Args : constant Struct_Member_Index := 14;
      Member_Index_For_Solver_Diagnostic_Location : constant Struct_Member_Index := 15;
      Member_Index_For_Solver_Diagnostic_Contexts : constant Struct_Member_Index := 16;
      Member_Index_For_Solver_Diagnostic_Round : constant Struct_Member_Index := 17;
      Member_Index_For_Solver_Result_Success : constant Struct_Member_Index := 18;
      Member_Index_For_Solver_Result_Diagnostics : constant Struct_Member_Index := 19;
      Member_Index_For_Argument_F_Name : constant Struct_Member_Index := 20;
      Member_Index_For_Argument_F_Value : constant Struct_Member_Index := 21;
      Member_Index_For_Import_F_Imported_Names : constant Struct_Member_Index := 22;
      Member_Index_For_Import_All_From_F_Module_Name : constant Struct_Member_Index := 23;
      Member_Index_For_Import_From_F_Module_Name : constant Struct_Member_Index := 24;
      Member_Index_For_Import_From_F_Imported_Names : constant Struct_Member_Index := 25;
      Member_Index_For_Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs : constant Struct_Member_Index := 26;
      Member_Index_For_Lexer_Case_Rule_Cond_Alt_F_Send : constant Struct_Member_Index := 27;
      Member_Index_For_Lexer_Case_Rule_Default_Alt_F_Send : constant Struct_Member_Index := 28;
      Member_Index_For_Base_Match_Branch_F_Expr : constant Struct_Member_Index := 29;
      Member_Index_For_Match_Branch_F_Decl : constant Struct_Member_Index := 30;
      Member_Index_For_Pattern_Match_Branch_F_Pattern : constant Struct_Member_Index := 31;
      Member_Index_For_Block_Expr_Clause_F_Clause : constant Struct_Member_Index := 32;
      Member_Index_For_Decl_F_Syn_Name : constant Struct_Member_Index := 33;
      Member_Index_For_Base_Grammar_Rule_Decl_F_Expr : constant Struct_Member_Index := 34;
      Member_Index_For_Explicitly_Typed_Decl_F_Decl_Type : constant Struct_Member_Index := 35;
      Member_Index_For_Component_Decl_F_Default_Val : constant Struct_Member_Index := 36;
      Member_Index_For_Field_Decl_F_Trait_Ref : constant Struct_Member_Index := 37;
      Member_Index_For_Fun_Param_Decl_F_Decl_Annotations : constant Struct_Member_Index := 38;
      Member_Index_For_Val_Decl_F_Expr : constant Struct_Member_Index := 39;
      Member_Index_For_Fun_Decl_F_Params : constant Struct_Member_Index := 40;
      Member_Index_For_Fun_Decl_F_Return_Type : constant Struct_Member_Index := 41;
      Member_Index_For_Fun_Decl_F_Trait_Ref : constant Struct_Member_Index := 42;
      Member_Index_For_Fun_Decl_F_Body : constant Struct_Member_Index := 43;
      Member_Index_For_Env_Spec_Decl_F_Actions : constant Struct_Member_Index := 44;
      Member_Index_For_Generic_Decl_F_Generic_Param_Decls : constant Struct_Member_Index := 45;
      Member_Index_For_Generic_Decl_F_Decl : constant Struct_Member_Index := 46;
      Member_Index_For_Grammar_Decl_F_Rules : constant Struct_Member_Index := 47;
      Member_Index_For_Langkit_Root_F_Doc : constant Struct_Member_Index := 48;
      Member_Index_For_Langkit_Root_F_Imports : constant Struct_Member_Index := 49;
      Member_Index_For_Langkit_Root_F_Decls : constant Struct_Member_Index := 50;
      Member_Index_For_Lexer_Decl_F_Rules : constant Struct_Member_Index := 51;
      Member_Index_For_Lexer_Family_Decl_F_Rules : constant Struct_Member_Index := 52;
      Member_Index_For_Type_Decl_F_Traits : constant Struct_Member_Index := 53;
      Member_Index_For_Type_Decl_F_Syn_Base_Type : constant Struct_Member_Index := 54;
      Member_Index_For_Generic_Param_Type_Decl_F_Has_Class : constant Struct_Member_Index := 55;
      Member_Index_For_Named_Type_Decl_F_Decls : constant Struct_Member_Index := 56;
      Member_Index_For_Enum_Class_Decl_F_Branches : constant Struct_Member_Index := 57;
      Member_Index_For_Enum_Type_Decl_F_Literals : constant Struct_Member_Index := 58;
      Member_Index_For_Decl_Annotation_F_Name : constant Struct_Member_Index := 59;
      Member_Index_For_Decl_Annotation_F_Args : constant Struct_Member_Index := 60;
      Member_Index_For_Decl_Annotation_Args_F_Args : constant Struct_Member_Index := 61;
      Member_Index_For_Elsif_Branch_F_Cond_Expr : constant Struct_Member_Index := 62;
      Member_Index_For_Elsif_Branch_F_Then_Expr : constant Struct_Member_Index := 63;
      Member_Index_For_Enum_Class_Case_F_Decls : constant Struct_Member_Index := 64;
      Member_Index_For_Any_Of_F_Expr : constant Struct_Member_Index := 65;
      Member_Index_For_Any_Of_F_Values : constant Struct_Member_Index := 66;
      Member_Index_For_Array_Literal_F_Exprs : constant Struct_Member_Index := 67;
      Member_Index_For_Array_Literal_F_Element_Type : constant Struct_Member_Index := 68;
      Member_Index_For_Base_Call_Expr_F_Name : constant Struct_Member_Index := 69;
      Member_Index_For_Base_Call_Expr_F_Args : constant Struct_Member_Index := 70;
      Member_Index_For_Bin_Op_F_Left : constant Struct_Member_Index := 71;
      Member_Index_For_Bin_Op_F_Op : constant Struct_Member_Index := 72;
      Member_Index_For_Bin_Op_F_Right : constant Struct_Member_Index := 73;
      Member_Index_For_Block_Expr_F_Clauses : constant Struct_Member_Index := 74;
      Member_Index_For_Cast_Expr_F_Expr : constant Struct_Member_Index := 75;
      Member_Index_For_Cast_Expr_F_Null_Cond : constant Struct_Member_Index := 76;
      Member_Index_For_Cast_Expr_F_Excludes_Null : constant Struct_Member_Index := 77;
      Member_Index_For_Cast_Expr_F_Dest_Type : constant Struct_Member_Index := 78;
      Member_Index_For_Dot_Expr_F_Prefix : constant Struct_Member_Index := 79;
      Member_Index_For_Dot_Expr_F_Null_Cond : constant Struct_Member_Index := 80;
      Member_Index_For_Dot_Expr_F_Suffix : constant Struct_Member_Index := 81;
      Member_Index_For_Error_On_Null_F_Expr : constant Struct_Member_Index := 82;
      Member_Index_For_Generic_Instantiation_F_Name : constant Struct_Member_Index := 83;
      Member_Index_For_Generic_Instantiation_F_Args : constant Struct_Member_Index := 84;
      Member_Index_For_Grammar_Discard_F_Expr : constant Struct_Member_Index := 85;
      Member_Index_For_Grammar_Dont_Skip_F_Expr : constant Struct_Member_Index := 86;
      Member_Index_For_Grammar_Dont_Skip_F_Dont_Skip : constant Struct_Member_Index := 87;
      Member_Index_For_Grammar_List_F_List_Type : constant Struct_Member_Index := 88;
      Member_Index_For_Grammar_List_F_Kind : constant Struct_Member_Index := 89;
      Member_Index_For_Grammar_List_F_Expr : constant Struct_Member_Index := 90;
      Member_Index_For_Grammar_List_F_Sep : constant Struct_Member_Index := 91;
      Member_Index_For_Grammar_Null_F_Name : constant Struct_Member_Index := 92;
      Member_Index_For_Grammar_Opt_F_Expr : constant Struct_Member_Index := 93;
      Member_Index_For_Grammar_Opt_Error_F_Expr : constant Struct_Member_Index := 94;
      Member_Index_For_Grammar_Opt_Error_Group_F_Expr : constant Struct_Member_Index := 95;
      Member_Index_For_Grammar_Opt_Group_F_Expr : constant Struct_Member_Index := 96;
      Member_Index_For_Grammar_Or_Expr_F_Sub_Exprs : constant Struct_Member_Index := 97;
      Member_Index_For_Grammar_Pick_F_Exprs : constant Struct_Member_Index := 98;
      Member_Index_For_Grammar_Predicate_F_Expr : constant Struct_Member_Index := 99;
      Member_Index_For_Grammar_Predicate_F_Prop_Ref : constant Struct_Member_Index := 100;
      Member_Index_For_Grammar_Rule_Ref_F_Node_Name : constant Struct_Member_Index := 101;
      Member_Index_For_Grammar_Skip_F_Name : constant Struct_Member_Index := 102;
      Member_Index_For_Grammar_Stop_Cut_F_Expr : constant Struct_Member_Index := 103;
      Member_Index_For_Parse_Node_Expr_F_Node_Name : constant Struct_Member_Index := 104;
      Member_Index_For_Parse_Node_Expr_F_Sub_Exprs : constant Struct_Member_Index := 105;
      Member_Index_For_Token_No_Case_Lit_F_Lit : constant Struct_Member_Index := 106;
      Member_Index_For_Token_Pattern_Concat_F_Left : constant Struct_Member_Index := 107;
      Member_Index_For_Token_Pattern_Concat_F_Right : constant Struct_Member_Index := 108;
      Member_Index_For_Token_Ref_F_Token_Name : constant Struct_Member_Index := 109;
      Member_Index_For_Token_Ref_F_Expr : constant Struct_Member_Index := 110;
      Member_Index_For_If_Expr_F_Cond_Expr : constant Struct_Member_Index := 111;
      Member_Index_For_If_Expr_F_Then_Expr : constant Struct_Member_Index := 112;
      Member_Index_For_If_Expr_F_Alternatives : constant Struct_Member_Index := 113;
      Member_Index_For_If_Expr_F_Else_Expr : constant Struct_Member_Index := 114;
      Member_Index_For_Isa_F_Expr : constant Struct_Member_Index := 115;
      Member_Index_For_Isa_F_Pattern : constant Struct_Member_Index := 116;
      Member_Index_For_Keep_Expr_F_Expr : constant Struct_Member_Index := 117;
      Member_Index_For_Keep_Expr_F_Null_Cond : constant Struct_Member_Index := 118;
      Member_Index_For_Keep_Expr_F_Keep_Type : constant Struct_Member_Index := 119;
      Member_Index_For_Lambda_Expr_F_Params : constant Struct_Member_Index := 120;
      Member_Index_For_Lambda_Expr_F_Return_Type : constant Struct_Member_Index := 121;
      Member_Index_For_Lambda_Expr_F_Body : constant Struct_Member_Index := 122;
      Member_Index_For_Null_Lit_F_Dest_Type : constant Struct_Member_Index := 123;
      Member_Index_For_Block_String_Lit_F_Lines : constant Struct_Member_Index := 124;
      Member_Index_For_Module_Doc_String_Lit_F_Lines : constant Struct_Member_Index := 125;
      Member_Index_For_Logic_Assign_F_Dest_Var : constant Struct_Member_Index := 126;
      Member_Index_For_Logic_Assign_F_Value : constant Struct_Member_Index := 127;
      Member_Index_For_Logic_Expr_F_Expr : constant Struct_Member_Index := 128;
      Member_Index_For_Logic_Propagate_F_Dest_Var : constant Struct_Member_Index := 129;
      Member_Index_For_Logic_Propagate_F_Call : constant Struct_Member_Index := 130;
      Member_Index_For_Logic_Unify_F_Lhs : constant Struct_Member_Index := 131;
      Member_Index_For_Logic_Unify_F_Rhs : constant Struct_Member_Index := 132;
      Member_Index_For_Match_Expr_F_Match_Expr : constant Struct_Member_Index := 133;
      Member_Index_For_Match_Expr_F_Branches : constant Struct_Member_Index := 134;
      Member_Index_For_Not_Expr_F_Expr : constant Struct_Member_Index := 135;
      Member_Index_For_Paren_Expr_F_Expr : constant Struct_Member_Index := 136;
      Member_Index_For_Query_F_Source : constant Struct_Member_Index := 137;
      Member_Index_For_Query_F_Pattern : constant Struct_Member_Index := 138;
      Member_Index_For_Query_F_Mapping : constant Struct_Member_Index := 139;
      Member_Index_For_Query_F_Guard : constant Struct_Member_Index := 140;
      Member_Index_For_Raise_Expr_F_Dest_Type : constant Struct_Member_Index := 141;
      Member_Index_For_Raise_Expr_F_Except_Expr : constant Struct_Member_Index := 142;
      Member_Index_For_Subscript_Expr_F_Prefix : constant Struct_Member_Index := 143;
      Member_Index_For_Subscript_Expr_F_Null_Cond : constant Struct_Member_Index := 144;
      Member_Index_For_Subscript_Expr_F_Index : constant Struct_Member_Index := 145;
      Member_Index_For_Try_Expr_F_Try_Expr : constant Struct_Member_Index := 146;
      Member_Index_For_Try_Expr_F_Or_Expr : constant Struct_Member_Index := 147;
      Member_Index_For_Un_Op_F_Op : constant Struct_Member_Index := 148;
      Member_Index_For_Un_Op_F_Expr : constant Struct_Member_Index := 149;
      Member_Index_For_Full_Decl_F_Doc : constant Struct_Member_Index := 150;
      Member_Index_For_Full_Decl_F_Decl_Annotations : constant Struct_Member_Index := 151;
      Member_Index_For_Full_Decl_F_Decl : constant Struct_Member_Index := 152;
      Member_Index_For_Grammar_List_Sep_F_Token : constant Struct_Member_Index := 153;
      Member_Index_For_Grammar_List_Sep_F_Extra : constant Struct_Member_Index := 154;
      Member_Index_For_Imported_Name_F_Original_Name : constant Struct_Member_Index := 155;
      Member_Index_For_Imported_Name_F_Renaming : constant Struct_Member_Index := 156;
      Member_Index_For_Lexer_Case_Rule_F_Expr : constant Struct_Member_Index := 157;
      Member_Index_For_Lexer_Case_Rule_F_Alts : constant Struct_Member_Index := 158;
      Member_Index_For_Lexer_Case_Rule_Send_F_Sent : constant Struct_Member_Index := 159;
      Member_Index_For_Lexer_Case_Rule_Send_F_Match_Size : constant Struct_Member_Index := 160;
      Member_Index_For_Complex_Pattern_F_Decl : constant Struct_Member_Index := 161;
      Member_Index_For_Complex_Pattern_F_Pattern : constant Struct_Member_Index := 162;
      Member_Index_For_Complex_Pattern_F_Details : constant Struct_Member_Index := 163;
      Member_Index_For_Complex_Pattern_F_Predicate : constant Struct_Member_Index := 164;
      Member_Index_For_Ellipsis_Pattern_F_Binding : constant Struct_Member_Index := 165;
      Member_Index_For_List_Pattern_F_Sub_Patterns : constant Struct_Member_Index := 166;
      Member_Index_For_Not_Pattern_F_Sub_Pattern : constant Struct_Member_Index := 167;
      Member_Index_For_Or_Pattern_F_Left_Sub_Pattern : constant Struct_Member_Index := 168;
      Member_Index_For_Or_Pattern_F_Right_Sub_Pattern : constant Struct_Member_Index := 169;
      Member_Index_For_Paren_Pattern_F_Sub_Pattern : constant Struct_Member_Index := 170;
      Member_Index_For_Type_Pattern_F_Type_Name : constant Struct_Member_Index := 171;
      Member_Index_For_Field_Pattern_Detail_F_Id : constant Struct_Member_Index := 172;
      Member_Index_For_Field_Pattern_Detail_F_Expected_Value : constant Struct_Member_Index := 173;
      Member_Index_For_Property_Pattern_Detail_F_Call : constant Struct_Member_Index := 174;
      Member_Index_For_Property_Pattern_Detail_F_Expected_Value : constant Struct_Member_Index := 175;
      Member_Index_For_Function_Type_Ref_F_Param_Types : constant Struct_Member_Index := 176;
      Member_Index_For_Function_Type_Ref_F_Return_Type : constant Struct_Member_Index := 177;
      Member_Index_For_Generic_Type_Ref_F_Type_Name : constant Struct_Member_Index := 178;
      Member_Index_For_Generic_Type_Ref_F_Args : constant Struct_Member_Index := 179;
      Member_Index_For_Simple_Type_Ref_F_Type_Name : constant Struct_Member_Index := 180;
      Member_Index_For_Var_Bind_F_Name : constant Struct_Member_Index := 181;
      Member_Index_For_Var_Bind_F_Expr : constant Struct_Member_Index := 182;
      Member_Index_For_Parent : constant Struct_Member_Index := 183;
      Member_Index_For_Parents : constant Struct_Member_Index := 184;
      Member_Index_For_Children : constant Struct_Member_Index := 185;
      Member_Index_For_Token_Start : constant Struct_Member_Index := 186;
      Member_Index_For_Token_End : constant Struct_Member_Index := 187;
      Member_Index_For_Child_Index : constant Struct_Member_Index := 188;
      Member_Index_For_Previous_Sibling : constant Struct_Member_Index := 189;
      Member_Index_For_Next_Sibling : constant Struct_Member_Index := 190;
      Member_Index_For_Unit : constant Struct_Member_Index := 191;
      Member_Index_For_Is_Ghost : constant Struct_Member_Index := 192;
      Member_Index_For_Full_Sloc_Image : constant Struct_Member_Index := 193;
      Member_Index_For_Completion_Item_Kind_To_Int : constant Struct_Member_Index := 194;
      Member_Index_For_Lkt_Node_P_Set_Solver_Debug_Mode : constant Struct_Member_Index := 195;
      Member_Index_For_Lkt_Node_P_Prelude_Unit : constant Struct_Member_Index := 196;
      Member_Index_For_Lkt_Node_P_Basic_Trait_Gen : constant Struct_Member_Index := 197;
      Member_Index_For_Lkt_Node_P_Basic_Trait : constant Struct_Member_Index := 198;
      Member_Index_For_Lkt_Node_P_Node_Gen_Trait : constant Struct_Member_Index := 199;
      Member_Index_For_Lkt_Node_P_Node_Trait : constant Struct_Member_Index := 200;
      Member_Index_For_Lkt_Node_P_Indexable_Gen_Trait : constant Struct_Member_Index := 201;
      Member_Index_For_Lkt_Node_P_Indexable_Trait : constant Struct_Member_Index := 202;
      Member_Index_For_Lkt_Node_P_Token_Node_Trait : constant Struct_Member_Index := 203;
      Member_Index_For_Lkt_Node_P_Error_Node_Trait : constant Struct_Member_Index := 204;
      Member_Index_For_Lkt_Node_P_Char_Type : constant Struct_Member_Index := 205;
      Member_Index_For_Lkt_Node_P_Int_Type : constant Struct_Member_Index := 206;
      Member_Index_For_Lkt_Node_P_Bool_Type : constant Struct_Member_Index := 207;
      Member_Index_For_Lkt_Node_P_Bigint_Type : constant Struct_Member_Index := 208;
      Member_Index_For_Lkt_Node_P_String_Type : constant Struct_Member_Index := 209;
      Member_Index_For_Lkt_Node_P_Symbol_Type : constant Struct_Member_Index := 210;
      Member_Index_For_Lkt_Node_P_Property_Error_Type : constant Struct_Member_Index := 211;
      Member_Index_For_Lkt_Node_P_Regexp_Type : constant Struct_Member_Index := 212;
      Member_Index_For_Lkt_Node_P_Entity_Gen_Type : constant Struct_Member_Index := 213;
      Member_Index_For_Lkt_Node_P_Entity_Type : constant Struct_Member_Index := 214;
      Member_Index_For_Lkt_Node_P_Logicvar_Type : constant Struct_Member_Index := 215;
      Member_Index_For_Lkt_Node_P_Equation_Type : constant Struct_Member_Index := 216;
      Member_Index_For_Lkt_Node_P_Array_Gen_Type : constant Struct_Member_Index := 217;
      Member_Index_For_Lkt_Node_P_Array_Type : constant Struct_Member_Index := 218;
      Member_Index_For_Lkt_Node_P_Astlist_Gen_Type : constant Struct_Member_Index := 219;
      Member_Index_For_Lkt_Node_P_Astlist_Type : constant Struct_Member_Index := 220;
      Member_Index_For_Lkt_Node_P_Node_Builder_Gen_Type : constant Struct_Member_Index := 221;
      Member_Index_For_Lkt_Node_P_Node_Builder_Type : constant Struct_Member_Index := 222;
      Member_Index_For_Lkt_Node_P_Iterator_Gen_Trait : constant Struct_Member_Index := 223;
      Member_Index_For_Lkt_Node_P_Iterator_Trait : constant Struct_Member_Index := 224;
      Member_Index_For_Lkt_Node_P_Analysis_Unit_Gen_Trait : constant Struct_Member_Index := 225;
      Member_Index_For_Lkt_Node_P_Analysis_Unit_Trait : constant Struct_Member_Index := 226;
      Member_Index_For_Lkt_Node_P_Topmost_Invalid_Decl : constant Struct_Member_Index := 227;
      Member_Index_For_Lkt_Node_P_Nameres_Diagnostics : constant Struct_Member_Index := 228;
      Member_Index_For_Lkt_Node_P_Solve_Enclosing_Context : constant Struct_Member_Index := 229;
      Member_Index_For_Lkt_Node_P_Xref_Entry_Point : constant Struct_Member_Index := 230;
      Member_Index_For_Lkt_Node_P_Complete : constant Struct_Member_Index := 231;
      Member_Index_For_Base_Import_P_Referenced_Units : constant Struct_Member_Index := 232;
      Member_Index_For_Base_Match_Branch_P_Match_Part : constant Struct_Member_Index := 233;
      Member_Index_For_Class_Qualifier_P_As_Bool : constant Struct_Member_Index := 234;
      Member_Index_For_Decl_P_Custom_Image : constant Struct_Member_Index := 235;
      Member_Index_For_Decl_P_Decl_Type_Name : constant Struct_Member_Index := 236;
      Member_Index_For_Decl_P_Def_Ids : constant Struct_Member_Index := 237;
      Member_Index_For_Decl_P_As_Bare_Decl : constant Struct_Member_Index := 238;
      Member_Index_For_Decl_P_Get_Type : constant Struct_Member_Index := 239;
      Member_Index_For_Decl_P_Get_Cast_Type : constant Struct_Member_Index := 240;
      Member_Index_For_Decl_P_Get_Keep_Type : constant Struct_Member_Index := 241;
      Member_Index_For_Decl_P_Get_Suffix_Type : constant Struct_Member_Index := 242;
      Member_Index_For_Decl_P_Is_Generic : constant Struct_Member_Index := 243;
      Member_Index_For_Decl_P_Return_Type_Is_Instantiated : constant Struct_Member_Index := 244;
      Member_Index_For_Decl_P_Is_Instantiated : constant Struct_Member_Index := 245;
      Member_Index_For_Decl_P_Name : constant Struct_Member_Index := 246;
      Member_Index_For_Decl_P_Full_Name : constant Struct_Member_Index := 247;
      Member_Index_For_Fun_Decl_P_Is_Dynamic_Combiner : constant Struct_Member_Index := 248;
      Member_Index_For_Fun_Decl_P_Find_All_Overrides : constant Struct_Member_Index := 249;
      Member_Index_For_Type_Decl_P_Def_Id : constant Struct_Member_Index := 250;
      Member_Index_For_Type_Decl_P_Base_Type : constant Struct_Member_Index := 251;
      Member_Index_For_Type_Decl_P_Base_Type_If_Entity : constant Struct_Member_Index := 252;
      Member_Index_For_Excludes_Null_P_As_Bool : constant Struct_Member_Index := 253;
      Member_Index_For_Expr_P_Get_Type : constant Struct_Member_Index := 254;
      Member_Index_For_Expr_P_Get_Generic_Type : constant Struct_Member_Index := 255;
      Member_Index_For_Expr_P_Get_Expected_Type : constant Struct_Member_Index := 256;
      Member_Index_For_Expr_P_Referenced_Decl : constant Struct_Member_Index := 257;
      Member_Index_For_Token_Lit_P_Denoted_Value : constant Struct_Member_Index := 258;
      Member_Index_For_Token_Pattern_Lit_P_Denoted_Value : constant Struct_Member_Index := 259;
      Member_Index_For_Id_P_Custom_Image : constant Struct_Member_Index := 260;
      Member_Index_For_Def_Id_P_Name : constant Struct_Member_Index := 261;
      Member_Index_For_Def_Id_P_Get_Implementatinons : constant Struct_Member_Index := 262;
      Member_Index_For_Def_Id_P_Decl_Detail : constant Struct_Member_Index := 263;
      Member_Index_For_Def_Id_P_Completion_Item_Kind : constant Struct_Member_Index := 264;
      Member_Index_For_Def_Id_P_Doc : constant Struct_Member_Index := 265;
      Member_Index_For_Def_Id_P_Find_All_References : constant Struct_Member_Index := 266;
      Member_Index_For_Ref_Id_P_Referenced_Defining_Name : constant Struct_Member_Index := 267;
      Member_Index_For_Char_Lit_P_Denoted_Value : constant Struct_Member_Index := 268;
      Member_Index_For_String_Lit_P_Denoted_Value : constant Struct_Member_Index := 269;
      Member_Index_For_String_Lit_P_Is_Prefixed_String : constant Struct_Member_Index := 270;
      Member_Index_For_String_Lit_P_Prefix : constant Struct_Member_Index := 271;
      Member_Index_For_String_Lit_P_Is_Regexp_Literal : constant Struct_Member_Index := 272;
      Member_Index_For_Full_Decl_P_Has_Annotation : constant Struct_Member_Index := 273;
      Member_Index_For_Null_Cond_Qualifier_P_As_Bool : constant Struct_Member_Index := 274;
      Member_Index_For_Type_Ref_P_Referenced_Decl : constant Struct_Member_Index := 275;

   --------------------------------
   -- Token kind index constants --
   --------------------------------

      Token_Index_For_Lkt_Amp : constant Token_Kind_Index := 1;
      Token_Index_For_Lkt_And_Kw : constant Token_Kind_Index := 2;
      Token_Index_For_Lkt_At : constant Token_Kind_Index := 3;
      Token_Index_For_Lkt_Big_Number : constant Token_Kind_Index := 4;
      Token_Index_For_Lkt_Bind_Kw : constant Token_Kind_Index := 5;
      Token_Index_For_Lkt_Block_String_Line : constant Token_Kind_Index := 6;
      Token_Index_For_Lkt_Case_Kw : constant Token_Kind_Index := 7;
      Token_Index_For_Lkt_Char : constant Token_Kind_Index := 8;
      Token_Index_For_Lkt_Class_Kw : constant Token_Kind_Index := 9;
      Token_Index_For_Lkt_Colon : constant Token_Kind_Index := 10;
      Token_Index_For_Lkt_Comb : constant Token_Kind_Index := 11;
      Token_Index_For_Lkt_Comma : constant Token_Kind_Index := 12;
      Token_Index_For_Lkt_Comment : constant Token_Kind_Index := 13;
      Token_Index_For_Lkt_Discard_Kw : constant Token_Kind_Index := 14;
      Token_Index_For_Lkt_Div : constant Token_Kind_Index := 15;
      Token_Index_For_Lkt_Dot : constant Token_Kind_Index := 16;
      Token_Index_For_Lkt_Double_Colon : constant Token_Kind_Index := 17;
      Token_Index_For_Lkt_Dyn_Var_Kw : constant Token_Kind_Index := 18;
      Token_Index_For_Lkt_E_Q : constant Token_Kind_Index := 19;
      Token_Index_For_Lkt_Elif_Kw : constant Token_Kind_Index := 20;
      Token_Index_For_Lkt_Ellipsis : constant Token_Kind_Index := 21;
      Token_Index_For_Lkt_Else_Kw : constant Token_Kind_Index := 22;
      Token_Index_For_Lkt_Enum_Kw : constant Token_Kind_Index := 23;
      Token_Index_For_Lkt_Equal : constant Token_Kind_Index := 24;
      Token_Index_For_Lkt_Excl_Mark : constant Token_Kind_Index := 25;
      Token_Index_For_Lkt_Fat_Right_Arrow : constant Token_Kind_Index := 26;
      Token_Index_For_Lkt_From_Kw : constant Token_Kind_Index := 27;
      Token_Index_For_Lkt_Fun_Kw : constant Token_Kind_Index := 28;
      Token_Index_For_Lkt_G_T : constant Token_Kind_Index := 29;
      Token_Index_For_Lkt_G_T_E : constant Token_Kind_Index := 30;
      Token_Index_For_Lkt_Generic_Kw : constant Token_Kind_Index := 31;
      Token_Index_For_Lkt_Grammar_Kw : constant Token_Kind_Index := 32;
      Token_Index_For_Lkt_Identifier : constant Token_Kind_Index := 33;
      Token_Index_For_Lkt_If_Kw : constant Token_Kind_Index := 34;
      Token_Index_For_Lkt_Implements_Kw : constant Token_Kind_Index := 35;
      Token_Index_For_Lkt_Import_Kw : constant Token_Kind_Index := 36;
      Token_Index_For_Lkt_In_Kw : constant Token_Kind_Index := 37;
      Token_Index_For_Lkt_Int_Mark : constant Token_Kind_Index := 38;
      Token_Index_For_Lkt_Is_Kw : constant Token_Kind_Index := 39;
      Token_Index_For_Lkt_L_Brace : constant Token_Kind_Index := 40;
      Token_Index_For_Lkt_L_Brack : constant Token_Kind_Index := 41;
      Token_Index_For_Lkt_L_Par : constant Token_Kind_Index := 42;
      Token_Index_For_Lkt_L_T : constant Token_Kind_Index := 43;
      Token_Index_For_Lkt_L_T_E : constant Token_Kind_Index := 44;
      Token_Index_For_Lkt_Left_Arrow : constant Token_Kind_Index := 45;
      Token_Index_For_Lkt_Lexer_Kw : constant Token_Kind_Index := 46;
      Token_Index_For_Lkt_Lexing_Failure : constant Token_Kind_Index := 47;
      Token_Index_For_Lkt_Match_Kw : constant Token_Kind_Index := 48;
      Token_Index_For_Lkt_Minus : constant Token_Kind_Index := 49;
      Token_Index_For_Lkt_Module_Doc_String_Line : constant Token_Kind_Index := 50;
      Token_Index_For_Lkt_N_E : constant Token_Kind_Index := 51;
      Token_Index_For_Lkt_Not_Kw : constant Token_Kind_Index := 52;
      Token_Index_For_Lkt_Null_Kw : constant Token_Kind_Index := 53;
      Token_Index_For_Lkt_Number : constant Token_Kind_Index := 54;
      Token_Index_For_Lkt_Or_Kw : constant Token_Kind_Index := 55;
      Token_Index_For_Lkt_P_String : constant Token_Kind_Index := 56;
      Token_Index_For_Lkt_Percent : constant Token_Kind_Index := 57;
      Token_Index_For_Lkt_Pipe : constant Token_Kind_Index := 58;
      Token_Index_For_Lkt_Plus : constant Token_Kind_Index := 59;
      Token_Index_For_Lkt_Private_Kw : constant Token_Kind_Index := 60;
      Token_Index_For_Lkt_Public_Kw : constant Token_Kind_Index := 61;
      Token_Index_For_Lkt_R_Brace : constant Token_Kind_Index := 62;
      Token_Index_For_Lkt_R_Brack : constant Token_Kind_Index := 63;
      Token_Index_For_Lkt_R_Par : constant Token_Kind_Index := 64;
      Token_Index_For_Lkt_Raise_Kw : constant Token_Kind_Index := 65;
      Token_Index_For_Lkt_Right_Arrow : constant Token_Kind_Index := 66;
      Token_Index_For_Lkt_Select_Kw : constant Token_Kind_Index := 67;
      Token_Index_For_Lkt_Semicolon : constant Token_Kind_Index := 68;
      Token_Index_For_Lkt_String : constant Token_Kind_Index := 69;
      Token_Index_For_Lkt_Struct_Kw : constant Token_Kind_Index := 70;
      Token_Index_For_Lkt_Termination : constant Token_Kind_Index := 71;
      Token_Index_For_Lkt_Then_Kw : constant Token_Kind_Index := 72;
      Token_Index_For_Lkt_Times : constant Token_Kind_Index := 73;
      Token_Index_For_Lkt_Trait_Kw : constant Token_Kind_Index := 74;
      Token_Index_For_Lkt_Triple_Colon : constant Token_Kind_Index := 75;
      Token_Index_For_Lkt_Try_Kw : constant Token_Kind_Index := 76;
      Token_Index_For_Lkt_Two_Sided_Arrow : constant Token_Kind_Index := 77;
      Token_Index_For_Lkt_Val_Kw : constant Token_Kind_Index := 78;
      Token_Index_For_Lkt_When_Kw : constant Token_Kind_Index := 79;
      Token_Index_For_Lkt_Whitespace : constant Token_Kind_Index := 80;

   ----------------------------------
   -- Token family index constants --
   ----------------------------------

      Token_Index_For_Alphanumericals : constant Token_Family_Index := 1;
      Token_Index_For_Default_Family : constant Token_Family_Index := 2;

   ------------------------------
   -- Grammar rule descriptors --
   ------------------------------

   
      
      Rule_Name_1 : aliased constant Text_Type :=
        "Main_Rule";
      Rule_Doc_1 : aliased constant Text_Type :=
        "";
      Rule_Desc_1 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_1'Access,
         Is_Public   => True,
         Doc         => Rule_Doc_1'Access,
         Return_Type => Type_Index_For_Langkit_Root);
      
      Rule_Name_2 : aliased constant Text_Type :=
        "Id";
      Rule_Doc_2 : aliased constant Text_Type :=
        "";
      Rule_Desc_2 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_2'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_2'Access,
         Return_Type => Type_Index_For_Id);
      
      Rule_Name_3 : aliased constant Text_Type :=
        "Ref_Id";
      Rule_Doc_3 : aliased constant Text_Type :=
        "";
      Rule_Desc_3 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_3'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_3'Access,
         Return_Type => Type_Index_For_Ref_Id);
      
      Rule_Name_4 : aliased constant Text_Type :=
        "Type_Ref_Id";
      Rule_Doc_4 : aliased constant Text_Type :=
        "";
      Rule_Desc_4 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_4'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_4'Access,
         Return_Type => Type_Index_For_Ref_Id);
      
      Rule_Name_5 : aliased constant Text_Type :=
        "Module_Id";
      Rule_Doc_5 : aliased constant Text_Type :=
        "";
      Rule_Desc_5 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_5'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_5'Access,
         Return_Type => Type_Index_For_Module_Id);
      
      Rule_Name_6 : aliased constant Text_Type :=
        "Def_Id";
      Rule_Doc_6 : aliased constant Text_Type :=
        "";
      Rule_Desc_6 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_6'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_6'Access,
         Return_Type => Type_Index_For_Def_Id);
      
      Rule_Name_7 : aliased constant Text_Type :=
        "Doc";
      Rule_Doc_7 : aliased constant Text_Type :=
        "";
      Rule_Desc_7 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_7'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_7'Access,
         Return_Type => Type_Index_For_String_Lit);
      
      Rule_Name_8 : aliased constant Text_Type :=
        "Module_Doc";
      Rule_Doc_8 : aliased constant Text_Type :=
        "";
      Rule_Desc_8 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_8'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_8'Access,
         Return_Type => Type_Index_For_Module_Doc_String_Lit);
      
      Rule_Name_9 : aliased constant Text_Type :=
        "Imported_Names";
      Rule_Doc_9 : aliased constant Text_Type :=
        "";
      Rule_Desc_9 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_9'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_9'Access,
         Return_Type => Type_Index_For_Imported_Name_List);
      
      Rule_Name_10 : aliased constant Text_Type :=
        "Import_Clause";
      Rule_Doc_10 : aliased constant Text_Type :=
        "";
      Rule_Desc_10 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_10'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_10'Access,
         Return_Type => Type_Index_For_Base_Import);
      
      Rule_Name_11 : aliased constant Text_Type :=
        "Imports";
      Rule_Doc_11 : aliased constant Text_Type :=
        "";
      Rule_Desc_11 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_11'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_11'Access,
         Return_Type => Type_Index_For_Base_Import_List);
      
      Rule_Name_12 : aliased constant Text_Type :=
        "Lexer_Decl";
      Rule_Doc_12 : aliased constant Text_Type :=
        "";
      Rule_Desc_12 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_12'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_12'Access,
         Return_Type => Type_Index_For_Lexer_Decl);
      
      Rule_Name_13 : aliased constant Text_Type :=
        "Grammar_Decl";
      Rule_Doc_13 : aliased constant Text_Type :=
        "";
      Rule_Desc_13 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_13'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_13'Access,
         Return_Type => Type_Index_For_Grammar_Decl);
      
      Rule_Name_14 : aliased constant Text_Type :=
        "Grammar_Rule";
      Rule_Doc_14 : aliased constant Text_Type :=
        "";
      Rule_Desc_14 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_14'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_14'Access,
         Return_Type => Type_Index_For_Grammar_Rule_Decl);
      
      Rule_Name_15 : aliased constant Text_Type :=
        "Lexer_Case_Rule";
      Rule_Doc_15 : aliased constant Text_Type :=
        "";
      Rule_Desc_15 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_15'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_15'Access,
         Return_Type => Type_Index_For_Lexer_Case_Rule);
      
      Rule_Name_16 : aliased constant Text_Type :=
        "Lexer_Case_Alt";
      Rule_Doc_16 : aliased constant Text_Type :=
        "";
      Rule_Desc_16 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_16'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_16'Access,
         Return_Type => Type_Index_For_Base_Lexer_Case_Rule_Alt);
      
      Rule_Name_17 : aliased constant Text_Type :=
        "Lexer_Case_Send";
      Rule_Doc_17 : aliased constant Text_Type :=
        "";
      Rule_Desc_17 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_17'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_17'Access,
         Return_Type => Type_Index_For_Lexer_Case_Rule_Send);
      
      Rule_Name_18 : aliased constant Text_Type :=
        "Grammar_Primary";
      Rule_Doc_18 : aliased constant Text_Type :=
        "";
      Rule_Desc_18 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_18'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_18'Access,
         Return_Type => Type_Index_For_Grammar_Expr);
      
      Rule_Name_19 : aliased constant Text_Type :=
        "Grammar_Expr";
      Rule_Doc_19 : aliased constant Text_Type :=
        "";
      Rule_Desc_19 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_19'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_19'Access,
         Return_Type => Type_Index_For_Grammar_Expr);
      
      Rule_Name_20 : aliased constant Text_Type :=
        "Grammar_Pick";
      Rule_Doc_20 : aliased constant Text_Type :=
        "";
      Rule_Desc_20 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_20'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_20'Access,
         Return_Type => Type_Index_For_Grammar_Pick);
      
      Rule_Name_21 : aliased constant Text_Type :=
        "Grammar_Implicit_Pick";
      Rule_Doc_21 : aliased constant Text_Type :=
        "";
      Rule_Desc_21 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_21'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_21'Access,
         Return_Type => Type_Index_For_Grammar_Implicit_Pick);
      
      Rule_Name_22 : aliased constant Text_Type :=
        "Grammar_Opt";
      Rule_Doc_22 : aliased constant Text_Type :=
        "";
      Rule_Desc_22 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_22'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_22'Access,
         Return_Type => Type_Index_For_Grammar_Expr);
      
      Rule_Name_23 : aliased constant Text_Type :=
        "Grammar_Opt_Error";
      Rule_Doc_23 : aliased constant Text_Type :=
        "";
      Rule_Desc_23 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_23'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_23'Access,
         Return_Type => Type_Index_For_Grammar_Expr);
      
      Rule_Name_24 : aliased constant Text_Type :=
        "Grammar_Cut";
      Rule_Doc_24 : aliased constant Text_Type :=
        "";
      Rule_Desc_24 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_24'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_24'Access,
         Return_Type => Type_Index_For_Grammar_Cut);
      
      Rule_Name_25 : aliased constant Text_Type :=
        "Grammar_Stopcut";
      Rule_Doc_25 : aliased constant Text_Type :=
        "";
      Rule_Desc_25 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_25'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_25'Access,
         Return_Type => Type_Index_For_Grammar_Stop_Cut);
      
      Rule_Name_26 : aliased constant Text_Type :=
        "Grammar_Or_Expr";
      Rule_Doc_26 : aliased constant Text_Type :=
        "";
      Rule_Desc_26 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_26'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_26'Access,
         Return_Type => Type_Index_For_Grammar_Or_Expr);
      
      Rule_Name_27 : aliased constant Text_Type :=
        "Grammar_Discard_Expr";
      Rule_Doc_27 : aliased constant Text_Type :=
        "";
      Rule_Desc_27 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_27'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_27'Access,
         Return_Type => Type_Index_For_Grammar_Discard);
      
      Rule_Name_28 : aliased constant Text_Type :=
        "Token_Literal";
      Rule_Doc_28 : aliased constant Text_Type :=
        "";
      Rule_Desc_28 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_28'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_28'Access,
         Return_Type => Type_Index_For_Token_Lit);
      
      Rule_Name_29 : aliased constant Text_Type :=
        "Token_No_Case_Literal";
      Rule_Doc_29 : aliased constant Text_Type :=
        "";
      Rule_Desc_29 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_29'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_29'Access,
         Return_Type => Type_Index_For_Token_No_Case_Lit);
      
      Rule_Name_30 : aliased constant Text_Type :=
        "Token_Pattern";
      Rule_Doc_30 : aliased constant Text_Type :=
        "";
      Rule_Desc_30 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_30'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_30'Access,
         Return_Type => Type_Index_For_Grammar_Expr);
      
      Rule_Name_31 : aliased constant Text_Type :=
        "Token_Pattern_Literal";
      Rule_Doc_31 : aliased constant Text_Type :=
        "";
      Rule_Desc_31 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_31'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_31'Access,
         Return_Type => Type_Index_For_Token_Pattern_Lit);
      
      Rule_Name_32 : aliased constant Text_Type :=
        "Parse_Node_Expr";
      Rule_Doc_32 : aliased constant Text_Type :=
        "";
      Rule_Desc_32 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_32'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_32'Access,
         Return_Type => Type_Index_For_Parse_Node_Expr);
      
      Rule_Name_33 : aliased constant Text_Type :=
        "Grammar_Rule_Ref";
      Rule_Doc_33 : aliased constant Text_Type :=
        "";
      Rule_Desc_33 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_33'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_33'Access,
         Return_Type => Type_Index_For_Grammar_Rule_Ref);
      
      Rule_Name_34 : aliased constant Text_Type :=
        "Grammar_List_Expr";
      Rule_Doc_34 : aliased constant Text_Type :=
        "";
      Rule_Desc_34 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_34'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_34'Access,
         Return_Type => Type_Index_For_Grammar_List);
      
      Rule_Name_35 : aliased constant Text_Type :=
        "Grammar_List_Sep";
      Rule_Doc_35 : aliased constant Text_Type :=
        "";
      Rule_Desc_35 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_35'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_35'Access,
         Return_Type => Type_Index_For_Grammar_List_Sep);
      
      Rule_Name_36 : aliased constant Text_Type :=
        "Grammar_Skip";
      Rule_Doc_36 : aliased constant Text_Type :=
        "";
      Rule_Desc_36 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_36'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_36'Access,
         Return_Type => Type_Index_For_Grammar_Skip);
      
      Rule_Name_37 : aliased constant Text_Type :=
        "Grammar_Null";
      Rule_Doc_37 : aliased constant Text_Type :=
        "";
      Rule_Desc_37 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_37'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_37'Access,
         Return_Type => Type_Index_For_Grammar_Null);
      
      Rule_Name_38 : aliased constant Text_Type :=
        "Grammar_Token";
      Rule_Doc_38 : aliased constant Text_Type :=
        "";
      Rule_Desc_38 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_38'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_38'Access,
         Return_Type => Type_Index_For_Token_Ref);
      
      Rule_Name_39 : aliased constant Text_Type :=
        "Type_Decl";
      Rule_Doc_39 : aliased constant Text_Type :=
        "";
      Rule_Desc_39 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_39'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_39'Access,
         Return_Type => Type_Index_For_Named_Type_Decl);
      
      Rule_Name_40 : aliased constant Text_Type :=
        "Generic_Decl";
      Rule_Doc_40 : aliased constant Text_Type :=
        "";
      Rule_Desc_40 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_40'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_40'Access,
         Return_Type => Type_Index_For_Generic_Decl);
      
      Rule_Name_41 : aliased constant Text_Type :=
        "Generic_Param_Type";
      Rule_Doc_41 : aliased constant Text_Type :=
        "";
      Rule_Desc_41 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_41'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_41'Access,
         Return_Type => Type_Index_For_Full_Decl);
      
      Rule_Name_42 : aliased constant Text_Type :=
        "Enum_Lit_Decl";
      Rule_Doc_42 : aliased constant Text_Type :=
        "";
      Rule_Desc_42 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_42'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_42'Access,
         Return_Type => Type_Index_For_Enum_Lit_Decl);
      
      Rule_Name_43 : aliased constant Text_Type :=
        "Fun_Decl";
      Rule_Doc_43 : aliased constant Text_Type :=
        "";
      Rule_Desc_43 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_43'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_43'Access,
         Return_Type => Type_Index_For_Fun_Decl);
      
      Rule_Name_44 : aliased constant Text_Type :=
        "Lambda_Param_Decl";
      Rule_Doc_44 : aliased constant Text_Type :=
        "";
      Rule_Desc_44 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_44'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_44'Access,
         Return_Type => Type_Index_For_Lambda_Param_Decl);
      
      Rule_Name_45 : aliased constant Text_Type :=
        "Fun_Param_Decl";
      Rule_Doc_45 : aliased constant Text_Type :=
        "";
      Rule_Desc_45 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_45'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_45'Access,
         Return_Type => Type_Index_For_Fun_Param_Decl);
      
      Rule_Name_46 : aliased constant Text_Type :=
        "Fun_Param_List";
      Rule_Doc_46 : aliased constant Text_Type :=
        "";
      Rule_Desc_46 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_46'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_46'Access,
         Return_Type => Type_Index_For_Fun_Param_Decl_List);
      
      Rule_Name_47 : aliased constant Text_Type :=
        "Lambda_Param_List";
      Rule_Doc_47 : aliased constant Text_Type :=
        "";
      Rule_Desc_47 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_47'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_47'Access,
         Return_Type => Type_Index_For_Lambda_Param_Decl_List);
      
      Rule_Name_48 : aliased constant Text_Type :=
        "Field_Decl";
      Rule_Doc_48 : aliased constant Text_Type :=
        "";
      Rule_Desc_48 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_48'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_48'Access,
         Return_Type => Type_Index_For_Field_Decl);
      
      Rule_Name_49 : aliased constant Text_Type :=
        "Lexer_Family_Decl";
      Rule_Doc_49 : aliased constant Text_Type :=
        "";
      Rule_Desc_49 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_49'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_49'Access,
         Return_Type => Type_Index_For_Lexer_Family_Decl);
      
      Rule_Name_50 : aliased constant Text_Type :=
        "Bare_Decl";
      Rule_Doc_50 : aliased constant Text_Type :=
        "";
      Rule_Desc_50 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_50'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_50'Access,
         Return_Type => Type_Index_For_Decl);
      
      Rule_Name_51 : aliased constant Text_Type :=
        "Decl";
      Rule_Doc_51 : aliased constant Text_Type :=
        "";
      Rule_Desc_51 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_51'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_51'Access,
         Return_Type => Type_Index_For_Full_Decl);
      
      Rule_Name_52 : aliased constant Text_Type :=
        "Type_Member_Ref";
      Rule_Doc_52 : aliased constant Text_Type :=
        "";
      Rule_Desc_52 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_52'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_52'Access,
         Return_Type => Type_Index_For_Dot_Expr);
      
      Rule_Name_53 : aliased constant Text_Type :=
        "Type_Expr";
      Rule_Doc_53 : aliased constant Text_Type :=
        "";
      Rule_Desc_53 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_53'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_53'Access,
         Return_Type => Type_Index_For_Expr);
      
      Rule_Name_54 : aliased constant Text_Type :=
        "Type_Ref";
      Rule_Doc_54 : aliased constant Text_Type :=
        "";
      Rule_Desc_54 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_54'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_54'Access,
         Return_Type => Type_Index_For_Type_Ref);
      
      Rule_Name_55 : aliased constant Text_Type :=
        "Type_List";
      Rule_Doc_55 : aliased constant Text_Type :=
        "";
      Rule_Desc_55 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_55'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_55'Access,
         Return_Type => Type_Index_For_Type_Ref_List);
      
      Rule_Name_56 : aliased constant Text_Type :=
        "Decls";
      Rule_Doc_56 : aliased constant Text_Type :=
        "";
      Rule_Desc_56 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_56'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_56'Access,
         Return_Type => Type_Index_For_Full_Decl_List);
      
      Rule_Name_57 : aliased constant Text_Type :=
        "Decl_Block";
      Rule_Doc_57 : aliased constant Text_Type :=
        "";
      Rule_Desc_57 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_57'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_57'Access,
         Return_Type => Type_Index_For_Decl_Block);
      
      Rule_Name_58 : aliased constant Text_Type :=
        "Val_Decl";
      Rule_Doc_58 : aliased constant Text_Type :=
        "";
      Rule_Desc_58 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_58'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_58'Access,
         Return_Type => Type_Index_For_Val_Decl);
      
      Rule_Name_59 : aliased constant Text_Type :=
        "Dynvar_Decl";
      Rule_Doc_59 : aliased constant Text_Type :=
        "";
      Rule_Desc_59 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_59'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_59'Access,
         Return_Type => Type_Index_For_Dyn_Var_Decl);
      
      Rule_Name_60 : aliased constant Text_Type :=
        "Var_Bind";
      Rule_Doc_60 : aliased constant Text_Type :=
        "";
      Rule_Desc_60 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_60'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_60'Access,
         Return_Type => Type_Index_For_Var_Bind);
      
      Rule_Name_61 : aliased constant Text_Type :=
        "Env_Spec_Action";
      Rule_Doc_61 : aliased constant Text_Type :=
        "";
      Rule_Desc_61 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_61'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_61'Access,
         Return_Type => Type_Index_For_Call_Expr);
      
      Rule_Name_62 : aliased constant Text_Type :=
        "Env_Spec_Decl";
      Rule_Doc_62 : aliased constant Text_Type :=
        "";
      Rule_Desc_62 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_62'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_62'Access,
         Return_Type => Type_Index_For_Env_Spec_Decl);
      
      Rule_Name_63 : aliased constant Text_Type :=
        "Block";
      Rule_Doc_63 : aliased constant Text_Type :=
        "";
      Rule_Desc_63 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_63'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_63'Access,
         Return_Type => Type_Index_For_Block_Expr);
      
      Rule_Name_64 : aliased constant Text_Type :=
        "Pattern";
      Rule_Doc_64 : aliased constant Text_Type :=
        "";
      Rule_Desc_64 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_64'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_64'Access,
         Return_Type => Type_Index_For_Pattern);
      
      Rule_Name_65 : aliased constant Text_Type :=
        "Neg_Pattern";
      Rule_Doc_65 : aliased constant Text_Type :=
        "";
      Rule_Desc_65 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_65'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_65'Access,
         Return_Type => Type_Index_For_Pattern);
      
      Rule_Name_66 : aliased constant Text_Type :=
        "Pattern_Binding";
      Rule_Doc_66 : aliased constant Text_Type :=
        "";
      Rule_Desc_66 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_66'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_66'Access,
         Return_Type => Type_Index_For_Binding_Val_Decl);
      
      Rule_Name_67 : aliased constant Text_Type :=
        "Complex_Pattern";
      Rule_Doc_67 : aliased constant Text_Type :=
        "";
      Rule_Desc_67 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_67'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_67'Access,
         Return_Type => Type_Index_For_Complex_Pattern);
      
      Rule_Name_68 : aliased constant Text_Type :=
        "Value_Pattern";
      Rule_Doc_68 : aliased constant Text_Type :=
        "";
      Rule_Desc_68 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_68'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_68'Access,
         Return_Type => Type_Index_For_Pattern);
      
      Rule_Name_69 : aliased constant Text_Type :=
        "Regex_Pattern";
      Rule_Doc_69 : aliased constant Text_Type :=
        "";
      Rule_Desc_69 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_69'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_69'Access,
         Return_Type => Type_Index_For_Regex_Pattern);
      
      Rule_Name_70 : aliased constant Text_Type :=
        "Bool_Pattern";
      Rule_Doc_70 : aliased constant Text_Type :=
        "";
      Rule_Desc_70 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_70'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_70'Access,
         Return_Type => Type_Index_For_Bool_Pattern);
      
      Rule_Name_71 : aliased constant Text_Type :=
        "Ellipsis_Pattern";
      Rule_Doc_71 : aliased constant Text_Type :=
        "";
      Rule_Desc_71 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_71'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_71'Access,
         Return_Type => Type_Index_For_Ellipsis_Pattern);
      
      Rule_Name_72 : aliased constant Text_Type :=
        "Integer_Pattern";
      Rule_Doc_72 : aliased constant Text_Type :=
        "";
      Rule_Desc_72 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_72'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_72'Access,
         Return_Type => Type_Index_For_Integer_Pattern);
      
      Rule_Name_73 : aliased constant Text_Type :=
        "List_Pattern";
      Rule_Doc_73 : aliased constant Text_Type :=
        "";
      Rule_Desc_73 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_73'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_73'Access,
         Return_Type => Type_Index_For_List_Pattern);
      
      Rule_Name_74 : aliased constant Text_Type :=
        "Pattern_Arg";
      Rule_Doc_74 : aliased constant Text_Type :=
        "";
      Rule_Desc_74 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_74'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_74'Access,
         Return_Type => Type_Index_For_Pattern_Detail);
      
      Rule_Name_75 : aliased constant Text_Type :=
        "Expr";
      Rule_Doc_75 : aliased constant Text_Type :=
        "";
      Rule_Desc_75 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_75'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_75'Access,
         Return_Type => Type_Index_For_Expr);
      
      Rule_Name_76 : aliased constant Text_Type :=
        "Stream_Concat";
      Rule_Doc_76 : aliased constant Text_Type :=
        "";
      Rule_Desc_76 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_76'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_76'Access,
         Return_Type => Type_Index_For_Expr);
      
      Rule_Name_77 : aliased constant Text_Type :=
        "Logic";
      Rule_Doc_77 : aliased constant Text_Type :=
        "";
      Rule_Desc_77 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_77'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_77'Access,
         Return_Type => Type_Index_For_Expr);
      
      Rule_Name_78 : aliased constant Text_Type :=
        "Rel";
      Rule_Doc_78 : aliased constant Text_Type :=
        "";
      Rule_Desc_78 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_78'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_78'Access,
         Return_Type => Type_Index_For_Expr);
      
      Rule_Name_79 : aliased constant Text_Type :=
        "Eq";
      Rule_Doc_79 : aliased constant Text_Type :=
        "";
      Rule_Desc_79 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_79'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_79'Access,
         Return_Type => Type_Index_For_Expr);
      
      Rule_Name_80 : aliased constant Text_Type :=
        "Arith_1";
      Rule_Doc_80 : aliased constant Text_Type :=
        "";
      Rule_Desc_80 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_80'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_80'Access,
         Return_Type => Type_Index_For_Expr);
      
      Rule_Name_81 : aliased constant Text_Type :=
        "Arith_2";
      Rule_Doc_81 : aliased constant Text_Type :=
        "";
      Rule_Desc_81 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_81'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_81'Access,
         Return_Type => Type_Index_For_Expr);
      
      Rule_Name_82 : aliased constant Text_Type :=
        "Arith_3";
      Rule_Doc_82 : aliased constant Text_Type :=
        "";
      Rule_Desc_82 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_82'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_82'Access,
         Return_Type => Type_Index_For_Expr);
      
      Rule_Name_83 : aliased constant Text_Type :=
        "Isa_Or_Primary";
      Rule_Doc_83 : aliased constant Text_Type :=
        "";
      Rule_Desc_83 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_83'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_83'Access,
         Return_Type => Type_Index_For_Expr);
      
      Rule_Name_84 : aliased constant Text_Type :=
        "Logic_Propagate_Call";
      Rule_Doc_84 : aliased constant Text_Type :=
        "";
      Rule_Desc_84 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_84'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_84'Access,
         Return_Type => Type_Index_For_Logic_Propagate_Call);
      
      Rule_Name_85 : aliased constant Text_Type :=
        "Primary";
      Rule_Doc_85 : aliased constant Text_Type :=
        "";
      Rule_Desc_85 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_85'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_85'Access,
         Return_Type => Type_Index_For_Expr);
      
      Rule_Name_86 : aliased constant Text_Type :=
        "Match_Expr";
      Rule_Doc_86 : aliased constant Text_Type :=
        "";
      Rule_Desc_86 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_86'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_86'Access,
         Return_Type => Type_Index_For_Match_Expr);
      
      Rule_Name_87 : aliased constant Text_Type :=
        "Num_Lit";
      Rule_Doc_87 : aliased constant Text_Type :=
        "";
      Rule_Desc_87 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_87'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_87'Access,
         Return_Type => Type_Index_For_Num_Lit);
      
      Rule_Name_88 : aliased constant Text_Type :=
        "Big_Num_Lit";
      Rule_Doc_88 : aliased constant Text_Type :=
        "";
      Rule_Desc_88 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_88'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_88'Access,
         Return_Type => Type_Index_For_Big_Num_Lit);
      
      Rule_Name_89 : aliased constant Text_Type :=
        "String_Lit";
      Rule_Doc_89 : aliased constant Text_Type :=
        "";
      Rule_Desc_89 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_89'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_89'Access,
         Return_Type => Type_Index_For_String_Lit);
      
      Rule_Name_90 : aliased constant Text_Type :=
        "Block_String_Lit";
      Rule_Doc_90 : aliased constant Text_Type :=
        "";
      Rule_Desc_90 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_90'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_90'Access,
         Return_Type => Type_Index_For_Block_String_Lit);
      
      Rule_Name_91 : aliased constant Text_Type :=
        "Char_Lit";
      Rule_Doc_91 : aliased constant Text_Type :=
        "";
      Rule_Desc_91 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_91'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_91'Access,
         Return_Type => Type_Index_For_Char_Lit);
      
      Rule_Name_92 : aliased constant Text_Type :=
        "If_Expr";
      Rule_Doc_92 : aliased constant Text_Type :=
        "";
      Rule_Desc_92 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_92'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_92'Access,
         Return_Type => Type_Index_For_If_Expr);
      
      Rule_Name_93 : aliased constant Text_Type :=
        "Raise_Expr";
      Rule_Doc_93 : aliased constant Text_Type :=
        "";
      Rule_Desc_93 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_93'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_93'Access,
         Return_Type => Type_Index_For_Raise_Expr);
      
      Rule_Name_94 : aliased constant Text_Type :=
        "Try_Expr";
      Rule_Doc_94 : aliased constant Text_Type :=
        "";
      Rule_Desc_94 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_94'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_94'Access,
         Return_Type => Type_Index_For_Try_Expr);
      
      Rule_Name_95 : aliased constant Text_Type :=
        "Array_Literal";
      Rule_Doc_95 : aliased constant Text_Type :=
        "";
      Rule_Desc_95 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_95'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_95'Access,
         Return_Type => Type_Index_For_Array_Literal);
      
      Rule_Name_96 : aliased constant Text_Type :=
        "Callable_Ref";
      Rule_Doc_96 : aliased constant Text_Type :=
        "";
      Rule_Desc_96 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_96'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_96'Access,
         Return_Type => Type_Index_For_Expr);
      
      Rule_Name_97 : aliased constant Text_Type :=
        "Null_Cond_Qual";
      Rule_Doc_97 : aliased constant Text_Type :=
        "";
      Rule_Desc_97 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_97'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_97'Access,
         Return_Type => Type_Index_For_Null_Cond_Qualifier);
      
      Rule_Name_98 : aliased constant Text_Type :=
        "Basic_Expr";
      Rule_Doc_98 : aliased constant Text_Type :=
        "";
      Rule_Desc_98 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_98'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_98'Access,
         Return_Type => Type_Index_For_Expr);
      
      Rule_Name_99 : aliased constant Text_Type :=
        "Term";
      Rule_Doc_99 : aliased constant Text_Type :=
        "";
      Rule_Desc_99 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_99'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_99'Access,
         Return_Type => Type_Index_For_Expr);
      
      Rule_Name_100 : aliased constant Text_Type :=
        "Basic_Name";
      Rule_Doc_100 : aliased constant Text_Type :=
        "";
      Rule_Desc_100 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_100'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_100'Access,
         Return_Type => Type_Index_For_Expr);
      
      Rule_Name_101 : aliased constant Text_Type :=
        "Lambda_Expr";
      Rule_Doc_101 : aliased constant Text_Type :=
        "";
      Rule_Desc_101 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_101'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_101'Access,
         Return_Type => Type_Index_For_Lambda_Expr);
      
      Rule_Name_102 : aliased constant Text_Type :=
        "Null_Lit";
      Rule_Doc_102 : aliased constant Text_Type :=
        "";
      Rule_Desc_102 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_102'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_102'Access,
         Return_Type => Type_Index_For_Null_Lit);
      
      Rule_Name_103 : aliased constant Text_Type :=
        "Argument";
      Rule_Doc_103 : aliased constant Text_Type :=
        "";
      Rule_Desc_103 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_103'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_103'Access,
         Return_Type => Type_Index_For_Argument);
      
      Rule_Name_104 : aliased constant Text_Type :=
        "Args";
      Rule_Doc_104 : aliased constant Text_Type :=
        "";
      Rule_Desc_104 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_104'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_104'Access,
         Return_Type => Type_Index_For_Argument_List);
      
      Rule_Name_105 : aliased constant Text_Type :=
        "Decl_Annotation_Args";
      Rule_Doc_105 : aliased constant Text_Type :=
        "";
      Rule_Desc_105 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_105'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_105'Access,
         Return_Type => Type_Index_For_Decl_Annotation_Args);
      
      Rule_Name_106 : aliased constant Text_Type :=
        "Decl_Annotation";
      Rule_Doc_106 : aliased constant Text_Type :=
        "";
      Rule_Desc_106 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_106'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_106'Access,
         Return_Type => Type_Index_For_Decl_Annotation);
      
      Rule_Name_107 : aliased constant Text_Type :=
        "Query_Comprehension";
      Rule_Doc_107 : aliased constant Text_Type :=
        "";
      Rule_Desc_107 : aliased constant Grammar_Rule_Descriptor :=
        (Name        => Rule_Name_107'Access,
         Is_Public   => False,
         Doc         => Rule_Doc_107'Access,
         Return_Type => Type_Index_For_Query);

   Grammar_Rules : aliased constant Grammar_Rule_Descriptor_Array := (
      1 => Rule_Desc_1'Access,
2 => Rule_Desc_2'Access,
3 => Rule_Desc_3'Access,
4 => Rule_Desc_4'Access,
5 => Rule_Desc_5'Access,
6 => Rule_Desc_6'Access,
7 => Rule_Desc_7'Access,
8 => Rule_Desc_8'Access,
9 => Rule_Desc_9'Access,
10 => Rule_Desc_10'Access,
11 => Rule_Desc_11'Access,
12 => Rule_Desc_12'Access,
13 => Rule_Desc_13'Access,
14 => Rule_Desc_14'Access,
15 => Rule_Desc_15'Access,
16 => Rule_Desc_16'Access,
17 => Rule_Desc_17'Access,
18 => Rule_Desc_18'Access,
19 => Rule_Desc_19'Access,
20 => Rule_Desc_20'Access,
21 => Rule_Desc_21'Access,
22 => Rule_Desc_22'Access,
23 => Rule_Desc_23'Access,
24 => Rule_Desc_24'Access,
25 => Rule_Desc_25'Access,
26 => Rule_Desc_26'Access,
27 => Rule_Desc_27'Access,
28 => Rule_Desc_28'Access,
29 => Rule_Desc_29'Access,
30 => Rule_Desc_30'Access,
31 => Rule_Desc_31'Access,
32 => Rule_Desc_32'Access,
33 => Rule_Desc_33'Access,
34 => Rule_Desc_34'Access,
35 => Rule_Desc_35'Access,
36 => Rule_Desc_36'Access,
37 => Rule_Desc_37'Access,
38 => Rule_Desc_38'Access,
39 => Rule_Desc_39'Access,
40 => Rule_Desc_40'Access,
41 => Rule_Desc_41'Access,
42 => Rule_Desc_42'Access,
43 => Rule_Desc_43'Access,
44 => Rule_Desc_44'Access,
45 => Rule_Desc_45'Access,
46 => Rule_Desc_46'Access,
47 => Rule_Desc_47'Access,
48 => Rule_Desc_48'Access,
49 => Rule_Desc_49'Access,
50 => Rule_Desc_50'Access,
51 => Rule_Desc_51'Access,
52 => Rule_Desc_52'Access,
53 => Rule_Desc_53'Access,
54 => Rule_Desc_54'Access,
55 => Rule_Desc_55'Access,
56 => Rule_Desc_56'Access,
57 => Rule_Desc_57'Access,
58 => Rule_Desc_58'Access,
59 => Rule_Desc_59'Access,
60 => Rule_Desc_60'Access,
61 => Rule_Desc_61'Access,
62 => Rule_Desc_62'Access,
63 => Rule_Desc_63'Access,
64 => Rule_Desc_64'Access,
65 => Rule_Desc_65'Access,
66 => Rule_Desc_66'Access,
67 => Rule_Desc_67'Access,
68 => Rule_Desc_68'Access,
69 => Rule_Desc_69'Access,
70 => Rule_Desc_70'Access,
71 => Rule_Desc_71'Access,
72 => Rule_Desc_72'Access,
73 => Rule_Desc_73'Access,
74 => Rule_Desc_74'Access,
75 => Rule_Desc_75'Access,
76 => Rule_Desc_76'Access,
77 => Rule_Desc_77'Access,
78 => Rule_Desc_78'Access,
79 => Rule_Desc_79'Access,
80 => Rule_Desc_80'Access,
81 => Rule_Desc_81'Access,
82 => Rule_Desc_82'Access,
83 => Rule_Desc_83'Access,
84 => Rule_Desc_84'Access,
85 => Rule_Desc_85'Access,
86 => Rule_Desc_86'Access,
87 => Rule_Desc_87'Access,
88 => Rule_Desc_88'Access,
89 => Rule_Desc_89'Access,
90 => Rule_Desc_90'Access,
91 => Rule_Desc_91'Access,
92 => Rule_Desc_92'Access,
93 => Rule_Desc_93'Access,
94 => Rule_Desc_94'Access,
95 => Rule_Desc_95'Access,
96 => Rule_Desc_96'Access,
97 => Rule_Desc_97'Access,
98 => Rule_Desc_98'Access,
99 => Rule_Desc_99'Access,
100 => Rule_Desc_100'Access,
101 => Rule_Desc_101'Access,
102 => Rule_Desc_102'Access,
103 => Rule_Desc_103'Access,
104 => Rule_Desc_104'Access,
105 => Rule_Desc_105'Access,
106 => Rule_Desc_106'Access,
107 => Rule_Desc_107'Access
   );

   ------------------------------------
   -- General value type descriptors --
   ------------------------------------

   
      
      Debug_Name_For_Internal_Unit : aliased constant String :=
        "AnalysisUnit";
      Type_Doc_For_Internal_Unit        : aliased constant Text_Type := "";
      Desc_For_Internal_Unit       : aliased constant Type_Descriptor :=
        (Category      => Analysis_Unit_Category,
         Debug_Name    => Debug_Name_For_Internal_Unit'Access,
         Documentation => Type_Doc_For_Internal_Unit'Access);
      
      Debug_Name_For_Big_Integer_Type : aliased constant String :=
        "BigInt";
      Type_Doc_For_Big_Integer_Type        : aliased constant Text_Type := "";
      Desc_For_Big_Integer_Type       : aliased constant Type_Descriptor :=
        (Category      => Big_Int_Category,
         Debug_Name    => Debug_Name_For_Big_Integer_Type'Access,
         Documentation => Type_Doc_For_Big_Integer_Type'Access);
      
      Debug_Name_For_Boolean : aliased constant String :=
        "Bool";
      Type_Doc_For_Boolean        : aliased constant Text_Type := "";
      Desc_For_Boolean       : aliased constant Type_Descriptor :=
        (Category      => Bool_Category,
         Debug_Name    => Debug_Name_For_Boolean'Access,
         Documentation => Type_Doc_For_Boolean'Access);
      
      Debug_Name_For_Character_Type : aliased constant String :=
        "Char";
      Type_Doc_For_Character_Type        : aliased constant Text_Type := "";
      Desc_For_Character_Type       : aliased constant Type_Descriptor :=
        (Category      => Char_Category,
         Debug_Name    => Debug_Name_For_Character_Type'Access,
         Documentation => Type_Doc_For_Character_Type'Access);
      
      Debug_Name_For_Integer : aliased constant String :=
        "Int";
      Type_Doc_For_Integer        : aliased constant Text_Type := "";
      Desc_For_Integer       : aliased constant Type_Descriptor :=
        (Category      => Int_Category,
         Debug_Name    => Debug_Name_For_Integer'Access,
         Documentation => Type_Doc_For_Integer'Access);
      
      Debug_Name_For_Source_Location : aliased constant String :=
        "SourceLocation";
      Type_Doc_For_Source_Location        : aliased constant Text_Type := "";
      Desc_For_Source_Location       : aliased constant Type_Descriptor :=
        (Category      => Source_Location_Category,
         Debug_Name    => Debug_Name_For_Source_Location'Access,
         Documentation => Type_Doc_For_Source_Location'Access);
      
      Debug_Name_For_Source_Location_Range : aliased constant String :=
        "SourceLocationRange";
      Type_Doc_For_Source_Location_Range        : aliased constant Text_Type := "";
      Desc_For_Source_Location_Range       : aliased constant Type_Descriptor :=
        (Category      => Source_Location_Range_Category,
         Debug_Name    => Debug_Name_For_Source_Location_Range'Access,
         Documentation => Type_Doc_For_Source_Location_Range'Access);
      
      Debug_Name_For_String_Type : aliased constant String :=
        "String";
      Type_Doc_For_String_Type        : aliased constant Text_Type := "";
      Desc_For_String_Type       : aliased constant Type_Descriptor :=
        (Category      => String_Category,
         Debug_Name    => Debug_Name_For_String_Type'Access,
         Documentation => Type_Doc_For_String_Type'Access);
      
      Debug_Name_For_Token_Reference : aliased constant String :=
        "Token";
      Type_Doc_For_Token_Reference        : aliased constant Text_Type := "";
      Desc_For_Token_Reference       : aliased constant Type_Descriptor :=
        (Category      => Token_Category,
         Debug_Name    => Debug_Name_For_Token_Reference'Access,
         Documentation => Type_Doc_For_Token_Reference'Access);
      
      Debug_Name_For_Symbol_Type : aliased constant String :=
        "Symbol";
      Type_Doc_For_Symbol_Type        : aliased constant Text_Type := "";
      Desc_For_Symbol_Type       : aliased constant Type_Descriptor :=
        (Category      => Symbol_Category,
         Debug_Name    => Debug_Name_For_Symbol_Type'Access,
         Documentation => Type_Doc_For_Symbol_Type'Access);
      
      Debug_Name_For_Analysis_Unit_Kind : aliased constant String :=
        "AnalysisUnitKind";
      Type_Doc_For_Analysis_Unit_Kind        : aliased constant Text_Type := "" & Character_Type'Val (10)
 & "        Specify a kind of analysis unit. Specification units provide an" & Character_Type'Val (10)
 & "        interface to the outer world while body units provide an" & Character_Type'Val (10)
 & "        implementation for the corresponding interface." & Character_Type'Val (10)
 & "        ";
      Desc_For_Analysis_Unit_Kind       : aliased constant Type_Descriptor :=
        (Category      => Enum_Category,
         Debug_Name    => Debug_Name_For_Analysis_Unit_Kind'Access,
         Documentation => Type_Doc_For_Analysis_Unit_Kind'Access);
      
      Debug_Name_For_Completion_Item_Kind : aliased constant String :=
        "CompletionItemKind";
      Type_Doc_For_Completion_Item_Kind        : aliased constant Text_Type := "" & Character_Type'Val (10)
 & "        Type of completion item. Refer to the official LSP specification." & Character_Type'Val (10)
 & "        ";
      Desc_For_Completion_Item_Kind       : aliased constant Type_Descriptor :=
        (Category      => Enum_Category,
         Debug_Name    => Debug_Name_For_Completion_Item_Kind'Access,
         Documentation => Type_Doc_For_Completion_Item_Kind'Access);
      
      Debug_Name_For_Designated_Env_Kind : aliased constant String :=
        "DesignatedEnvKind";
      Type_Doc_For_Designated_Env_Kind        : aliased constant Text_Type := "Discriminant for DesignatedEnv structures.";
      Desc_For_Designated_Env_Kind       : aliased constant Type_Descriptor :=
        (Category      => Enum_Category,
         Debug_Name    => Debug_Name_For_Designated_Env_Kind'Access,
         Documentation => Type_Doc_For_Designated_Env_Kind'Access);
      
      Debug_Name_For_Grammar_Rule : aliased constant String :=
        "GrammarRule";
      Type_Doc_For_Grammar_Rule        : aliased constant Text_Type := "Gramar rule to use for parsing.";
      Desc_For_Grammar_Rule       : aliased constant Type_Descriptor :=
        (Category      => Enum_Category,
         Debug_Name    => Debug_Name_For_Grammar_Rule'Access,
         Documentation => Type_Doc_For_Grammar_Rule'Access);
      
      Debug_Name_For_Lookup_Kind : aliased constant String :=
        "LookupKind";
      Type_Doc_For_Lookup_Kind        : aliased constant Text_Type := "";
      Desc_For_Lookup_Kind       : aliased constant Type_Descriptor :=
        (Category      => Enum_Category,
         Debug_Name    => Debug_Name_For_Lookup_Kind'Access,
         Documentation => Type_Doc_For_Lookup_Kind'Access);
      
      Debug_Name_For_Internal_Complete_Item_Array_Access : aliased constant String :=
        "CompleteItem.array";
      Type_Doc_For_Internal_Complete_Item_Array_Access        : aliased constant Text_Type := "";
      Desc_For_Internal_Complete_Item_Array_Access       : aliased constant Type_Descriptor :=
        (Category      => Array_Category,
         Debug_Name    => Debug_Name_For_Internal_Complete_Item_Array_Access'Access,
         Documentation => Type_Doc_For_Internal_Complete_Item_Array_Access'Access);
      
      Debug_Name_For_Internal_Entity_Array_Access : aliased constant String :=
        "LktNode.array";
      Type_Doc_For_Internal_Entity_Array_Access        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Array_Access       : aliased constant Type_Descriptor :=
        (Category      => Array_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Array_Access'Access,
         Documentation => Type_Doc_For_Internal_Entity_Array_Access'Access);
      
      Debug_Name_For_Internal_Entity_Def_Id_Array_Access : aliased constant String :=
        "DefId.array";
      Type_Doc_For_Internal_Entity_Def_Id_Array_Access        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Def_Id_Array_Access       : aliased constant Type_Descriptor :=
        (Category      => Array_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Def_Id_Array_Access'Access,
         Documentation => Type_Doc_For_Internal_Entity_Def_Id_Array_Access'Access);
      
      Debug_Name_For_Internal_Entity_Fun_Decl_Array_Access : aliased constant String :=
        "FunDecl.array";
      Type_Doc_For_Internal_Entity_Fun_Decl_Array_Access        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Fun_Decl_Array_Access       : aliased constant Type_Descriptor :=
        (Category      => Array_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Fun_Decl_Array_Access'Access,
         Documentation => Type_Doc_For_Internal_Entity_Fun_Decl_Array_Access'Access);
      
      Debug_Name_For_Internal_Logic_Context_Array_Access : aliased constant String :=
        "LogicContext.array";
      Type_Doc_For_Internal_Logic_Context_Array_Access        : aliased constant Text_Type := "";
      Desc_For_Internal_Logic_Context_Array_Access       : aliased constant Type_Descriptor :=
        (Category      => Array_Category,
         Debug_Name    => Debug_Name_For_Internal_Logic_Context_Array_Access'Access,
         Documentation => Type_Doc_For_Internal_Logic_Context_Array_Access'Access);
      
      Debug_Name_For_Internal_Ref_Result_Array_Access : aliased constant String :=
        "RefResult.array";
      Type_Doc_For_Internal_Ref_Result_Array_Access        : aliased constant Text_Type := "";
      Desc_For_Internal_Ref_Result_Array_Access       : aliased constant Type_Descriptor :=
        (Category      => Array_Category,
         Debug_Name    => Debug_Name_For_Internal_Ref_Result_Array_Access'Access,
         Documentation => Type_Doc_For_Internal_Ref_Result_Array_Access'Access);
      
      Debug_Name_For_Internal_Solver_Diagnostic_Array_Access : aliased constant String :=
        "SolverDiagnostic.array";
      Type_Doc_For_Internal_Solver_Diagnostic_Array_Access        : aliased constant Text_Type := "";
      Desc_For_Internal_Solver_Diagnostic_Array_Access       : aliased constant Type_Descriptor :=
        (Category      => Array_Category,
         Debug_Name    => Debug_Name_For_Internal_Solver_Diagnostic_Array_Access'Access,
         Documentation => Type_Doc_For_Internal_Solver_Diagnostic_Array_Access'Access);
      
      Debug_Name_For_Internal_Unit_Array_Access : aliased constant String :=
        "AnalysisUnit.array";
      Type_Doc_For_Internal_Unit_Array_Access        : aliased constant Text_Type := "";
      Desc_For_Internal_Unit_Array_Access       : aliased constant Type_Descriptor :=
        (Category      => Array_Category,
         Debug_Name    => Debug_Name_For_Internal_Unit_Array_Access'Access,
         Documentation => Type_Doc_For_Internal_Unit_Array_Access'Access);
      
      Debug_Name_For_Internal_Complete_Item : aliased constant String :=
        "CompleteItem";
      Type_Doc_For_Internal_Complete_Item        : aliased constant Text_Type := "Completion item for language servers" & Character_Type'Val (10)
;
      Desc_For_Internal_Complete_Item       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Complete_Item'Access,
         Documentation => Type_Doc_For_Internal_Complete_Item'Access);
      
      Debug_Name_For_Internal_Decoded_Char_Value : aliased constant String :=
        "DecodedCharValue";
      Type_Doc_For_Internal_Decoded_Char_Value        : aliased constant Text_Type := "Result for ``CharLit.p_denoted_value``." & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "If that property is successful, set ``has_error`` to false and ``value`` to" & Character_Type'Val (10)
 & "the decoded character value. Otherwise, set ``has_error`` to true and" & Character_Type'Val (10)
 & "``error_sloc`` and ``error_message`` to give information about the decoding" & Character_Type'Val (10)
 & "failure." & Character_Type'Val (10)
;
      Desc_For_Internal_Decoded_Char_Value       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Decoded_Char_Value'Access,
         Documentation => Type_Doc_For_Internal_Decoded_Char_Value'Access);
      
      Debug_Name_For_Internal_Decoded_String_Value : aliased constant String :=
        "DecodedStringValue";
      Type_Doc_For_Internal_Decoded_String_Value        : aliased constant Text_Type := "Result for ``StringLit.p_denoted_value``." & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "If that property is successful, set ``has_error`` to false and ``value`` to" & Character_Type'Val (10)
 & "the decoded string value. Otherwise, set ``has_error`` to true and" & Character_Type'Val (10)
 & "``error_sloc`` and ``error_message`` to give information about the decoding" & Character_Type'Val (10)
 & "failure." & Character_Type'Val (10)
;
      Desc_For_Internal_Decoded_String_Value       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Decoded_String_Value'Access,
         Documentation => Type_Doc_For_Internal_Decoded_String_Value'Access);
      
      Debug_Name_For_Internal_Logic_Context : aliased constant String :=
        "LogicContext";
      Type_Doc_For_Internal_Logic_Context        : aliased constant Text_Type := "" & Character_Type'Val (10)
 & "            Describes an interpretation of a reference. Can be attached" & Character_Type'Val (10)
 & "            to logic atoms (e.g. Binds) to indicate under which interpretation" & Character_Type'Val (10)
 & "            this particular atom was produced, which can in turn be used to" & Character_Type'Val (10)
 & "            produce informative diagnostics for resolution failures." & Character_Type'Val (10)
 & "        ";
      Desc_For_Internal_Logic_Context       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Logic_Context'Access,
         Documentation => Type_Doc_For_Internal_Logic_Context'Access);
      
      Debug_Name_For_Internal_Ref_Result : aliased constant String :=
        "RefResult";
      Type_Doc_For_Internal_Ref_Result        : aliased constant Text_Type := "Reference result struct" & Character_Type'Val (10)
;
      Desc_For_Internal_Ref_Result       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Ref_Result'Access,
         Documentation => Type_Doc_For_Internal_Ref_Result'Access);
      
      Debug_Name_For_Internal_Solver_Diagnostic : aliased constant String :=
        "SolverDiagnostic";
      Type_Doc_For_Internal_Solver_Diagnostic        : aliased constant Text_Type := "" & Character_Type'Val (10)
 & "            A raw diagnostic produced by a solver resolution failure." & Character_Type'Val (10)
 & "            This contains as much information as possible to allow formatters" & Character_Type'Val (10)
 & "            down the chain to filter/choose which diagnostics to show among" & Character_Type'Val (10)
 & "            a set of diagnostics produced for a single equation." & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "            * ``Message_Template`` is a string explaining the error, which may" & Character_Type'Val (10)
 & "              contain holes represented by the ``{}`` characters. Literal" & Character_Type'Val (10)
 & "              opening braces are encoded as ``{{``." & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "            * ``Args`` is an array of nodes, which are to be plugged in the" & Character_Type'Val (10)
 & "              holes of the template in the same order (i.e. the first argument" & Character_Type'Val (10)
 & "              goes into the first hole of the template, etc.)." & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "            * ``Location`` is a node which indicates the location of the error." & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "            * ``Contexts`` is the array of contexts that were deemed relevant" & Character_Type'Val (10)
 & "              for this error." & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "            * ``Round`` is the solver round during which this diagnostic was" & Character_Type'Val (10)
 & "              emitted." & Character_Type'Val (10)
 & "        ";
      Desc_For_Internal_Solver_Diagnostic       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Solver_Diagnostic'Access,
         Documentation => Type_Doc_For_Internal_Solver_Diagnostic'Access);
      
      Debug_Name_For_Internal_Solver_Result : aliased constant String :=
        "SolverResult";
      Type_Doc_For_Internal_Solver_Result        : aliased constant Text_Type := "" & Character_Type'Val (10)
 & "            A pair returned by the ``Solve_With_Diagnostic`` primitive," & Character_Type'Val (10)
 & "            consisting of:" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "            * A ``Success`` field indicating whether resolution was successful" & Character_Type'Val (10)
 & "              or not." & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "            * A ``Diagnostics`` field containing an array of diagnostics which" & Character_Type'Val (10)
 & "              may be non-empty if ``Success`` is ``False``." & Character_Type'Val (10)
 & "        ";
      Desc_For_Internal_Solver_Result       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Solver_Result'Access,
         Documentation => Type_Doc_For_Internal_Solver_Result'Access);
      
      Debug_Name_For_Internal_Entity : aliased constant String :=
        "LktNode";
      Type_Doc_For_Internal_Entity        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity'Access,
         Documentation => Type_Doc_For_Internal_Entity'Access);
      
      Debug_Name_For_Internal_Entity_Argument : aliased constant String :=
        "Argument";
      Type_Doc_For_Internal_Entity_Argument        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Argument       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Argument'Access,
         Documentation => Type_Doc_For_Internal_Entity_Argument'Access);
      
      Debug_Name_For_Internal_Entity_Base_Import : aliased constant String :=
        "BaseImport";
      Type_Doc_For_Internal_Entity_Base_Import        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Base_Import       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Base_Import'Access,
         Documentation => Type_Doc_For_Internal_Entity_Base_Import'Access);
      
      Debug_Name_For_Internal_Entity_Import : aliased constant String :=
        "Import";
      Type_Doc_For_Internal_Entity_Import        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Import       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Import'Access,
         Documentation => Type_Doc_For_Internal_Entity_Import'Access);
      
      Debug_Name_For_Internal_Entity_Import_All_From : aliased constant String :=
        "ImportAllFrom";
      Type_Doc_For_Internal_Entity_Import_All_From        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Import_All_From       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Import_All_From'Access,
         Documentation => Type_Doc_For_Internal_Entity_Import_All_From'Access);
      
      Debug_Name_For_Internal_Entity_Import_From : aliased constant String :=
        "ImportFrom";
      Type_Doc_For_Internal_Entity_Import_From        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Import_From       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Import_From'Access,
         Documentation => Type_Doc_For_Internal_Entity_Import_From'Access);
      
      Debug_Name_For_Internal_Entity_Base_Lexer_Case_Rule_Alt : aliased constant String :=
        "BaseLexerCaseRuleAlt";
      Type_Doc_For_Internal_Entity_Base_Lexer_Case_Rule_Alt        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Base_Lexer_Case_Rule_Alt       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Base_Lexer_Case_Rule_Alt'Access,
         Documentation => Type_Doc_For_Internal_Entity_Base_Lexer_Case_Rule_Alt'Access);
      
      Debug_Name_For_Internal_Entity_Error_Lexer_Case_Rule_Alt : aliased constant String :=
        "ErrorLexerCaseRuleAlt";
      Type_Doc_For_Internal_Entity_Error_Lexer_Case_Rule_Alt        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Error_Lexer_Case_Rule_Alt       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Error_Lexer_Case_Rule_Alt'Access,
         Documentation => Type_Doc_For_Internal_Entity_Error_Lexer_Case_Rule_Alt'Access);
      
      Debug_Name_For_Internal_Entity_Lexer_Case_Rule_Cond_Alt : aliased constant String :=
        "LexerCaseRuleCondAlt";
      Type_Doc_For_Internal_Entity_Lexer_Case_Rule_Cond_Alt        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Lexer_Case_Rule_Cond_Alt       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Lexer_Case_Rule_Cond_Alt'Access,
         Documentation => Type_Doc_For_Internal_Entity_Lexer_Case_Rule_Cond_Alt'Access);
      
      Debug_Name_For_Internal_Entity_Lexer_Case_Rule_Default_Alt : aliased constant String :=
        "LexerCaseRuleDefaultAlt";
      Type_Doc_For_Internal_Entity_Lexer_Case_Rule_Default_Alt        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Lexer_Case_Rule_Default_Alt       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Lexer_Case_Rule_Default_Alt'Access,
         Documentation => Type_Doc_For_Internal_Entity_Lexer_Case_Rule_Default_Alt'Access);
      
      Debug_Name_For_Internal_Entity_Base_Match_Branch : aliased constant String :=
        "BaseMatchBranch";
      Type_Doc_For_Internal_Entity_Base_Match_Branch        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Base_Match_Branch       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Base_Match_Branch'Access,
         Documentation => Type_Doc_For_Internal_Entity_Base_Match_Branch'Access);
      
      Debug_Name_For_Internal_Entity_Match_Branch : aliased constant String :=
        "MatchBranch";
      Type_Doc_For_Internal_Entity_Match_Branch        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Match_Branch       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Match_Branch'Access,
         Documentation => Type_Doc_For_Internal_Entity_Match_Branch'Access);
      
      Debug_Name_For_Internal_Entity_Pattern_Match_Branch : aliased constant String :=
        "PatternMatchBranch";
      Type_Doc_For_Internal_Entity_Pattern_Match_Branch        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Pattern_Match_Branch       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Pattern_Match_Branch'Access,
         Documentation => Type_Doc_For_Internal_Entity_Pattern_Match_Branch'Access);
      
      Debug_Name_For_Internal_Entity_Block_Expr_Clause : aliased constant String :=
        "BlockExprClause";
      Type_Doc_For_Internal_Entity_Block_Expr_Clause        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Block_Expr_Clause       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Block_Expr_Clause'Access,
         Documentation => Type_Doc_For_Internal_Entity_Block_Expr_Clause'Access);
      
      Debug_Name_For_Internal_Entity_Block_String_Line : aliased constant String :=
        "BlockStringLine";
      Type_Doc_For_Internal_Entity_Block_String_Line        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Block_String_Line       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Block_String_Line'Access,
         Documentation => Type_Doc_For_Internal_Entity_Block_String_Line'Access);
      
      Debug_Name_For_Internal_Entity_Class_Qualifier : aliased constant String :=
        "ClassQualifier";
      Type_Doc_For_Internal_Entity_Class_Qualifier        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Class_Qualifier       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Class_Qualifier'Access,
         Documentation => Type_Doc_For_Internal_Entity_Class_Qualifier'Access);
      
      Debug_Name_For_Internal_Entity_Class_Qualifier_Absent : aliased constant String :=
        "ClassQualifier.Absent";
      Type_Doc_For_Internal_Entity_Class_Qualifier_Absent        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Class_Qualifier_Absent       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Class_Qualifier_Absent'Access,
         Documentation => Type_Doc_For_Internal_Entity_Class_Qualifier_Absent'Access);
      
      Debug_Name_For_Internal_Entity_Class_Qualifier_Present : aliased constant String :=
        "ClassQualifier.Present";
      Type_Doc_For_Internal_Entity_Class_Qualifier_Present        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Class_Qualifier_Present       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Class_Qualifier_Present'Access,
         Documentation => Type_Doc_For_Internal_Entity_Class_Qualifier_Present'Access);
      
      Debug_Name_For_Internal_Entity_Decl : aliased constant String :=
        "Decl";
      Type_Doc_For_Internal_Entity_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Base_Grammar_Rule_Decl : aliased constant String :=
        "BaseGrammarRuleDecl";
      Type_Doc_For_Internal_Entity_Base_Grammar_Rule_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Base_Grammar_Rule_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Base_Grammar_Rule_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Base_Grammar_Rule_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Rule_Decl : aliased constant String :=
        "GrammarRuleDecl";
      Type_Doc_For_Internal_Entity_Grammar_Rule_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Rule_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Rule_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Rule_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Synthetic_Lexer_Decl : aliased constant String :=
        "SyntheticLexerDecl";
      Type_Doc_For_Internal_Entity_Synthetic_Lexer_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Synthetic_Lexer_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Synthetic_Lexer_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Synthetic_Lexer_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Base_Val_Decl : aliased constant String :=
        "BaseValDecl";
      Type_Doc_For_Internal_Entity_Base_Val_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Base_Val_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Base_Val_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Base_Val_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Node_Decl : aliased constant String :=
        "NodeDecl";
      Type_Doc_For_Internal_Entity_Node_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Node_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Node_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Node_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Self_Decl : aliased constant String :=
        "SelfDecl";
      Type_Doc_For_Internal_Entity_Self_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Self_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Self_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Self_Decl'Access);
      
      Debug_Name_For_Internal_Entity_User_Val_Decl : aliased constant String :=
        "UserValDecl";
      Type_Doc_For_Internal_Entity_User_Val_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_User_Val_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_User_Val_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_User_Val_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Binding_Val_Decl : aliased constant String :=
        "BindingValDecl";
      Type_Doc_For_Internal_Entity_Binding_Val_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Binding_Val_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Binding_Val_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Binding_Val_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Enum_Lit_Decl : aliased constant String :=
        "EnumLitDecl";
      Type_Doc_For_Internal_Entity_Enum_Lit_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Enum_Lit_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Enum_Lit_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Enum_Lit_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Explicitly_Typed_Decl : aliased constant String :=
        "ExplicitlyTypedDecl";
      Type_Doc_For_Internal_Entity_Explicitly_Typed_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Explicitly_Typed_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Explicitly_Typed_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Explicitly_Typed_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Component_Decl : aliased constant String :=
        "ComponentDecl";
      Type_Doc_For_Internal_Entity_Component_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Component_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Component_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Component_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Field_Decl : aliased constant String :=
        "FieldDecl";
      Type_Doc_For_Internal_Entity_Field_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Field_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Field_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Field_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Fun_Param_Decl : aliased constant String :=
        "FunParamDecl";
      Type_Doc_For_Internal_Entity_Fun_Param_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Fun_Param_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Fun_Param_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Fun_Param_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Lambda_Param_Decl : aliased constant String :=
        "LambdaParamDecl";
      Type_Doc_For_Internal_Entity_Lambda_Param_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Lambda_Param_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Lambda_Param_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Lambda_Param_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Dyn_Var_Decl : aliased constant String :=
        "DynVarDecl";
      Type_Doc_For_Internal_Entity_Dyn_Var_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Dyn_Var_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Dyn_Var_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Dyn_Var_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Match_Val_Decl : aliased constant String :=
        "MatchValDecl";
      Type_Doc_For_Internal_Entity_Match_Val_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Match_Val_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Match_Val_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Match_Val_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Val_Decl : aliased constant String :=
        "ValDecl";
      Type_Doc_For_Internal_Entity_Val_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Val_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Val_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Val_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Fun_Decl : aliased constant String :=
        "FunDecl";
      Type_Doc_For_Internal_Entity_Fun_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Fun_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Fun_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Fun_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Env_Spec_Decl : aliased constant String :=
        "EnvSpecDecl";
      Type_Doc_For_Internal_Entity_Env_Spec_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Env_Spec_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Env_Spec_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Env_Spec_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Error_Decl : aliased constant String :=
        "ErrorDecl";
      Type_Doc_For_Internal_Entity_Error_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Error_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Error_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Error_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Generic_Decl : aliased constant String :=
        "GenericDecl";
      Type_Doc_For_Internal_Entity_Generic_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Generic_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Generic_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Generic_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Decl : aliased constant String :=
        "GrammarDecl";
      Type_Doc_For_Internal_Entity_Grammar_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Langkit_Root : aliased constant String :=
        "LangkitRoot";
      Type_Doc_For_Internal_Entity_Langkit_Root        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Langkit_Root       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Langkit_Root'Access,
         Documentation => Type_Doc_For_Internal_Entity_Langkit_Root'Access);
      
      Debug_Name_For_Internal_Entity_Lexer_Decl : aliased constant String :=
        "LexerDecl";
      Type_Doc_For_Internal_Entity_Lexer_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Lexer_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Lexer_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Lexer_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Lexer_Family_Decl : aliased constant String :=
        "LexerFamilyDecl";
      Type_Doc_For_Internal_Entity_Lexer_Family_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Lexer_Family_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Lexer_Family_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Lexer_Family_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Synth_Fun_Decl : aliased constant String :=
        "SynthFunDecl";
      Type_Doc_For_Internal_Entity_Synth_Fun_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Synth_Fun_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Synth_Fun_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Synth_Fun_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Synth_Param_Decl : aliased constant String :=
        "SynthParamDecl";
      Type_Doc_For_Internal_Entity_Synth_Param_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Synth_Param_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Synth_Param_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Synth_Param_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Type_Decl : aliased constant String :=
        "TypeDecl";
      Type_Doc_For_Internal_Entity_Type_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Type_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Type_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Type_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Any_Type_Decl : aliased constant String :=
        "AnyTypeDecl";
      Type_Doc_For_Internal_Entity_Any_Type_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Any_Type_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Any_Type_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Any_Type_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Enum_Class_Alt_Decl : aliased constant String :=
        "EnumClassAltDecl";
      Type_Doc_For_Internal_Entity_Enum_Class_Alt_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Enum_Class_Alt_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Enum_Class_Alt_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Enum_Class_Alt_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Function_Type : aliased constant String :=
        "FunctionType";
      Type_Doc_For_Internal_Entity_Function_Type        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Function_Type       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Function_Type'Access,
         Documentation => Type_Doc_For_Internal_Entity_Function_Type'Access);
      
      Debug_Name_For_Internal_Entity_Generic_Param_Type_Decl : aliased constant String :=
        "GenericParamTypeDecl";
      Type_Doc_For_Internal_Entity_Generic_Param_Type_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Generic_Param_Type_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Generic_Param_Type_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Generic_Param_Type_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Named_Type_Decl : aliased constant String :=
        "NamedTypeDecl";
      Type_Doc_For_Internal_Entity_Named_Type_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Named_Type_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Named_Type_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Named_Type_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Basic_Class_Decl : aliased constant String :=
        "BasicClassDecl";
      Type_Doc_For_Internal_Entity_Basic_Class_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Basic_Class_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Basic_Class_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Basic_Class_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Class_Decl : aliased constant String :=
        "ClassDecl";
      Type_Doc_For_Internal_Entity_Class_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Class_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Class_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Class_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Enum_Class_Decl : aliased constant String :=
        "EnumClassDecl";
      Type_Doc_For_Internal_Entity_Enum_Class_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Enum_Class_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Enum_Class_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Enum_Class_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Enum_Type_Decl : aliased constant String :=
        "EnumTypeDecl";
      Type_Doc_For_Internal_Entity_Enum_Type_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Enum_Type_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Enum_Type_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Enum_Type_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Struct_Decl : aliased constant String :=
        "StructDecl";
      Type_Doc_For_Internal_Entity_Struct_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Struct_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Struct_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Struct_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Trait_Decl : aliased constant String :=
        "TraitDecl";
      Type_Doc_For_Internal_Entity_Trait_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Trait_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Trait_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Trait_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Decl_Annotation : aliased constant String :=
        "DeclAnnotation";
      Type_Doc_For_Internal_Entity_Decl_Annotation        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Decl_Annotation       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Decl_Annotation'Access,
         Documentation => Type_Doc_For_Internal_Entity_Decl_Annotation'Access);
      
      Debug_Name_For_Internal_Entity_Decl_Annotation_Args : aliased constant String :=
        "DeclAnnotationArgs";
      Type_Doc_For_Internal_Entity_Decl_Annotation_Args        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Decl_Annotation_Args       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Decl_Annotation_Args'Access,
         Documentation => Type_Doc_For_Internal_Entity_Decl_Annotation_Args'Access);
      
      Debug_Name_For_Internal_Entity_Dyn_Env_Wrapper : aliased constant String :=
        "DynEnvWrapper";
      Type_Doc_For_Internal_Entity_Dyn_Env_Wrapper        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Dyn_Env_Wrapper       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Dyn_Env_Wrapper'Access,
         Documentation => Type_Doc_For_Internal_Entity_Dyn_Env_Wrapper'Access);
      
      Debug_Name_For_Internal_Entity_Elsif_Branch : aliased constant String :=
        "ElsifBranch";
      Type_Doc_For_Internal_Entity_Elsif_Branch        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Elsif_Branch       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Elsif_Branch'Access,
         Documentation => Type_Doc_For_Internal_Entity_Elsif_Branch'Access);
      
      Debug_Name_For_Internal_Entity_Enum_Class_Case : aliased constant String :=
        "EnumClassCase";
      Type_Doc_For_Internal_Entity_Enum_Class_Case        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Enum_Class_Case       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Enum_Class_Case'Access,
         Documentation => Type_Doc_For_Internal_Entity_Enum_Class_Case'Access);
      
      Debug_Name_For_Internal_Entity_Excludes_Null : aliased constant String :=
        "ExcludesNull";
      Type_Doc_For_Internal_Entity_Excludes_Null        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Excludes_Null       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Excludes_Null'Access,
         Documentation => Type_Doc_For_Internal_Entity_Excludes_Null'Access);
      
      Debug_Name_For_Internal_Entity_Excludes_Null_Absent : aliased constant String :=
        "ExcludesNull.Absent";
      Type_Doc_For_Internal_Entity_Excludes_Null_Absent        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Excludes_Null_Absent       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Excludes_Null_Absent'Access,
         Documentation => Type_Doc_For_Internal_Entity_Excludes_Null_Absent'Access);
      
      Debug_Name_For_Internal_Entity_Excludes_Null_Present : aliased constant String :=
        "ExcludesNull.Present";
      Type_Doc_For_Internal_Entity_Excludes_Null_Present        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Excludes_Null_Present       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Excludes_Null_Present'Access,
         Documentation => Type_Doc_For_Internal_Entity_Excludes_Null_Present'Access);
      
      Debug_Name_For_Internal_Entity_Expr : aliased constant String :=
        "Expr";
      Type_Doc_For_Internal_Entity_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Any_Of : aliased constant String :=
        "AnyOf";
      Type_Doc_For_Internal_Entity_Any_Of        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Any_Of       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Any_Of'Access,
         Documentation => Type_Doc_For_Internal_Entity_Any_Of'Access);
      
      Debug_Name_For_Internal_Entity_Array_Literal : aliased constant String :=
        "ArrayLiteral";
      Type_Doc_For_Internal_Entity_Array_Literal        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Array_Literal       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Array_Literal'Access,
         Documentation => Type_Doc_For_Internal_Entity_Array_Literal'Access);
      
      Debug_Name_For_Internal_Entity_Base_Call_Expr : aliased constant String :=
        "BaseCallExpr";
      Type_Doc_For_Internal_Entity_Base_Call_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Base_Call_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Base_Call_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Base_Call_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Call_Expr : aliased constant String :=
        "CallExpr";
      Type_Doc_For_Internal_Entity_Call_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Call_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Call_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Call_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Logic_Call_Expr : aliased constant String :=
        "LogicCallExpr";
      Type_Doc_For_Internal_Entity_Logic_Call_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Logic_Call_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Logic_Call_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Logic_Call_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Logic_Predicate : aliased constant String :=
        "LogicPredicate";
      Type_Doc_For_Internal_Entity_Logic_Predicate        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Logic_Predicate       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Logic_Predicate'Access,
         Documentation => Type_Doc_For_Internal_Entity_Logic_Predicate'Access);
      
      Debug_Name_For_Internal_Entity_Logic_Propagate_Call : aliased constant String :=
        "LogicPropagateCall";
      Type_Doc_For_Internal_Entity_Logic_Propagate_Call        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Logic_Propagate_Call       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Logic_Propagate_Call'Access,
         Documentation => Type_Doc_For_Internal_Entity_Logic_Propagate_Call'Access);
      
      Debug_Name_For_Internal_Entity_Bin_Op : aliased constant String :=
        "BinOp";
      Type_Doc_For_Internal_Entity_Bin_Op        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Bin_Op       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Bin_Op'Access,
         Documentation => Type_Doc_For_Internal_Entity_Bin_Op'Access);
      
      Debug_Name_For_Internal_Entity_Block_Expr : aliased constant String :=
        "BlockExpr";
      Type_Doc_For_Internal_Entity_Block_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Block_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Block_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Block_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Cast_Expr : aliased constant String :=
        "CastExpr";
      Type_Doc_For_Internal_Entity_Cast_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Cast_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Cast_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Cast_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Dot_Expr : aliased constant String :=
        "DotExpr";
      Type_Doc_For_Internal_Entity_Dot_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Dot_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Dot_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Dot_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Error_On_Null : aliased constant String :=
        "ErrorOnNull";
      Type_Doc_For_Internal_Entity_Error_On_Null        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Error_On_Null       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Error_On_Null'Access,
         Documentation => Type_Doc_For_Internal_Entity_Error_On_Null'Access);
      
      Debug_Name_For_Internal_Entity_Generic_Instantiation : aliased constant String :=
        "GenericInstantiation";
      Type_Doc_For_Internal_Entity_Generic_Instantiation        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Generic_Instantiation       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Generic_Instantiation'Access,
         Documentation => Type_Doc_For_Internal_Entity_Generic_Instantiation'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Expr : aliased constant String :=
        "GrammarExpr";
      Type_Doc_For_Internal_Entity_Grammar_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Error_Grammar_Expr : aliased constant String :=
        "ErrorGrammarExpr";
      Type_Doc_For_Internal_Entity_Error_Grammar_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Error_Grammar_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Error_Grammar_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Error_Grammar_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Cut : aliased constant String :=
        "GrammarCut";
      Type_Doc_For_Internal_Entity_Grammar_Cut        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Cut       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Cut'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Cut'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Discard : aliased constant String :=
        "GrammarDiscard";
      Type_Doc_For_Internal_Entity_Grammar_Discard        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Discard       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Discard'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Discard'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Dont_Skip : aliased constant String :=
        "GrammarDontSkip";
      Type_Doc_For_Internal_Entity_Grammar_Dont_Skip        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Dont_Skip       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Dont_Skip'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Dont_Skip'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_List : aliased constant String :=
        "GrammarList";
      Type_Doc_For_Internal_Entity_Grammar_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_List'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Null : aliased constant String :=
        "GrammarNull";
      Type_Doc_For_Internal_Entity_Grammar_Null        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Null       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Null'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Null'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Opt : aliased constant String :=
        "GrammarOpt";
      Type_Doc_For_Internal_Entity_Grammar_Opt        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Opt       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Opt'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Opt'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Opt_Error : aliased constant String :=
        "GrammarOptError";
      Type_Doc_For_Internal_Entity_Grammar_Opt_Error        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Opt_Error       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Opt_Error'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Opt_Error'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Opt_Error_Group : aliased constant String :=
        "GrammarOptErrorGroup";
      Type_Doc_For_Internal_Entity_Grammar_Opt_Error_Group        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Opt_Error_Group       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Opt_Error_Group'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Opt_Error_Group'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Opt_Group : aliased constant String :=
        "GrammarOptGroup";
      Type_Doc_For_Internal_Entity_Grammar_Opt_Group        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Opt_Group       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Opt_Group'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Opt_Group'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Or_Expr : aliased constant String :=
        "GrammarOrExpr";
      Type_Doc_For_Internal_Entity_Grammar_Or_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Or_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Or_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Or_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Pick : aliased constant String :=
        "GrammarPick";
      Type_Doc_For_Internal_Entity_Grammar_Pick        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Pick       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Pick'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Pick'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Implicit_Pick : aliased constant String :=
        "GrammarImplicitPick";
      Type_Doc_For_Internal_Entity_Grammar_Implicit_Pick        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Implicit_Pick       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Implicit_Pick'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Implicit_Pick'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Predicate : aliased constant String :=
        "GrammarPredicate";
      Type_Doc_For_Internal_Entity_Grammar_Predicate        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Predicate       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Predicate'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Predicate'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Rule_Ref : aliased constant String :=
        "GrammarRuleRef";
      Type_Doc_For_Internal_Entity_Grammar_Rule_Ref        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Rule_Ref       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Rule_Ref'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Rule_Ref'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Skip : aliased constant String :=
        "GrammarSkip";
      Type_Doc_For_Internal_Entity_Grammar_Skip        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Skip       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Skip'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Skip'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Stop_Cut : aliased constant String :=
        "GrammarStopCut";
      Type_Doc_For_Internal_Entity_Grammar_Stop_Cut        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Stop_Cut       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Stop_Cut'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Stop_Cut'Access);
      
      Debug_Name_For_Internal_Entity_Parse_Node_Expr : aliased constant String :=
        "ParseNodeExpr";
      Type_Doc_For_Internal_Entity_Parse_Node_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Parse_Node_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Parse_Node_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Parse_Node_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Token_Lit : aliased constant String :=
        "TokenLit";
      Type_Doc_For_Internal_Entity_Token_Lit        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Token_Lit       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Token_Lit'Access,
         Documentation => Type_Doc_For_Internal_Entity_Token_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Token_No_Case_Lit : aliased constant String :=
        "TokenNoCaseLit";
      Type_Doc_For_Internal_Entity_Token_No_Case_Lit        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Token_No_Case_Lit       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Token_No_Case_Lit'Access,
         Documentation => Type_Doc_For_Internal_Entity_Token_No_Case_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Token_Pattern_Concat : aliased constant String :=
        "TokenPatternConcat";
      Type_Doc_For_Internal_Entity_Token_Pattern_Concat        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Token_Pattern_Concat       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Token_Pattern_Concat'Access,
         Documentation => Type_Doc_For_Internal_Entity_Token_Pattern_Concat'Access);
      
      Debug_Name_For_Internal_Entity_Token_Pattern_Lit : aliased constant String :=
        "TokenPatternLit";
      Type_Doc_For_Internal_Entity_Token_Pattern_Lit        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Token_Pattern_Lit       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Token_Pattern_Lit'Access,
         Documentation => Type_Doc_For_Internal_Entity_Token_Pattern_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Token_Ref : aliased constant String :=
        "TokenRef";
      Type_Doc_For_Internal_Entity_Token_Ref        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Token_Ref       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Token_Ref'Access,
         Documentation => Type_Doc_For_Internal_Entity_Token_Ref'Access);
      
      Debug_Name_For_Internal_Entity_Id : aliased constant String :=
        "Id";
      Type_Doc_For_Internal_Entity_Id        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Id       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Id'Access,
         Documentation => Type_Doc_For_Internal_Entity_Id'Access);
      
      Debug_Name_For_Internal_Entity_Def_Id : aliased constant String :=
        "DefId";
      Type_Doc_For_Internal_Entity_Def_Id        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Def_Id       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Def_Id'Access,
         Documentation => Type_Doc_For_Internal_Entity_Def_Id'Access);
      
      Debug_Name_For_Internal_Entity_Imported_Id : aliased constant String :=
        "ImportedId";
      Type_Doc_For_Internal_Entity_Imported_Id        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Imported_Id       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Imported_Id'Access,
         Documentation => Type_Doc_For_Internal_Entity_Imported_Id'Access);
      
      Debug_Name_For_Internal_Entity_Module_Id : aliased constant String :=
        "ModuleId";
      Type_Doc_For_Internal_Entity_Module_Id        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Module_Id       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Module_Id'Access,
         Documentation => Type_Doc_For_Internal_Entity_Module_Id'Access);
      
      Debug_Name_For_Internal_Entity_Ref_Id : aliased constant String :=
        "RefId";
      Type_Doc_For_Internal_Entity_Ref_Id        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Ref_Id       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Ref_Id'Access,
         Documentation => Type_Doc_For_Internal_Entity_Ref_Id'Access);
      
      Debug_Name_For_Internal_Entity_If_Expr : aliased constant String :=
        "IfExpr";
      Type_Doc_For_Internal_Entity_If_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_If_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_If_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_If_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Isa : aliased constant String :=
        "Isa";
      Type_Doc_For_Internal_Entity_Isa        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Isa       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Isa'Access,
         Documentation => Type_Doc_For_Internal_Entity_Isa'Access);
      
      Debug_Name_For_Internal_Entity_Keep_Expr : aliased constant String :=
        "KeepExpr";
      Type_Doc_For_Internal_Entity_Keep_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Keep_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Keep_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Keep_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Lambda_Expr : aliased constant String :=
        "LambdaExpr";
      Type_Doc_For_Internal_Entity_Lambda_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Lambda_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Lambda_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Lambda_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Lit : aliased constant String :=
        "Lit";
      Type_Doc_For_Internal_Entity_Lit        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Lit       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Lit'Access,
         Documentation => Type_Doc_For_Internal_Entity_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Big_Num_Lit : aliased constant String :=
        "BigNumLit";
      Type_Doc_For_Internal_Entity_Big_Num_Lit        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Big_Num_Lit       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Big_Num_Lit'Access,
         Documentation => Type_Doc_For_Internal_Entity_Big_Num_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Char_Lit : aliased constant String :=
        "CharLit";
      Type_Doc_For_Internal_Entity_Char_Lit        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Char_Lit       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Char_Lit'Access,
         Documentation => Type_Doc_For_Internal_Entity_Char_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Null_Lit : aliased constant String :=
        "NullLit";
      Type_Doc_For_Internal_Entity_Null_Lit        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Null_Lit       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Null_Lit'Access,
         Documentation => Type_Doc_For_Internal_Entity_Null_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Num_Lit : aliased constant String :=
        "NumLit";
      Type_Doc_For_Internal_Entity_Num_Lit        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Num_Lit       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Num_Lit'Access,
         Documentation => Type_Doc_For_Internal_Entity_Num_Lit'Access);
      
      Debug_Name_For_Internal_Entity_String_Lit : aliased constant String :=
        "StringLit";
      Type_Doc_For_Internal_Entity_String_Lit        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_String_Lit       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_String_Lit'Access,
         Documentation => Type_Doc_For_Internal_Entity_String_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Block_String_Lit : aliased constant String :=
        "BlockStringLit";
      Type_Doc_For_Internal_Entity_Block_String_Lit        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Block_String_Lit       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Block_String_Lit'Access,
         Documentation => Type_Doc_For_Internal_Entity_Block_String_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Module_Doc_String_Lit : aliased constant String :=
        "ModuleDocStringLit";
      Type_Doc_For_Internal_Entity_Module_Doc_String_Lit        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Module_Doc_String_Lit       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Module_Doc_String_Lit'Access,
         Documentation => Type_Doc_For_Internal_Entity_Module_Doc_String_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Single_Line_String_Lit : aliased constant String :=
        "SingleLineStringLit";
      Type_Doc_For_Internal_Entity_Single_Line_String_Lit        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Single_Line_String_Lit       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Single_Line_String_Lit'Access,
         Documentation => Type_Doc_For_Internal_Entity_Single_Line_String_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Pattern_Single_Line_String_Lit : aliased constant String :=
        "PatternSingleLineStringLit";
      Type_Doc_For_Internal_Entity_Pattern_Single_Line_String_Lit        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Pattern_Single_Line_String_Lit       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Pattern_Single_Line_String_Lit'Access,
         Documentation => Type_Doc_For_Internal_Entity_Pattern_Single_Line_String_Lit'Access);
      
      Debug_Name_For_Internal_Entity_Logic_Assign : aliased constant String :=
        "LogicAssign";
      Type_Doc_For_Internal_Entity_Logic_Assign        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Logic_Assign       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Logic_Assign'Access,
         Documentation => Type_Doc_For_Internal_Entity_Logic_Assign'Access);
      
      Debug_Name_For_Internal_Entity_Logic_Expr : aliased constant String :=
        "LogicExpr";
      Type_Doc_For_Internal_Entity_Logic_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Logic_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Logic_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Logic_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Logic_Propagate : aliased constant String :=
        "LogicPropagate";
      Type_Doc_For_Internal_Entity_Logic_Propagate        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Logic_Propagate       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Logic_Propagate'Access,
         Documentation => Type_Doc_For_Internal_Entity_Logic_Propagate'Access);
      
      Debug_Name_For_Internal_Entity_Logic_Unify : aliased constant String :=
        "LogicUnify";
      Type_Doc_For_Internal_Entity_Logic_Unify        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Logic_Unify       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Logic_Unify'Access,
         Documentation => Type_Doc_For_Internal_Entity_Logic_Unify'Access);
      
      Debug_Name_For_Internal_Entity_Match_Expr : aliased constant String :=
        "MatchExpr";
      Type_Doc_For_Internal_Entity_Match_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Match_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Match_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Match_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Not_Expr : aliased constant String :=
        "NotExpr";
      Type_Doc_For_Internal_Entity_Not_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Not_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Not_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Not_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Paren_Expr : aliased constant String :=
        "ParenExpr";
      Type_Doc_For_Internal_Entity_Paren_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Paren_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Paren_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Paren_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Query : aliased constant String :=
        "Query";
      Type_Doc_For_Internal_Entity_Query        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Query       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Query'Access,
         Documentation => Type_Doc_For_Internal_Entity_Query'Access);
      
      Debug_Name_For_Internal_Entity_Raise_Expr : aliased constant String :=
        "RaiseExpr";
      Type_Doc_For_Internal_Entity_Raise_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Raise_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Raise_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Raise_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Subscript_Expr : aliased constant String :=
        "SubscriptExpr";
      Type_Doc_For_Internal_Entity_Subscript_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Subscript_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Subscript_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Subscript_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Try_Expr : aliased constant String :=
        "TryExpr";
      Type_Doc_For_Internal_Entity_Try_Expr        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Try_Expr       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Try_Expr'Access,
         Documentation => Type_Doc_For_Internal_Entity_Try_Expr'Access);
      
      Debug_Name_For_Internal_Entity_Un_Op : aliased constant String :=
        "UnOp";
      Type_Doc_For_Internal_Entity_Un_Op        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Un_Op       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Un_Op'Access,
         Documentation => Type_Doc_For_Internal_Entity_Un_Op'Access);
      
      Debug_Name_For_Internal_Entity_Full_Decl : aliased constant String :=
        "FullDecl";
      Type_Doc_For_Internal_Entity_Full_Decl        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Full_Decl       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Full_Decl'Access,
         Documentation => Type_Doc_For_Internal_Entity_Full_Decl'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_List_Sep : aliased constant String :=
        "GrammarListSep";
      Type_Doc_For_Internal_Entity_Grammar_List_Sep        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_List_Sep       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_List_Sep'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_List_Sep'Access);
      
      Debug_Name_For_Internal_Entity_Imported_Name : aliased constant String :=
        "ImportedName";
      Type_Doc_For_Internal_Entity_Imported_Name        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Imported_Name       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Imported_Name'Access,
         Documentation => Type_Doc_For_Internal_Entity_Imported_Name'Access);
      
      Debug_Name_For_Internal_Entity_Lexer_Case_Rule : aliased constant String :=
        "LexerCaseRule";
      Type_Doc_For_Internal_Entity_Lexer_Case_Rule        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Lexer_Case_Rule       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Lexer_Case_Rule'Access,
         Documentation => Type_Doc_For_Internal_Entity_Lexer_Case_Rule'Access);
      
      Debug_Name_For_Internal_Entity_Lexer_Case_Rule_Send : aliased constant String :=
        "LexerCaseRuleSend";
      Type_Doc_For_Internal_Entity_Lexer_Case_Rule_Send        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Lexer_Case_Rule_Send       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Lexer_Case_Rule_Send'Access,
         Documentation => Type_Doc_For_Internal_Entity_Lexer_Case_Rule_Send'Access);
      
      Debug_Name_For_Internal_Entity_List_Kind : aliased constant String :=
        "ListKind";
      Type_Doc_For_Internal_Entity_List_Kind        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_List_Kind       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_List_Kind'Access,
         Documentation => Type_Doc_For_Internal_Entity_List_Kind'Access);
      
      Debug_Name_For_Internal_Entity_List_Kind_One : aliased constant String :=
        "ListKind.One";
      Type_Doc_For_Internal_Entity_List_Kind_One        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_List_Kind_One       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_List_Kind_One'Access,
         Documentation => Type_Doc_For_Internal_Entity_List_Kind_One'Access);
      
      Debug_Name_For_Internal_Entity_List_Kind_Zero : aliased constant String :=
        "ListKind.Zero";
      Type_Doc_For_Internal_Entity_List_Kind_Zero        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_List_Kind_Zero       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_List_Kind_Zero'Access,
         Documentation => Type_Doc_For_Internal_Entity_List_Kind_Zero'Access);
      
      Debug_Name_For_Internal_Entity_Lkt_Node_Base_List : aliased constant String :=
        "LktNodeBaseList";
      Type_Doc_For_Internal_Entity_Lkt_Node_Base_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Lkt_Node_Base_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Lkt_Node_Base_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Lkt_Node_Base_List'Access);
      
      Debug_Name_For_Internal_Entity_Argument_List : aliased constant String :=
        "ASTList[Argument]";
      Type_Doc_For_Internal_Entity_Argument_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Argument_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Argument_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Argument_List'Access);
      
      Debug_Name_For_Internal_Entity_Base_Import_List : aliased constant String :=
        "ASTList[BaseImport]";
      Type_Doc_For_Internal_Entity_Base_Import_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Base_Import_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Base_Import_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Base_Import_List'Access);
      
      Debug_Name_For_Internal_Entity_Base_Lexer_Case_Rule_Alt_List : aliased constant String :=
        "ASTList[BaseLexerCaseRuleAlt]";
      Type_Doc_For_Internal_Entity_Base_Lexer_Case_Rule_Alt_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Base_Lexer_Case_Rule_Alt_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Base_Lexer_Case_Rule_Alt_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Base_Lexer_Case_Rule_Alt_List'Access);
      
      Debug_Name_For_Internal_Entity_Base_Match_Branch_List : aliased constant String :=
        "ASTList[BaseMatchBranch]";
      Type_Doc_For_Internal_Entity_Base_Match_Branch_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Base_Match_Branch_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Base_Match_Branch_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Base_Match_Branch_List'Access);
      
      Debug_Name_For_Internal_Entity_Block_String_Line_List : aliased constant String :=
        "ASTList[BlockStringLine]";
      Type_Doc_For_Internal_Entity_Block_String_Line_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Block_String_Line_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Block_String_Line_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Block_String_Line_List'Access);
      
      Debug_Name_For_Internal_Entity_Call_Expr_List : aliased constant String :=
        "ASTList[CallExpr]";
      Type_Doc_For_Internal_Entity_Call_Expr_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Call_Expr_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Call_Expr_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Call_Expr_List'Access);
      
      Debug_Name_For_Internal_Entity_Decl_Annotation_List : aliased constant String :=
        "ASTList[DeclAnnotation]";
      Type_Doc_For_Internal_Entity_Decl_Annotation_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Decl_Annotation_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Decl_Annotation_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Decl_Annotation_List'Access);
      
      Debug_Name_For_Internal_Entity_Elsif_Branch_List : aliased constant String :=
        "ASTList[ElsifBranch]";
      Type_Doc_For_Internal_Entity_Elsif_Branch_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Elsif_Branch_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Elsif_Branch_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Elsif_Branch_List'Access);
      
      Debug_Name_For_Internal_Entity_Enum_Class_Alt_Decl_List : aliased constant String :=
        "ASTList[EnumClassAltDecl]";
      Type_Doc_For_Internal_Entity_Enum_Class_Alt_Decl_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Enum_Class_Alt_Decl_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Enum_Class_Alt_Decl_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Enum_Class_Alt_Decl_List'Access);
      
      Debug_Name_For_Internal_Entity_Enum_Class_Case_List : aliased constant String :=
        "ASTList[EnumClassCase]";
      Type_Doc_For_Internal_Entity_Enum_Class_Case_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Enum_Class_Case_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Enum_Class_Case_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Enum_Class_Case_List'Access);
      
      Debug_Name_For_Internal_Entity_Enum_Lit_Decl_List : aliased constant String :=
        "ASTList[EnumLitDecl]";
      Type_Doc_For_Internal_Entity_Enum_Lit_Decl_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Enum_Lit_Decl_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Enum_Lit_Decl_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Enum_Lit_Decl_List'Access);
      
      Debug_Name_For_Internal_Entity_Expr_List : aliased constant String :=
        "ASTList[Expr]";
      Type_Doc_For_Internal_Entity_Expr_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Expr_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Expr_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Expr_List'Access);
      
      Debug_Name_For_Internal_Entity_Any_Of_List : aliased constant String :=
        "AnyOfList";
      Type_Doc_For_Internal_Entity_Any_Of_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Any_Of_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Any_Of_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Any_Of_List'Access);
      
      Debug_Name_For_Internal_Entity_Full_Decl_List : aliased constant String :=
        "ASTList[FullDecl]";
      Type_Doc_For_Internal_Entity_Full_Decl_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Full_Decl_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Full_Decl_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Full_Decl_List'Access);
      
      Debug_Name_For_Internal_Entity_Decl_Block : aliased constant String :=
        "DeclBlock";
      Type_Doc_For_Internal_Entity_Decl_Block        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Decl_Block       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Decl_Block'Access,
         Documentation => Type_Doc_For_Internal_Entity_Decl_Block'Access);
      
      Debug_Name_For_Internal_Entity_Generic_Param_Decl_List : aliased constant String :=
        "GenericParamDeclList";
      Type_Doc_For_Internal_Entity_Generic_Param_Decl_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Generic_Param_Decl_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Generic_Param_Decl_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Generic_Param_Decl_List'Access);
      
      Debug_Name_For_Internal_Entity_Fun_Param_Decl_List : aliased constant String :=
        "ASTList[FunParamDecl]";
      Type_Doc_For_Internal_Entity_Fun_Param_Decl_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Fun_Param_Decl_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Fun_Param_Decl_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Fun_Param_Decl_List'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Expr_List : aliased constant String :=
        "ASTList[GrammarExpr]";
      Type_Doc_For_Internal_Entity_Grammar_Expr_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Expr_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Expr_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Expr_List'Access);
      
      Debug_Name_For_Internal_Entity_Grammar_Expr_List_List : aliased constant String :=
        "ASTList[ASTList[GrammarExpr]]";
      Type_Doc_For_Internal_Entity_Grammar_Expr_List_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Grammar_Expr_List_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Grammar_Expr_List_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Grammar_Expr_List_List'Access);
      
      Debug_Name_For_Internal_Entity_Imported_Name_List : aliased constant String :=
        "ASTList[ImportedName]";
      Type_Doc_For_Internal_Entity_Imported_Name_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Imported_Name_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Imported_Name_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Imported_Name_List'Access);
      
      Debug_Name_For_Internal_Entity_Lambda_Param_Decl_List : aliased constant String :=
        "ASTList[LambdaParamDecl]";
      Type_Doc_For_Internal_Entity_Lambda_Param_Decl_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Lambda_Param_Decl_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Lambda_Param_Decl_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Lambda_Param_Decl_List'Access);
      
      Debug_Name_For_Internal_Entity_Lkt_Node_List : aliased constant String :=
        "ASTList[LktNode]";
      Type_Doc_For_Internal_Entity_Lkt_Node_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Lkt_Node_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Lkt_Node_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Lkt_Node_List'Access);
      
      Debug_Name_For_Internal_Entity_Module_Doc_String_Line_List : aliased constant String :=
        "ASTList[ModuleDocStringLine]";
      Type_Doc_For_Internal_Entity_Module_Doc_String_Line_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Module_Doc_String_Line_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Module_Doc_String_Line_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Module_Doc_String_Line_List'Access);
      
      Debug_Name_For_Internal_Entity_Pattern_Detail_List : aliased constant String :=
        "ASTList[PatternDetail]";
      Type_Doc_For_Internal_Entity_Pattern_Detail_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Pattern_Detail_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Pattern_Detail_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Pattern_Detail_List'Access);
      
      Debug_Name_For_Internal_Entity_Pattern_List : aliased constant String :=
        "ASTList[Pattern]";
      Type_Doc_For_Internal_Entity_Pattern_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Pattern_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Pattern_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Pattern_List'Access);
      
      Debug_Name_For_Internal_Entity_Ref_Id_List : aliased constant String :=
        "ASTList[RefId]";
      Type_Doc_For_Internal_Entity_Ref_Id_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Ref_Id_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Ref_Id_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Ref_Id_List'Access);
      
      Debug_Name_For_Internal_Entity_Type_Ref_List : aliased constant String :=
        "ASTList[TypeRef]";
      Type_Doc_For_Internal_Entity_Type_Ref_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Type_Ref_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Type_Ref_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Type_Ref_List'Access);
      
      Debug_Name_For_Internal_Entity_Synthetic_Type_Ref_List : aliased constant String :=
        "SyntheticTypeRefList";
      Type_Doc_For_Internal_Entity_Synthetic_Type_Ref_List        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Synthetic_Type_Ref_List       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Synthetic_Type_Ref_List'Access,
         Documentation => Type_Doc_For_Internal_Entity_Synthetic_Type_Ref_List'Access);
      
      Debug_Name_For_Internal_Entity_Module_Doc_String_Line : aliased constant String :=
        "ModuleDocStringLine";
      Type_Doc_For_Internal_Entity_Module_Doc_String_Line        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Module_Doc_String_Line       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Module_Doc_String_Line'Access,
         Documentation => Type_Doc_For_Internal_Entity_Module_Doc_String_Line'Access);
      
      Debug_Name_For_Internal_Entity_Null_Cond_Qualifier : aliased constant String :=
        "NullCondQualifier";
      Type_Doc_For_Internal_Entity_Null_Cond_Qualifier        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Null_Cond_Qualifier       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Null_Cond_Qualifier'Access,
         Documentation => Type_Doc_For_Internal_Entity_Null_Cond_Qualifier'Access);
      
      Debug_Name_For_Internal_Entity_Null_Cond_Qualifier_Absent : aliased constant String :=
        "NullCondQualifier.Absent";
      Type_Doc_For_Internal_Entity_Null_Cond_Qualifier_Absent        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Null_Cond_Qualifier_Absent       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Null_Cond_Qualifier_Absent'Access,
         Documentation => Type_Doc_For_Internal_Entity_Null_Cond_Qualifier_Absent'Access);
      
      Debug_Name_For_Internal_Entity_Null_Cond_Qualifier_Present : aliased constant String :=
        "NullCondQualifier.Present";
      Type_Doc_For_Internal_Entity_Null_Cond_Qualifier_Present        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Null_Cond_Qualifier_Present       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Null_Cond_Qualifier_Present'Access,
         Documentation => Type_Doc_For_Internal_Entity_Null_Cond_Qualifier_Present'Access);
      
      Debug_Name_For_Internal_Entity_Op : aliased constant String :=
        "Op";
      Type_Doc_For_Internal_Entity_Op        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op'Access);
      
      Debug_Name_For_Internal_Entity_Op_Amp : aliased constant String :=
        "Op.Amp";
      Type_Doc_For_Internal_Entity_Op_Amp        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Amp       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Amp'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Amp'Access);
      
      Debug_Name_For_Internal_Entity_Op_And : aliased constant String :=
        "Op.And";
      Type_Doc_For_Internal_Entity_Op_And        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_And       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_And'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_And'Access);
      
      Debug_Name_For_Internal_Entity_Op_Div : aliased constant String :=
        "Op.Div";
      Type_Doc_For_Internal_Entity_Op_Div        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Div       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Div'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Div'Access);
      
      Debug_Name_For_Internal_Entity_Op_Eq : aliased constant String :=
        "Op.Eq";
      Type_Doc_For_Internal_Entity_Op_Eq        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Eq       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Eq'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Eq'Access);
      
      Debug_Name_For_Internal_Entity_Op_Gt : aliased constant String :=
        "Op.Gt";
      Type_Doc_For_Internal_Entity_Op_Gt        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Gt       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Gt'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Gt'Access);
      
      Debug_Name_For_Internal_Entity_Op_Gte : aliased constant String :=
        "Op.Gte";
      Type_Doc_For_Internal_Entity_Op_Gte        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Gte       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Gte'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Gte'Access);
      
      Debug_Name_For_Internal_Entity_Op_Logic_And : aliased constant String :=
        "Op.LogicAnd";
      Type_Doc_For_Internal_Entity_Op_Logic_And        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Logic_And       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Logic_And'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Logic_And'Access);
      
      Debug_Name_For_Internal_Entity_Op_Logic_Or : aliased constant String :=
        "Op.LogicOr";
      Type_Doc_For_Internal_Entity_Op_Logic_Or        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Logic_Or       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Logic_Or'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Logic_Or'Access);
      
      Debug_Name_For_Internal_Entity_Op_Lt : aliased constant String :=
        "Op.Lt";
      Type_Doc_For_Internal_Entity_Op_Lt        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Lt       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Lt'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Lt'Access);
      
      Debug_Name_For_Internal_Entity_Op_Lte : aliased constant String :=
        "Op.Lte";
      Type_Doc_For_Internal_Entity_Op_Lte        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Lte       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Lte'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Lte'Access);
      
      Debug_Name_For_Internal_Entity_Op_Minus : aliased constant String :=
        "Op.Minus";
      Type_Doc_For_Internal_Entity_Op_Minus        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Minus       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Minus'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Minus'Access);
      
      Debug_Name_For_Internal_Entity_Op_Mult : aliased constant String :=
        "Op.Mult";
      Type_Doc_For_Internal_Entity_Op_Mult        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Mult       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Mult'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Mult'Access);
      
      Debug_Name_For_Internal_Entity_Op_Ne : aliased constant String :=
        "Op.Ne";
      Type_Doc_For_Internal_Entity_Op_Ne        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Ne       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Ne'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Ne'Access);
      
      Debug_Name_For_Internal_Entity_Op_Or : aliased constant String :=
        "Op.Or";
      Type_Doc_For_Internal_Entity_Op_Or        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Or       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Or'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Or'Access);
      
      Debug_Name_For_Internal_Entity_Op_Or_Int : aliased constant String :=
        "Op.OrInt";
      Type_Doc_For_Internal_Entity_Op_Or_Int        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Or_Int       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Or_Int'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Or_Int'Access);
      
      Debug_Name_For_Internal_Entity_Op_Plus : aliased constant String :=
        "Op.Plus";
      Type_Doc_For_Internal_Entity_Op_Plus        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Plus       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Plus'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Plus'Access);
      
      Debug_Name_For_Internal_Entity_Op_Stream_Concat : aliased constant String :=
        "Op.StreamConcat";
      Type_Doc_For_Internal_Entity_Op_Stream_Concat        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Stream_Concat       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Stream_Concat'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Stream_Concat'Access);
      
      Debug_Name_For_Internal_Entity_Op_Stream_Cons : aliased constant String :=
        "Op.StreamCons";
      Type_Doc_For_Internal_Entity_Op_Stream_Cons        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Op_Stream_Cons       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Op_Stream_Cons'Access,
         Documentation => Type_Doc_For_Internal_Entity_Op_Stream_Cons'Access);
      
      Debug_Name_For_Internal_Entity_Pattern : aliased constant String :=
        "Pattern";
      Type_Doc_For_Internal_Entity_Pattern        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Pattern       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Pattern'Access,
         Documentation => Type_Doc_For_Internal_Entity_Pattern'Access);
      
      Debug_Name_For_Internal_Entity_Any_Type_Pattern : aliased constant String :=
        "AnyTypePattern";
      Type_Doc_For_Internal_Entity_Any_Type_Pattern        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Any_Type_Pattern       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Any_Type_Pattern'Access,
         Documentation => Type_Doc_For_Internal_Entity_Any_Type_Pattern'Access);
      
      Debug_Name_For_Internal_Entity_Bool_Pattern : aliased constant String :=
        "BoolPattern";
      Type_Doc_For_Internal_Entity_Bool_Pattern        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Bool_Pattern       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Bool_Pattern'Access,
         Documentation => Type_Doc_For_Internal_Entity_Bool_Pattern'Access);
      
      Debug_Name_For_Internal_Entity_Bool_Pattern_False : aliased constant String :=
        "BoolPattern.False";
      Type_Doc_For_Internal_Entity_Bool_Pattern_False        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Bool_Pattern_False       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Bool_Pattern_False'Access,
         Documentation => Type_Doc_For_Internal_Entity_Bool_Pattern_False'Access);
      
      Debug_Name_For_Internal_Entity_Bool_Pattern_True : aliased constant String :=
        "BoolPattern.True";
      Type_Doc_For_Internal_Entity_Bool_Pattern_True        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Bool_Pattern_True       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Bool_Pattern_True'Access,
         Documentation => Type_Doc_For_Internal_Entity_Bool_Pattern_True'Access);
      
      Debug_Name_For_Internal_Entity_Complex_Pattern : aliased constant String :=
        "ComplexPattern";
      Type_Doc_For_Internal_Entity_Complex_Pattern        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Complex_Pattern       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Complex_Pattern'Access,
         Documentation => Type_Doc_For_Internal_Entity_Complex_Pattern'Access);
      
      Debug_Name_For_Internal_Entity_Renaming_Complex_Pattern : aliased constant String :=
        "RenamingComplexPattern";
      Type_Doc_For_Internal_Entity_Renaming_Complex_Pattern        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Renaming_Complex_Pattern       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Renaming_Complex_Pattern'Access,
         Documentation => Type_Doc_For_Internal_Entity_Renaming_Complex_Pattern'Access);
      
      Debug_Name_For_Internal_Entity_Ellipsis_Pattern : aliased constant String :=
        "EllipsisPattern";
      Type_Doc_For_Internal_Entity_Ellipsis_Pattern        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Ellipsis_Pattern       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Ellipsis_Pattern'Access,
         Documentation => Type_Doc_For_Internal_Entity_Ellipsis_Pattern'Access);
      
      Debug_Name_For_Internal_Entity_Integer_Pattern : aliased constant String :=
        "IntegerPattern";
      Type_Doc_For_Internal_Entity_Integer_Pattern        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Integer_Pattern       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Integer_Pattern'Access,
         Documentation => Type_Doc_For_Internal_Entity_Integer_Pattern'Access);
      
      Debug_Name_For_Internal_Entity_List_Pattern : aliased constant String :=
        "ListPattern";
      Type_Doc_For_Internal_Entity_List_Pattern        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_List_Pattern       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_List_Pattern'Access,
         Documentation => Type_Doc_For_Internal_Entity_List_Pattern'Access);
      
      Debug_Name_For_Internal_Entity_Not_Pattern : aliased constant String :=
        "NotPattern";
      Type_Doc_For_Internal_Entity_Not_Pattern        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Not_Pattern       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Not_Pattern'Access,
         Documentation => Type_Doc_For_Internal_Entity_Not_Pattern'Access);
      
      Debug_Name_For_Internal_Entity_Null_Pattern : aliased constant String :=
        "NullPattern";
      Type_Doc_For_Internal_Entity_Null_Pattern        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Null_Pattern       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Null_Pattern'Access,
         Documentation => Type_Doc_For_Internal_Entity_Null_Pattern'Access);
      
      Debug_Name_For_Internal_Entity_Or_Pattern : aliased constant String :=
        "OrPattern";
      Type_Doc_For_Internal_Entity_Or_Pattern        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Or_Pattern       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Or_Pattern'Access,
         Documentation => Type_Doc_For_Internal_Entity_Or_Pattern'Access);
      
      Debug_Name_For_Internal_Entity_Paren_Pattern : aliased constant String :=
        "ParenPattern";
      Type_Doc_For_Internal_Entity_Paren_Pattern        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Paren_Pattern       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Paren_Pattern'Access,
         Documentation => Type_Doc_For_Internal_Entity_Paren_Pattern'Access);
      
      Debug_Name_For_Internal_Entity_Regex_Pattern : aliased constant String :=
        "RegexPattern";
      Type_Doc_For_Internal_Entity_Regex_Pattern        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Regex_Pattern       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Regex_Pattern'Access,
         Documentation => Type_Doc_For_Internal_Entity_Regex_Pattern'Access);
      
      Debug_Name_For_Internal_Entity_Type_Pattern : aliased constant String :=
        "TypePattern";
      Type_Doc_For_Internal_Entity_Type_Pattern        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Type_Pattern       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Type_Pattern'Access,
         Documentation => Type_Doc_For_Internal_Entity_Type_Pattern'Access);
      
      Debug_Name_For_Internal_Entity_Pattern_Detail : aliased constant String :=
        "PatternDetail";
      Type_Doc_For_Internal_Entity_Pattern_Detail        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Pattern_Detail       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Pattern_Detail'Access,
         Documentation => Type_Doc_For_Internal_Entity_Pattern_Detail'Access);
      
      Debug_Name_For_Internal_Entity_Field_Pattern_Detail : aliased constant String :=
        "FieldPatternDetail";
      Type_Doc_For_Internal_Entity_Field_Pattern_Detail        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Field_Pattern_Detail       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Field_Pattern_Detail'Access,
         Documentation => Type_Doc_For_Internal_Entity_Field_Pattern_Detail'Access);
      
      Debug_Name_For_Internal_Entity_Property_Pattern_Detail : aliased constant String :=
        "PropertyPatternDetail";
      Type_Doc_For_Internal_Entity_Property_Pattern_Detail        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Property_Pattern_Detail       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Property_Pattern_Detail'Access,
         Documentation => Type_Doc_For_Internal_Entity_Property_Pattern_Detail'Access);
      
      Debug_Name_For_Internal_Entity_Type_Ref : aliased constant String :=
        "TypeRef";
      Type_Doc_For_Internal_Entity_Type_Ref        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Type_Ref       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Type_Ref'Access,
         Documentation => Type_Doc_For_Internal_Entity_Type_Ref'Access);
      
      Debug_Name_For_Internal_Entity_Default_List_Type_Ref : aliased constant String :=
        "DefaultListTypeRef";
      Type_Doc_For_Internal_Entity_Default_List_Type_Ref        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Default_List_Type_Ref       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Default_List_Type_Ref'Access,
         Documentation => Type_Doc_For_Internal_Entity_Default_List_Type_Ref'Access);
      
      Debug_Name_For_Internal_Entity_Function_Type_Ref : aliased constant String :=
        "FunctionTypeRef";
      Type_Doc_For_Internal_Entity_Function_Type_Ref        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Function_Type_Ref       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Function_Type_Ref'Access,
         Documentation => Type_Doc_For_Internal_Entity_Function_Type_Ref'Access);
      
      Debug_Name_For_Internal_Entity_Generic_Type_Ref : aliased constant String :=
        "GenericTypeRef";
      Type_Doc_For_Internal_Entity_Generic_Type_Ref        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Generic_Type_Ref       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Generic_Type_Ref'Access,
         Documentation => Type_Doc_For_Internal_Entity_Generic_Type_Ref'Access);
      
      Debug_Name_For_Internal_Entity_Simple_Type_Ref : aliased constant String :=
        "SimpleTypeRef";
      Type_Doc_For_Internal_Entity_Simple_Type_Ref        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Simple_Type_Ref       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Simple_Type_Ref'Access,
         Documentation => Type_Doc_For_Internal_Entity_Simple_Type_Ref'Access);
      
      Debug_Name_For_Internal_Entity_Var_Bind : aliased constant String :=
        "VarBind";
      Type_Doc_For_Internal_Entity_Var_Bind        : aliased constant Text_Type := "";
      Desc_For_Internal_Entity_Var_Bind       : aliased constant Type_Descriptor :=
        (Category      => Struct_Category,
         Debug_Name    => Debug_Name_For_Internal_Entity_Var_Bind'Access,
         Documentation => Type_Doc_For_Internal_Entity_Var_Bind'Access);

   Types : aliased constant Type_Descriptor_Array := (
      Desc_For_Internal_Unit'Access,
Desc_For_Big_Integer_Type'Access,
Desc_For_Boolean'Access,
Desc_For_Character_Type'Access,
Desc_For_Integer'Access,
Desc_For_Source_Location'Access,
Desc_For_Source_Location_Range'Access,
Desc_For_String_Type'Access,
Desc_For_Token_Reference'Access,
Desc_For_Symbol_Type'Access,
Desc_For_Analysis_Unit_Kind'Access,
Desc_For_Completion_Item_Kind'Access,
Desc_For_Designated_Env_Kind'Access,
Desc_For_Grammar_Rule'Access,
Desc_For_Lookup_Kind'Access,
Desc_For_Internal_Complete_Item_Array_Access'Access,
Desc_For_Internal_Entity_Array_Access'Access,
Desc_For_Internal_Entity_Def_Id_Array_Access'Access,
Desc_For_Internal_Entity_Fun_Decl_Array_Access'Access,
Desc_For_Internal_Logic_Context_Array_Access'Access,
Desc_For_Internal_Ref_Result_Array_Access'Access,
Desc_For_Internal_Solver_Diagnostic_Array_Access'Access,
Desc_For_Internal_Unit_Array_Access'Access,
Desc_For_Internal_Complete_Item'Access,
Desc_For_Internal_Decoded_Char_Value'Access,
Desc_For_Internal_Decoded_String_Value'Access,
Desc_For_Internal_Logic_Context'Access,
Desc_For_Internal_Ref_Result'Access,
Desc_For_Internal_Solver_Diagnostic'Access,
Desc_For_Internal_Solver_Result'Access,
Desc_For_Internal_Entity'Access,
Desc_For_Internal_Entity_Argument'Access,
Desc_For_Internal_Entity_Base_Import'Access,
Desc_For_Internal_Entity_Import'Access,
Desc_For_Internal_Entity_Import_All_From'Access,
Desc_For_Internal_Entity_Import_From'Access,
Desc_For_Internal_Entity_Base_Lexer_Case_Rule_Alt'Access,
Desc_For_Internal_Entity_Error_Lexer_Case_Rule_Alt'Access,
Desc_For_Internal_Entity_Lexer_Case_Rule_Cond_Alt'Access,
Desc_For_Internal_Entity_Lexer_Case_Rule_Default_Alt'Access,
Desc_For_Internal_Entity_Base_Match_Branch'Access,
Desc_For_Internal_Entity_Match_Branch'Access,
Desc_For_Internal_Entity_Pattern_Match_Branch'Access,
Desc_For_Internal_Entity_Block_Expr_Clause'Access,
Desc_For_Internal_Entity_Block_String_Line'Access,
Desc_For_Internal_Entity_Class_Qualifier'Access,
Desc_For_Internal_Entity_Class_Qualifier_Absent'Access,
Desc_For_Internal_Entity_Class_Qualifier_Present'Access,
Desc_For_Internal_Entity_Decl'Access,
Desc_For_Internal_Entity_Base_Grammar_Rule_Decl'Access,
Desc_For_Internal_Entity_Grammar_Rule_Decl'Access,
Desc_For_Internal_Entity_Synthetic_Lexer_Decl'Access,
Desc_For_Internal_Entity_Base_Val_Decl'Access,
Desc_For_Internal_Entity_Node_Decl'Access,
Desc_For_Internal_Entity_Self_Decl'Access,
Desc_For_Internal_Entity_User_Val_Decl'Access,
Desc_For_Internal_Entity_Binding_Val_Decl'Access,
Desc_For_Internal_Entity_Enum_Lit_Decl'Access,
Desc_For_Internal_Entity_Explicitly_Typed_Decl'Access,
Desc_For_Internal_Entity_Component_Decl'Access,
Desc_For_Internal_Entity_Field_Decl'Access,
Desc_For_Internal_Entity_Fun_Param_Decl'Access,
Desc_For_Internal_Entity_Lambda_Param_Decl'Access,
Desc_For_Internal_Entity_Dyn_Var_Decl'Access,
Desc_For_Internal_Entity_Match_Val_Decl'Access,
Desc_For_Internal_Entity_Val_Decl'Access,
Desc_For_Internal_Entity_Fun_Decl'Access,
Desc_For_Internal_Entity_Env_Spec_Decl'Access,
Desc_For_Internal_Entity_Error_Decl'Access,
Desc_For_Internal_Entity_Generic_Decl'Access,
Desc_For_Internal_Entity_Grammar_Decl'Access,
Desc_For_Internal_Entity_Langkit_Root'Access,
Desc_For_Internal_Entity_Lexer_Decl'Access,
Desc_For_Internal_Entity_Lexer_Family_Decl'Access,
Desc_For_Internal_Entity_Synth_Fun_Decl'Access,
Desc_For_Internal_Entity_Synth_Param_Decl'Access,
Desc_For_Internal_Entity_Type_Decl'Access,
Desc_For_Internal_Entity_Any_Type_Decl'Access,
Desc_For_Internal_Entity_Enum_Class_Alt_Decl'Access,
Desc_For_Internal_Entity_Function_Type'Access,
Desc_For_Internal_Entity_Generic_Param_Type_Decl'Access,
Desc_For_Internal_Entity_Named_Type_Decl'Access,
Desc_For_Internal_Entity_Basic_Class_Decl'Access,
Desc_For_Internal_Entity_Class_Decl'Access,
Desc_For_Internal_Entity_Enum_Class_Decl'Access,
Desc_For_Internal_Entity_Enum_Type_Decl'Access,
Desc_For_Internal_Entity_Struct_Decl'Access,
Desc_For_Internal_Entity_Trait_Decl'Access,
Desc_For_Internal_Entity_Decl_Annotation'Access,
Desc_For_Internal_Entity_Decl_Annotation_Args'Access,
Desc_For_Internal_Entity_Dyn_Env_Wrapper'Access,
Desc_For_Internal_Entity_Elsif_Branch'Access,
Desc_For_Internal_Entity_Enum_Class_Case'Access,
Desc_For_Internal_Entity_Excludes_Null'Access,
Desc_For_Internal_Entity_Excludes_Null_Absent'Access,
Desc_For_Internal_Entity_Excludes_Null_Present'Access,
Desc_For_Internal_Entity_Expr'Access,
Desc_For_Internal_Entity_Any_Of'Access,
Desc_For_Internal_Entity_Array_Literal'Access,
Desc_For_Internal_Entity_Base_Call_Expr'Access,
Desc_For_Internal_Entity_Call_Expr'Access,
Desc_For_Internal_Entity_Logic_Call_Expr'Access,
Desc_For_Internal_Entity_Logic_Predicate'Access,
Desc_For_Internal_Entity_Logic_Propagate_Call'Access,
Desc_For_Internal_Entity_Bin_Op'Access,
Desc_For_Internal_Entity_Block_Expr'Access,
Desc_For_Internal_Entity_Cast_Expr'Access,
Desc_For_Internal_Entity_Dot_Expr'Access,
Desc_For_Internal_Entity_Error_On_Null'Access,
Desc_For_Internal_Entity_Generic_Instantiation'Access,
Desc_For_Internal_Entity_Grammar_Expr'Access,
Desc_For_Internal_Entity_Error_Grammar_Expr'Access,
Desc_For_Internal_Entity_Grammar_Cut'Access,
Desc_For_Internal_Entity_Grammar_Discard'Access,
Desc_For_Internal_Entity_Grammar_Dont_Skip'Access,
Desc_For_Internal_Entity_Grammar_List'Access,
Desc_For_Internal_Entity_Grammar_Null'Access,
Desc_For_Internal_Entity_Grammar_Opt'Access,
Desc_For_Internal_Entity_Grammar_Opt_Error'Access,
Desc_For_Internal_Entity_Grammar_Opt_Error_Group'Access,
Desc_For_Internal_Entity_Grammar_Opt_Group'Access,
Desc_For_Internal_Entity_Grammar_Or_Expr'Access,
Desc_For_Internal_Entity_Grammar_Pick'Access,
Desc_For_Internal_Entity_Grammar_Implicit_Pick'Access,
Desc_For_Internal_Entity_Grammar_Predicate'Access,
Desc_For_Internal_Entity_Grammar_Rule_Ref'Access,
Desc_For_Internal_Entity_Grammar_Skip'Access,
Desc_For_Internal_Entity_Grammar_Stop_Cut'Access,
Desc_For_Internal_Entity_Parse_Node_Expr'Access,
Desc_For_Internal_Entity_Token_Lit'Access,
Desc_For_Internal_Entity_Token_No_Case_Lit'Access,
Desc_For_Internal_Entity_Token_Pattern_Concat'Access,
Desc_For_Internal_Entity_Token_Pattern_Lit'Access,
Desc_For_Internal_Entity_Token_Ref'Access,
Desc_For_Internal_Entity_Id'Access,
Desc_For_Internal_Entity_Def_Id'Access,
Desc_For_Internal_Entity_Imported_Id'Access,
Desc_For_Internal_Entity_Module_Id'Access,
Desc_For_Internal_Entity_Ref_Id'Access,
Desc_For_Internal_Entity_If_Expr'Access,
Desc_For_Internal_Entity_Isa'Access,
Desc_For_Internal_Entity_Keep_Expr'Access,
Desc_For_Internal_Entity_Lambda_Expr'Access,
Desc_For_Internal_Entity_Lit'Access,
Desc_For_Internal_Entity_Big_Num_Lit'Access,
Desc_For_Internal_Entity_Char_Lit'Access,
Desc_For_Internal_Entity_Null_Lit'Access,
Desc_For_Internal_Entity_Num_Lit'Access,
Desc_For_Internal_Entity_String_Lit'Access,
Desc_For_Internal_Entity_Block_String_Lit'Access,
Desc_For_Internal_Entity_Module_Doc_String_Lit'Access,
Desc_For_Internal_Entity_Single_Line_String_Lit'Access,
Desc_For_Internal_Entity_Pattern_Single_Line_String_Lit'Access,
Desc_For_Internal_Entity_Logic_Assign'Access,
Desc_For_Internal_Entity_Logic_Expr'Access,
Desc_For_Internal_Entity_Logic_Propagate'Access,
Desc_For_Internal_Entity_Logic_Unify'Access,
Desc_For_Internal_Entity_Match_Expr'Access,
Desc_For_Internal_Entity_Not_Expr'Access,
Desc_For_Internal_Entity_Paren_Expr'Access,
Desc_For_Internal_Entity_Query'Access,
Desc_For_Internal_Entity_Raise_Expr'Access,
Desc_For_Internal_Entity_Subscript_Expr'Access,
Desc_For_Internal_Entity_Try_Expr'Access,
Desc_For_Internal_Entity_Un_Op'Access,
Desc_For_Internal_Entity_Full_Decl'Access,
Desc_For_Internal_Entity_Grammar_List_Sep'Access,
Desc_For_Internal_Entity_Imported_Name'Access,
Desc_For_Internal_Entity_Lexer_Case_Rule'Access,
Desc_For_Internal_Entity_Lexer_Case_Rule_Send'Access,
Desc_For_Internal_Entity_List_Kind'Access,
Desc_For_Internal_Entity_List_Kind_One'Access,
Desc_For_Internal_Entity_List_Kind_Zero'Access,
Desc_For_Internal_Entity_Lkt_Node_Base_List'Access,
Desc_For_Internal_Entity_Argument_List'Access,
Desc_For_Internal_Entity_Base_Import_List'Access,
Desc_For_Internal_Entity_Base_Lexer_Case_Rule_Alt_List'Access,
Desc_For_Internal_Entity_Base_Match_Branch_List'Access,
Desc_For_Internal_Entity_Block_String_Line_List'Access,
Desc_For_Internal_Entity_Call_Expr_List'Access,
Desc_For_Internal_Entity_Decl_Annotation_List'Access,
Desc_For_Internal_Entity_Elsif_Branch_List'Access,
Desc_For_Internal_Entity_Enum_Class_Alt_Decl_List'Access,
Desc_For_Internal_Entity_Enum_Class_Case_List'Access,
Desc_For_Internal_Entity_Enum_Lit_Decl_List'Access,
Desc_For_Internal_Entity_Expr_List'Access,
Desc_For_Internal_Entity_Any_Of_List'Access,
Desc_For_Internal_Entity_Full_Decl_List'Access,
Desc_For_Internal_Entity_Decl_Block'Access,
Desc_For_Internal_Entity_Generic_Param_Decl_List'Access,
Desc_For_Internal_Entity_Fun_Param_Decl_List'Access,
Desc_For_Internal_Entity_Grammar_Expr_List'Access,
Desc_For_Internal_Entity_Grammar_Expr_List_List'Access,
Desc_For_Internal_Entity_Imported_Name_List'Access,
Desc_For_Internal_Entity_Lambda_Param_Decl_List'Access,
Desc_For_Internal_Entity_Lkt_Node_List'Access,
Desc_For_Internal_Entity_Module_Doc_String_Line_List'Access,
Desc_For_Internal_Entity_Pattern_Detail_List'Access,
Desc_For_Internal_Entity_Pattern_List'Access,
Desc_For_Internal_Entity_Ref_Id_List'Access,
Desc_For_Internal_Entity_Type_Ref_List'Access,
Desc_For_Internal_Entity_Synthetic_Type_Ref_List'Access,
Desc_For_Internal_Entity_Module_Doc_String_Line'Access,
Desc_For_Internal_Entity_Null_Cond_Qualifier'Access,
Desc_For_Internal_Entity_Null_Cond_Qualifier_Absent'Access,
Desc_For_Internal_Entity_Null_Cond_Qualifier_Present'Access,
Desc_For_Internal_Entity_Op'Access,
Desc_For_Internal_Entity_Op_Amp'Access,
Desc_For_Internal_Entity_Op_And'Access,
Desc_For_Internal_Entity_Op_Div'Access,
Desc_For_Internal_Entity_Op_Eq'Access,
Desc_For_Internal_Entity_Op_Gt'Access,
Desc_For_Internal_Entity_Op_Gte'Access,
Desc_For_Internal_Entity_Op_Logic_And'Access,
Desc_For_Internal_Entity_Op_Logic_Or'Access,
Desc_For_Internal_Entity_Op_Lt'Access,
Desc_For_Internal_Entity_Op_Lte'Access,
Desc_For_Internal_Entity_Op_Minus'Access,
Desc_For_Internal_Entity_Op_Mult'Access,
Desc_For_Internal_Entity_Op_Ne'Access,
Desc_For_Internal_Entity_Op_Or'Access,
Desc_For_Internal_Entity_Op_Or_Int'Access,
Desc_For_Internal_Entity_Op_Plus'Access,
Desc_For_Internal_Entity_Op_Stream_Concat'Access,
Desc_For_Internal_Entity_Op_Stream_Cons'Access,
Desc_For_Internal_Entity_Pattern'Access,
Desc_For_Internal_Entity_Any_Type_Pattern'Access,
Desc_For_Internal_Entity_Bool_Pattern'Access,
Desc_For_Internal_Entity_Bool_Pattern_False'Access,
Desc_For_Internal_Entity_Bool_Pattern_True'Access,
Desc_For_Internal_Entity_Complex_Pattern'Access,
Desc_For_Internal_Entity_Renaming_Complex_Pattern'Access,
Desc_For_Internal_Entity_Ellipsis_Pattern'Access,
Desc_For_Internal_Entity_Integer_Pattern'Access,
Desc_For_Internal_Entity_List_Pattern'Access,
Desc_For_Internal_Entity_Not_Pattern'Access,
Desc_For_Internal_Entity_Null_Pattern'Access,
Desc_For_Internal_Entity_Or_Pattern'Access,
Desc_For_Internal_Entity_Paren_Pattern'Access,
Desc_For_Internal_Entity_Regex_Pattern'Access,
Desc_For_Internal_Entity_Type_Pattern'Access,
Desc_For_Internal_Entity_Pattern_Detail'Access,
Desc_For_Internal_Entity_Field_Pattern_Detail'Access,
Desc_For_Internal_Entity_Property_Pattern_Detail'Access,
Desc_For_Internal_Entity_Type_Ref'Access,
Desc_For_Internal_Entity_Default_List_Type_Ref'Access,
Desc_For_Internal_Entity_Function_Type_Ref'Access,
Desc_For_Internal_Entity_Generic_Type_Ref'Access,
Desc_For_Internal_Entity_Simple_Type_Ref'Access,
Desc_For_Internal_Entity_Var_Bind'Access
   );

   ---------------------------
   -- Enum type descriptors --
   ---------------------------

   
      

         Enum_Name_For_Analysis_Unit_Kind_1 : aliased constant Text_Type :=
           "Unit_Specification";
         Enum_Name_For_Analysis_Unit_Kind_2 : aliased constant Text_Type :=
           "Unit_Body";

      Enum_Name_For_Analysis_Unit_Kind : aliased constant Text_Type :=
        "Analysis_Unit_Kind";
      Enum_Desc_For_Analysis_Unit_Kind : aliased constant Enum_Type_Descriptor := (
         Last_Value    => 2,
         Name          => Enum_Name_For_Analysis_Unit_Kind'Access,
         Default_Value => 0,
         Value_Names   => (
            1 => Enum_Name_For_Analysis_Unit_Kind_1'Access,
2 => Enum_Name_For_Analysis_Unit_Kind_2'Access
         )
      );
      

         Enum_Name_For_Completion_Item_Kind_1 : aliased constant Text_Type :=
           "Text_Kind";
         Enum_Name_For_Completion_Item_Kind_2 : aliased constant Text_Type :=
           "Method_Kind";
         Enum_Name_For_Completion_Item_Kind_3 : aliased constant Text_Type :=
           "Function_Kind";
         Enum_Name_For_Completion_Item_Kind_4 : aliased constant Text_Type :=
           "Constructor_Kind";
         Enum_Name_For_Completion_Item_Kind_5 : aliased constant Text_Type :=
           "Field_Kind";
         Enum_Name_For_Completion_Item_Kind_6 : aliased constant Text_Type :=
           "Variable_Kind";
         Enum_Name_For_Completion_Item_Kind_7 : aliased constant Text_Type :=
           "Class_Kind";
         Enum_Name_For_Completion_Item_Kind_8 : aliased constant Text_Type :=
           "Interface_Kind";
         Enum_Name_For_Completion_Item_Kind_9 : aliased constant Text_Type :=
           "Module_Kind";
         Enum_Name_For_Completion_Item_Kind_10 : aliased constant Text_Type :=
           "Property_Kind";
         Enum_Name_For_Completion_Item_Kind_11 : aliased constant Text_Type :=
           "Unit_Kind";
         Enum_Name_For_Completion_Item_Kind_12 : aliased constant Text_Type :=
           "Value_Kind";
         Enum_Name_For_Completion_Item_Kind_13 : aliased constant Text_Type :=
           "Enum_Kind";
         Enum_Name_For_Completion_Item_Kind_14 : aliased constant Text_Type :=
           "Keyword_Kind";
         Enum_Name_For_Completion_Item_Kind_15 : aliased constant Text_Type :=
           "Snippet_Kind";
         Enum_Name_For_Completion_Item_Kind_16 : aliased constant Text_Type :=
           "Color_Kind";
         Enum_Name_For_Completion_Item_Kind_17 : aliased constant Text_Type :=
           "File_Kind";
         Enum_Name_For_Completion_Item_Kind_18 : aliased constant Text_Type :=
           "Reference_Kind";
         Enum_Name_For_Completion_Item_Kind_19 : aliased constant Text_Type :=
           "Folder_Kind";
         Enum_Name_For_Completion_Item_Kind_20 : aliased constant Text_Type :=
           "Enum_Member_Kind";
         Enum_Name_For_Completion_Item_Kind_21 : aliased constant Text_Type :=
           "Constant_Kind";
         Enum_Name_For_Completion_Item_Kind_22 : aliased constant Text_Type :=
           "Struct_Kind";
         Enum_Name_For_Completion_Item_Kind_23 : aliased constant Text_Type :=
           "Event_Kind";
         Enum_Name_For_Completion_Item_Kind_24 : aliased constant Text_Type :=
           "Operator_Kind";
         Enum_Name_For_Completion_Item_Kind_25 : aliased constant Text_Type :=
           "Type_Parameter_Kind";

      Enum_Name_For_Completion_Item_Kind : aliased constant Text_Type :=
        "Completion_Item_Kind";
      Enum_Desc_For_Completion_Item_Kind : aliased constant Enum_Type_Descriptor := (
         Last_Value    => 25,
         Name          => Enum_Name_For_Completion_Item_Kind'Access,
         Default_Value => 0,
         Value_Names   => (
            1 => Enum_Name_For_Completion_Item_Kind_1'Access,
2 => Enum_Name_For_Completion_Item_Kind_2'Access,
3 => Enum_Name_For_Completion_Item_Kind_3'Access,
4 => Enum_Name_For_Completion_Item_Kind_4'Access,
5 => Enum_Name_For_Completion_Item_Kind_5'Access,
6 => Enum_Name_For_Completion_Item_Kind_6'Access,
7 => Enum_Name_For_Completion_Item_Kind_7'Access,
8 => Enum_Name_For_Completion_Item_Kind_8'Access,
9 => Enum_Name_For_Completion_Item_Kind_9'Access,
10 => Enum_Name_For_Completion_Item_Kind_10'Access,
11 => Enum_Name_For_Completion_Item_Kind_11'Access,
12 => Enum_Name_For_Completion_Item_Kind_12'Access,
13 => Enum_Name_For_Completion_Item_Kind_13'Access,
14 => Enum_Name_For_Completion_Item_Kind_14'Access,
15 => Enum_Name_For_Completion_Item_Kind_15'Access,
16 => Enum_Name_For_Completion_Item_Kind_16'Access,
17 => Enum_Name_For_Completion_Item_Kind_17'Access,
18 => Enum_Name_For_Completion_Item_Kind_18'Access,
19 => Enum_Name_For_Completion_Item_Kind_19'Access,
20 => Enum_Name_For_Completion_Item_Kind_20'Access,
21 => Enum_Name_For_Completion_Item_Kind_21'Access,
22 => Enum_Name_For_Completion_Item_Kind_22'Access,
23 => Enum_Name_For_Completion_Item_Kind_23'Access,
24 => Enum_Name_For_Completion_Item_Kind_24'Access,
25 => Enum_Name_For_Completion_Item_Kind_25'Access
         )
      );
      

         Enum_Name_For_Designated_Env_Kind_1 : aliased constant Text_Type :=
           "None";
         Enum_Name_For_Designated_Env_Kind_2 : aliased constant Text_Type :=
           "Current_Env";
         Enum_Name_For_Designated_Env_Kind_3 : aliased constant Text_Type :=
           "Named_Env";
         Enum_Name_For_Designated_Env_Kind_4 : aliased constant Text_Type :=
           "Direct_Env";

      Enum_Name_For_Designated_Env_Kind : aliased constant Text_Type :=
        "Designated_Env_Kind";
      Enum_Desc_For_Designated_Env_Kind : aliased constant Enum_Type_Descriptor := (
         Last_Value    => 4,
         Name          => Enum_Name_For_Designated_Env_Kind'Access,
         Default_Value => 1,
         Value_Names   => (
            1 => Enum_Name_For_Designated_Env_Kind_1'Access,
2 => Enum_Name_For_Designated_Env_Kind_2'Access,
3 => Enum_Name_For_Designated_Env_Kind_3'Access,
4 => Enum_Name_For_Designated_Env_Kind_4'Access
         )
      );
      

         Enum_Name_For_Grammar_Rule_1 : aliased constant Text_Type :=
           "Main_Rule_Rule";
         Enum_Name_For_Grammar_Rule_2 : aliased constant Text_Type :=
           "Id_Rule";
         Enum_Name_For_Grammar_Rule_3 : aliased constant Text_Type :=
           "Ref_Id_Rule";
         Enum_Name_For_Grammar_Rule_4 : aliased constant Text_Type :=
           "Type_Ref_Id_Rule";
         Enum_Name_For_Grammar_Rule_5 : aliased constant Text_Type :=
           "Module_Id_Rule";
         Enum_Name_For_Grammar_Rule_6 : aliased constant Text_Type :=
           "Def_Id_Rule";
         Enum_Name_For_Grammar_Rule_7 : aliased constant Text_Type :=
           "Doc_Rule";
         Enum_Name_For_Grammar_Rule_8 : aliased constant Text_Type :=
           "Module_Doc_Rule";
         Enum_Name_For_Grammar_Rule_9 : aliased constant Text_Type :=
           "Imported_Names_Rule";
         Enum_Name_For_Grammar_Rule_10 : aliased constant Text_Type :=
           "Import_Clause_Rule";
         Enum_Name_For_Grammar_Rule_11 : aliased constant Text_Type :=
           "Imports_Rule";
         Enum_Name_For_Grammar_Rule_12 : aliased constant Text_Type :=
           "Lexer_Decl_Rule";
         Enum_Name_For_Grammar_Rule_13 : aliased constant Text_Type :=
           "Grammar_Decl_Rule";
         Enum_Name_For_Grammar_Rule_14 : aliased constant Text_Type :=
           "Grammar_Rule_Rule";
         Enum_Name_For_Grammar_Rule_15 : aliased constant Text_Type :=
           "Lexer_Case_Rule_Rule";
         Enum_Name_For_Grammar_Rule_16 : aliased constant Text_Type :=
           "Lexer_Case_Alt_Rule";
         Enum_Name_For_Grammar_Rule_17 : aliased constant Text_Type :=
           "Lexer_Case_Send_Rule";
         Enum_Name_For_Grammar_Rule_18 : aliased constant Text_Type :=
           "Grammar_Primary_Rule";
         Enum_Name_For_Grammar_Rule_19 : aliased constant Text_Type :=
           "Grammar_Expr_Rule";
         Enum_Name_For_Grammar_Rule_20 : aliased constant Text_Type :=
           "Grammar_Pick_Rule";
         Enum_Name_For_Grammar_Rule_21 : aliased constant Text_Type :=
           "Grammar_Implicit_Pick_Rule";
         Enum_Name_For_Grammar_Rule_22 : aliased constant Text_Type :=
           "Grammar_Opt_Rule";
         Enum_Name_For_Grammar_Rule_23 : aliased constant Text_Type :=
           "Grammar_Opt_Error_Rule";
         Enum_Name_For_Grammar_Rule_24 : aliased constant Text_Type :=
           "Grammar_Cut_Rule";
         Enum_Name_For_Grammar_Rule_25 : aliased constant Text_Type :=
           "Grammar_Stopcut_Rule";
         Enum_Name_For_Grammar_Rule_26 : aliased constant Text_Type :=
           "Grammar_Or_Expr_Rule";
         Enum_Name_For_Grammar_Rule_27 : aliased constant Text_Type :=
           "Grammar_Discard_Expr_Rule";
         Enum_Name_For_Grammar_Rule_28 : aliased constant Text_Type :=
           "Token_Literal_Rule";
         Enum_Name_For_Grammar_Rule_29 : aliased constant Text_Type :=
           "Token_No_Case_Literal_Rule";
         Enum_Name_For_Grammar_Rule_30 : aliased constant Text_Type :=
           "Token_Pattern_Rule";
         Enum_Name_For_Grammar_Rule_31 : aliased constant Text_Type :=
           "Token_Pattern_Literal_Rule";
         Enum_Name_For_Grammar_Rule_32 : aliased constant Text_Type :=
           "Parse_Node_Expr_Rule";
         Enum_Name_For_Grammar_Rule_33 : aliased constant Text_Type :=
           "Grammar_Rule_Ref_Rule";
         Enum_Name_For_Grammar_Rule_34 : aliased constant Text_Type :=
           "Grammar_List_Expr_Rule";
         Enum_Name_For_Grammar_Rule_35 : aliased constant Text_Type :=
           "Grammar_List_Sep_Rule";
         Enum_Name_For_Grammar_Rule_36 : aliased constant Text_Type :=
           "Grammar_Skip_Rule";
         Enum_Name_For_Grammar_Rule_37 : aliased constant Text_Type :=
           "Grammar_Null_Rule";
         Enum_Name_For_Grammar_Rule_38 : aliased constant Text_Type :=
           "Grammar_Token_Rule";
         Enum_Name_For_Grammar_Rule_39 : aliased constant Text_Type :=
           "Type_Decl_Rule";
         Enum_Name_For_Grammar_Rule_40 : aliased constant Text_Type :=
           "Generic_Decl_Rule";
         Enum_Name_For_Grammar_Rule_41 : aliased constant Text_Type :=
           "Generic_Param_Type_Rule";
         Enum_Name_For_Grammar_Rule_42 : aliased constant Text_Type :=
           "Enum_Lit_Decl_Rule";
         Enum_Name_For_Grammar_Rule_43 : aliased constant Text_Type :=
           "Fun_Decl_Rule";
         Enum_Name_For_Grammar_Rule_44 : aliased constant Text_Type :=
           "Lambda_Param_Decl_Rule";
         Enum_Name_For_Grammar_Rule_45 : aliased constant Text_Type :=
           "Fun_Param_Decl_Rule";
         Enum_Name_For_Grammar_Rule_46 : aliased constant Text_Type :=
           "Fun_Param_List_Rule";
         Enum_Name_For_Grammar_Rule_47 : aliased constant Text_Type :=
           "Lambda_Param_List_Rule";
         Enum_Name_For_Grammar_Rule_48 : aliased constant Text_Type :=
           "Field_Decl_Rule";
         Enum_Name_For_Grammar_Rule_49 : aliased constant Text_Type :=
           "Lexer_Family_Decl_Rule";
         Enum_Name_For_Grammar_Rule_50 : aliased constant Text_Type :=
           "Bare_Decl_Rule";
         Enum_Name_For_Grammar_Rule_51 : aliased constant Text_Type :=
           "Decl_Rule";
         Enum_Name_For_Grammar_Rule_52 : aliased constant Text_Type :=
           "Type_Member_Ref_Rule";
         Enum_Name_For_Grammar_Rule_53 : aliased constant Text_Type :=
           "Type_Expr_Rule";
         Enum_Name_For_Grammar_Rule_54 : aliased constant Text_Type :=
           "Type_Ref_Rule";
         Enum_Name_For_Grammar_Rule_55 : aliased constant Text_Type :=
           "Type_List_Rule";
         Enum_Name_For_Grammar_Rule_56 : aliased constant Text_Type :=
           "Decls_Rule";
         Enum_Name_For_Grammar_Rule_57 : aliased constant Text_Type :=
           "Decl_Block_Rule";
         Enum_Name_For_Grammar_Rule_58 : aliased constant Text_Type :=
           "Val_Decl_Rule";
         Enum_Name_For_Grammar_Rule_59 : aliased constant Text_Type :=
           "Dynvar_Decl_Rule";
         Enum_Name_For_Grammar_Rule_60 : aliased constant Text_Type :=
           "Var_Bind_Rule";
         Enum_Name_For_Grammar_Rule_61 : aliased constant Text_Type :=
           "Env_Spec_Action_Rule";
         Enum_Name_For_Grammar_Rule_62 : aliased constant Text_Type :=
           "Env_Spec_Decl_Rule";
         Enum_Name_For_Grammar_Rule_63 : aliased constant Text_Type :=
           "Block_Rule";
         Enum_Name_For_Grammar_Rule_64 : aliased constant Text_Type :=
           "Pattern_Rule";
         Enum_Name_For_Grammar_Rule_65 : aliased constant Text_Type :=
           "Neg_Pattern_Rule";
         Enum_Name_For_Grammar_Rule_66 : aliased constant Text_Type :=
           "Pattern_Binding_Rule";
         Enum_Name_For_Grammar_Rule_67 : aliased constant Text_Type :=
           "Complex_Pattern_Rule";
         Enum_Name_For_Grammar_Rule_68 : aliased constant Text_Type :=
           "Value_Pattern_Rule";
         Enum_Name_For_Grammar_Rule_69 : aliased constant Text_Type :=
           "Regex_Pattern_Rule";
         Enum_Name_For_Grammar_Rule_70 : aliased constant Text_Type :=
           "Bool_Pattern_Rule";
         Enum_Name_For_Grammar_Rule_71 : aliased constant Text_Type :=
           "Ellipsis_Pattern_Rule";
         Enum_Name_For_Grammar_Rule_72 : aliased constant Text_Type :=
           "Integer_Pattern_Rule";
         Enum_Name_For_Grammar_Rule_73 : aliased constant Text_Type :=
           "List_Pattern_Rule";
         Enum_Name_For_Grammar_Rule_74 : aliased constant Text_Type :=
           "Pattern_Arg_Rule";
         Enum_Name_For_Grammar_Rule_75 : aliased constant Text_Type :=
           "Expr_Rule";
         Enum_Name_For_Grammar_Rule_76 : aliased constant Text_Type :=
           "Stream_Concat_Rule";
         Enum_Name_For_Grammar_Rule_77 : aliased constant Text_Type :=
           "Logic_Rule";
         Enum_Name_For_Grammar_Rule_78 : aliased constant Text_Type :=
           "Rel_Rule";
         Enum_Name_For_Grammar_Rule_79 : aliased constant Text_Type :=
           "Eq_Rule";
         Enum_Name_For_Grammar_Rule_80 : aliased constant Text_Type :=
           "Arith_1_Rule";
         Enum_Name_For_Grammar_Rule_81 : aliased constant Text_Type :=
           "Arith_2_Rule";
         Enum_Name_For_Grammar_Rule_82 : aliased constant Text_Type :=
           "Arith_3_Rule";
         Enum_Name_For_Grammar_Rule_83 : aliased constant Text_Type :=
           "Isa_Or_Primary_Rule";
         Enum_Name_For_Grammar_Rule_84 : aliased constant Text_Type :=
           "Logic_Propagate_Call_Rule";
         Enum_Name_For_Grammar_Rule_85 : aliased constant Text_Type :=
           "Primary_Rule";
         Enum_Name_For_Grammar_Rule_86 : aliased constant Text_Type :=
           "Match_Expr_Rule";
         Enum_Name_For_Grammar_Rule_87 : aliased constant Text_Type :=
           "Num_Lit_Rule";
         Enum_Name_For_Grammar_Rule_88 : aliased constant Text_Type :=
           "Big_Num_Lit_Rule";
         Enum_Name_For_Grammar_Rule_89 : aliased constant Text_Type :=
           "String_Lit_Rule";
         Enum_Name_For_Grammar_Rule_90 : aliased constant Text_Type :=
           "Block_String_Lit_Rule";
         Enum_Name_For_Grammar_Rule_91 : aliased constant Text_Type :=
           "Char_Lit_Rule";
         Enum_Name_For_Grammar_Rule_92 : aliased constant Text_Type :=
           "If_Expr_Rule";
         Enum_Name_For_Grammar_Rule_93 : aliased constant Text_Type :=
           "Raise_Expr_Rule";
         Enum_Name_For_Grammar_Rule_94 : aliased constant Text_Type :=
           "Try_Expr_Rule";
         Enum_Name_For_Grammar_Rule_95 : aliased constant Text_Type :=
           "Array_Literal_Rule";
         Enum_Name_For_Grammar_Rule_96 : aliased constant Text_Type :=
           "Callable_Ref_Rule";
         Enum_Name_For_Grammar_Rule_97 : aliased constant Text_Type :=
           "Null_Cond_Qual_Rule";
         Enum_Name_For_Grammar_Rule_98 : aliased constant Text_Type :=
           "Basic_Expr_Rule";
         Enum_Name_For_Grammar_Rule_99 : aliased constant Text_Type :=
           "Term_Rule";
         Enum_Name_For_Grammar_Rule_100 : aliased constant Text_Type :=
           "Basic_Name_Rule";
         Enum_Name_For_Grammar_Rule_101 : aliased constant Text_Type :=
           "Lambda_Expr_Rule";
         Enum_Name_For_Grammar_Rule_102 : aliased constant Text_Type :=
           "Null_Lit_Rule";
         Enum_Name_For_Grammar_Rule_103 : aliased constant Text_Type :=
           "Argument_Rule";
         Enum_Name_For_Grammar_Rule_104 : aliased constant Text_Type :=
           "Args_Rule";
         Enum_Name_For_Grammar_Rule_105 : aliased constant Text_Type :=
           "Decl_Annotation_Args_Rule";
         Enum_Name_For_Grammar_Rule_106 : aliased constant Text_Type :=
           "Decl_Annotation_Rule";
         Enum_Name_For_Grammar_Rule_107 : aliased constant Text_Type :=
           "Query_Comprehension_Rule";

      Enum_Name_For_Grammar_Rule : aliased constant Text_Type :=
        "Grammar_Rule";
      Enum_Desc_For_Grammar_Rule : aliased constant Enum_Type_Descriptor := (
         Last_Value    => 107,
         Name          => Enum_Name_For_Grammar_Rule'Access,
         Default_Value => 0,
         Value_Names   => (
            1 => Enum_Name_For_Grammar_Rule_1'Access,
2 => Enum_Name_For_Grammar_Rule_2'Access,
3 => Enum_Name_For_Grammar_Rule_3'Access,
4 => Enum_Name_For_Grammar_Rule_4'Access,
5 => Enum_Name_For_Grammar_Rule_5'Access,
6 => Enum_Name_For_Grammar_Rule_6'Access,
7 => Enum_Name_For_Grammar_Rule_7'Access,
8 => Enum_Name_For_Grammar_Rule_8'Access,
9 => Enum_Name_For_Grammar_Rule_9'Access,
10 => Enum_Name_For_Grammar_Rule_10'Access,
11 => Enum_Name_For_Grammar_Rule_11'Access,
12 => Enum_Name_For_Grammar_Rule_12'Access,
13 => Enum_Name_For_Grammar_Rule_13'Access,
14 => Enum_Name_For_Grammar_Rule_14'Access,
15 => Enum_Name_For_Grammar_Rule_15'Access,
16 => Enum_Name_For_Grammar_Rule_16'Access,
17 => Enum_Name_For_Grammar_Rule_17'Access,
18 => Enum_Name_For_Grammar_Rule_18'Access,
19 => Enum_Name_For_Grammar_Rule_19'Access,
20 => Enum_Name_For_Grammar_Rule_20'Access,
21 => Enum_Name_For_Grammar_Rule_21'Access,
22 => Enum_Name_For_Grammar_Rule_22'Access,
23 => Enum_Name_For_Grammar_Rule_23'Access,
24 => Enum_Name_For_Grammar_Rule_24'Access,
25 => Enum_Name_For_Grammar_Rule_25'Access,
26 => Enum_Name_For_Grammar_Rule_26'Access,
27 => Enum_Name_For_Grammar_Rule_27'Access,
28 => Enum_Name_For_Grammar_Rule_28'Access,
29 => Enum_Name_For_Grammar_Rule_29'Access,
30 => Enum_Name_For_Grammar_Rule_30'Access,
31 => Enum_Name_For_Grammar_Rule_31'Access,
32 => Enum_Name_For_Grammar_Rule_32'Access,
33 => Enum_Name_For_Grammar_Rule_33'Access,
34 => Enum_Name_For_Grammar_Rule_34'Access,
35 => Enum_Name_For_Grammar_Rule_35'Access,
36 => Enum_Name_For_Grammar_Rule_36'Access,
37 => Enum_Name_For_Grammar_Rule_37'Access,
38 => Enum_Name_For_Grammar_Rule_38'Access,
39 => Enum_Name_For_Grammar_Rule_39'Access,
40 => Enum_Name_For_Grammar_Rule_40'Access,
41 => Enum_Name_For_Grammar_Rule_41'Access,
42 => Enum_Name_For_Grammar_Rule_42'Access,
43 => Enum_Name_For_Grammar_Rule_43'Access,
44 => Enum_Name_For_Grammar_Rule_44'Access,
45 => Enum_Name_For_Grammar_Rule_45'Access,
46 => Enum_Name_For_Grammar_Rule_46'Access,
47 => Enum_Name_For_Grammar_Rule_47'Access,
48 => Enum_Name_For_Grammar_Rule_48'Access,
49 => Enum_Name_For_Grammar_Rule_49'Access,
50 => Enum_Name_For_Grammar_Rule_50'Access,
51 => Enum_Name_For_Grammar_Rule_51'Access,
52 => Enum_Name_For_Grammar_Rule_52'Access,
53 => Enum_Name_For_Grammar_Rule_53'Access,
54 => Enum_Name_For_Grammar_Rule_54'Access,
55 => Enum_Name_For_Grammar_Rule_55'Access,
56 => Enum_Name_For_Grammar_Rule_56'Access,
57 => Enum_Name_For_Grammar_Rule_57'Access,
58 => Enum_Name_For_Grammar_Rule_58'Access,
59 => Enum_Name_For_Grammar_Rule_59'Access,
60 => Enum_Name_For_Grammar_Rule_60'Access,
61 => Enum_Name_For_Grammar_Rule_61'Access,
62 => Enum_Name_For_Grammar_Rule_62'Access,
63 => Enum_Name_For_Grammar_Rule_63'Access,
64 => Enum_Name_For_Grammar_Rule_64'Access,
65 => Enum_Name_For_Grammar_Rule_65'Access,
66 => Enum_Name_For_Grammar_Rule_66'Access,
67 => Enum_Name_For_Grammar_Rule_67'Access,
68 => Enum_Name_For_Grammar_Rule_68'Access,
69 => Enum_Name_For_Grammar_Rule_69'Access,
70 => Enum_Name_For_Grammar_Rule_70'Access,
71 => Enum_Name_For_Grammar_Rule_71'Access,
72 => Enum_Name_For_Grammar_Rule_72'Access,
73 => Enum_Name_For_Grammar_Rule_73'Access,
74 => Enum_Name_For_Grammar_Rule_74'Access,
75 => Enum_Name_For_Grammar_Rule_75'Access,
76 => Enum_Name_For_Grammar_Rule_76'Access,
77 => Enum_Name_For_Grammar_Rule_77'Access,
78 => Enum_Name_For_Grammar_Rule_78'Access,
79 => Enum_Name_For_Grammar_Rule_79'Access,
80 => Enum_Name_For_Grammar_Rule_80'Access,
81 => Enum_Name_For_Grammar_Rule_81'Access,
82 => Enum_Name_For_Grammar_Rule_82'Access,
83 => Enum_Name_For_Grammar_Rule_83'Access,
84 => Enum_Name_For_Grammar_Rule_84'Access,
85 => Enum_Name_For_Grammar_Rule_85'Access,
86 => Enum_Name_For_Grammar_Rule_86'Access,
87 => Enum_Name_For_Grammar_Rule_87'Access,
88 => Enum_Name_For_Grammar_Rule_88'Access,
89 => Enum_Name_For_Grammar_Rule_89'Access,
90 => Enum_Name_For_Grammar_Rule_90'Access,
91 => Enum_Name_For_Grammar_Rule_91'Access,
92 => Enum_Name_For_Grammar_Rule_92'Access,
93 => Enum_Name_For_Grammar_Rule_93'Access,
94 => Enum_Name_For_Grammar_Rule_94'Access,
95 => Enum_Name_For_Grammar_Rule_95'Access,
96 => Enum_Name_For_Grammar_Rule_96'Access,
97 => Enum_Name_For_Grammar_Rule_97'Access,
98 => Enum_Name_For_Grammar_Rule_98'Access,
99 => Enum_Name_For_Grammar_Rule_99'Access,
100 => Enum_Name_For_Grammar_Rule_100'Access,
101 => Enum_Name_For_Grammar_Rule_101'Access,
102 => Enum_Name_For_Grammar_Rule_102'Access,
103 => Enum_Name_For_Grammar_Rule_103'Access,
104 => Enum_Name_For_Grammar_Rule_104'Access,
105 => Enum_Name_For_Grammar_Rule_105'Access,
106 => Enum_Name_For_Grammar_Rule_106'Access,
107 => Enum_Name_For_Grammar_Rule_107'Access
         )
      );
      

         Enum_Name_For_Lookup_Kind_1 : aliased constant Text_Type :=
           "Recursive";
         Enum_Name_For_Lookup_Kind_2 : aliased constant Text_Type :=
           "Flat";
         Enum_Name_For_Lookup_Kind_3 : aliased constant Text_Type :=
           "Minimal";

      Enum_Name_For_Lookup_Kind : aliased constant Text_Type :=
        "Lookup_Kind";
      Enum_Desc_For_Lookup_Kind : aliased constant Enum_Type_Descriptor := (
         Last_Value    => 3,
         Name          => Enum_Name_For_Lookup_Kind'Access,
         Default_Value => 0,
         Value_Names   => (
            1 => Enum_Name_For_Lookup_Kind_1'Access,
2 => Enum_Name_For_Lookup_Kind_2'Access,
3 => Enum_Name_For_Lookup_Kind_3'Access
         )
      );
   Enum_Types : aliased constant Enum_Type_Descriptor_Array := (
      Type_Index_For_Analysis_Unit_Kind => Enum_Desc_For_Analysis_Unit_Kind'Access,
Type_Index_For_Completion_Item_Kind => Enum_Desc_For_Completion_Item_Kind'Access,
Type_Index_For_Designated_Env_Kind => Enum_Desc_For_Designated_Env_Kind'Access,
Type_Index_For_Grammar_Rule => Enum_Desc_For_Grammar_Rule'Access,
Type_Index_For_Lookup_Kind => Enum_Desc_For_Lookup_Kind'Access
   );

   ------------------------------------
   -- Introspection values for enums --
   ------------------------------------

      
      type Internal_Rec_Analysis_Unit_Kind is new Base_Internal_Enum_Value with record
         Value : Analysis_Unit_Kind;
      end record;
      type Internal_Acc_Analysis_Unit_Kind is access all Internal_Rec_Analysis_Unit_Kind;

      overriding function "=" (Left, Right : Internal_Rec_Analysis_Unit_Kind) return Boolean;
      overriding function Type_Of (Value : Internal_Rec_Analysis_Unit_Kind) return Type_Index;
      overriding function Image (Value : Internal_Rec_Analysis_Unit_Kind) return String;
      overriding function Value_Index (Value : Internal_Rec_Analysis_Unit_Kind) return Enum_Value_Index;
      
      type Internal_Rec_Completion_Item_Kind is new Base_Internal_Enum_Value with record
         Value : Completion_Item_Kind;
      end record;
      type Internal_Acc_Completion_Item_Kind is access all Internal_Rec_Completion_Item_Kind;

      overriding function "=" (Left, Right : Internal_Rec_Completion_Item_Kind) return Boolean;
      overriding function Type_Of (Value : Internal_Rec_Completion_Item_Kind) return Type_Index;
      overriding function Image (Value : Internal_Rec_Completion_Item_Kind) return String;
      overriding function Value_Index (Value : Internal_Rec_Completion_Item_Kind) return Enum_Value_Index;
      
      type Internal_Rec_Designated_Env_Kind is new Base_Internal_Enum_Value with record
         Value : Designated_Env_Kind;
      end record;
      type Internal_Acc_Designated_Env_Kind is access all Internal_Rec_Designated_Env_Kind;

      overriding function "=" (Left, Right : Internal_Rec_Designated_Env_Kind) return Boolean;
      overriding function Type_Of (Value : Internal_Rec_Designated_Env_Kind) return Type_Index;
      overriding function Image (Value : Internal_Rec_Designated_Env_Kind) return String;
      overriding function Value_Index (Value : Internal_Rec_Designated_Env_Kind) return Enum_Value_Index;
      
      type Internal_Rec_Grammar_Rule is new Base_Internal_Enum_Value with record
         Value : Grammar_Rule;
      end record;
      type Internal_Acc_Grammar_Rule is access all Internal_Rec_Grammar_Rule;

      overriding function "=" (Left, Right : Internal_Rec_Grammar_Rule) return Boolean;
      overriding function Type_Of (Value : Internal_Rec_Grammar_Rule) return Type_Index;
      overriding function Image (Value : Internal_Rec_Grammar_Rule) return String;
      overriding function Value_Index (Value : Internal_Rec_Grammar_Rule) return Enum_Value_Index;
      
      type Internal_Rec_Lookup_Kind is new Base_Internal_Enum_Value with record
         Value : Lookup_Kind;
      end record;
      type Internal_Acc_Lookup_Kind is access all Internal_Rec_Lookup_Kind;

      overriding function "=" (Left, Right : Internal_Rec_Lookup_Kind) return Boolean;
      overriding function Type_Of (Value : Internal_Rec_Lookup_Kind) return Type_Index;
      overriding function Image (Value : Internal_Rec_Lookup_Kind) return String;
      overriding function Value_Index (Value : Internal_Rec_Lookup_Kind) return Enum_Value_Index;

   function Create_Enum
     (Enum_Type   : Type_Index;
      Value_Index : Enum_Value_Index) return Internal_Value_Access;
   --  Implementation of the Create_Enum operation in the lanugage descriptor

   ----------------------------
   -- Array type descriptors --
   ----------------------------

   
   Array_Types : aliased constant Array_Type_Descriptor_Array := (
      Type_Index_For_Complete_Item_Array => (Element_Type => Type_Index_For_Complete_Item),
Type_Index_For_Lkt_Node_Array => (Element_Type => Type_Index_For_Lkt_Node),
Type_Index_For_Def_Id_Array => (Element_Type => Type_Index_For_Def_Id),
Type_Index_For_Fun_Decl_Array => (Element_Type => Type_Index_For_Fun_Decl),
Type_Index_For_Logic_Context_Array => (Element_Type => Type_Index_For_Logic_Context),
Type_Index_For_Ref_Result_Array => (Element_Type => Type_Index_For_Ref_Result),
Type_Index_For_Solver_Diagnostic_Array => (Element_Type => Type_Index_For_Solver_Diagnostic),
Type_Index_For_Analysis_Unit_Array => (Element_Type => Type_Index_For_Analysis_Unit)
   );

   -------------------------------------
   -- Introspection values for arrays --
   -------------------------------------

      

      type Internal_Stored_Complete_Item_Array is access all Analysis.Complete_Item_Array;
      procedure Free is new Ada.Unchecked_Deallocation
        (Analysis.Complete_Item_Array, Internal_Stored_Complete_Item_Array);

      type Internal_Rec_Complete_Item_Array is new Base_Internal_Array_Value with record
         Value : Internal_Stored_Complete_Item_Array;
      end record;
      type Internal_Acc_Complete_Item_Array is access all Internal_Rec_Complete_Item_Array;

      overriding function "=" (Left, Right : Internal_Rec_Complete_Item_Array) return Boolean;
      overriding procedure Destroy (Value : in out Internal_Rec_Complete_Item_Array);
      overriding function Type_Of (Value : Internal_Rec_Complete_Item_Array) return Type_Index;
      overriding function Array_Length (Value : Internal_Rec_Complete_Item_Array) return Natural;
      overriding function Array_Item
        (Value : Internal_Rec_Complete_Item_Array; Index : Positive) return Internal_Value_Access;

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Complete_Item_Array;
      

      type Internal_Stored_Lkt_Node_Array is access all Analysis.Lkt_Node_Array;
      procedure Free is new Ada.Unchecked_Deallocation
        (Analysis.Lkt_Node_Array, Internal_Stored_Lkt_Node_Array);

      type Internal_Rec_Lkt_Node_Array is new Base_Internal_Array_Value with record
         Value : Internal_Stored_Lkt_Node_Array;
      end record;
      type Internal_Acc_Lkt_Node_Array is access all Internal_Rec_Lkt_Node_Array;

      overriding function "=" (Left, Right : Internal_Rec_Lkt_Node_Array) return Boolean;
      overriding procedure Destroy (Value : in out Internal_Rec_Lkt_Node_Array);
      overriding function Type_Of (Value : Internal_Rec_Lkt_Node_Array) return Type_Index;
      overriding function Array_Length (Value : Internal_Rec_Lkt_Node_Array) return Natural;
      overriding function Array_Item
        (Value : Internal_Rec_Lkt_Node_Array; Index : Positive) return Internal_Value_Access;

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Lkt_Node_Array;
      

      type Internal_Stored_Def_Id_Array is access all Analysis.Def_Id_Array;
      procedure Free is new Ada.Unchecked_Deallocation
        (Analysis.Def_Id_Array, Internal_Stored_Def_Id_Array);

      type Internal_Rec_Def_Id_Array is new Base_Internal_Array_Value with record
         Value : Internal_Stored_Def_Id_Array;
      end record;
      type Internal_Acc_Def_Id_Array is access all Internal_Rec_Def_Id_Array;

      overriding function "=" (Left, Right : Internal_Rec_Def_Id_Array) return Boolean;
      overriding procedure Destroy (Value : in out Internal_Rec_Def_Id_Array);
      overriding function Type_Of (Value : Internal_Rec_Def_Id_Array) return Type_Index;
      overriding function Array_Length (Value : Internal_Rec_Def_Id_Array) return Natural;
      overriding function Array_Item
        (Value : Internal_Rec_Def_Id_Array; Index : Positive) return Internal_Value_Access;

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Def_Id_Array;
      

      type Internal_Stored_Fun_Decl_Array is access all Analysis.Fun_Decl_Array;
      procedure Free is new Ada.Unchecked_Deallocation
        (Analysis.Fun_Decl_Array, Internal_Stored_Fun_Decl_Array);

      type Internal_Rec_Fun_Decl_Array is new Base_Internal_Array_Value with record
         Value : Internal_Stored_Fun_Decl_Array;
      end record;
      type Internal_Acc_Fun_Decl_Array is access all Internal_Rec_Fun_Decl_Array;

      overriding function "=" (Left, Right : Internal_Rec_Fun_Decl_Array) return Boolean;
      overriding procedure Destroy (Value : in out Internal_Rec_Fun_Decl_Array);
      overriding function Type_Of (Value : Internal_Rec_Fun_Decl_Array) return Type_Index;
      overriding function Array_Length (Value : Internal_Rec_Fun_Decl_Array) return Natural;
      overriding function Array_Item
        (Value : Internal_Rec_Fun_Decl_Array; Index : Positive) return Internal_Value_Access;

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Fun_Decl_Array;
      

      type Internal_Stored_Logic_Context_Array is access all Analysis.Logic_Context_Array;
      procedure Free is new Ada.Unchecked_Deallocation
        (Analysis.Logic_Context_Array, Internal_Stored_Logic_Context_Array);

      type Internal_Rec_Logic_Context_Array is new Base_Internal_Array_Value with record
         Value : Internal_Stored_Logic_Context_Array;
      end record;
      type Internal_Acc_Logic_Context_Array is access all Internal_Rec_Logic_Context_Array;

      overriding function "=" (Left, Right : Internal_Rec_Logic_Context_Array) return Boolean;
      overriding procedure Destroy (Value : in out Internal_Rec_Logic_Context_Array);
      overriding function Type_Of (Value : Internal_Rec_Logic_Context_Array) return Type_Index;
      overriding function Array_Length (Value : Internal_Rec_Logic_Context_Array) return Natural;
      overriding function Array_Item
        (Value : Internal_Rec_Logic_Context_Array; Index : Positive) return Internal_Value_Access;

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Logic_Context_Array;
      

      type Internal_Stored_Ref_Result_Array is access all Analysis.Ref_Result_Array;
      procedure Free is new Ada.Unchecked_Deallocation
        (Analysis.Ref_Result_Array, Internal_Stored_Ref_Result_Array);

      type Internal_Rec_Ref_Result_Array is new Base_Internal_Array_Value with record
         Value : Internal_Stored_Ref_Result_Array;
      end record;
      type Internal_Acc_Ref_Result_Array is access all Internal_Rec_Ref_Result_Array;

      overriding function "=" (Left, Right : Internal_Rec_Ref_Result_Array) return Boolean;
      overriding procedure Destroy (Value : in out Internal_Rec_Ref_Result_Array);
      overriding function Type_Of (Value : Internal_Rec_Ref_Result_Array) return Type_Index;
      overriding function Array_Length (Value : Internal_Rec_Ref_Result_Array) return Natural;
      overriding function Array_Item
        (Value : Internal_Rec_Ref_Result_Array; Index : Positive) return Internal_Value_Access;

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Ref_Result_Array;
      

      type Internal_Stored_Solver_Diagnostic_Array is access all Analysis.Solver_Diagnostic_Array;
      procedure Free is new Ada.Unchecked_Deallocation
        (Analysis.Solver_Diagnostic_Array, Internal_Stored_Solver_Diagnostic_Array);

      type Internal_Rec_Solver_Diagnostic_Array is new Base_Internal_Array_Value with record
         Value : Internal_Stored_Solver_Diagnostic_Array;
      end record;
      type Internal_Acc_Solver_Diagnostic_Array is access all Internal_Rec_Solver_Diagnostic_Array;

      overriding function "=" (Left, Right : Internal_Rec_Solver_Diagnostic_Array) return Boolean;
      overriding procedure Destroy (Value : in out Internal_Rec_Solver_Diagnostic_Array);
      overriding function Type_Of (Value : Internal_Rec_Solver_Diagnostic_Array) return Type_Index;
      overriding function Array_Length (Value : Internal_Rec_Solver_Diagnostic_Array) return Natural;
      overriding function Array_Item
        (Value : Internal_Rec_Solver_Diagnostic_Array; Index : Positive) return Internal_Value_Access;

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Solver_Diagnostic_Array;
      

      type Internal_Stored_Analysis_Unit_Array is access all Analysis.Analysis_Unit_Array;
      procedure Free is new Ada.Unchecked_Deallocation
        (Analysis.Analysis_Unit_Array, Internal_Stored_Analysis_Unit_Array);

      type Internal_Rec_Analysis_Unit_Array is new Base_Internal_Array_Value with record
         Value : Internal_Stored_Analysis_Unit_Array;
      end record;
      type Internal_Acc_Analysis_Unit_Array is access all Internal_Rec_Analysis_Unit_Array;

      overriding function "=" (Left, Right : Internal_Rec_Analysis_Unit_Array) return Boolean;
      overriding procedure Destroy (Value : in out Internal_Rec_Analysis_Unit_Array);
      overriding function Type_Of (Value : Internal_Rec_Analysis_Unit_Array) return Type_Index;
      overriding function Array_Length (Value : Internal_Rec_Analysis_Unit_Array) return Natural;
      overriding function Array_Item
        (Value : Internal_Rec_Analysis_Unit_Array; Index : Positive) return Internal_Value_Access;

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Analysis_Unit_Array;

   function Create_Array
     (Array_Type : Type_Index;
      Values     : Internal_Value_Array) return Internal_Value_Access;
   --  Implementation of the Create_Array operation in the language descriptor

   -------------------------------
   -- Iterator type descriptors --
   -------------------------------

   
   Iterator_Types : aliased constant Iterator_Type_Descriptor_Array := (
         1 .. 0 => <>
   );


   --------------------------------------
   -- Introspection values for structs --
   --------------------------------------

      

      type Internal_Rec_Complete_Item is new Base_Internal_Struct_Value with record
         Value : Complete_Item;
      end record;
      type Internal_Acc_Complete_Item is access all Internal_Rec_Complete_Item;

      overriding function "=" (Left, Right : Internal_Rec_Complete_Item) return Boolean;
      overriding function Type_Of (Value : Internal_Rec_Complete_Item) return Type_Index;
      overriding function Eval_Member
        (Value  : Internal_Rec_Complete_Item;
         Member : Struct_Member_Index) return Internal_Value_Access;
      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Complete_Item;
      

      type Internal_Rec_Decoded_Char_Value is new Base_Internal_Struct_Value with record
         Value : Decoded_Char_Value;
      end record;
      type Internal_Acc_Decoded_Char_Value is access all Internal_Rec_Decoded_Char_Value;

      overriding function "=" (Left, Right : Internal_Rec_Decoded_Char_Value) return Boolean;
      overriding function Type_Of (Value : Internal_Rec_Decoded_Char_Value) return Type_Index;
      overriding function Eval_Member
        (Value  : Internal_Rec_Decoded_Char_Value;
         Member : Struct_Member_Index) return Internal_Value_Access;
      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Decoded_Char_Value;
      

      type Internal_Rec_Decoded_String_Value is new Base_Internal_Struct_Value with record
         Value : Decoded_String_Value;
      end record;
      type Internal_Acc_Decoded_String_Value is access all Internal_Rec_Decoded_String_Value;

      overriding function "=" (Left, Right : Internal_Rec_Decoded_String_Value) return Boolean;
      overriding function Type_Of (Value : Internal_Rec_Decoded_String_Value) return Type_Index;
      overriding function Eval_Member
        (Value  : Internal_Rec_Decoded_String_Value;
         Member : Struct_Member_Index) return Internal_Value_Access;
      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Decoded_String_Value;
      

      type Internal_Rec_Logic_Context is new Base_Internal_Struct_Value with record
         Value : Logic_Context;
      end record;
      type Internal_Acc_Logic_Context is access all Internal_Rec_Logic_Context;

      overriding function "=" (Left, Right : Internal_Rec_Logic_Context) return Boolean;
      overriding function Type_Of (Value : Internal_Rec_Logic_Context) return Type_Index;
      overriding function Eval_Member
        (Value  : Internal_Rec_Logic_Context;
         Member : Struct_Member_Index) return Internal_Value_Access;
      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Logic_Context;
      

      type Internal_Rec_Ref_Result is new Base_Internal_Struct_Value with record
         Value : Ref_Result;
      end record;
      type Internal_Acc_Ref_Result is access all Internal_Rec_Ref_Result;

      overriding function "=" (Left, Right : Internal_Rec_Ref_Result) return Boolean;
      overriding function Type_Of (Value : Internal_Rec_Ref_Result) return Type_Index;
      overriding function Eval_Member
        (Value  : Internal_Rec_Ref_Result;
         Member : Struct_Member_Index) return Internal_Value_Access;
      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Ref_Result;
      

      type Internal_Rec_Solver_Diagnostic is new Base_Internal_Struct_Value with record
         Value : Solver_Diagnostic;
      end record;
      type Internal_Acc_Solver_Diagnostic is access all Internal_Rec_Solver_Diagnostic;

      overriding function "=" (Left, Right : Internal_Rec_Solver_Diagnostic) return Boolean;
      overriding function Type_Of (Value : Internal_Rec_Solver_Diagnostic) return Type_Index;
      overriding function Eval_Member
        (Value  : Internal_Rec_Solver_Diagnostic;
         Member : Struct_Member_Index) return Internal_Value_Access;
      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Solver_Diagnostic;
      

      type Internal_Rec_Solver_Result is new Base_Internal_Struct_Value with record
         Value : Solver_Result;
      end record;
      type Internal_Acc_Solver_Result is access all Internal_Rec_Solver_Result;

      overriding function "=" (Left, Right : Internal_Rec_Solver_Result) return Boolean;
      overriding function Type_Of (Value : Internal_Rec_Solver_Result) return Type_Index;
      overriding function Eval_Member
        (Value  : Internal_Rec_Solver_Result;
         Member : Struct_Member_Index) return Internal_Value_Access;
      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Solver_Result;

   function Create_Struct
     (Struct_Type : Type_Index;
      Values      : Internal_Value_Array) return Internal_Value_Access;
   --  Implementation for the Create_Struct operation in the language
   --  descriptor.

   -------------------------------
   -- Struct member descriptors --
   -------------------------------

   
      


      

      

      Member_Name_For_Complete_Item_Declaration : aliased constant Text_Type :=
        "Declaration";
      Member_Doc_For_Complete_Item_Declaration  : aliased constant Text_Type := "";
      Member_Desc_For_Complete_Item_Declaration  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Complete_Item_Declaration'Access,
         Documentation => Member_Doc_For_Complete_Item_Declaration'Access,
         Owner         => Type_Index_For_Complete_Item,
         Member_Type   => Type_Index_For_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decoded_Char_Value_Value : aliased constant Text_Type :=
        "Value";
      Member_Doc_For_Decoded_Char_Value_Value  : aliased constant Text_Type := "";
      Member_Desc_For_Decoded_Char_Value_Value  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decoded_Char_Value_Value'Access,
         Documentation => Member_Doc_For_Decoded_Char_Value_Value'Access,
         Owner         => Type_Index_For_Decoded_Char_Value,
         Member_Type   => Type_Index_For_Char,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decoded_Char_Value_Has_Error : aliased constant Text_Type :=
        "Has_Error";
      Member_Doc_For_Decoded_Char_Value_Has_Error  : aliased constant Text_Type := "";
      Member_Desc_For_Decoded_Char_Value_Has_Error  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decoded_Char_Value_Has_Error'Access,
         Documentation => Member_Doc_For_Decoded_Char_Value_Has_Error'Access,
         Owner         => Type_Index_For_Decoded_Char_Value,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decoded_Char_Value_Error_Sloc : aliased constant Text_Type :=
        "Error_Sloc";
      Member_Doc_For_Decoded_Char_Value_Error_Sloc  : aliased constant Text_Type := "";
      Member_Desc_For_Decoded_Char_Value_Error_Sloc  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decoded_Char_Value_Error_Sloc'Access,
         Documentation => Member_Doc_For_Decoded_Char_Value_Error_Sloc'Access,
         Owner         => Type_Index_For_Decoded_Char_Value,
         Member_Type   => Type_Index_For_Source_Location,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decoded_Char_Value_Error_Message : aliased constant Text_Type :=
        "Error_Message";
      Member_Doc_For_Decoded_Char_Value_Error_Message  : aliased constant Text_Type := "";
      Member_Desc_For_Decoded_Char_Value_Error_Message  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decoded_Char_Value_Error_Message'Access,
         Documentation => Member_Doc_For_Decoded_Char_Value_Error_Message'Access,
         Owner         => Type_Index_For_Decoded_Char_Value,
         Member_Type   => Type_Index_For_String,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decoded_String_Value_Value : aliased constant Text_Type :=
        "Value";
      Member_Doc_For_Decoded_String_Value_Value  : aliased constant Text_Type := "";
      Member_Desc_For_Decoded_String_Value_Value  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decoded_String_Value_Value'Access,
         Documentation => Member_Doc_For_Decoded_String_Value_Value'Access,
         Owner         => Type_Index_For_Decoded_String_Value,
         Member_Type   => Type_Index_For_String,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decoded_String_Value_Has_Error : aliased constant Text_Type :=
        "Has_Error";
      Member_Doc_For_Decoded_String_Value_Has_Error  : aliased constant Text_Type := "";
      Member_Desc_For_Decoded_String_Value_Has_Error  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decoded_String_Value_Has_Error'Access,
         Documentation => Member_Doc_For_Decoded_String_Value_Has_Error'Access,
         Owner         => Type_Index_For_Decoded_String_Value,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decoded_String_Value_Error_Sloc : aliased constant Text_Type :=
        "Error_Sloc";
      Member_Doc_For_Decoded_String_Value_Error_Sloc  : aliased constant Text_Type := "";
      Member_Desc_For_Decoded_String_Value_Error_Sloc  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decoded_String_Value_Error_Sloc'Access,
         Documentation => Member_Doc_For_Decoded_String_Value_Error_Sloc'Access,
         Owner         => Type_Index_For_Decoded_String_Value,
         Member_Type   => Type_Index_For_Source_Location,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decoded_String_Value_Error_Message : aliased constant Text_Type :=
        "Error_Message";
      Member_Doc_For_Decoded_String_Value_Error_Message  : aliased constant Text_Type := "";
      Member_Desc_For_Decoded_String_Value_Error_Message  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decoded_String_Value_Error_Message'Access,
         Documentation => Member_Doc_For_Decoded_String_Value_Error_Message'Access,
         Owner         => Type_Index_For_Decoded_String_Value,
         Member_Type   => Type_Index_For_String,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Logic_Context_Ref_Node : aliased constant Text_Type :=
        "Ref_Node";
      Member_Doc_For_Logic_Context_Ref_Node  : aliased constant Text_Type := "";
      Member_Desc_For_Logic_Context_Ref_Node  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Logic_Context_Ref_Node'Access,
         Documentation => Member_Doc_For_Logic_Context_Ref_Node'Access,
         Owner         => Type_Index_For_Logic_Context,
         Member_Type   => Type_Index_For_Lkt_Node,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Logic_Context_Decl_Node : aliased constant Text_Type :=
        "Decl_Node";
      Member_Doc_For_Logic_Context_Decl_Node  : aliased constant Text_Type := "";
      Member_Desc_For_Logic_Context_Decl_Node  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Logic_Context_Decl_Node'Access,
         Documentation => Member_Doc_For_Logic_Context_Decl_Node'Access,
         Owner         => Type_Index_For_Logic_Context,
         Member_Type   => Type_Index_For_Lkt_Node,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Ref_Result_Ref : aliased constant Text_Type :=
        "Ref";
      Member_Doc_For_Ref_Result_Ref  : aliased constant Text_Type := "";
      Member_Desc_For_Ref_Result_Ref  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Ref_Result_Ref'Access,
         Documentation => Member_Doc_For_Ref_Result_Ref'Access,
         Owner         => Type_Index_For_Ref_Result,
         Member_Type   => Type_Index_For_Ref_Id,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Solver_Diagnostic_Message_Template : aliased constant Text_Type :=
        "Message_Template";
      Member_Doc_For_Solver_Diagnostic_Message_Template  : aliased constant Text_Type := "";
      Member_Desc_For_Solver_Diagnostic_Message_Template  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Solver_Diagnostic_Message_Template'Access,
         Documentation => Member_Doc_For_Solver_Diagnostic_Message_Template'Access,
         Owner         => Type_Index_For_Solver_Diagnostic,
         Member_Type   => Type_Index_For_String,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Solver_Diagnostic_Args : aliased constant Text_Type :=
        "Args";
      Member_Doc_For_Solver_Diagnostic_Args  : aliased constant Text_Type := "";
      Member_Desc_For_Solver_Diagnostic_Args  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Solver_Diagnostic_Args'Access,
         Documentation => Member_Doc_For_Solver_Diagnostic_Args'Access,
         Owner         => Type_Index_For_Solver_Diagnostic,
         Member_Type   => Type_Index_For_Lkt_Node_Array,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Solver_Diagnostic_Location : aliased constant Text_Type :=
        "Location";
      Member_Doc_For_Solver_Diagnostic_Location  : aliased constant Text_Type := "";
      Member_Desc_For_Solver_Diagnostic_Location  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Solver_Diagnostic_Location'Access,
         Documentation => Member_Doc_For_Solver_Diagnostic_Location'Access,
         Owner         => Type_Index_For_Solver_Diagnostic,
         Member_Type   => Type_Index_For_Lkt_Node,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Solver_Diagnostic_Contexts : aliased constant Text_Type :=
        "Contexts";
      Member_Doc_For_Solver_Diagnostic_Contexts  : aliased constant Text_Type := "";
      Member_Desc_For_Solver_Diagnostic_Contexts  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Solver_Diagnostic_Contexts'Access,
         Documentation => Member_Doc_For_Solver_Diagnostic_Contexts'Access,
         Owner         => Type_Index_For_Solver_Diagnostic,
         Member_Type   => Type_Index_For_Logic_Context_Array,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Solver_Diagnostic_Round : aliased constant Text_Type :=
        "Round";
      Member_Doc_For_Solver_Diagnostic_Round  : aliased constant Text_Type := "";
      Member_Desc_For_Solver_Diagnostic_Round  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Solver_Diagnostic_Round'Access,
         Documentation => Member_Doc_For_Solver_Diagnostic_Round'Access,
         Owner         => Type_Index_For_Solver_Diagnostic,
         Member_Type   => Type_Index_For_Int,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Solver_Result_Success : aliased constant Text_Type :=
        "Success";
      Member_Doc_For_Solver_Result_Success  : aliased constant Text_Type := "";
      Member_Desc_For_Solver_Result_Success  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Solver_Result_Success'Access,
         Documentation => Member_Doc_For_Solver_Result_Success'Access,
         Owner         => Type_Index_For_Solver_Result,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Solver_Result_Diagnostics : aliased constant Text_Type :=
        "Diagnostics";
      Member_Doc_For_Solver_Result_Diagnostics  : aliased constant Text_Type := "";
      Member_Desc_For_Solver_Result_Diagnostics  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Solver_Result_Diagnostics'Access,
         Documentation => Member_Doc_For_Solver_Result_Diagnostics'Access,
         Owner         => Type_Index_For_Solver_Result,
         Member_Type   => Type_Index_For_Solver_Diagnostic_Array,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Argument_F_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Argument => 1);

      Member_Name_For_Argument_F_Name : aliased constant Text_Type :=
        "F_Name";
      Member_Doc_For_Argument_F_Name  : aliased constant Text_Type := "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Argument_F_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Argument_F_Name'Access,
         Documentation => Member_Doc_For_Argument_F_Name'Access,
         Owner         => Type_Index_For_Argument,
         Member_Type   => Type_Index_For_Ref_Id,
         Null_For      => null,
         Indexes       => Indexes_For_Argument_F_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Argument_F_Value : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Argument => 2);

      Member_Name_For_Argument_F_Value : aliased constant Text_Type :=
        "F_Value";
      Member_Doc_For_Argument_F_Value  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Argument_F_Value  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Argument_F_Value'Access,
         Documentation => Member_Doc_For_Argument_F_Value'Access,
         Owner         => Type_Index_For_Argument,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Argument_F_Value'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Import_F_Imported_Names : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Import => 1);

      Member_Name_For_Import_F_Imported_Names : aliased constant Text_Type :=
        "F_Imported_Names";
      Member_Doc_For_Import_F_Imported_Names  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Import_F_Imported_Names  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Import_F_Imported_Names'Access,
         Documentation => Member_Doc_For_Import_F_Imported_Names'Access,
         Owner         => Type_Index_For_Import,
         Member_Type   => Type_Index_For_Imported_Name_List,
         Null_For      => null,
         Indexes       => Indexes_For_Import_F_Imported_Names'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Import_All_From_F_Module_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Import_All_From => 1);

      Member_Name_For_Import_All_From_F_Module_Name : aliased constant Text_Type :=
        "F_Module_Name";
      Member_Doc_For_Import_All_From_F_Module_Name  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Import_All_From_F_Module_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Import_All_From_F_Module_Name'Access,
         Documentation => Member_Doc_For_Import_All_From_F_Module_Name'Access,
         Owner         => Type_Index_For_Import_All_From,
         Member_Type   => Type_Index_For_Module_Id,
         Null_For      => null,
         Indexes       => Indexes_For_Import_All_From_F_Module_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Import_From_F_Module_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Import_From => 1);

      Member_Name_For_Import_From_F_Module_Name : aliased constant Text_Type :=
        "F_Module_Name";
      Member_Doc_For_Import_From_F_Module_Name  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Import_From_F_Module_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Import_From_F_Module_Name'Access,
         Documentation => Member_Doc_For_Import_From_F_Module_Name'Access,
         Owner         => Type_Index_For_Import_From,
         Member_Type   => Type_Index_For_Module_Id,
         Null_For      => null,
         Indexes       => Indexes_For_Import_From_F_Module_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Import_From_F_Imported_Names : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Import_From => 2);

      Member_Name_For_Import_From_F_Imported_Names : aliased constant Text_Type :=
        "F_Imported_Names";
      Member_Doc_For_Import_From_F_Imported_Names  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Import_From_F_Imported_Names  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Import_From_F_Imported_Names'Access,
         Documentation => Member_Doc_For_Import_From_F_Imported_Names'Access,
         Owner         => Type_Index_For_Import_From,
         Member_Type   => Type_Index_For_Imported_Name_List,
         Null_For      => null,
         Indexes       => Indexes_For_Import_From_F_Imported_Names'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Lexer_Case_Rule_Cond_Alt => 1);

      Member_Name_For_Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs : aliased constant Text_Type :=
        "F_Cond_Exprs";
      Member_Doc_For_Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs'Access,
         Documentation => Member_Doc_For_Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs'Access,
         Owner         => Type_Index_For_Lexer_Case_Rule_Cond_Alt,
         Member_Type   => Type_Index_For_Ref_Id_List,
         Null_For      => null,
         Indexes       => Indexes_For_Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Lexer_Case_Rule_Cond_Alt_F_Send : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Lexer_Case_Rule_Cond_Alt => 2);

      Member_Name_For_Lexer_Case_Rule_Cond_Alt_F_Send : aliased constant Text_Type :=
        "F_Send";
      Member_Doc_For_Lexer_Case_Rule_Cond_Alt_F_Send  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Lexer_Case_Rule_Cond_Alt_F_Send  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lexer_Case_Rule_Cond_Alt_F_Send'Access,
         Documentation => Member_Doc_For_Lexer_Case_Rule_Cond_Alt_F_Send'Access,
         Owner         => Type_Index_For_Lexer_Case_Rule_Cond_Alt,
         Member_Type   => Type_Index_For_Lexer_Case_Rule_Send,
         Null_For      => null,
         Indexes       => Indexes_For_Lexer_Case_Rule_Cond_Alt_F_Send'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Lexer_Case_Rule_Default_Alt_F_Send : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Lexer_Case_Rule_Default_Alt => 1);

      Member_Name_For_Lexer_Case_Rule_Default_Alt_F_Send : aliased constant Text_Type :=
        "F_Send";
      Member_Doc_For_Lexer_Case_Rule_Default_Alt_F_Send  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Lexer_Case_Rule_Default_Alt_F_Send  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lexer_Case_Rule_Default_Alt_F_Send'Access,
         Documentation => Member_Doc_For_Lexer_Case_Rule_Default_Alt_F_Send'Access,
         Owner         => Type_Index_For_Lexer_Case_Rule_Default_Alt,
         Member_Type   => Type_Index_For_Lexer_Case_Rule_Send,
         Null_For      => null,
         Indexes       => Indexes_For_Lexer_Case_Rule_Default_Alt_F_Send'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Base_Match_Branch_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Base_Match_Branch => 0,
Type_Index_For_Match_Branch => 2,
Type_Index_For_Pattern_Match_Branch => 2);

      Member_Name_For_Base_Match_Branch_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Base_Match_Branch_F_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Base_Match_Branch_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Base_Match_Branch_F_Expr'Access,
         Documentation => Member_Doc_For_Base_Match_Branch_F_Expr'Access,
         Owner         => Type_Index_For_Base_Match_Branch,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Base_Match_Branch_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Match_Branch_F_Decl : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Match_Branch => 1);

      Member_Name_For_Match_Branch_F_Decl : aliased constant Text_Type :=
        "F_Decl";
      Member_Doc_For_Match_Branch_F_Decl  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Match_Branch_F_Decl  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Match_Branch_F_Decl'Access,
         Documentation => Member_Doc_For_Match_Branch_F_Decl'Access,
         Owner         => Type_Index_For_Match_Branch,
         Member_Type   => Type_Index_For_Match_Val_Decl,
         Null_For      => null,
         Indexes       => Indexes_For_Match_Branch_F_Decl'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Pattern_Match_Branch_F_Pattern : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Pattern_Match_Branch => 1);

      Member_Name_For_Pattern_Match_Branch_F_Pattern : aliased constant Text_Type :=
        "F_Pattern";
      Member_Doc_For_Pattern_Match_Branch_F_Pattern  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ComplexPattern`, :typeref:`NotPattern`, :typeref:`OrPattern`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Pattern_Match_Branch_F_Pattern  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Pattern_Match_Branch_F_Pattern'Access,
         Documentation => Member_Doc_For_Pattern_Match_Branch_F_Pattern'Access,
         Owner         => Type_Index_For_Pattern_Match_Branch,
         Member_Type   => Type_Index_For_Pattern,
         Null_For      => null,
         Indexes       => Indexes_For_Pattern_Match_Branch_F_Pattern'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Block_Expr_Clause_F_Clause : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Block_Expr_Clause => 1);

      Member_Name_For_Block_Expr_Clause_F_Clause : aliased constant Text_Type :=
        "F_Clause";
      Member_Doc_For_Block_Expr_Clause_F_Clause  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`FullDecl`, :typeref:`VarBind`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Block_Expr_Clause_F_Clause  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Block_Expr_Clause_F_Clause'Access,
         Documentation => Member_Doc_For_Block_Expr_Clause_F_Clause'Access,
         Owner         => Type_Index_For_Block_Expr_Clause,
         Member_Type   => Type_Index_For_Lkt_Node,
         Null_For      => null,
         Indexes       => Indexes_For_Block_Expr_Clause_F_Clause'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      
         Null_For_Decl_F_Syn_Name : aliased constant Type_Flags := (Type_Index_For_Decl => False,
Type_Index_For_Base_Grammar_Rule_Decl => False,
Type_Index_For_Grammar_Rule_Decl => False,
Type_Index_For_Synthetic_Lexer_Decl => True,
Type_Index_For_Base_Val_Decl => False,
Type_Index_For_Node_Decl => True,
Type_Index_For_Self_Decl => True,
Type_Index_For_User_Val_Decl => False,
Type_Index_For_Binding_Val_Decl => False,
Type_Index_For_Enum_Lit_Decl => False,
Type_Index_For_Explicitly_Typed_Decl => False,
Type_Index_For_Component_Decl => False,
Type_Index_For_Field_Decl => False,
Type_Index_For_Fun_Param_Decl => False,
Type_Index_For_Lambda_Param_Decl => False,
Type_Index_For_Dyn_Var_Decl => False,
Type_Index_For_Match_Val_Decl => False,
Type_Index_For_Val_Decl => False,
Type_Index_For_Fun_Decl => False,
Type_Index_For_Env_Spec_Decl => False,
Type_Index_For_Error_Decl => True,
Type_Index_For_Generic_Decl => True,
Type_Index_For_Grammar_Decl => False,
Type_Index_For_Langkit_Root => True,
Type_Index_For_Lexer_Decl => False,
Type_Index_For_Lexer_Family_Decl => False,
Type_Index_For_Synth_Fun_Decl => True,
Type_Index_For_Synth_Param_Decl => True,
Type_Index_For_Type_Decl => False,
Type_Index_For_Any_Type_Decl => True,
Type_Index_For_Enum_Class_Alt_Decl => False,
Type_Index_For_Function_Type => True,
Type_Index_For_Generic_Param_Type_Decl => False,
Type_Index_For_Named_Type_Decl => False,
Type_Index_For_Basic_Class_Decl => False,
Type_Index_For_Class_Decl => False,
Type_Index_For_Enum_Class_Decl => False,
Type_Index_For_Enum_Type_Decl => False,
Type_Index_For_Struct_Decl => False,
Type_Index_For_Trait_Decl => False);

      
         Indexes_For_Decl_F_Syn_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Decl => 0,
Type_Index_For_Base_Grammar_Rule_Decl => 0,
Type_Index_For_Grammar_Rule_Decl => 1,
Type_Index_For_Synthetic_Lexer_Decl => 0,
Type_Index_For_Base_Val_Decl => 0,
Type_Index_For_Node_Decl => 0,
Type_Index_For_Self_Decl => 0,
Type_Index_For_User_Val_Decl => 0,
Type_Index_For_Binding_Val_Decl => 1,
Type_Index_For_Enum_Lit_Decl => 1,
Type_Index_For_Explicitly_Typed_Decl => 0,
Type_Index_For_Component_Decl => 0,
Type_Index_For_Field_Decl => 1,
Type_Index_For_Fun_Param_Decl => 2,
Type_Index_For_Lambda_Param_Decl => 1,
Type_Index_For_Dyn_Var_Decl => 1,
Type_Index_For_Match_Val_Decl => 1,
Type_Index_For_Val_Decl => 1,
Type_Index_For_Fun_Decl => 1,
Type_Index_For_Env_Spec_Decl => 1,
Type_Index_For_Error_Decl => 0,
Type_Index_For_Generic_Decl => 0,
Type_Index_For_Grammar_Decl => 1,
Type_Index_For_Langkit_Root => 0,
Type_Index_For_Lexer_Decl => 1,
Type_Index_For_Lexer_Family_Decl => 1,
Type_Index_For_Synth_Fun_Decl => 0,
Type_Index_For_Synth_Param_Decl => 0,
Type_Index_For_Type_Decl => 0,
Type_Index_For_Any_Type_Decl => 0,
Type_Index_For_Enum_Class_Alt_Decl => 1,
Type_Index_For_Function_Type => 0,
Type_Index_For_Generic_Param_Type_Decl => 2,
Type_Index_For_Named_Type_Decl => 0,
Type_Index_For_Basic_Class_Decl => 0,
Type_Index_For_Class_Decl => 1,
Type_Index_For_Enum_Class_Decl => 1,
Type_Index_For_Enum_Type_Decl => 1,
Type_Index_For_Struct_Decl => 1,
Type_Index_For_Trait_Decl => 1);

      Member_Name_For_Decl_F_Syn_Name : aliased constant Text_Type :=
        "F_Syn_Name";
      Member_Doc_For_Decl_F_Syn_Name  : aliased constant Text_Type := "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Decl_F_Syn_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decl_F_Syn_Name'Access,
         Documentation => Member_Doc_For_Decl_F_Syn_Name'Access,
         Owner         => Type_Index_For_Decl,
         Member_Type   => Type_Index_For_Def_Id,
         Null_For      => Null_For_Decl_F_Syn_Name'Access,
         Indexes       => Indexes_For_Decl_F_Syn_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      
         Null_For_Base_Grammar_Rule_Decl_F_Expr : aliased constant Type_Flags := (Type_Index_For_Base_Grammar_Rule_Decl => False,
Type_Index_For_Grammar_Rule_Decl => False,
Type_Index_For_Synthetic_Lexer_Decl => True);

      
         Indexes_For_Base_Grammar_Rule_Decl_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Base_Grammar_Rule_Decl => 0,
Type_Index_For_Grammar_Rule_Decl => 2,
Type_Index_For_Synthetic_Lexer_Decl => 0);

      Member_Name_For_Base_Grammar_Rule_Decl_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Base_Grammar_Rule_Decl_F_Expr  : aliased constant Text_Type := "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Base_Grammar_Rule_Decl_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Base_Grammar_Rule_Decl_F_Expr'Access,
         Documentation => Member_Doc_For_Base_Grammar_Rule_Decl_F_Expr'Access,
         Owner         => Type_Index_For_Base_Grammar_Rule_Decl,
         Member_Type   => Type_Index_For_Grammar_Expr,
         Null_For      => Null_For_Base_Grammar_Rule_Decl_F_Expr'Access,
         Indexes       => Indexes_For_Base_Grammar_Rule_Decl_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Explicitly_Typed_Decl_F_Decl_Type : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Explicitly_Typed_Decl => 0,
Type_Index_For_Component_Decl => 0,
Type_Index_For_Field_Decl => 2,
Type_Index_For_Fun_Param_Decl => 3,
Type_Index_For_Lambda_Param_Decl => 2,
Type_Index_For_Dyn_Var_Decl => 2,
Type_Index_For_Match_Val_Decl => 2,
Type_Index_For_Val_Decl => 2);

      Member_Name_For_Explicitly_Typed_Decl_F_Decl_Type : aliased constant Text_Type :=
        "F_Decl_Type";
      Member_Doc_For_Explicitly_Typed_Decl_F_Decl_Type  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`FunctionTypeRef`, :typeref:`GenericTypeRef`, :typeref:`SimpleTypeRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Explicitly_Typed_Decl_F_Decl_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Explicitly_Typed_Decl_F_Decl_Type'Access,
         Documentation => Member_Doc_For_Explicitly_Typed_Decl_F_Decl_Type'Access,
         Owner         => Type_Index_For_Explicitly_Typed_Decl,
         Member_Type   => Type_Index_For_Type_Ref,
         Null_For      => null,
         Indexes       => Indexes_For_Explicitly_Typed_Decl_F_Decl_Type'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Component_Decl_F_Default_Val : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Component_Decl => 0,
Type_Index_For_Field_Decl => 4,
Type_Index_For_Fun_Param_Decl => 4,
Type_Index_For_Lambda_Param_Decl => 3);

      Member_Name_For_Component_Decl_F_Default_Val : aliased constant Text_Type :=
        "F_Default_Val";
      Member_Doc_For_Component_Decl_F_Default_Val  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Component_Decl_F_Default_Val  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Component_Decl_F_Default_Val'Access,
         Documentation => Member_Doc_For_Component_Decl_F_Default_Val'Access,
         Owner         => Type_Index_For_Component_Decl,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Component_Decl_F_Default_Val'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Field_Decl_F_Trait_Ref : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Field_Decl => 3);

      Member_Name_For_Field_Decl_F_Trait_Ref : aliased constant Text_Type :=
        "F_Trait_Ref";
      Member_Doc_For_Field_Decl_F_Trait_Ref  : aliased constant Text_Type := "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Field_Decl_F_Trait_Ref  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Field_Decl_F_Trait_Ref'Access,
         Documentation => Member_Doc_For_Field_Decl_F_Trait_Ref'Access,
         Owner         => Type_Index_For_Field_Decl,
         Member_Type   => Type_Index_For_Dot_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Field_Decl_F_Trait_Ref'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Fun_Param_Decl_F_Decl_Annotations : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Fun_Param_Decl => 1);

      Member_Name_For_Fun_Param_Decl_F_Decl_Annotations : aliased constant Text_Type :=
        "F_Decl_Annotations";
      Member_Doc_For_Fun_Param_Decl_F_Decl_Annotations  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Fun_Param_Decl_F_Decl_Annotations  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Fun_Param_Decl_F_Decl_Annotations'Access,
         Documentation => Member_Doc_For_Fun_Param_Decl_F_Decl_Annotations'Access,
         Owner         => Type_Index_For_Fun_Param_Decl,
         Member_Type   => Type_Index_For_Decl_Annotation_List,
         Null_For      => null,
         Indexes       => Indexes_For_Fun_Param_Decl_F_Decl_Annotations'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Val_Decl_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Val_Decl => 3);

      Member_Name_For_Val_Decl_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Val_Decl_F_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Val_Decl_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Val_Decl_F_Expr'Access,
         Documentation => Member_Doc_For_Val_Decl_F_Expr'Access,
         Owner         => Type_Index_For_Val_Decl,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Val_Decl_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Fun_Decl_F_Params : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Fun_Decl => 2);

      Member_Name_For_Fun_Decl_F_Params : aliased constant Text_Type :=
        "F_Params";
      Member_Doc_For_Fun_Decl_F_Params  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Fun_Decl_F_Params  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Fun_Decl_F_Params'Access,
         Documentation => Member_Doc_For_Fun_Decl_F_Params'Access,
         Owner         => Type_Index_For_Fun_Decl,
         Member_Type   => Type_Index_For_Fun_Param_Decl_List,
         Null_For      => null,
         Indexes       => Indexes_For_Fun_Decl_F_Params'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Fun_Decl_F_Return_Type : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Fun_Decl => 3);

      Member_Name_For_Fun_Decl_F_Return_Type : aliased constant Text_Type :=
        "F_Return_Type";
      Member_Doc_For_Fun_Decl_F_Return_Type  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`FunctionTypeRef`, :typeref:`GenericTypeRef`, :typeref:`SimpleTypeRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Fun_Decl_F_Return_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Fun_Decl_F_Return_Type'Access,
         Documentation => Member_Doc_For_Fun_Decl_F_Return_Type'Access,
         Owner         => Type_Index_For_Fun_Decl,
         Member_Type   => Type_Index_For_Type_Ref,
         Null_For      => null,
         Indexes       => Indexes_For_Fun_Decl_F_Return_Type'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Fun_Decl_F_Trait_Ref : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Fun_Decl => 4);

      Member_Name_For_Fun_Decl_F_Trait_Ref : aliased constant Text_Type :=
        "F_Trait_Ref";
      Member_Doc_For_Fun_Decl_F_Trait_Ref  : aliased constant Text_Type := "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Fun_Decl_F_Trait_Ref  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Fun_Decl_F_Trait_Ref'Access,
         Documentation => Member_Doc_For_Fun_Decl_F_Trait_Ref'Access,
         Owner         => Type_Index_For_Fun_Decl,
         Member_Type   => Type_Index_For_Dot_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Fun_Decl_F_Trait_Ref'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Fun_Decl_F_Body : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Fun_Decl => 5);

      Member_Name_For_Fun_Decl_F_Body : aliased constant Text_Type :=
        "F_Body";
      Member_Doc_For_Fun_Decl_F_Body  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Fun_Decl_F_Body  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Fun_Decl_F_Body'Access,
         Documentation => Member_Doc_For_Fun_Decl_F_Body'Access,
         Owner         => Type_Index_For_Fun_Decl,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Fun_Decl_F_Body'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Env_Spec_Decl_F_Actions : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Env_Spec_Decl => 2);

      Member_Name_For_Env_Spec_Decl_F_Actions : aliased constant Text_Type :=
        "F_Actions";
      Member_Doc_For_Env_Spec_Decl_F_Actions  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Env_Spec_Decl_F_Actions  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Env_Spec_Decl_F_Actions'Access,
         Documentation => Member_Doc_For_Env_Spec_Decl_F_Actions'Access,
         Owner         => Type_Index_For_Env_Spec_Decl,
         Member_Type   => Type_Index_For_Call_Expr_List,
         Null_For      => null,
         Indexes       => Indexes_For_Env_Spec_Decl_F_Actions'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Generic_Decl_F_Generic_Param_Decls : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Generic_Decl => 1);

      Member_Name_For_Generic_Decl_F_Generic_Param_Decls : aliased constant Text_Type :=
        "F_Generic_Param_Decls";
      Member_Doc_For_Generic_Decl_F_Generic_Param_Decls  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Generic_Decl_F_Generic_Param_Decls  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Generic_Decl_F_Generic_Param_Decls'Access,
         Documentation => Member_Doc_For_Generic_Decl_F_Generic_Param_Decls'Access,
         Owner         => Type_Index_For_Generic_Decl,
         Member_Type   => Type_Index_For_Generic_Param_Decl_List,
         Null_For      => null,
         Indexes       => Indexes_For_Generic_Decl_F_Generic_Param_Decls'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Generic_Decl_F_Decl : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Generic_Decl => 2);

      Member_Name_For_Generic_Decl_F_Decl : aliased constant Text_Type :=
        "F_Decl";
      Member_Doc_For_Generic_Decl_F_Decl  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`DynVarDecl`, :typeref:`EnvSpecDecl`, :typeref:`ErrorDecl`, :typeref:`FieldDecl`, :typeref:`FunDecl`, :typeref:`GenericDecl`, :typeref:`GrammarDecl`, :typeref:`GrammarRuleDecl`, :typeref:`LexerDecl`, :typeref:`LexerFamilyDecl`, :typeref:`NamedTypeDecl`, :typeref:`ValDecl`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Generic_Decl_F_Decl  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Generic_Decl_F_Decl'Access,
         Documentation => Member_Doc_For_Generic_Decl_F_Decl'Access,
         Owner         => Type_Index_For_Generic_Decl,
         Member_Type   => Type_Index_For_Decl,
         Null_For      => null,
         Indexes       => Indexes_For_Generic_Decl_F_Decl'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_Decl_F_Rules : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_Decl => 2);

      Member_Name_For_Grammar_Decl_F_Rules : aliased constant Text_Type :=
        "F_Rules";
      Member_Doc_For_Grammar_Decl_F_Rules  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_Decl_F_Rules  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_Decl_F_Rules'Access,
         Documentation => Member_Doc_For_Grammar_Decl_F_Rules'Access,
         Owner         => Type_Index_For_Grammar_Decl,
         Member_Type   => Type_Index_For_Full_Decl_List,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_Decl_F_Rules'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Langkit_Root_F_Doc : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Langkit_Root => 1);

      Member_Name_For_Langkit_Root_F_Doc : aliased constant Text_Type :=
        "F_Doc";
      Member_Doc_For_Langkit_Root_F_Doc  : aliased constant Text_Type := "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Langkit_Root_F_Doc  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Langkit_Root_F_Doc'Access,
         Documentation => Member_Doc_For_Langkit_Root_F_Doc'Access,
         Owner         => Type_Index_For_Langkit_Root,
         Member_Type   => Type_Index_For_Module_Doc_String_Lit,
         Null_For      => null,
         Indexes       => Indexes_For_Langkit_Root_F_Doc'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Langkit_Root_F_Imports : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Langkit_Root => 2);

      Member_Name_For_Langkit_Root_F_Imports : aliased constant Text_Type :=
        "F_Imports";
      Member_Doc_For_Langkit_Root_F_Imports  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Langkit_Root_F_Imports  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Langkit_Root_F_Imports'Access,
         Documentation => Member_Doc_For_Langkit_Root_F_Imports'Access,
         Owner         => Type_Index_For_Langkit_Root,
         Member_Type   => Type_Index_For_Base_Import_List,
         Null_For      => null,
         Indexes       => Indexes_For_Langkit_Root_F_Imports'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Langkit_Root_F_Decls : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Langkit_Root => 3);

      Member_Name_For_Langkit_Root_F_Decls : aliased constant Text_Type :=
        "F_Decls";
      Member_Doc_For_Langkit_Root_F_Decls  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Langkit_Root_F_Decls  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Langkit_Root_F_Decls'Access,
         Documentation => Member_Doc_For_Langkit_Root_F_Decls'Access,
         Owner         => Type_Index_For_Langkit_Root,
         Member_Type   => Type_Index_For_Full_Decl_List,
         Null_For      => null,
         Indexes       => Indexes_For_Langkit_Root_F_Decls'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Lexer_Decl_F_Rules : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Lexer_Decl => 2);

      Member_Name_For_Lexer_Decl_F_Rules : aliased constant Text_Type :=
        "F_Rules";
      Member_Doc_For_Lexer_Decl_F_Rules  : aliased constant Text_Type := "This field contains a list that itself contains one of the following nodes: :typeref:`FullDecl`, :typeref:`LexerCaseRule`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Lexer_Decl_F_Rules  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lexer_Decl_F_Rules'Access,
         Documentation => Member_Doc_For_Lexer_Decl_F_Rules'Access,
         Owner         => Type_Index_For_Lexer_Decl,
         Member_Type   => Type_Index_For_Lkt_Node_List,
         Null_For      => null,
         Indexes       => Indexes_For_Lexer_Decl_F_Rules'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Lexer_Family_Decl_F_Rules : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Lexer_Family_Decl => 2);

      Member_Name_For_Lexer_Family_Decl_F_Rules : aliased constant Text_Type :=
        "F_Rules";
      Member_Doc_For_Lexer_Family_Decl_F_Rules  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Lexer_Family_Decl_F_Rules  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lexer_Family_Decl_F_Rules'Access,
         Documentation => Member_Doc_For_Lexer_Family_Decl_F_Rules'Access,
         Owner         => Type_Index_For_Lexer_Family_Decl,
         Member_Type   => Type_Index_For_Full_Decl_List,
         Null_For      => null,
         Indexes       => Indexes_For_Lexer_Family_Decl_F_Rules'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Type_Decl_F_Traits : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Type_Decl => 0,
Type_Index_For_Any_Type_Decl => 1,
Type_Index_For_Enum_Class_Alt_Decl => 2,
Type_Index_For_Function_Type => 1,
Type_Index_For_Generic_Param_Type_Decl => 3,
Type_Index_For_Named_Type_Decl => 0,
Type_Index_For_Basic_Class_Decl => 0,
Type_Index_For_Class_Decl => 3,
Type_Index_For_Enum_Class_Decl => 3,
Type_Index_For_Enum_Type_Decl => 2,
Type_Index_For_Struct_Decl => 2,
Type_Index_For_Trait_Decl => 2);

      Member_Name_For_Type_Decl_F_Traits : aliased constant Text_Type :=
        "F_Traits";
      Member_Doc_For_Type_Decl_F_Traits  : aliased constant Text_Type := "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Type_Decl_F_Traits  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Type_Decl_F_Traits'Access,
         Documentation => Member_Doc_For_Type_Decl_F_Traits'Access,
         Owner         => Type_Index_For_Type_Decl,
         Member_Type   => Type_Index_For_Type_Ref_List,
         Null_For      => null,
         Indexes       => Indexes_For_Type_Decl_F_Traits'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      
         Null_For_Type_Decl_F_Syn_Base_Type : aliased constant Type_Flags := (Type_Index_For_Type_Decl => False,
Type_Index_For_Any_Type_Decl => True,
Type_Index_For_Enum_Class_Alt_Decl => True,
Type_Index_For_Function_Type => True,
Type_Index_For_Generic_Param_Type_Decl => True,
Type_Index_For_Named_Type_Decl => False,
Type_Index_For_Basic_Class_Decl => False,
Type_Index_For_Class_Decl => False,
Type_Index_For_Enum_Class_Decl => False,
Type_Index_For_Enum_Type_Decl => True,
Type_Index_For_Struct_Decl => True,
Type_Index_For_Trait_Decl => True);

      
         Indexes_For_Type_Decl_F_Syn_Base_Type : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Type_Decl => 0,
Type_Index_For_Any_Type_Decl => 0,
Type_Index_For_Enum_Class_Alt_Decl => 0,
Type_Index_For_Function_Type => 0,
Type_Index_For_Generic_Param_Type_Decl => 0,
Type_Index_For_Named_Type_Decl => 0,
Type_Index_For_Basic_Class_Decl => 0,
Type_Index_For_Class_Decl => 2,
Type_Index_For_Enum_Class_Decl => 2,
Type_Index_For_Enum_Type_Decl => 0,
Type_Index_For_Struct_Decl => 0,
Type_Index_For_Trait_Decl => 0);

      Member_Name_For_Type_Decl_F_Syn_Base_Type : aliased constant Text_Type :=
        "F_Syn_Base_Type";
      Member_Doc_For_Type_Decl_F_Syn_Base_Type  : aliased constant Text_Type := "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Type_Decl_F_Syn_Base_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Type_Decl_F_Syn_Base_Type'Access,
         Documentation => Member_Doc_For_Type_Decl_F_Syn_Base_Type'Access,
         Owner         => Type_Index_For_Type_Decl,
         Member_Type   => Type_Index_For_Type_Ref,
         Null_For      => Null_For_Type_Decl_F_Syn_Base_Type'Access,
         Indexes       => Indexes_For_Type_Decl_F_Syn_Base_Type'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Generic_Param_Type_Decl_F_Has_Class : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Generic_Param_Type_Decl => 1);

      Member_Name_For_Generic_Param_Type_Decl_F_Has_Class : aliased constant Text_Type :=
        "F_Has_Class";
      Member_Doc_For_Generic_Param_Type_Decl_F_Has_Class  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Generic_Param_Type_Decl_F_Has_Class  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Generic_Param_Type_Decl_F_Has_Class'Access,
         Documentation => Member_Doc_For_Generic_Param_Type_Decl_F_Has_Class'Access,
         Owner         => Type_Index_For_Generic_Param_Type_Decl,
         Member_Type   => Type_Index_For_Class_Qualifier,
         Null_For      => null,
         Indexes       => Indexes_For_Generic_Param_Type_Decl_F_Has_Class'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Named_Type_Decl_F_Decls : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Named_Type_Decl => 0,
Type_Index_For_Basic_Class_Decl => 0,
Type_Index_For_Class_Decl => 4,
Type_Index_For_Enum_Class_Decl => 5,
Type_Index_For_Enum_Type_Decl => 4,
Type_Index_For_Struct_Decl => 3,
Type_Index_For_Trait_Decl => 3);

      Member_Name_For_Named_Type_Decl_F_Decls : aliased constant Text_Type :=
        "F_Decls";
      Member_Doc_For_Named_Type_Decl_F_Decls  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Named_Type_Decl_F_Decls  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Named_Type_Decl_F_Decls'Access,
         Documentation => Member_Doc_For_Named_Type_Decl_F_Decls'Access,
         Owner         => Type_Index_For_Named_Type_Decl,
         Member_Type   => Type_Index_For_Decl_Block,
         Null_For      => null,
         Indexes       => Indexes_For_Named_Type_Decl_F_Decls'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Enum_Class_Decl_F_Branches : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Enum_Class_Decl => 4);

      Member_Name_For_Enum_Class_Decl_F_Branches : aliased constant Text_Type :=
        "F_Branches";
      Member_Doc_For_Enum_Class_Decl_F_Branches  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Enum_Class_Decl_F_Branches  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Enum_Class_Decl_F_Branches'Access,
         Documentation => Member_Doc_For_Enum_Class_Decl_F_Branches'Access,
         Owner         => Type_Index_For_Enum_Class_Decl,
         Member_Type   => Type_Index_For_Enum_Class_Case_List,
         Null_For      => null,
         Indexes       => Indexes_For_Enum_Class_Decl_F_Branches'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Enum_Type_Decl_F_Literals : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Enum_Type_Decl => 3);

      Member_Name_For_Enum_Type_Decl_F_Literals : aliased constant Text_Type :=
        "F_Literals";
      Member_Doc_For_Enum_Type_Decl_F_Literals  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Enum_Type_Decl_F_Literals  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Enum_Type_Decl_F_Literals'Access,
         Documentation => Member_Doc_For_Enum_Type_Decl_F_Literals'Access,
         Owner         => Type_Index_For_Enum_Type_Decl,
         Member_Type   => Type_Index_For_Enum_Lit_Decl_List,
         Null_For      => null,
         Indexes       => Indexes_For_Enum_Type_Decl_F_Literals'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Decl_Annotation_F_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Decl_Annotation => 1);

      Member_Name_For_Decl_Annotation_F_Name : aliased constant Text_Type :=
        "F_Name";
      Member_Doc_For_Decl_Annotation_F_Name  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Decl_Annotation_F_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decl_Annotation_F_Name'Access,
         Documentation => Member_Doc_For_Decl_Annotation_F_Name'Access,
         Owner         => Type_Index_For_Decl_Annotation,
         Member_Type   => Type_Index_For_Id,
         Null_For      => null,
         Indexes       => Indexes_For_Decl_Annotation_F_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Decl_Annotation_F_Args : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Decl_Annotation => 2);

      Member_Name_For_Decl_Annotation_F_Args : aliased constant Text_Type :=
        "F_Args";
      Member_Doc_For_Decl_Annotation_F_Args  : aliased constant Text_Type := "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Decl_Annotation_F_Args  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decl_Annotation_F_Args'Access,
         Documentation => Member_Doc_For_Decl_Annotation_F_Args'Access,
         Owner         => Type_Index_For_Decl_Annotation,
         Member_Type   => Type_Index_For_Decl_Annotation_Args,
         Null_For      => null,
         Indexes       => Indexes_For_Decl_Annotation_F_Args'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Decl_Annotation_Args_F_Args : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Decl_Annotation_Args => 1);

      Member_Name_For_Decl_Annotation_Args_F_Args : aliased constant Text_Type :=
        "F_Args";
      Member_Doc_For_Decl_Annotation_Args_F_Args  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Decl_Annotation_Args_F_Args  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decl_Annotation_Args_F_Args'Access,
         Documentation => Member_Doc_For_Decl_Annotation_Args_F_Args'Access,
         Owner         => Type_Index_For_Decl_Annotation_Args,
         Member_Type   => Type_Index_For_Argument_List,
         Null_For      => null,
         Indexes       => Indexes_For_Decl_Annotation_Args_F_Args'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Elsif_Branch_F_Cond_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Elsif_Branch => 1);

      Member_Name_For_Elsif_Branch_F_Cond_Expr : aliased constant Text_Type :=
        "F_Cond_Expr";
      Member_Doc_For_Elsif_Branch_F_Cond_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Elsif_Branch_F_Cond_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Elsif_Branch_F_Cond_Expr'Access,
         Documentation => Member_Doc_For_Elsif_Branch_F_Cond_Expr'Access,
         Owner         => Type_Index_For_Elsif_Branch,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Elsif_Branch_F_Cond_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Elsif_Branch_F_Then_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Elsif_Branch => 2);

      Member_Name_For_Elsif_Branch_F_Then_Expr : aliased constant Text_Type :=
        "F_Then_Expr";
      Member_Doc_For_Elsif_Branch_F_Then_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Elsif_Branch_F_Then_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Elsif_Branch_F_Then_Expr'Access,
         Documentation => Member_Doc_For_Elsif_Branch_F_Then_Expr'Access,
         Owner         => Type_Index_For_Elsif_Branch,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Elsif_Branch_F_Then_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Enum_Class_Case_F_Decls : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Enum_Class_Case => 1);

      Member_Name_For_Enum_Class_Case_F_Decls : aliased constant Text_Type :=
        "F_Decls";
      Member_Doc_For_Enum_Class_Case_F_Decls  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Enum_Class_Case_F_Decls  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Enum_Class_Case_F_Decls'Access,
         Documentation => Member_Doc_For_Enum_Class_Case_F_Decls'Access,
         Owner         => Type_Index_For_Enum_Class_Case,
         Member_Type   => Type_Index_For_Enum_Class_Alt_Decl_List,
         Null_For      => null,
         Indexes       => Indexes_For_Enum_Class_Case_F_Decls'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Any_Of_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Any_Of => 1);

      Member_Name_For_Any_Of_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Any_Of_F_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Any_Of_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Any_Of_F_Expr'Access,
         Documentation => Member_Doc_For_Any_Of_F_Expr'Access,
         Owner         => Type_Index_For_Any_Of,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Any_Of_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Any_Of_F_Values : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Any_Of => 2);

      Member_Name_For_Any_Of_F_Values : aliased constant Text_Type :=
        "F_Values";
      Member_Doc_For_Any_Of_F_Values  : aliased constant Text_Type := "This field contains a list that itself contains one of the following nodes: :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Any_Of_F_Values  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Any_Of_F_Values'Access,
         Documentation => Member_Doc_For_Any_Of_F_Values'Access,
         Owner         => Type_Index_For_Any_Of,
         Member_Type   => Type_Index_For_Any_Of_List,
         Null_For      => null,
         Indexes       => Indexes_For_Any_Of_F_Values'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Array_Literal_F_Exprs : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Array_Literal => 1);

      Member_Name_For_Array_Literal_F_Exprs : aliased constant Text_Type :=
        "F_Exprs";
      Member_Doc_For_Array_Literal_F_Exprs  : aliased constant Text_Type := "This field contains a list that itself contains one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Array_Literal_F_Exprs  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Array_Literal_F_Exprs'Access,
         Documentation => Member_Doc_For_Array_Literal_F_Exprs'Access,
         Owner         => Type_Index_For_Array_Literal,
         Member_Type   => Type_Index_For_Expr_List,
         Null_For      => null,
         Indexes       => Indexes_For_Array_Literal_F_Exprs'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Array_Literal_F_Element_Type : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Array_Literal => 2);

      Member_Name_For_Array_Literal_F_Element_Type : aliased constant Text_Type :=
        "F_Element_Type";
      Member_Doc_For_Array_Literal_F_Element_Type  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`FunctionTypeRef`, :typeref:`GenericTypeRef`, :typeref:`SimpleTypeRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Array_Literal_F_Element_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Array_Literal_F_Element_Type'Access,
         Documentation => Member_Doc_For_Array_Literal_F_Element_Type'Access,
         Owner         => Type_Index_For_Array_Literal,
         Member_Type   => Type_Index_For_Type_Ref,
         Null_For      => null,
         Indexes       => Indexes_For_Array_Literal_F_Element_Type'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Base_Call_Expr_F_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Base_Call_Expr => 0,
Type_Index_For_Call_Expr => 1,
Type_Index_For_Logic_Call_Expr => 0,
Type_Index_For_Logic_Predicate => 1,
Type_Index_For_Logic_Propagate_Call => 1);

      Member_Name_For_Base_Call_Expr_F_Name : aliased constant Text_Type :=
        "F_Name";
      Member_Doc_For_Base_Call_Expr_F_Name  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`KeepExpr`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Base_Call_Expr_F_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Base_Call_Expr_F_Name'Access,
         Documentation => Member_Doc_For_Base_Call_Expr_F_Name'Access,
         Owner         => Type_Index_For_Base_Call_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Base_Call_Expr_F_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Base_Call_Expr_F_Args : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Base_Call_Expr => 0,
Type_Index_For_Call_Expr => 2,
Type_Index_For_Logic_Call_Expr => 0,
Type_Index_For_Logic_Predicate => 2,
Type_Index_For_Logic_Propagate_Call => 2);

      Member_Name_For_Base_Call_Expr_F_Args : aliased constant Text_Type :=
        "F_Args";
      Member_Doc_For_Base_Call_Expr_F_Args  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Base_Call_Expr_F_Args  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Base_Call_Expr_F_Args'Access,
         Documentation => Member_Doc_For_Base_Call_Expr_F_Args'Access,
         Owner         => Type_Index_For_Base_Call_Expr,
         Member_Type   => Type_Index_For_Argument_List,
         Null_For      => null,
         Indexes       => Indexes_For_Base_Call_Expr_F_Args'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Bin_Op_F_Left : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Bin_Op => 1);

      Member_Name_For_Bin_Op_F_Left : aliased constant Text_Type :=
        "F_Left";
      Member_Doc_For_Bin_Op_F_Left  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Bin_Op_F_Left  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Bin_Op_F_Left'Access,
         Documentation => Member_Doc_For_Bin_Op_F_Left'Access,
         Owner         => Type_Index_For_Bin_Op,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Bin_Op_F_Left'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Bin_Op_F_Op : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Bin_Op => 2);

      Member_Name_For_Bin_Op_F_Op : aliased constant Text_Type :=
        "F_Op";
      Member_Doc_For_Bin_Op_F_Op  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Bin_Op_F_Op  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Bin_Op_F_Op'Access,
         Documentation => Member_Doc_For_Bin_Op_F_Op'Access,
         Owner         => Type_Index_For_Bin_Op,
         Member_Type   => Type_Index_For_Op,
         Null_For      => null,
         Indexes       => Indexes_For_Bin_Op_F_Op'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Bin_Op_F_Right : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Bin_Op => 3);

      Member_Name_For_Bin_Op_F_Right : aliased constant Text_Type :=
        "F_Right";
      Member_Doc_For_Bin_Op_F_Right  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Bin_Op_F_Right  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Bin_Op_F_Right'Access,
         Documentation => Member_Doc_For_Bin_Op_F_Right'Access,
         Owner         => Type_Index_For_Bin_Op,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Bin_Op_F_Right'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Block_Expr_F_Clauses : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Block_Expr => 1);

      Member_Name_For_Block_Expr_F_Clauses : aliased constant Text_Type :=
        "F_Clauses";
      Member_Doc_For_Block_Expr_F_Clauses  : aliased constant Text_Type := "This field contains a list that itself contains one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExprClause`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorDecl`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Block_Expr_F_Clauses  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Block_Expr_F_Clauses'Access,
         Documentation => Member_Doc_For_Block_Expr_F_Clauses'Access,
         Owner         => Type_Index_For_Block_Expr,
         Member_Type   => Type_Index_For_Lkt_Node_List,
         Null_For      => null,
         Indexes       => Indexes_For_Block_Expr_F_Clauses'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Cast_Expr_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Cast_Expr => 1);

      Member_Name_For_Cast_Expr_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Cast_Expr_F_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`KeepExpr`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Cast_Expr_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Cast_Expr_F_Expr'Access,
         Documentation => Member_Doc_For_Cast_Expr_F_Expr'Access,
         Owner         => Type_Index_For_Cast_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Cast_Expr_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Cast_Expr_F_Null_Cond : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Cast_Expr => 2);

      Member_Name_For_Cast_Expr_F_Null_Cond : aliased constant Text_Type :=
        "F_Null_Cond";
      Member_Doc_For_Cast_Expr_F_Null_Cond  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Cast_Expr_F_Null_Cond  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Cast_Expr_F_Null_Cond'Access,
         Documentation => Member_Doc_For_Cast_Expr_F_Null_Cond'Access,
         Owner         => Type_Index_For_Cast_Expr,
         Member_Type   => Type_Index_For_Null_Cond_Qualifier,
         Null_For      => null,
         Indexes       => Indexes_For_Cast_Expr_F_Null_Cond'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Cast_Expr_F_Excludes_Null : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Cast_Expr => 3);

      Member_Name_For_Cast_Expr_F_Excludes_Null : aliased constant Text_Type :=
        "F_Excludes_Null";
      Member_Doc_For_Cast_Expr_F_Excludes_Null  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Cast_Expr_F_Excludes_Null  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Cast_Expr_F_Excludes_Null'Access,
         Documentation => Member_Doc_For_Cast_Expr_F_Excludes_Null'Access,
         Owner         => Type_Index_For_Cast_Expr,
         Member_Type   => Type_Index_For_Excludes_Null,
         Null_For      => null,
         Indexes       => Indexes_For_Cast_Expr_F_Excludes_Null'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Cast_Expr_F_Dest_Type : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Cast_Expr => 4);

      Member_Name_For_Cast_Expr_F_Dest_Type : aliased constant Text_Type :=
        "F_Dest_Type";
      Member_Doc_For_Cast_Expr_F_Dest_Type  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`FunctionTypeRef`, :typeref:`GenericTypeRef`, :typeref:`SimpleTypeRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Cast_Expr_F_Dest_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Cast_Expr_F_Dest_Type'Access,
         Documentation => Member_Doc_For_Cast_Expr_F_Dest_Type'Access,
         Owner         => Type_Index_For_Cast_Expr,
         Member_Type   => Type_Index_For_Type_Ref,
         Null_For      => null,
         Indexes       => Indexes_For_Cast_Expr_F_Dest_Type'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Dot_Expr_F_Prefix : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Dot_Expr => 1);

      Member_Name_For_Dot_Expr_F_Prefix : aliased constant Text_Type :=
        "F_Prefix";
      Member_Doc_For_Dot_Expr_F_Prefix  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`KeepExpr`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`MatchExpr`, :typeref:`ModuleId`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Dot_Expr_F_Prefix  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Dot_Expr_F_Prefix'Access,
         Documentation => Member_Doc_For_Dot_Expr_F_Prefix'Access,
         Owner         => Type_Index_For_Dot_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Dot_Expr_F_Prefix'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Dot_Expr_F_Null_Cond : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Dot_Expr => 2);

      Member_Name_For_Dot_Expr_F_Null_Cond : aliased constant Text_Type :=
        "F_Null_Cond";
      Member_Doc_For_Dot_Expr_F_Null_Cond  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Dot_Expr_F_Null_Cond  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Dot_Expr_F_Null_Cond'Access,
         Documentation => Member_Doc_For_Dot_Expr_F_Null_Cond'Access,
         Owner         => Type_Index_For_Dot_Expr,
         Member_Type   => Type_Index_For_Null_Cond_Qualifier,
         Null_For      => null,
         Indexes       => Indexes_For_Dot_Expr_F_Null_Cond'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Dot_Expr_F_Suffix : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Dot_Expr => 3);

      Member_Name_For_Dot_Expr_F_Suffix : aliased constant Text_Type :=
        "F_Suffix";
      Member_Doc_For_Dot_Expr_F_Suffix  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Dot_Expr_F_Suffix  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Dot_Expr_F_Suffix'Access,
         Documentation => Member_Doc_For_Dot_Expr_F_Suffix'Access,
         Owner         => Type_Index_For_Dot_Expr,
         Member_Type   => Type_Index_For_Ref_Id,
         Null_For      => null,
         Indexes       => Indexes_For_Dot_Expr_F_Suffix'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Error_On_Null_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Error_On_Null => 1);

      Member_Name_For_Error_On_Null_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Error_On_Null_F_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`KeepExpr`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Error_On_Null_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Error_On_Null_F_Expr'Access,
         Documentation => Member_Doc_For_Error_On_Null_F_Expr'Access,
         Owner         => Type_Index_For_Error_On_Null,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Error_On_Null_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Generic_Instantiation_F_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Generic_Instantiation => 1);

      Member_Name_For_Generic_Instantiation_F_Name : aliased constant Text_Type :=
        "F_Name";
      Member_Doc_For_Generic_Instantiation_F_Name  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`KeepExpr`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Generic_Instantiation_F_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Generic_Instantiation_F_Name'Access,
         Documentation => Member_Doc_For_Generic_Instantiation_F_Name'Access,
         Owner         => Type_Index_For_Generic_Instantiation,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Generic_Instantiation_F_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Generic_Instantiation_F_Args : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Generic_Instantiation => 2);

      Member_Name_For_Generic_Instantiation_F_Args : aliased constant Text_Type :=
        "F_Args";
      Member_Doc_For_Generic_Instantiation_F_Args  : aliased constant Text_Type := "This field contains a list that itself contains one of the following nodes: :typeref:`FunctionTypeRef`, :typeref:`GenericTypeRef`, :typeref:`SimpleTypeRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Generic_Instantiation_F_Args  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Generic_Instantiation_F_Args'Access,
         Documentation => Member_Doc_For_Generic_Instantiation_F_Args'Access,
         Owner         => Type_Index_For_Generic_Instantiation,
         Member_Type   => Type_Index_For_Type_Ref_List,
         Null_For      => null,
         Indexes       => Indexes_For_Generic_Instantiation_F_Args'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_Discard_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_Discard => 1);

      Member_Name_For_Grammar_Discard_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Grammar_Discard_F_Expr  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_Discard_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_Discard_F_Expr'Access,
         Documentation => Member_Doc_For_Grammar_Discard_F_Expr'Access,
         Owner         => Type_Index_For_Grammar_Discard,
         Member_Type   => Type_Index_For_Grammar_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_Discard_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_Dont_Skip_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_Dont_Skip => 1);

      Member_Name_For_Grammar_Dont_Skip_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Grammar_Dont_Skip_F_Expr  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_Dont_Skip_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_Dont_Skip_F_Expr'Access,
         Documentation => Member_Doc_For_Grammar_Dont_Skip_F_Expr'Access,
         Owner         => Type_Index_For_Grammar_Dont_Skip,
         Member_Type   => Type_Index_For_Grammar_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_Dont_Skip_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_Dont_Skip_F_Dont_Skip : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_Dont_Skip => 2);

      Member_Name_For_Grammar_Dont_Skip_F_Dont_Skip : aliased constant Text_Type :=
        "F_Dont_Skip";
      Member_Doc_For_Grammar_Dont_Skip_F_Dont_Skip  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_Dont_Skip_F_Dont_Skip  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_Dont_Skip_F_Dont_Skip'Access,
         Documentation => Member_Doc_For_Grammar_Dont_Skip_F_Dont_Skip'Access,
         Owner         => Type_Index_For_Grammar_Dont_Skip,
         Member_Type   => Type_Index_For_Grammar_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_Dont_Skip_F_Dont_Skip'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_List_F_List_Type : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_List => 1);

      Member_Name_For_Grammar_List_F_List_Type : aliased constant Text_Type :=
        "F_List_Type";
      Member_Doc_For_Grammar_List_F_List_Type  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_List_F_List_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_List_F_List_Type'Access,
         Documentation => Member_Doc_For_Grammar_List_F_List_Type'Access,
         Owner         => Type_Index_For_Grammar_List,
         Member_Type   => Type_Index_For_Type_Ref,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_List_F_List_Type'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_List_F_Kind : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_List => 2);

      Member_Name_For_Grammar_List_F_Kind : aliased constant Text_Type :=
        "F_Kind";
      Member_Doc_For_Grammar_List_F_Kind  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_List_F_Kind  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_List_F_Kind'Access,
         Documentation => Member_Doc_For_Grammar_List_F_Kind'Access,
         Owner         => Type_Index_For_Grammar_List,
         Member_Type   => Type_Index_For_List_Kind,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_List_F_Kind'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_List_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_List => 3);

      Member_Name_For_Grammar_List_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Grammar_List_F_Expr  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_List_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_List_F_Expr'Access,
         Documentation => Member_Doc_For_Grammar_List_F_Expr'Access,
         Owner         => Type_Index_For_Grammar_List,
         Member_Type   => Type_Index_For_Grammar_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_List_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_List_F_Sep : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_List => 4);

      Member_Name_For_Grammar_List_F_Sep : aliased constant Text_Type :=
        "F_Sep";
      Member_Doc_For_Grammar_List_F_Sep  : aliased constant Text_Type := "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Grammar_List_F_Sep  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_List_F_Sep'Access,
         Documentation => Member_Doc_For_Grammar_List_F_Sep'Access,
         Owner         => Type_Index_For_Grammar_List,
         Member_Type   => Type_Index_For_Grammar_List_Sep,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_List_F_Sep'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_Null_F_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_Null => 1);

      Member_Name_For_Grammar_Null_F_Name : aliased constant Text_Type :=
        "F_Name";
      Member_Doc_For_Grammar_Null_F_Name  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`FunctionTypeRef`, :typeref:`GenericTypeRef`, :typeref:`SimpleTypeRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Grammar_Null_F_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_Null_F_Name'Access,
         Documentation => Member_Doc_For_Grammar_Null_F_Name'Access,
         Owner         => Type_Index_For_Grammar_Null,
         Member_Type   => Type_Index_For_Type_Ref,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_Null_F_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_Opt_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_Opt => 1);

      Member_Name_For_Grammar_Opt_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Grammar_Opt_F_Expr  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_Opt_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_Opt_F_Expr'Access,
         Documentation => Member_Doc_For_Grammar_Opt_F_Expr'Access,
         Owner         => Type_Index_For_Grammar_Opt,
         Member_Type   => Type_Index_For_Grammar_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_Opt_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_Opt_Error_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_Opt_Error => 1);

      Member_Name_For_Grammar_Opt_Error_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Grammar_Opt_Error_F_Expr  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_Opt_Error_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_Opt_Error_F_Expr'Access,
         Documentation => Member_Doc_For_Grammar_Opt_Error_F_Expr'Access,
         Owner         => Type_Index_For_Grammar_Opt_Error,
         Member_Type   => Type_Index_For_Grammar_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_Opt_Error_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_Opt_Error_Group_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_Opt_Error_Group => 1);

      Member_Name_For_Grammar_Opt_Error_Group_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Grammar_Opt_Error_Group_F_Expr  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_Opt_Error_Group_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_Opt_Error_Group_F_Expr'Access,
         Documentation => Member_Doc_For_Grammar_Opt_Error_Group_F_Expr'Access,
         Owner         => Type_Index_For_Grammar_Opt_Error_Group,
         Member_Type   => Type_Index_For_Grammar_Expr_List,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_Opt_Error_Group_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_Opt_Group_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_Opt_Group => 1);

      Member_Name_For_Grammar_Opt_Group_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Grammar_Opt_Group_F_Expr  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_Opt_Group_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_Opt_Group_F_Expr'Access,
         Documentation => Member_Doc_For_Grammar_Opt_Group_F_Expr'Access,
         Owner         => Type_Index_For_Grammar_Opt_Group,
         Member_Type   => Type_Index_For_Grammar_Expr_List,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_Opt_Group_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_Or_Expr_F_Sub_Exprs : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_Or_Expr => 1);

      Member_Name_For_Grammar_Or_Expr_F_Sub_Exprs : aliased constant Text_Type :=
        "F_Sub_Exprs";
      Member_Doc_For_Grammar_Or_Expr_F_Sub_Exprs  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_Or_Expr_F_Sub_Exprs  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_Or_Expr_F_Sub_Exprs'Access,
         Documentation => Member_Doc_For_Grammar_Or_Expr_F_Sub_Exprs'Access,
         Owner         => Type_Index_For_Grammar_Or_Expr,
         Member_Type   => Type_Index_For_Grammar_Expr_List_List,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_Or_Expr_F_Sub_Exprs'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_Pick_F_Exprs : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_Pick => 1,
Type_Index_For_Grammar_Implicit_Pick => 1);

      Member_Name_For_Grammar_Pick_F_Exprs : aliased constant Text_Type :=
        "F_Exprs";
      Member_Doc_For_Grammar_Pick_F_Exprs  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_Pick_F_Exprs  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_Pick_F_Exprs'Access,
         Documentation => Member_Doc_For_Grammar_Pick_F_Exprs'Access,
         Owner         => Type_Index_For_Grammar_Pick,
         Member_Type   => Type_Index_For_Grammar_Expr_List,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_Pick_F_Exprs'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_Predicate_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_Predicate => 1);

      Member_Name_For_Grammar_Predicate_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Grammar_Predicate_F_Expr  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_Predicate_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_Predicate_F_Expr'Access,
         Documentation => Member_Doc_For_Grammar_Predicate_F_Expr'Access,
         Owner         => Type_Index_For_Grammar_Predicate,
         Member_Type   => Type_Index_For_Grammar_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_Predicate_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_Predicate_F_Prop_Ref : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_Predicate => 2);

      Member_Name_For_Grammar_Predicate_F_Prop_Ref : aliased constant Text_Type :=
        "F_Prop_Ref";
      Member_Doc_For_Grammar_Predicate_F_Prop_Ref  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`DotExpr`, :typeref:`RefId`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_Predicate_F_Prop_Ref  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_Predicate_F_Prop_Ref'Access,
         Documentation => Member_Doc_For_Grammar_Predicate_F_Prop_Ref'Access,
         Owner         => Type_Index_For_Grammar_Predicate,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_Predicate_F_Prop_Ref'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_Rule_Ref_F_Node_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_Rule_Ref => 1);

      Member_Name_For_Grammar_Rule_Ref_F_Node_Name : aliased constant Text_Type :=
        "F_Node_Name";
      Member_Doc_For_Grammar_Rule_Ref_F_Node_Name  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_Rule_Ref_F_Node_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_Rule_Ref_F_Node_Name'Access,
         Documentation => Member_Doc_For_Grammar_Rule_Ref_F_Node_Name'Access,
         Owner         => Type_Index_For_Grammar_Rule_Ref,
         Member_Type   => Type_Index_For_Ref_Id,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_Rule_Ref_F_Node_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_Skip_F_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_Skip => 1);

      Member_Name_For_Grammar_Skip_F_Name : aliased constant Text_Type :=
        "F_Name";
      Member_Doc_For_Grammar_Skip_F_Name  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`FunctionTypeRef`, :typeref:`GenericTypeRef`, :typeref:`SimpleTypeRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_Skip_F_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_Skip_F_Name'Access,
         Documentation => Member_Doc_For_Grammar_Skip_F_Name'Access,
         Owner         => Type_Index_For_Grammar_Skip,
         Member_Type   => Type_Index_For_Type_Ref,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_Skip_F_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_Stop_Cut_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_Stop_Cut => 1);

      Member_Name_For_Grammar_Stop_Cut_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Grammar_Stop_Cut_F_Expr  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_Stop_Cut_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_Stop_Cut_F_Expr'Access,
         Documentation => Member_Doc_For_Grammar_Stop_Cut_F_Expr'Access,
         Owner         => Type_Index_For_Grammar_Stop_Cut,
         Member_Type   => Type_Index_For_Grammar_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_Stop_Cut_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Parse_Node_Expr_F_Node_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Parse_Node_Expr => 1);

      Member_Name_For_Parse_Node_Expr_F_Node_Name : aliased constant Text_Type :=
        "F_Node_Name";
      Member_Doc_For_Parse_Node_Expr_F_Node_Name  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`FunctionTypeRef`, :typeref:`GenericTypeRef`, :typeref:`SimpleTypeRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Parse_Node_Expr_F_Node_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Parse_Node_Expr_F_Node_Name'Access,
         Documentation => Member_Doc_For_Parse_Node_Expr_F_Node_Name'Access,
         Owner         => Type_Index_For_Parse_Node_Expr,
         Member_Type   => Type_Index_For_Type_Ref,
         Null_For      => null,
         Indexes       => Indexes_For_Parse_Node_Expr_F_Node_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Parse_Node_Expr_F_Sub_Exprs : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Parse_Node_Expr => 2);

      Member_Name_For_Parse_Node_Expr_F_Sub_Exprs : aliased constant Text_Type :=
        "F_Sub_Exprs";
      Member_Doc_For_Parse_Node_Expr_F_Sub_Exprs  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Parse_Node_Expr_F_Sub_Exprs  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Parse_Node_Expr_F_Sub_Exprs'Access,
         Documentation => Member_Doc_For_Parse_Node_Expr_F_Sub_Exprs'Access,
         Owner         => Type_Index_For_Parse_Node_Expr,
         Member_Type   => Type_Index_For_Grammar_Expr_List,
         Null_For      => null,
         Indexes       => Indexes_For_Parse_Node_Expr_F_Sub_Exprs'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Token_No_Case_Lit_F_Lit : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Token_No_Case_Lit => 1);

      Member_Name_For_Token_No_Case_Lit_F_Lit : aliased constant Text_Type :=
        "F_Lit";
      Member_Doc_For_Token_No_Case_Lit_F_Lit  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Token_No_Case_Lit_F_Lit  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Token_No_Case_Lit_F_Lit'Access,
         Documentation => Member_Doc_For_Token_No_Case_Lit_F_Lit'Access,
         Owner         => Type_Index_For_Token_No_Case_Lit,
         Member_Type   => Type_Index_For_Token_Lit,
         Null_For      => null,
         Indexes       => Indexes_For_Token_No_Case_Lit_F_Lit'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Token_Pattern_Concat_F_Left : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Token_Pattern_Concat => 1);

      Member_Name_For_Token_Pattern_Concat_F_Left : aliased constant Text_Type :=
        "F_Left";
      Member_Doc_For_Token_Pattern_Concat_F_Left  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`TokenPatternConcat`, :typeref:`TokenPatternLit`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Token_Pattern_Concat_F_Left  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Token_Pattern_Concat_F_Left'Access,
         Documentation => Member_Doc_For_Token_Pattern_Concat_F_Left'Access,
         Owner         => Type_Index_For_Token_Pattern_Concat,
         Member_Type   => Type_Index_For_Grammar_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Token_Pattern_Concat_F_Left'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Token_Pattern_Concat_F_Right : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Token_Pattern_Concat => 2);

      Member_Name_For_Token_Pattern_Concat_F_Right : aliased constant Text_Type :=
        "F_Right";
      Member_Doc_For_Token_Pattern_Concat_F_Right  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Token_Pattern_Concat_F_Right  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Token_Pattern_Concat_F_Right'Access,
         Documentation => Member_Doc_For_Token_Pattern_Concat_F_Right'Access,
         Owner         => Type_Index_For_Token_Pattern_Concat,
         Member_Type   => Type_Index_For_Token_Pattern_Lit,
         Null_For      => null,
         Indexes       => Indexes_For_Token_Pattern_Concat_F_Right'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Token_Ref_F_Token_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Token_Ref => 1);

      Member_Name_For_Token_Ref_F_Token_Name : aliased constant Text_Type :=
        "F_Token_Name";
      Member_Doc_For_Token_Ref_F_Token_Name  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Token_Ref_F_Token_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Token_Ref_F_Token_Name'Access,
         Documentation => Member_Doc_For_Token_Ref_F_Token_Name'Access,
         Owner         => Type_Index_For_Token_Ref,
         Member_Type   => Type_Index_For_Ref_Id,
         Null_For      => null,
         Indexes       => Indexes_For_Token_Ref_F_Token_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Token_Ref_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Token_Ref => 2);

      Member_Name_For_Token_Ref_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Token_Ref_F_Expr  : aliased constant Text_Type := "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Token_Ref_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Token_Ref_F_Expr'Access,
         Documentation => Member_Doc_For_Token_Ref_F_Expr'Access,
         Owner         => Type_Index_For_Token_Ref,
         Member_Type   => Type_Index_For_Token_Lit,
         Null_For      => null,
         Indexes       => Indexes_For_Token_Ref_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_If_Expr_F_Cond_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_If_Expr => 1);

      Member_Name_For_If_Expr_F_Cond_Expr : aliased constant Text_Type :=
        "F_Cond_Expr";
      Member_Doc_For_If_Expr_F_Cond_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_If_Expr_F_Cond_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_If_Expr_F_Cond_Expr'Access,
         Documentation => Member_Doc_For_If_Expr_F_Cond_Expr'Access,
         Owner         => Type_Index_For_If_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_If_Expr_F_Cond_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_If_Expr_F_Then_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_If_Expr => 2);

      Member_Name_For_If_Expr_F_Then_Expr : aliased constant Text_Type :=
        "F_Then_Expr";
      Member_Doc_For_If_Expr_F_Then_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_If_Expr_F_Then_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_If_Expr_F_Then_Expr'Access,
         Documentation => Member_Doc_For_If_Expr_F_Then_Expr'Access,
         Owner         => Type_Index_For_If_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_If_Expr_F_Then_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_If_Expr_F_Alternatives : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_If_Expr => 3);

      Member_Name_For_If_Expr_F_Alternatives : aliased constant Text_Type :=
        "F_Alternatives";
      Member_Doc_For_If_Expr_F_Alternatives  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_If_Expr_F_Alternatives  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_If_Expr_F_Alternatives'Access,
         Documentation => Member_Doc_For_If_Expr_F_Alternatives'Access,
         Owner         => Type_Index_For_If_Expr,
         Member_Type   => Type_Index_For_Elsif_Branch_List,
         Null_For      => null,
         Indexes       => Indexes_For_If_Expr_F_Alternatives'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_If_Expr_F_Else_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_If_Expr => 4);

      Member_Name_For_If_Expr_F_Else_Expr : aliased constant Text_Type :=
        "F_Else_Expr";
      Member_Doc_For_If_Expr_F_Else_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_If_Expr_F_Else_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_If_Expr_F_Else_Expr'Access,
         Documentation => Member_Doc_For_If_Expr_F_Else_Expr'Access,
         Owner         => Type_Index_For_If_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_If_Expr_F_Else_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Isa_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Isa => 1);

      Member_Name_For_Isa_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Isa_F_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Isa_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Isa_F_Expr'Access,
         Documentation => Member_Doc_For_Isa_F_Expr'Access,
         Owner         => Type_Index_For_Isa,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Isa_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Isa_F_Pattern : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Isa => 2);

      Member_Name_For_Isa_F_Pattern : aliased constant Text_Type :=
        "F_Pattern";
      Member_Doc_For_Isa_F_Pattern  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ComplexPattern`, :typeref:`NotPattern`, :typeref:`OrPattern`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Isa_F_Pattern  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Isa_F_Pattern'Access,
         Documentation => Member_Doc_For_Isa_F_Pattern'Access,
         Owner         => Type_Index_For_Isa,
         Member_Type   => Type_Index_For_Pattern,
         Null_For      => null,
         Indexes       => Indexes_For_Isa_F_Pattern'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Keep_Expr_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Keep_Expr => 1);

      Member_Name_For_Keep_Expr_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Keep_Expr_F_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`KeepExpr`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Keep_Expr_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Keep_Expr_F_Expr'Access,
         Documentation => Member_Doc_For_Keep_Expr_F_Expr'Access,
         Owner         => Type_Index_For_Keep_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Keep_Expr_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Keep_Expr_F_Null_Cond : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Keep_Expr => 2);

      Member_Name_For_Keep_Expr_F_Null_Cond : aliased constant Text_Type :=
        "F_Null_Cond";
      Member_Doc_For_Keep_Expr_F_Null_Cond  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Keep_Expr_F_Null_Cond  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Keep_Expr_F_Null_Cond'Access,
         Documentation => Member_Doc_For_Keep_Expr_F_Null_Cond'Access,
         Owner         => Type_Index_For_Keep_Expr,
         Member_Type   => Type_Index_For_Null_Cond_Qualifier,
         Null_For      => null,
         Indexes       => Indexes_For_Keep_Expr_F_Null_Cond'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Keep_Expr_F_Keep_Type : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Keep_Expr => 3);

      Member_Name_For_Keep_Expr_F_Keep_Type : aliased constant Text_Type :=
        "F_Keep_Type";
      Member_Doc_For_Keep_Expr_F_Keep_Type  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`FunctionTypeRef`, :typeref:`GenericTypeRef`, :typeref:`SimpleTypeRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Keep_Expr_F_Keep_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Keep_Expr_F_Keep_Type'Access,
         Documentation => Member_Doc_For_Keep_Expr_F_Keep_Type'Access,
         Owner         => Type_Index_For_Keep_Expr,
         Member_Type   => Type_Index_For_Type_Ref,
         Null_For      => null,
         Indexes       => Indexes_For_Keep_Expr_F_Keep_Type'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Lambda_Expr_F_Params : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Lambda_Expr => 1);

      Member_Name_For_Lambda_Expr_F_Params : aliased constant Text_Type :=
        "F_Params";
      Member_Doc_For_Lambda_Expr_F_Params  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Lambda_Expr_F_Params  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lambda_Expr_F_Params'Access,
         Documentation => Member_Doc_For_Lambda_Expr_F_Params'Access,
         Owner         => Type_Index_For_Lambda_Expr,
         Member_Type   => Type_Index_For_Lambda_Param_Decl_List,
         Null_For      => null,
         Indexes       => Indexes_For_Lambda_Expr_F_Params'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Lambda_Expr_F_Return_Type : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Lambda_Expr => 2);

      Member_Name_For_Lambda_Expr_F_Return_Type : aliased constant Text_Type :=
        "F_Return_Type";
      Member_Doc_For_Lambda_Expr_F_Return_Type  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`FunctionTypeRef`, :typeref:`GenericTypeRef`, :typeref:`SimpleTypeRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Lambda_Expr_F_Return_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lambda_Expr_F_Return_Type'Access,
         Documentation => Member_Doc_For_Lambda_Expr_F_Return_Type'Access,
         Owner         => Type_Index_For_Lambda_Expr,
         Member_Type   => Type_Index_For_Type_Ref,
         Null_For      => null,
         Indexes       => Indexes_For_Lambda_Expr_F_Return_Type'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Lambda_Expr_F_Body : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Lambda_Expr => 3);

      Member_Name_For_Lambda_Expr_F_Body : aliased constant Text_Type :=
        "F_Body";
      Member_Doc_For_Lambda_Expr_F_Body  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Lambda_Expr_F_Body  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lambda_Expr_F_Body'Access,
         Documentation => Member_Doc_For_Lambda_Expr_F_Body'Access,
         Owner         => Type_Index_For_Lambda_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Lambda_Expr_F_Body'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Null_Lit_F_Dest_Type : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Null_Lit => 1);

      Member_Name_For_Null_Lit_F_Dest_Type : aliased constant Text_Type :=
        "F_Dest_Type";
      Member_Doc_For_Null_Lit_F_Dest_Type  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`FunctionTypeRef`, :typeref:`GenericTypeRef`, :typeref:`SimpleTypeRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Null_Lit_F_Dest_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Null_Lit_F_Dest_Type'Access,
         Documentation => Member_Doc_For_Null_Lit_F_Dest_Type'Access,
         Owner         => Type_Index_For_Null_Lit,
         Member_Type   => Type_Index_For_Type_Ref,
         Null_For      => null,
         Indexes       => Indexes_For_Null_Lit_F_Dest_Type'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Block_String_Lit_F_Lines : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Block_String_Lit => 1);

      Member_Name_For_Block_String_Lit_F_Lines : aliased constant Text_Type :=
        "F_Lines";
      Member_Doc_For_Block_String_Lit_F_Lines  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Block_String_Lit_F_Lines  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Block_String_Lit_F_Lines'Access,
         Documentation => Member_Doc_For_Block_String_Lit_F_Lines'Access,
         Owner         => Type_Index_For_Block_String_Lit,
         Member_Type   => Type_Index_For_Block_String_Line_List,
         Null_For      => null,
         Indexes       => Indexes_For_Block_String_Lit_F_Lines'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Module_Doc_String_Lit_F_Lines : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Module_Doc_String_Lit => 1);

      Member_Name_For_Module_Doc_String_Lit_F_Lines : aliased constant Text_Type :=
        "F_Lines";
      Member_Doc_For_Module_Doc_String_Lit_F_Lines  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Module_Doc_String_Lit_F_Lines  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Module_Doc_String_Lit_F_Lines'Access,
         Documentation => Member_Doc_For_Module_Doc_String_Lit_F_Lines'Access,
         Owner         => Type_Index_For_Module_Doc_String_Lit,
         Member_Type   => Type_Index_For_Module_Doc_String_Line_List,
         Null_For      => null,
         Indexes       => Indexes_For_Module_Doc_String_Lit_F_Lines'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Logic_Assign_F_Dest_Var : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Logic_Assign => 1);

      Member_Name_For_Logic_Assign_F_Dest_Var : aliased constant Text_Type :=
        "F_Dest_Var";
      Member_Doc_For_Logic_Assign_F_Dest_Var  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Logic_Assign_F_Dest_Var  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Logic_Assign_F_Dest_Var'Access,
         Documentation => Member_Doc_For_Logic_Assign_F_Dest_Var'Access,
         Owner         => Type_Index_For_Logic_Assign,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Logic_Assign_F_Dest_Var'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Logic_Assign_F_Value : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Logic_Assign => 2);

      Member_Name_For_Logic_Assign_F_Value : aliased constant Text_Type :=
        "F_Value";
      Member_Doc_For_Logic_Assign_F_Value  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Logic_Assign_F_Value  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Logic_Assign_F_Value'Access,
         Documentation => Member_Doc_For_Logic_Assign_F_Value'Access,
         Owner         => Type_Index_For_Logic_Assign,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Logic_Assign_F_Value'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Logic_Expr_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Logic_Expr => 1);

      Member_Name_For_Logic_Expr_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Logic_Expr_F_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`CallExpr`, :typeref:`RefId`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Logic_Expr_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Logic_Expr_F_Expr'Access,
         Documentation => Member_Doc_For_Logic_Expr_F_Expr'Access,
         Owner         => Type_Index_For_Logic_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Logic_Expr_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Logic_Propagate_F_Dest_Var : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Logic_Propagate => 1);

      Member_Name_For_Logic_Propagate_F_Dest_Var : aliased constant Text_Type :=
        "F_Dest_Var";
      Member_Doc_For_Logic_Propagate_F_Dest_Var  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Logic_Propagate_F_Dest_Var  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Logic_Propagate_F_Dest_Var'Access,
         Documentation => Member_Doc_For_Logic_Propagate_F_Dest_Var'Access,
         Owner         => Type_Index_For_Logic_Propagate,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Logic_Propagate_F_Dest_Var'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Logic_Propagate_F_Call : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Logic_Propagate => 2);

      Member_Name_For_Logic_Propagate_F_Call : aliased constant Text_Type :=
        "F_Call";
      Member_Doc_For_Logic_Propagate_F_Call  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Logic_Propagate_F_Call  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Logic_Propagate_F_Call'Access,
         Documentation => Member_Doc_For_Logic_Propagate_F_Call'Access,
         Owner         => Type_Index_For_Logic_Propagate,
         Member_Type   => Type_Index_For_Logic_Propagate_Call,
         Null_For      => null,
         Indexes       => Indexes_For_Logic_Propagate_F_Call'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Logic_Unify_F_Lhs : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Logic_Unify => 1);

      Member_Name_For_Logic_Unify_F_Lhs : aliased constant Text_Type :=
        "F_Lhs";
      Member_Doc_For_Logic_Unify_F_Lhs  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Logic_Unify_F_Lhs  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Logic_Unify_F_Lhs'Access,
         Documentation => Member_Doc_For_Logic_Unify_F_Lhs'Access,
         Owner         => Type_Index_For_Logic_Unify,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Logic_Unify_F_Lhs'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Logic_Unify_F_Rhs : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Logic_Unify => 2);

      Member_Name_For_Logic_Unify_F_Rhs : aliased constant Text_Type :=
        "F_Rhs";
      Member_Doc_For_Logic_Unify_F_Rhs  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Logic_Unify_F_Rhs  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Logic_Unify_F_Rhs'Access,
         Documentation => Member_Doc_For_Logic_Unify_F_Rhs'Access,
         Owner         => Type_Index_For_Logic_Unify,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Logic_Unify_F_Rhs'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Match_Expr_F_Match_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Match_Expr => 1);

      Member_Name_For_Match_Expr_F_Match_Expr : aliased constant Text_Type :=
        "F_Match_Expr";
      Member_Doc_For_Match_Expr_F_Match_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Match_Expr_F_Match_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Match_Expr_F_Match_Expr'Access,
         Documentation => Member_Doc_For_Match_Expr_F_Match_Expr'Access,
         Owner         => Type_Index_For_Match_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Match_Expr_F_Match_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Match_Expr_F_Branches : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Match_Expr => 2);

      Member_Name_For_Match_Expr_F_Branches : aliased constant Text_Type :=
        "F_Branches";
      Member_Doc_For_Match_Expr_F_Branches  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Match_Expr_F_Branches  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Match_Expr_F_Branches'Access,
         Documentation => Member_Doc_For_Match_Expr_F_Branches'Access,
         Owner         => Type_Index_For_Match_Expr,
         Member_Type   => Type_Index_For_Base_Match_Branch_List,
         Null_For      => null,
         Indexes       => Indexes_For_Match_Expr_F_Branches'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Not_Expr_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Not_Expr => 1);

      Member_Name_For_Not_Expr_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Not_Expr_F_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Not_Expr_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Not_Expr_F_Expr'Access,
         Documentation => Member_Doc_For_Not_Expr_F_Expr'Access,
         Owner         => Type_Index_For_Not_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Not_Expr_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Paren_Expr_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Paren_Expr => 1);

      Member_Name_For_Paren_Expr_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Paren_Expr_F_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Paren_Expr_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Paren_Expr_F_Expr'Access,
         Documentation => Member_Doc_For_Paren_Expr_F_Expr'Access,
         Owner         => Type_Index_For_Paren_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Paren_Expr_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Query_F_Source : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Query => 1);

      Member_Name_For_Query_F_Source : aliased constant Text_Type :=
        "F_Source";
      Member_Doc_For_Query_F_Source  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Query_F_Source  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Query_F_Source'Access,
         Documentation => Member_Doc_For_Query_F_Source'Access,
         Owner         => Type_Index_For_Query,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Query_F_Source'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Query_F_Pattern : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Query => 2);

      Member_Name_For_Query_F_Pattern : aliased constant Text_Type :=
        "F_Pattern";
      Member_Doc_For_Query_F_Pattern  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ComplexPattern`, :typeref:`NotPattern`, :typeref:`OrPattern`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Query_F_Pattern  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Query_F_Pattern'Access,
         Documentation => Member_Doc_For_Query_F_Pattern'Access,
         Owner         => Type_Index_For_Query,
         Member_Type   => Type_Index_For_Pattern,
         Null_For      => null,
         Indexes       => Indexes_For_Query_F_Pattern'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Query_F_Mapping : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Query => 3);

      Member_Name_For_Query_F_Mapping : aliased constant Text_Type :=
        "F_Mapping";
      Member_Doc_For_Query_F_Mapping  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Query_F_Mapping  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Query_F_Mapping'Access,
         Documentation => Member_Doc_For_Query_F_Mapping'Access,
         Owner         => Type_Index_For_Query,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Query_F_Mapping'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Query_F_Guard : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Query => 4);

      Member_Name_For_Query_F_Guard : aliased constant Text_Type :=
        "F_Guard";
      Member_Doc_For_Query_F_Guard  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Query_F_Guard  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Query_F_Guard'Access,
         Documentation => Member_Doc_For_Query_F_Guard'Access,
         Owner         => Type_Index_For_Query,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Query_F_Guard'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Raise_Expr_F_Dest_Type : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Raise_Expr => 1);

      Member_Name_For_Raise_Expr_F_Dest_Type : aliased constant Text_Type :=
        "F_Dest_Type";
      Member_Doc_For_Raise_Expr_F_Dest_Type  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`FunctionTypeRef`, :typeref:`GenericTypeRef`, :typeref:`SimpleTypeRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Raise_Expr_F_Dest_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Raise_Expr_F_Dest_Type'Access,
         Documentation => Member_Doc_For_Raise_Expr_F_Dest_Type'Access,
         Owner         => Type_Index_For_Raise_Expr,
         Member_Type   => Type_Index_For_Type_Ref,
         Null_For      => null,
         Indexes       => Indexes_For_Raise_Expr_F_Dest_Type'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Raise_Expr_F_Except_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Raise_Expr => 2);

      Member_Name_For_Raise_Expr_F_Except_Expr : aliased constant Text_Type :=
        "F_Except_Expr";
      Member_Doc_For_Raise_Expr_F_Except_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Raise_Expr_F_Except_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Raise_Expr_F_Except_Expr'Access,
         Documentation => Member_Doc_For_Raise_Expr_F_Except_Expr'Access,
         Owner         => Type_Index_For_Raise_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Raise_Expr_F_Except_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Subscript_Expr_F_Prefix : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Subscript_Expr => 1);

      Member_Name_For_Subscript_Expr_F_Prefix : aliased constant Text_Type :=
        "F_Prefix";
      Member_Doc_For_Subscript_Expr_F_Prefix  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`KeepExpr`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Subscript_Expr_F_Prefix  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Subscript_Expr_F_Prefix'Access,
         Documentation => Member_Doc_For_Subscript_Expr_F_Prefix'Access,
         Owner         => Type_Index_For_Subscript_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Subscript_Expr_F_Prefix'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Subscript_Expr_F_Null_Cond : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Subscript_Expr => 2);

      Member_Name_For_Subscript_Expr_F_Null_Cond : aliased constant Text_Type :=
        "F_Null_Cond";
      Member_Doc_For_Subscript_Expr_F_Null_Cond  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Subscript_Expr_F_Null_Cond  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Subscript_Expr_F_Null_Cond'Access,
         Documentation => Member_Doc_For_Subscript_Expr_F_Null_Cond'Access,
         Owner         => Type_Index_For_Subscript_Expr,
         Member_Type   => Type_Index_For_Null_Cond_Qualifier,
         Null_For      => null,
         Indexes       => Indexes_For_Subscript_Expr_F_Null_Cond'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Subscript_Expr_F_Index : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Subscript_Expr => 3);

      Member_Name_For_Subscript_Expr_F_Index : aliased constant Text_Type :=
        "F_Index";
      Member_Doc_For_Subscript_Expr_F_Index  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Subscript_Expr_F_Index  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Subscript_Expr_F_Index'Access,
         Documentation => Member_Doc_For_Subscript_Expr_F_Index'Access,
         Owner         => Type_Index_For_Subscript_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Subscript_Expr_F_Index'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Try_Expr_F_Try_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Try_Expr => 1);

      Member_Name_For_Try_Expr_F_Try_Expr : aliased constant Text_Type :=
        "F_Try_Expr";
      Member_Doc_For_Try_Expr_F_Try_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Try_Expr_F_Try_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Try_Expr_F_Try_Expr'Access,
         Documentation => Member_Doc_For_Try_Expr_F_Try_Expr'Access,
         Owner         => Type_Index_For_Try_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Try_Expr_F_Try_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Try_Expr_F_Or_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Try_Expr => 2);

      Member_Name_For_Try_Expr_F_Or_Expr : aliased constant Text_Type :=
        "F_Or_Expr";
      Member_Doc_For_Try_Expr_F_Or_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Try_Expr_F_Or_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Try_Expr_F_Or_Expr'Access,
         Documentation => Member_Doc_For_Try_Expr_F_Or_Expr'Access,
         Owner         => Type_Index_For_Try_Expr,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Try_Expr_F_Or_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Un_Op_F_Op : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Un_Op => 1);

      Member_Name_For_Un_Op_F_Op : aliased constant Text_Type :=
        "F_Op";
      Member_Doc_For_Un_Op_F_Op  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`OpMinus`, :typeref:`OpPlus`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Un_Op_F_Op  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Un_Op_F_Op'Access,
         Documentation => Member_Doc_For_Un_Op_F_Op'Access,
         Owner         => Type_Index_For_Un_Op,
         Member_Type   => Type_Index_For_Op,
         Null_For      => null,
         Indexes       => Indexes_For_Un_Op_F_Op'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Un_Op_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Un_Op => 2);

      Member_Name_For_Un_Op_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Un_Op_F_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Un_Op_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Un_Op_F_Expr'Access,
         Documentation => Member_Doc_For_Un_Op_F_Expr'Access,
         Owner         => Type_Index_For_Un_Op,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Un_Op_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Full_Decl_F_Doc : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Full_Decl => 1);

      Member_Name_For_Full_Decl_F_Doc : aliased constant Text_Type :=
        "F_Doc";
      Member_Doc_For_Full_Decl_F_Doc  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`BlockStringLit`, :typeref:`SingleLineStringLit`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Full_Decl_F_Doc  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Full_Decl_F_Doc'Access,
         Documentation => Member_Doc_For_Full_Decl_F_Doc'Access,
         Owner         => Type_Index_For_Full_Decl,
         Member_Type   => Type_Index_For_String_Lit,
         Null_For      => null,
         Indexes       => Indexes_For_Full_Decl_F_Doc'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Full_Decl_F_Decl_Annotations : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Full_Decl => 2);

      Member_Name_For_Full_Decl_F_Decl_Annotations : aliased constant Text_Type :=
        "F_Decl_Annotations";
      Member_Doc_For_Full_Decl_F_Decl_Annotations  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Full_Decl_F_Decl_Annotations  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Full_Decl_F_Decl_Annotations'Access,
         Documentation => Member_Doc_For_Full_Decl_F_Decl_Annotations'Access,
         Owner         => Type_Index_For_Full_Decl,
         Member_Type   => Type_Index_For_Decl_Annotation_List,
         Null_For      => null,
         Indexes       => Indexes_For_Full_Decl_F_Decl_Annotations'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Full_Decl_F_Decl : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Full_Decl => 3);

      Member_Name_For_Full_Decl_F_Decl : aliased constant Text_Type :=
        "F_Decl";
      Member_Doc_For_Full_Decl_F_Decl  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`DynVarDecl`, :typeref:`EnvSpecDecl`, :typeref:`ErrorDecl`, :typeref:`FieldDecl`, :typeref:`FunDecl`, :typeref:`GenericDecl`, :typeref:`GenericParamTypeDecl`, :typeref:`GrammarDecl`, :typeref:`GrammarRuleDecl`, :typeref:`LexerDecl`, :typeref:`LexerFamilyDecl`, :typeref:`NamedTypeDecl`, :typeref:`ValDecl`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Full_Decl_F_Decl  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Full_Decl_F_Decl'Access,
         Documentation => Member_Doc_For_Full_Decl_F_Decl'Access,
         Owner         => Type_Index_For_Full_Decl,
         Member_Type   => Type_Index_For_Decl,
         Null_For      => null,
         Indexes       => Indexes_For_Full_Decl_F_Decl'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_List_Sep_F_Token : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_List_Sep => 1);

      Member_Name_For_Grammar_List_Sep_F_Token : aliased constant Text_Type :=
        "F_Token";
      Member_Doc_For_Grammar_List_Sep_F_Token  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Grammar_List_Sep_F_Token  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_List_Sep_F_Token'Access,
         Documentation => Member_Doc_For_Grammar_List_Sep_F_Token'Access,
         Owner         => Type_Index_For_Grammar_List_Sep,
         Member_Type   => Type_Index_For_Grammar_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_List_Sep_F_Token'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Grammar_List_Sep_F_Extra : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Grammar_List_Sep => 2);

      Member_Name_For_Grammar_List_Sep_F_Extra : aliased constant Text_Type :=
        "F_Extra";
      Member_Doc_For_Grammar_List_Sep_F_Extra  : aliased constant Text_Type := "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Grammar_List_Sep_F_Extra  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Grammar_List_Sep_F_Extra'Access,
         Documentation => Member_Doc_For_Grammar_List_Sep_F_Extra'Access,
         Owner         => Type_Index_For_Grammar_List_Sep,
         Member_Type   => Type_Index_For_Id,
         Null_For      => null,
         Indexes       => Indexes_For_Grammar_List_Sep_F_Extra'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Imported_Name_F_Original_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Imported_Name => 1);

      Member_Name_For_Imported_Name_F_Original_Name : aliased constant Text_Type :=
        "F_Original_Name";
      Member_Doc_For_Imported_Name_F_Original_Name  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Imported_Name_F_Original_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Imported_Name_F_Original_Name'Access,
         Documentation => Member_Doc_For_Imported_Name_F_Original_Name'Access,
         Owner         => Type_Index_For_Imported_Name,
         Member_Type   => Type_Index_For_Imported_Id,
         Null_For      => null,
         Indexes       => Indexes_For_Imported_Name_F_Original_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Imported_Name_F_Renaming : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Imported_Name => 2);

      Member_Name_For_Imported_Name_F_Renaming : aliased constant Text_Type :=
        "F_Renaming";
      Member_Doc_For_Imported_Name_F_Renaming  : aliased constant Text_Type := "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Imported_Name_F_Renaming  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Imported_Name_F_Renaming'Access,
         Documentation => Member_Doc_For_Imported_Name_F_Renaming'Access,
         Owner         => Type_Index_For_Imported_Name,
         Member_Type   => Type_Index_For_Def_Id,
         Null_For      => null,
         Indexes       => Indexes_For_Imported_Name_F_Renaming'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Lexer_Case_Rule_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Lexer_Case_Rule => 1);

      Member_Name_For_Lexer_Case_Rule_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Lexer_Case_Rule_F_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ErrorGrammarExpr`, :typeref:`GrammarCut`, :typeref:`GrammarDiscard`, :typeref:`GrammarList`, :typeref:`GrammarNull`, :typeref:`GrammarOptErrorGroup`, :typeref:`GrammarOptError`, :typeref:`GrammarOptGroup`, :typeref:`GrammarOpt`, :typeref:`GrammarOrExpr`, :typeref:`GrammarPick`, :typeref:`GrammarRuleRef`, :typeref:`GrammarSkip`, :typeref:`GrammarStopCut`, :typeref:`ParseNodeExpr`, :typeref:`TokenLit`, :typeref:`TokenNoCaseLit`, :typeref:`TokenPatternConcat`, :typeref:`TokenPatternLit`, :typeref:`TokenRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Lexer_Case_Rule_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lexer_Case_Rule_F_Expr'Access,
         Documentation => Member_Doc_For_Lexer_Case_Rule_F_Expr'Access,
         Owner         => Type_Index_For_Lexer_Case_Rule,
         Member_Type   => Type_Index_For_Grammar_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Lexer_Case_Rule_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Lexer_Case_Rule_F_Alts : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Lexer_Case_Rule => 2);

      Member_Name_For_Lexer_Case_Rule_F_Alts : aliased constant Text_Type :=
        "F_Alts";
      Member_Doc_For_Lexer_Case_Rule_F_Alts  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Lexer_Case_Rule_F_Alts  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lexer_Case_Rule_F_Alts'Access,
         Documentation => Member_Doc_For_Lexer_Case_Rule_F_Alts'Access,
         Owner         => Type_Index_For_Lexer_Case_Rule,
         Member_Type   => Type_Index_For_Base_Lexer_Case_Rule_Alt_List,
         Null_For      => null,
         Indexes       => Indexes_For_Lexer_Case_Rule_F_Alts'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Lexer_Case_Rule_Send_F_Sent : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Lexer_Case_Rule_Send => 1);

      Member_Name_For_Lexer_Case_Rule_Send_F_Sent : aliased constant Text_Type :=
        "F_Sent";
      Member_Doc_For_Lexer_Case_Rule_Send_F_Sent  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Lexer_Case_Rule_Send_F_Sent  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lexer_Case_Rule_Send_F_Sent'Access,
         Documentation => Member_Doc_For_Lexer_Case_Rule_Send_F_Sent'Access,
         Owner         => Type_Index_For_Lexer_Case_Rule_Send,
         Member_Type   => Type_Index_For_Ref_Id,
         Null_For      => null,
         Indexes       => Indexes_For_Lexer_Case_Rule_Send_F_Sent'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Lexer_Case_Rule_Send_F_Match_Size : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Lexer_Case_Rule_Send => 2);

      Member_Name_For_Lexer_Case_Rule_Send_F_Match_Size : aliased constant Text_Type :=
        "F_Match_Size";
      Member_Doc_For_Lexer_Case_Rule_Send_F_Match_Size  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Lexer_Case_Rule_Send_F_Match_Size  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lexer_Case_Rule_Send_F_Match_Size'Access,
         Documentation => Member_Doc_For_Lexer_Case_Rule_Send_F_Match_Size'Access,
         Owner         => Type_Index_For_Lexer_Case_Rule_Send,
         Member_Type   => Type_Index_For_Num_Lit,
         Null_For      => null,
         Indexes       => Indexes_For_Lexer_Case_Rule_Send_F_Match_Size'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Complex_Pattern_F_Decl : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Complex_Pattern => 1,
Type_Index_For_Renaming_Complex_Pattern => 1);

      Member_Name_For_Complex_Pattern_F_Decl : aliased constant Text_Type :=
        "F_Decl";
      Member_Doc_For_Complex_Pattern_F_Decl  : aliased constant Text_Type := "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Complex_Pattern_F_Decl  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Complex_Pattern_F_Decl'Access,
         Documentation => Member_Doc_For_Complex_Pattern_F_Decl'Access,
         Owner         => Type_Index_For_Complex_Pattern,
         Member_Type   => Type_Index_For_Binding_Val_Decl,
         Null_For      => null,
         Indexes       => Indexes_For_Complex_Pattern_F_Decl'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Complex_Pattern_F_Pattern : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Complex_Pattern => 2,
Type_Index_For_Renaming_Complex_Pattern => 2);

      Member_Name_For_Complex_Pattern_F_Pattern : aliased constant Text_Type :=
        "F_Pattern";
      Member_Doc_For_Complex_Pattern_F_Pattern  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyTypePattern`, :typeref:`BoolPattern`, :typeref:`IntegerPattern`, :typeref:`ListPattern`, :typeref:`NullPattern`, :typeref:`ParenPattern`, :typeref:`RegexPattern`, :typeref:`TypePattern`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Complex_Pattern_F_Pattern  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Complex_Pattern_F_Pattern'Access,
         Documentation => Member_Doc_For_Complex_Pattern_F_Pattern'Access,
         Owner         => Type_Index_For_Complex_Pattern,
         Member_Type   => Type_Index_For_Pattern,
         Null_For      => null,
         Indexes       => Indexes_For_Complex_Pattern_F_Pattern'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Complex_Pattern_F_Details : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Complex_Pattern => 3,
Type_Index_For_Renaming_Complex_Pattern => 3);

      Member_Name_For_Complex_Pattern_F_Details : aliased constant Text_Type :=
        "F_Details";
      Member_Doc_For_Complex_Pattern_F_Details  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Complex_Pattern_F_Details  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Complex_Pattern_F_Details'Access,
         Documentation => Member_Doc_For_Complex_Pattern_F_Details'Access,
         Owner         => Type_Index_For_Complex_Pattern,
         Member_Type   => Type_Index_For_Pattern_Detail_List,
         Null_For      => null,
         Indexes       => Indexes_For_Complex_Pattern_F_Details'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Complex_Pattern_F_Predicate : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Complex_Pattern => 4,
Type_Index_For_Renaming_Complex_Pattern => 4);

      Member_Name_For_Complex_Pattern_F_Predicate : aliased constant Text_Type :=
        "F_Predicate";
      Member_Doc_For_Complex_Pattern_F_Predicate  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Complex_Pattern_F_Predicate  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Complex_Pattern_F_Predicate'Access,
         Documentation => Member_Doc_For_Complex_Pattern_F_Predicate'Access,
         Owner         => Type_Index_For_Complex_Pattern,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Complex_Pattern_F_Predicate'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Ellipsis_Pattern_F_Binding : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Ellipsis_Pattern => 1);

      Member_Name_For_Ellipsis_Pattern_F_Binding : aliased constant Text_Type :=
        "F_Binding";
      Member_Doc_For_Ellipsis_Pattern_F_Binding  : aliased constant Text_Type := "This field may be null even when there are no parsing errors.";
      Member_Desc_For_Ellipsis_Pattern_F_Binding  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Ellipsis_Pattern_F_Binding'Access,
         Documentation => Member_Doc_For_Ellipsis_Pattern_F_Binding'Access,
         Owner         => Type_Index_For_Ellipsis_Pattern,
         Member_Type   => Type_Index_For_Id,
         Null_For      => null,
         Indexes       => Indexes_For_Ellipsis_Pattern_F_Binding'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_List_Pattern_F_Sub_Patterns : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_List_Pattern => 1);

      Member_Name_For_List_Pattern_F_Sub_Patterns : aliased constant Text_Type :=
        "F_Sub_Patterns";
      Member_Doc_For_List_Pattern_F_Sub_Patterns  : aliased constant Text_Type := "This field contains a list that itself contains one of the following nodes: :typeref:`ComplexPattern`, :typeref:`EllipsisPattern`, :typeref:`NotPattern`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_List_Pattern_F_Sub_Patterns  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_List_Pattern_F_Sub_Patterns'Access,
         Documentation => Member_Doc_For_List_Pattern_F_Sub_Patterns'Access,
         Owner         => Type_Index_For_List_Pattern,
         Member_Type   => Type_Index_For_Pattern_List,
         Null_For      => null,
         Indexes       => Indexes_For_List_Pattern_F_Sub_Patterns'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Not_Pattern_F_Sub_Pattern : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Not_Pattern => 1);

      Member_Name_For_Not_Pattern_F_Sub_Pattern : aliased constant Text_Type :=
        "F_Sub_Pattern";
      Member_Doc_For_Not_Pattern_F_Sub_Pattern  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ComplexPattern`, :typeref:`NotPattern`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Not_Pattern_F_Sub_Pattern  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Not_Pattern_F_Sub_Pattern'Access,
         Documentation => Member_Doc_For_Not_Pattern_F_Sub_Pattern'Access,
         Owner         => Type_Index_For_Not_Pattern,
         Member_Type   => Type_Index_For_Pattern,
         Null_For      => null,
         Indexes       => Indexes_For_Not_Pattern_F_Sub_Pattern'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Or_Pattern_F_Left_Sub_Pattern : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Or_Pattern => 1);

      Member_Name_For_Or_Pattern_F_Left_Sub_Pattern : aliased constant Text_Type :=
        "F_Left_Sub_Pattern";
      Member_Doc_For_Or_Pattern_F_Left_Sub_Pattern  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ComplexPattern`, :typeref:`NotPattern`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Or_Pattern_F_Left_Sub_Pattern  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Or_Pattern_F_Left_Sub_Pattern'Access,
         Documentation => Member_Doc_For_Or_Pattern_F_Left_Sub_Pattern'Access,
         Owner         => Type_Index_For_Or_Pattern,
         Member_Type   => Type_Index_For_Pattern,
         Null_For      => null,
         Indexes       => Indexes_For_Or_Pattern_F_Left_Sub_Pattern'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Or_Pattern_F_Right_Sub_Pattern : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Or_Pattern => 2);

      Member_Name_For_Or_Pattern_F_Right_Sub_Pattern : aliased constant Text_Type :=
        "F_Right_Sub_Pattern";
      Member_Doc_For_Or_Pattern_F_Right_Sub_Pattern  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ComplexPattern`, :typeref:`NotPattern`, :typeref:`OrPattern`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Or_Pattern_F_Right_Sub_Pattern  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Or_Pattern_F_Right_Sub_Pattern'Access,
         Documentation => Member_Doc_For_Or_Pattern_F_Right_Sub_Pattern'Access,
         Owner         => Type_Index_For_Or_Pattern,
         Member_Type   => Type_Index_For_Pattern,
         Null_For      => null,
         Indexes       => Indexes_For_Or_Pattern_F_Right_Sub_Pattern'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Paren_Pattern_F_Sub_Pattern : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Paren_Pattern => 1);

      Member_Name_For_Paren_Pattern_F_Sub_Pattern : aliased constant Text_Type :=
        "F_Sub_Pattern";
      Member_Doc_For_Paren_Pattern_F_Sub_Pattern  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ComplexPattern`, :typeref:`NotPattern`, :typeref:`OrPattern`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Paren_Pattern_F_Sub_Pattern  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Paren_Pattern_F_Sub_Pattern'Access,
         Documentation => Member_Doc_For_Paren_Pattern_F_Sub_Pattern'Access,
         Owner         => Type_Index_For_Paren_Pattern,
         Member_Type   => Type_Index_For_Pattern,
         Null_For      => null,
         Indexes       => Indexes_For_Paren_Pattern_F_Sub_Pattern'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Type_Pattern_F_Type_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Type_Pattern => 1);

      Member_Name_For_Type_Pattern_F_Type_Name : aliased constant Text_Type :=
        "F_Type_Name";
      Member_Doc_For_Type_Pattern_F_Type_Name  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`FunctionTypeRef`, :typeref:`GenericTypeRef`, :typeref:`SimpleTypeRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Type_Pattern_F_Type_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Type_Pattern_F_Type_Name'Access,
         Documentation => Member_Doc_For_Type_Pattern_F_Type_Name'Access,
         Owner         => Type_Index_For_Type_Pattern,
         Member_Type   => Type_Index_For_Type_Ref,
         Null_For      => null,
         Indexes       => Indexes_For_Type_Pattern_F_Type_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Field_Pattern_Detail_F_Id : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Field_Pattern_Detail => 1);

      Member_Name_For_Field_Pattern_Detail_F_Id : aliased constant Text_Type :=
        "F_Id";
      Member_Doc_For_Field_Pattern_Detail_F_Id  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Field_Pattern_Detail_F_Id  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Field_Pattern_Detail_F_Id'Access,
         Documentation => Member_Doc_For_Field_Pattern_Detail_F_Id'Access,
         Owner         => Type_Index_For_Field_Pattern_Detail,
         Member_Type   => Type_Index_For_Id,
         Null_For      => null,
         Indexes       => Indexes_For_Field_Pattern_Detail_F_Id'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Field_Pattern_Detail_F_Expected_Value : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Field_Pattern_Detail => 2);

      Member_Name_For_Field_Pattern_Detail_F_Expected_Value : aliased constant Text_Type :=
        "F_Expected_Value";
      Member_Doc_For_Field_Pattern_Detail_F_Expected_Value  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ComplexPattern`, :typeref:`NotPattern`, :typeref:`OrPattern`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Field_Pattern_Detail_F_Expected_Value  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Field_Pattern_Detail_F_Expected_Value'Access,
         Documentation => Member_Doc_For_Field_Pattern_Detail_F_Expected_Value'Access,
         Owner         => Type_Index_For_Field_Pattern_Detail,
         Member_Type   => Type_Index_For_Pattern,
         Null_For      => null,
         Indexes       => Indexes_For_Field_Pattern_Detail_F_Expected_Value'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Property_Pattern_Detail_F_Call : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Property_Pattern_Detail => 1);

      Member_Name_For_Property_Pattern_Detail_F_Call : aliased constant Text_Type :=
        "F_Call";
      Member_Doc_For_Property_Pattern_Detail_F_Call  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`KeepExpr`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`MatchExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Property_Pattern_Detail_F_Call  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Property_Pattern_Detail_F_Call'Access,
         Documentation => Member_Doc_For_Property_Pattern_Detail_F_Call'Access,
         Owner         => Type_Index_For_Property_Pattern_Detail,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Property_Pattern_Detail_F_Call'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Property_Pattern_Detail_F_Expected_Value : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Property_Pattern_Detail => 2);

      Member_Name_For_Property_Pattern_Detail_F_Expected_Value : aliased constant Text_Type :=
        "F_Expected_Value";
      Member_Doc_For_Property_Pattern_Detail_F_Expected_Value  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`ComplexPattern`, :typeref:`NotPattern`, :typeref:`OrPattern`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Property_Pattern_Detail_F_Expected_Value  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Property_Pattern_Detail_F_Expected_Value'Access,
         Documentation => Member_Doc_For_Property_Pattern_Detail_F_Expected_Value'Access,
         Owner         => Type_Index_For_Property_Pattern_Detail,
         Member_Type   => Type_Index_For_Pattern,
         Null_For      => null,
         Indexes       => Indexes_For_Property_Pattern_Detail_F_Expected_Value'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Function_Type_Ref_F_Param_Types : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Function_Type_Ref => 1);

      Member_Name_For_Function_Type_Ref_F_Param_Types : aliased constant Text_Type :=
        "F_Param_Types";
      Member_Doc_For_Function_Type_Ref_F_Param_Types  : aliased constant Text_Type := "This field contains a list that itself contains one of the following nodes: :typeref:`FunctionTypeRef`, :typeref:`GenericTypeRef`, :typeref:`SimpleTypeRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Function_Type_Ref_F_Param_Types  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Function_Type_Ref_F_Param_Types'Access,
         Documentation => Member_Doc_For_Function_Type_Ref_F_Param_Types'Access,
         Owner         => Type_Index_For_Function_Type_Ref,
         Member_Type   => Type_Index_For_Type_Ref_List,
         Null_For      => null,
         Indexes       => Indexes_For_Function_Type_Ref_F_Param_Types'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Function_Type_Ref_F_Return_Type : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Function_Type_Ref => 2);

      Member_Name_For_Function_Type_Ref_F_Return_Type : aliased constant Text_Type :=
        "F_Return_Type";
      Member_Doc_For_Function_Type_Ref_F_Return_Type  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`FunctionTypeRef`, :typeref:`GenericTypeRef`, :typeref:`SimpleTypeRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Function_Type_Ref_F_Return_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Function_Type_Ref_F_Return_Type'Access,
         Documentation => Member_Doc_For_Function_Type_Ref_F_Return_Type'Access,
         Owner         => Type_Index_For_Function_Type_Ref,
         Member_Type   => Type_Index_For_Type_Ref,
         Null_For      => null,
         Indexes       => Indexes_For_Function_Type_Ref_F_Return_Type'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Generic_Type_Ref_F_Type_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Generic_Type_Ref => 1);

      Member_Name_For_Generic_Type_Ref_F_Type_Name : aliased constant Text_Type :=
        "F_Type_Name";
      Member_Doc_For_Generic_Type_Ref_F_Type_Name  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`DotExpr`, :typeref:`RefId`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Generic_Type_Ref_F_Type_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Generic_Type_Ref_F_Type_Name'Access,
         Documentation => Member_Doc_For_Generic_Type_Ref_F_Type_Name'Access,
         Owner         => Type_Index_For_Generic_Type_Ref,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Generic_Type_Ref_F_Type_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Generic_Type_Ref_F_Args : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Generic_Type_Ref => 2);

      Member_Name_For_Generic_Type_Ref_F_Args : aliased constant Text_Type :=
        "F_Args";
      Member_Doc_For_Generic_Type_Ref_F_Args  : aliased constant Text_Type := "This field contains a list that itself contains one of the following nodes: :typeref:`FunctionTypeRef`, :typeref:`GenericTypeRef`, :typeref:`SimpleTypeRef`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Generic_Type_Ref_F_Args  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Generic_Type_Ref_F_Args'Access,
         Documentation => Member_Doc_For_Generic_Type_Ref_F_Args'Access,
         Owner         => Type_Index_For_Generic_Type_Ref,
         Member_Type   => Type_Index_For_Type_Ref_List,
         Null_For      => null,
         Indexes       => Indexes_For_Generic_Type_Ref_F_Args'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Simple_Type_Ref_F_Type_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Simple_Type_Ref => 1);

      Member_Name_For_Simple_Type_Ref_F_Type_Name : aliased constant Text_Type :=
        "F_Type_Name";
      Member_Doc_For_Simple_Type_Ref_F_Type_Name  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`DotExpr`, :typeref:`RefId`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Simple_Type_Ref_F_Type_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Simple_Type_Ref_F_Type_Name'Access,
         Documentation => Member_Doc_For_Simple_Type_Ref_F_Type_Name'Access,
         Owner         => Type_Index_For_Simple_Type_Ref,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Simple_Type_Ref_F_Type_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Var_Bind_F_Name : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Var_Bind => 1);

      Member_Name_For_Var_Bind_F_Name : aliased constant Text_Type :=
        "F_Name";
      Member_Doc_For_Var_Bind_F_Name  : aliased constant Text_Type := "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Var_Bind_F_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Var_Bind_F_Name'Access,
         Documentation => Member_Doc_For_Var_Bind_F_Name'Access,
         Owner         => Type_Index_For_Var_Bind,
         Member_Type   => Type_Index_For_Ref_Id,
         Null_For      => null,
         Indexes       => Indexes_For_Var_Bind_F_Name'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      
         Indexes_For_Var_Bind_F_Expr : aliased constant Syntax_Field_Indexes :=
           (Type_Index_For_Var_Bind => 2);

      Member_Name_For_Var_Bind_F_Expr : aliased constant Text_Type :=
        "F_Expr";
      Member_Doc_For_Var_Bind_F_Expr  : aliased constant Text_Type := "This field can contain one of the following nodes: :typeref:`AnyOf`, :typeref:`ArrayLiteral`, :typeref:`BigNumLit`, :typeref:`BinOp`, :typeref:`BlockExpr`, :typeref:`BlockStringLit`, :typeref:`CallExpr`, :typeref:`CastExpr`, :typeref:`CharLit`, :typeref:`DotExpr`, :typeref:`ErrorOnNull`, :typeref:`GenericInstantiation`, :typeref:`IfExpr`, :typeref:`Isa`, :typeref:`KeepExpr`, :typeref:`LambdaExpr`, :typeref:`LogicAssign`, :typeref:`LogicExpr`, :typeref:`LogicPredicate`, :typeref:`LogicPropagate`, :typeref:`LogicUnify`, :typeref:`MatchExpr`, :typeref:`NotExpr`, :typeref:`NullLit`, :typeref:`NumLit`, :typeref:`ParenExpr`, :typeref:`Query`, :typeref:`RaiseExpr`, :typeref:`RefId`, :typeref:`SingleLineStringLit`, :typeref:`SubscriptExpr`, :typeref:`TryExpr`, :typeref:`UnOp`" & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "When there are no parsing errors, this field is never null.";
      Member_Desc_For_Var_Bind_F_Expr  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Var_Bind_F_Expr'Access,
         Documentation => Member_Doc_For_Var_Bind_F_Expr'Access,
         Owner         => Type_Index_For_Var_Bind,
         Member_Type   => Type_Index_For_Expr,
         Null_For      => null,
         Indexes       => Indexes_For_Var_Bind_F_Expr'Access,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Parent : aliased constant Text_Type :=
        "Parent";
      Member_Doc_For_Parent  : aliased constant Text_Type := "Return the syntactic parent for this node. Return null for the root node.";
      Member_Desc_For_Parent  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Parent'Access,
         Documentation => Member_Doc_For_Parent'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Lkt_Node,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      

         Arg_Name_1 : aliased constant Text_Type :=
           "With_Self";
         

      

      

      Member_Name_For_Parents : aliased constant Text_Type :=
        "Parents";
      Member_Doc_For_Parents  : aliased constant Text_Type := "Return an array that contains the lexical parents, this node included iff ``with_self`` is True. Nearer parents are first in the list.";
      Member_Desc_For_Parents  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 1,
         Name          => Member_Name_For_Parents'Access,
         Documentation => Member_Doc_For_Parents'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Lkt_Node_Array,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 =>
              (Name          => Arg_Name_1'Access,
               Argument_Type => Type_Index_For_Bool,
               Default_Value => (Kind => Boolean_Value, Boolean_Value => True)))
        );

      


      

      

      Member_Name_For_Children : aliased constant Text_Type :=
        "Children";
      Member_Doc_For_Children  : aliased constant Text_Type := "" & Character_Type'Val (10)
 & "                Return an array that contains the direct lexical" & Character_Type'Val (10)
 & "                children." & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "                .. warning:: This constructs a whole array every-time you call" & Character_Type'Val (10)
 & "                    it, and as such is less efficient than calling the" & Character_Type'Val (10)
 & "                    ``Child`` built-in." & Character_Type'Val (10)
 & "                ";
      Member_Desc_For_Children  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Children'Access,
         Documentation => Member_Doc_For_Children'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Lkt_Node_Array,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Token_Start : aliased constant Text_Type :=
        "Token_Start";
      Member_Doc_For_Token_Start  : aliased constant Text_Type := "Return the first token used to parse this node.";
      Member_Desc_For_Token_Start  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Token_Start'Access,
         Documentation => Member_Doc_For_Token_Start'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Token,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Token_End : aliased constant Text_Type :=
        "Token_End";
      Member_Doc_For_Token_End  : aliased constant Text_Type := "Return the last token used to parse this node.";
      Member_Desc_For_Token_End  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Token_End'Access,
         Documentation => Member_Doc_For_Token_End'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Token,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Child_Index : aliased constant Text_Type :=
        "Child_Index";
      Member_Doc_For_Child_Index  : aliased constant Text_Type := "Return the 0-based index for Node in its parent's children.";
      Member_Desc_For_Child_Index  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Child_Index'Access,
         Documentation => Member_Doc_For_Child_Index'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Int,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Previous_Sibling : aliased constant Text_Type :=
        "Previous_Sibling";
      Member_Doc_For_Previous_Sibling  : aliased constant Text_Type := "" & Character_Type'Val (10)
 & "                Return the node's previous sibling, or null if there is no such" & Character_Type'Val (10)
 & "                sibling." & Character_Type'Val (10)
 & "                ";
      Member_Desc_For_Previous_Sibling  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Previous_Sibling'Access,
         Documentation => Member_Doc_For_Previous_Sibling'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Lkt_Node,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Next_Sibling : aliased constant Text_Type :=
        "Next_Sibling";
      Member_Doc_For_Next_Sibling  : aliased constant Text_Type := "" & Character_Type'Val (10)
 & "                Return the node's next sibling, or null if there is no such" & Character_Type'Val (10)
 & "                sibling." & Character_Type'Val (10)
 & "                ";
      Member_Desc_For_Next_Sibling  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Next_Sibling'Access,
         Documentation => Member_Doc_For_Next_Sibling'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Lkt_Node,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Unit : aliased constant Text_Type :=
        "Unit";
      Member_Doc_For_Unit  : aliased constant Text_Type := "Return the analysis unit owning this node.";
      Member_Desc_For_Unit  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Unit'Access,
         Documentation => Member_Doc_For_Unit'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Analysis_Unit,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Is_Ghost : aliased constant Text_Type :=
        "Is_Ghost";
      Member_Doc_For_Is_Ghost  : aliased constant Text_Type := "" & Character_Type'Val (10)
 & "                Return whether the node is a ghost." & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "                Unlike regular nodes, ghost nodes cover no token in the input" & Character_Type'Val (10)
 & "                source: they are logically located instead between two tokens." & Character_Type'Val (10)
 & "                Both the ``token_start`` and the ``token_end`` of all ghost" & Character_Type'Val (10)
 & "                nodes is the token right after this logical position." & Character_Type'Val (10)
 & "                ";
      Member_Desc_For_Is_Ghost  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Is_Ghost'Access,
         Documentation => Member_Doc_For_Is_Ghost'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Full_Sloc_Image : aliased constant Text_Type :=
        "Full_Sloc_Image";
      Member_Doc_For_Full_Sloc_Image  : aliased constant Text_Type := "" & Character_Type'Val (10)
 & "                Return a string containing the filename + the sloc in GNU" & Character_Type'Val (10)
 & "                conformant format. Useful to create diagnostics from a node." & Character_Type'Val (10)
 & "                ";
      Member_Desc_For_Full_Sloc_Image  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Full_Sloc_Image'Access,
         Documentation => Member_Doc_For_Full_Sloc_Image'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_String,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      

         Arg_Name_2 : aliased constant Text_Type :=
           "Kind";
         

      

      

      Member_Name_For_Completion_Item_Kind_To_Int : aliased constant Text_Type :=
        "Completion_Item_Kind_To_Int";
      Member_Doc_For_Completion_Item_Kind_To_Int  : aliased constant Text_Type := "" & Character_Type'Val (10)
 & "                Convert a CompletionItemKind enum to its corresponding" & Character_Type'Val (10)
 & "                integer value." & Character_Type'Val (10)
 & "                ";
      Member_Desc_For_Completion_Item_Kind_To_Int  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 1,
         Name          => Member_Name_For_Completion_Item_Kind_To_Int'Access,
         Documentation => Member_Doc_For_Completion_Item_Kind_To_Int'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Int,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (2 =>
              (Name          => Arg_Name_2'Access,
               Argument_Type => Type_Index_For_Completion_Item_Kind,
               Default_Value => (Kind => None)))
        );

      

         Arg_Name_3 : aliased constant Text_Type :=
           "Enable";
         

      

      

      Member_Name_For_Lkt_Node_P_Set_Solver_Debug_Mode : aliased constant Text_Type :=
        "P_Set_Solver_Debug_Mode";
      Member_Doc_For_Lkt_Node_P_Set_Solver_Debug_Mode  : aliased constant Text_Type := "Enable or disable the solver traces for debugging purposes." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Set_Solver_Debug_Mode  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 1,
         Name          => Member_Name_For_Lkt_Node_P_Set_Solver_Debug_Mode'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Set_Solver_Debug_Mode'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (3 =>
              (Name          => Arg_Name_3'Access,
               Argument_Type => Type_Index_For_Bool,
               Default_Value => (Kind => None)))
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Prelude_Unit : aliased constant Text_Type :=
        "P_Prelude_Unit";
      Member_Doc_For_Lkt_Node_P_Prelude_Unit  : aliased constant Text_Type := "Return the unit that contains the Lkt prelude (predefined types and" & Character_Type'Val (10)
 & "values)." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Prelude_Unit  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Prelude_Unit'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Prelude_Unit'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Analysis_Unit,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Basic_Trait_Gen : aliased constant Text_Type :=
        "P_Basic_Trait_Gen";
      Member_Doc_For_Lkt_Node_P_Basic_Trait_Gen  : aliased constant Text_Type := "Unit method. Return the ``BasicTrait`` builtin generic trait." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Basic_Trait_Gen  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Basic_Trait_Gen'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Basic_Trait_Gen'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Generic_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Basic_Trait : aliased constant Text_Type :=
        "P_Basic_Trait";
      Member_Doc_For_Lkt_Node_P_Basic_Trait  : aliased constant Text_Type := "Unit method. Return the ``BasicTrait`` builtin trait." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Basic_Trait  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Basic_Trait'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Basic_Trait'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Trait_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Node_Gen_Trait : aliased constant Text_Type :=
        "P_Node_Gen_Trait";
      Member_Doc_For_Lkt_Node_P_Node_Gen_Trait  : aliased constant Text_Type := "Unit method. Return the ``Node`` builtin generic trait." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Node_Gen_Trait  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Node_Gen_Trait'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Node_Gen_Trait'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Generic_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Node_Trait : aliased constant Text_Type :=
        "P_Node_Trait";
      Member_Doc_For_Lkt_Node_P_Node_Trait  : aliased constant Text_Type := "Unit method. Return the ``Node`` builtin trait." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Node_Trait  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Node_Trait'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Node_Trait'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Trait_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Indexable_Gen_Trait : aliased constant Text_Type :=
        "P_Indexable_Gen_Trait";
      Member_Doc_For_Lkt_Node_P_Indexable_Gen_Trait  : aliased constant Text_Type := "Unit method. Return the ``Node`` builtin generic trait." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Indexable_Gen_Trait  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Indexable_Gen_Trait'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Indexable_Gen_Trait'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Generic_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Indexable_Trait : aliased constant Text_Type :=
        "P_Indexable_Trait";
      Member_Doc_For_Lkt_Node_P_Indexable_Trait  : aliased constant Text_Type := "Unit method. Return the ``Node`` builtin trait." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Indexable_Trait  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Indexable_Trait'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Indexable_Trait'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Trait_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Token_Node_Trait : aliased constant Text_Type :=
        "P_Token_Node_Trait";
      Member_Doc_For_Lkt_Node_P_Token_Node_Trait  : aliased constant Text_Type := "Unit method. Return the ``TokenNode`` builtin trait." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Token_Node_Trait  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Token_Node_Trait'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Token_Node_Trait'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Named_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Error_Node_Trait : aliased constant Text_Type :=
        "P_Error_Node_Trait";
      Member_Doc_For_Lkt_Node_P_Error_Node_Trait  : aliased constant Text_Type := "Unit method. Return the ``ErrorNode`` builtin trait." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Error_Node_Trait  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Error_Node_Trait'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Error_Node_Trait'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Named_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Char_Type : aliased constant Text_Type :=
        "P_Char_Type";
      Member_Doc_For_Lkt_Node_P_Char_Type  : aliased constant Text_Type := "Unit method. Return the character builtin type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Char_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Char_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Char_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Named_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Int_Type : aliased constant Text_Type :=
        "P_Int_Type";
      Member_Doc_For_Lkt_Node_P_Int_Type  : aliased constant Text_Type := "Unit method. Return the integer builtin type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Int_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Int_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Int_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Named_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Bool_Type : aliased constant Text_Type :=
        "P_Bool_Type";
      Member_Doc_For_Lkt_Node_P_Bool_Type  : aliased constant Text_Type := "Unit method. Return the boolean builtin type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Bool_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Bool_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Bool_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Named_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Bigint_Type : aliased constant Text_Type :=
        "P_Bigint_Type";
      Member_Doc_For_Lkt_Node_P_Bigint_Type  : aliased constant Text_Type := "Unit method. Return the big integer builtin type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Bigint_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Bigint_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Bigint_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Named_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_String_Type : aliased constant Text_Type :=
        "P_String_Type";
      Member_Doc_For_Lkt_Node_P_String_Type  : aliased constant Text_Type := "Unit method. Return the string builtin type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_String_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_String_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_String_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Named_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Symbol_Type : aliased constant Text_Type :=
        "P_Symbol_Type";
      Member_Doc_For_Lkt_Node_P_Symbol_Type  : aliased constant Text_Type := "Unit method. Return the string builtin type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Symbol_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Symbol_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Symbol_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Named_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Property_Error_Type : aliased constant Text_Type :=
        "P_Property_Error_Type";
      Member_Doc_For_Lkt_Node_P_Property_Error_Type  : aliased constant Text_Type := "Unit method. Return the property error builtin type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Property_Error_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Property_Error_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Property_Error_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Named_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Regexp_Type : aliased constant Text_Type :=
        "P_Regexp_Type";
      Member_Doc_For_Lkt_Node_P_Regexp_Type  : aliased constant Text_Type := "Unit method. Return the regexp builtin type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Regexp_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Regexp_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Regexp_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Named_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Entity_Gen_Type : aliased constant Text_Type :=
        "P_Entity_Gen_Type";
      Member_Doc_For_Lkt_Node_P_Entity_Gen_Type  : aliased constant Text_Type := "Unit method. Return the logicvar builtin type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Entity_Gen_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Entity_Gen_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Entity_Gen_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Generic_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Entity_Type : aliased constant Text_Type :=
        "P_Entity_Type";
      Member_Doc_For_Lkt_Node_P_Entity_Type  : aliased constant Text_Type := "Unit method. Return the logicvar builtin type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Entity_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Entity_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Entity_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Named_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Logicvar_Type : aliased constant Text_Type :=
        "P_Logicvar_Type";
      Member_Doc_For_Lkt_Node_P_Logicvar_Type  : aliased constant Text_Type := "Unit method. Return the logicvar builtin type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Logicvar_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Logicvar_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Logicvar_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Named_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Equation_Type : aliased constant Text_Type :=
        "P_Equation_Type";
      Member_Doc_For_Lkt_Node_P_Equation_Type  : aliased constant Text_Type := "Unit method. Return the logicvar builtin type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Equation_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Equation_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Equation_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Named_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Array_Gen_Type : aliased constant Text_Type :=
        "P_Array_Gen_Type";
      Member_Doc_For_Lkt_Node_P_Array_Gen_Type  : aliased constant Text_Type := "Unit method. Return the array builtin generic type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Array_Gen_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Array_Gen_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Array_Gen_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Generic_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Array_Type : aliased constant Text_Type :=
        "P_Array_Type";
      Member_Doc_For_Lkt_Node_P_Array_Type  : aliased constant Text_Type := "Unit method. Return the array builtin type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Array_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Array_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Array_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Named_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Astlist_Gen_Type : aliased constant Text_Type :=
        "P_Astlist_Gen_Type";
      Member_Doc_For_Lkt_Node_P_Astlist_Gen_Type  : aliased constant Text_Type := "Unit method. Return the ASTList builtin generic type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Astlist_Gen_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Astlist_Gen_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Astlist_Gen_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Generic_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Astlist_Type : aliased constant Text_Type :=
        "P_Astlist_Type";
      Member_Doc_For_Lkt_Node_P_Astlist_Type  : aliased constant Text_Type := "Unit method. Return the ASTList builtin type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Astlist_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Astlist_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Astlist_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Named_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Node_Builder_Gen_Type : aliased constant Text_Type :=
        "P_Node_Builder_Gen_Type";
      Member_Doc_For_Lkt_Node_P_Node_Builder_Gen_Type  : aliased constant Text_Type := "Unit method. Return the NodeBuilder builtin generic type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Node_Builder_Gen_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Node_Builder_Gen_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Node_Builder_Gen_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Generic_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Node_Builder_Type : aliased constant Text_Type :=
        "P_Node_Builder_Type";
      Member_Doc_For_Lkt_Node_P_Node_Builder_Type  : aliased constant Text_Type := "Unit method. Return the NodeBuilder builtin type." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Node_Builder_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Node_Builder_Type'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Node_Builder_Type'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Named_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Iterator_Gen_Trait : aliased constant Text_Type :=
        "P_Iterator_Gen_Trait";
      Member_Doc_For_Lkt_Node_P_Iterator_Gen_Trait  : aliased constant Text_Type := "Unit method. Return the Iterator builtin generic trait." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Iterator_Gen_Trait  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Iterator_Gen_Trait'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Iterator_Gen_Trait'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Generic_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Iterator_Trait : aliased constant Text_Type :=
        "P_Iterator_Trait";
      Member_Doc_For_Lkt_Node_P_Iterator_Trait  : aliased constant Text_Type := "Unit method. Return the Iterator builtin trait." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Iterator_Trait  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Iterator_Trait'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Iterator_Trait'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Trait_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Analysis_Unit_Gen_Trait : aliased constant Text_Type :=
        "P_Analysis_Unit_Gen_Trait";
      Member_Doc_For_Lkt_Node_P_Analysis_Unit_Gen_Trait  : aliased constant Text_Type := "Unit method. Return the ``AnalysisUnit`` builtin generic trait." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Analysis_Unit_Gen_Trait  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Analysis_Unit_Gen_Trait'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Analysis_Unit_Gen_Trait'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Generic_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Analysis_Unit_Trait : aliased constant Text_Type :=
        "P_Analysis_Unit_Trait";
      Member_Doc_For_Lkt_Node_P_Analysis_Unit_Trait  : aliased constant Text_Type := "Unit method. Return the ``AnalysisUnit`` builtin trait." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Analysis_Unit_Trait  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Analysis_Unit_Trait'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Analysis_Unit_Trait'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Trait_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Topmost_Invalid_Decl : aliased constant Text_Type :=
        "P_Topmost_Invalid_Decl";
      Member_Doc_For_Lkt_Node_P_Topmost_Invalid_Decl  : aliased constant Text_Type := "Return the topmost (from ``Self`` to the root node) FullDecl annotated" & Character_Type'Val (10)
 & "with ``@invalid``, null otherwise." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Topmost_Invalid_Decl  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Topmost_Invalid_Decl'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Topmost_Invalid_Decl'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Lkt_Node,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Nameres_Diagnostics : aliased constant Text_Type :=
        "P_Nameres_Diagnostics";
      Member_Doc_For_Lkt_Node_P_Nameres_Diagnostics  : aliased constant Text_Type := "If name resolution on this lkt compilation unit fails, this returns all" & Character_Type'Val (10)
 & "the diagnostics that were produced while resolving it." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Nameres_Diagnostics  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Nameres_Diagnostics'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Nameres_Diagnostics'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Solver_Diagnostic_Array,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Solve_Enclosing_Context : aliased constant Text_Type :=
        "P_Solve_Enclosing_Context";
      Member_Doc_For_Lkt_Node_P_Solve_Enclosing_Context  : aliased constant Text_Type := "Finds the nearest parent that is an xref_entry_point and solve its" & Character_Type'Val (10)
 & "equation." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Solve_Enclosing_Context  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Solve_Enclosing_Context'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Solve_Enclosing_Context'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Solver_Result,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Xref_Entry_Point : aliased constant Text_Type :=
        "P_Xref_Entry_Point";
      Member_Doc_For_Lkt_Node_P_Xref_Entry_Point  : aliased constant Text_Type := "Designates entities that are entry point for the xref solving" & Character_Type'Val (10)
 & "infrastructure. If this returns true, then nameres_diagnostics can be" & Character_Type'Val (10)
 & "called on it." & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Xref_Entry_Point  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Xref_Entry_Point'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Xref_Entry_Point'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Lkt_Node_P_Complete : aliased constant Text_Type :=
        "P_Complete";
      Member_Doc_For_Lkt_Node_P_Complete  : aliased constant Text_Type := "Return an array of completion item for language server clients" & Character_Type'Val (10)
;
      Member_Desc_For_Lkt_Node_P_Complete  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Lkt_Node_P_Complete'Access,
         Documentation => Member_Doc_For_Lkt_Node_P_Complete'Access,
         Owner         => Type_Index_For_Lkt_Node,
         Member_Type   => Type_Index_For_Complete_Item_Array,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Base_Import_P_Referenced_Units : aliased constant Text_Type :=
        "P_Referenced_Units";
      Member_Doc_For_Base_Import_P_Referenced_Units  : aliased constant Text_Type := "Return the list of units that contain the modules that this clause" & Character_Type'Val (10)
 & "imports. Load them if needed." & Character_Type'Val (10)
;
      Member_Desc_For_Base_Import_P_Referenced_Units  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Base_Import_P_Referenced_Units'Access,
         Documentation => Member_Doc_For_Base_Import_P_Referenced_Units'Access,
         Owner         => Type_Index_For_Base_Import,
         Member_Type   => Type_Index_For_Analysis_Unit_Array,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Base_Match_Branch_P_Match_Part : aliased constant Text_Type :=
        "P_Match_Part";
      Member_Doc_For_Base_Match_Branch_P_Match_Part  : aliased constant Text_Type := "Return the ""match"" part of the branch, either a pattern branch or a" & Character_Type'Val (10)
 & "legacy match branch with variable declaration." & Character_Type'Val (10)
;
      Member_Desc_For_Base_Match_Branch_P_Match_Part  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Base_Match_Branch_P_Match_Part'Access,
         Documentation => Member_Doc_For_Base_Match_Branch_P_Match_Part'Access,
         Owner         => Type_Index_For_Base_Match_Branch,
         Member_Type   => Type_Index_For_Lkt_Node,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Class_Qualifier_P_As_Bool : aliased constant Text_Type :=
        "P_As_Bool";
      Member_Doc_For_Class_Qualifier_P_As_Bool  : aliased constant Text_Type := "Return whether this node is present";
      Member_Desc_For_Class_Qualifier_P_As_Bool  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Class_Qualifier_P_As_Bool'Access,
         Documentation => Member_Doc_For_Class_Qualifier_P_As_Bool'Access,
         Owner         => Type_Index_For_Class_Qualifier,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decl_P_Custom_Image : aliased constant Text_Type :=
        "P_Custom_Image";
      Member_Doc_For_Decl_P_Custom_Image  : aliased constant Text_Type := "Return the image string using entity information." & Character_Type'Val (10)
;
      Member_Desc_For_Decl_P_Custom_Image  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decl_P_Custom_Image'Access,
         Documentation => Member_Doc_For_Decl_P_Custom_Image'Access,
         Owner         => Type_Index_For_Decl,
         Member_Type   => Type_Index_For_String,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decl_P_Decl_Type_Name : aliased constant Text_Type :=
        "P_Decl_Type_Name";
      Member_Doc_For_Decl_P_Decl_Type_Name  : aliased constant Text_Type := "Return the name of the declaration type, as it should be seen by" & Character_Type'Val (10)
 & "users/shown in diagnostics." & Character_Type'Val (10)
;
      Member_Desc_For_Decl_P_Decl_Type_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decl_P_Decl_Type_Name'Access,
         Documentation => Member_Doc_For_Decl_P_Decl_Type_Name'Access,
         Owner         => Type_Index_For_Decl,
         Member_Type   => Type_Index_For_String,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decl_P_Def_Ids : aliased constant Text_Type :=
        "P_Def_Ids";
      Member_Doc_For_Decl_P_Def_Ids  : aliased constant Text_Type := "Return all the defining names that this declaration defines." & Character_Type'Val (10)
;
      Member_Desc_For_Decl_P_Def_Ids  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decl_P_Def_Ids'Access,
         Documentation => Member_Doc_For_Decl_P_Def_Ids'Access,
         Owner         => Type_Index_For_Decl,
         Member_Type   => Type_Index_For_Def_Id_Array,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decl_P_As_Bare_Decl : aliased constant Text_Type :=
        "P_As_Bare_Decl";
      Member_Doc_For_Decl_P_As_Bare_Decl  : aliased constant Text_Type := "Get this declaration without rebindings information." & Character_Type'Val (10)
;
      Member_Desc_For_Decl_P_As_Bare_Decl  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decl_P_As_Bare_Decl'Access,
         Documentation => Member_Doc_For_Decl_P_As_Bare_Decl'Access,
         Owner         => Type_Index_For_Decl,
         Member_Type   => Type_Index_For_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decl_P_Get_Type : aliased constant Text_Type :=
        "P_Get_Type";
      Member_Doc_For_Decl_P_Get_Type  : aliased constant Text_Type := "Return the type of the Decl." & Character_Type'Val (10)
;
      Member_Desc_For_Decl_P_Get_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decl_P_Get_Type'Access,
         Documentation => Member_Doc_For_Decl_P_Get_Type'Access,
         Owner         => Type_Index_For_Decl,
         Member_Type   => Type_Index_For_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      

         Arg_Name_4 : aliased constant Text_Type :=
           "Cast_To";
         

      

      

      Member_Name_For_Decl_P_Get_Cast_Type : aliased constant Text_Type :=
        "P_Get_Cast_Type";
      Member_Doc_For_Decl_P_Get_Cast_Type  : aliased constant Text_Type := "If we are casting an entity (Self) to something that is not an entity," & Character_Type'Val (10)
 & "make it an entity." & Character_Type'Val (10)
;
      Member_Desc_For_Decl_P_Get_Cast_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 1,
         Name          => Member_Name_For_Decl_P_Get_Cast_Type'Access,
         Documentation => Member_Doc_For_Decl_P_Get_Cast_Type'Access,
         Owner         => Type_Index_For_Decl,
         Member_Type   => Type_Index_For_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (4 =>
              (Name          => Arg_Name_4'Access,
               Argument_Type => Type_Index_For_Type_Decl,
               Default_Value => (Kind => None)))
        );

      

         Arg_Name_5 : aliased constant Text_Type :=
           "Keep_Type";
         

      

      

      Member_Name_For_Decl_P_Get_Keep_Type : aliased constant Text_Type :=
        "P_Get_Keep_Type";
      Member_Doc_For_Decl_P_Get_Keep_Type  : aliased constant Text_Type := "Return the type of Entity when we only keep elements of type keep_type." & Character_Type'Val (10)
 & "If we are casting an entity (Self) to something that is not an entity," & Character_Type'Val (10)
 & "make it an entity." & Character_Type'Val (10)
;
      Member_Desc_For_Decl_P_Get_Keep_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 1,
         Name          => Member_Name_For_Decl_P_Get_Keep_Type'Access,
         Documentation => Member_Doc_For_Decl_P_Get_Keep_Type'Access,
         Owner         => Type_Index_For_Decl,
         Member_Type   => Type_Index_For_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (5 =>
              (Name          => Arg_Name_5'Access,
               Argument_Type => Type_Index_For_Type_Decl,
               Default_Value => (Kind => None)))
        );

      

         Arg_Name_6 : aliased constant Text_Type :=
           "Prefix_Type";
         

      

      

      Member_Name_For_Decl_P_Get_Suffix_Type : aliased constant Text_Type :=
        "P_Get_Suffix_Type";
      Member_Doc_For_Decl_P_Get_Suffix_Type  : aliased constant Text_Type := "If we are accessing a ParseField of an entity, then that field's type" & Character_Type'Val (10)
 & "also needs to be an entity." & Character_Type'Val (10)
;
      Member_Desc_For_Decl_P_Get_Suffix_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 1,
         Name          => Member_Name_For_Decl_P_Get_Suffix_Type'Access,
         Documentation => Member_Doc_For_Decl_P_Get_Suffix_Type'Access,
         Owner         => Type_Index_For_Decl,
         Member_Type   => Type_Index_For_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (6 =>
              (Name          => Arg_Name_6'Access,
               Argument_Type => Type_Index_For_Type_Decl,
               Default_Value => (Kind => None)))
        );

      


      

      

      Member_Name_For_Decl_P_Is_Generic : aliased constant Text_Type :=
        "P_Is_Generic";
      Member_Doc_For_Decl_P_Is_Generic  : aliased constant Text_Type := "Returns whether the Decl is generic." & Character_Type'Val (10)
;
      Member_Desc_For_Decl_P_Is_Generic  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decl_P_Is_Generic'Access,
         Documentation => Member_Doc_For_Decl_P_Is_Generic'Access,
         Owner         => Type_Index_For_Decl,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decl_P_Return_Type_Is_Instantiated : aliased constant Text_Type :=
        "P_Return_Type_Is_Instantiated";
      Member_Doc_For_Decl_P_Return_Type_Is_Instantiated  : aliased constant Text_Type := "Return True if the return type of this function is instantiated." & Character_Type'Val (10)
;
      Member_Desc_For_Decl_P_Return_Type_Is_Instantiated  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decl_P_Return_Type_Is_Instantiated'Access,
         Documentation => Member_Doc_For_Decl_P_Return_Type_Is_Instantiated'Access,
         Owner         => Type_Index_For_Decl,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decl_P_Is_Instantiated : aliased constant Text_Type :=
        "P_Is_Instantiated";
      Member_Doc_For_Decl_P_Is_Instantiated  : aliased constant Text_Type := "Return True if Self is an instantiated declaration, meaning that it" & Character_Type'Val (10)
 & "does not use any of its declared generic types." & Character_Type'Val (10)
;
      Member_Desc_For_Decl_P_Is_Instantiated  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decl_P_Is_Instantiated'Access,
         Documentation => Member_Doc_For_Decl_P_Is_Instantiated'Access,
         Owner         => Type_Index_For_Decl,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decl_P_Name : aliased constant Text_Type :=
        "P_Name";
      Member_Doc_For_Decl_P_Name  : aliased constant Text_Type := "Return the symbol corresponding to the name of this declaration." & Character_Type'Val (10)
;
      Member_Desc_For_Decl_P_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decl_P_Name'Access,
         Documentation => Member_Doc_For_Decl_P_Name'Access,
         Owner         => Type_Index_For_Decl,
         Member_Type   => Type_Index_For_Symbol,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Decl_P_Full_Name : aliased constant Text_Type :=
        "P_Full_Name";
      Member_Doc_For_Decl_P_Full_Name  : aliased constant Text_Type := "Return the full name of this decl, as it should be seen by users/shown" & Character_Type'Val (10)
 & "in diagnostics." & Character_Type'Val (10)
;
      Member_Desc_For_Decl_P_Full_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Decl_P_Full_Name'Access,
         Documentation => Member_Doc_For_Decl_P_Full_Name'Access,
         Owner         => Type_Index_For_Decl,
         Member_Type   => Type_Index_For_String,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Fun_Decl_P_Is_Dynamic_Combiner : aliased constant Text_Type :=
        "P_Is_Dynamic_Combiner";
      Member_Doc_For_Fun_Decl_P_Is_Dynamic_Combiner  : aliased constant Text_Type := "When this property is called by a LogicCallExpr, return whether it" & Character_Type'Val (10)
 & "expects a dynamic number of arguments. In other words, it expects an" & Character_Type'Val (10)
 & "array of entities as its first argument." & Character_Type'Val (10)
;
      Member_Desc_For_Fun_Decl_P_Is_Dynamic_Combiner  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Fun_Decl_P_Is_Dynamic_Combiner'Access,
         Documentation => Member_Doc_For_Fun_Decl_P_Is_Dynamic_Combiner'Access,
         Owner         => Type_Index_For_Fun_Decl,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      

         Arg_Name_7 : aliased constant Text_Type :=
           "Units";
         

      

      

      Member_Name_For_Fun_Decl_P_Find_All_Overrides : aliased constant Text_Type :=
        "P_Find_All_Overrides";
      Member_Doc_For_Fun_Decl_P_Find_All_Overrides  : aliased constant Text_Type := "Return the list of all RefId that refer to this DefId." & Character_Type'Val (10)
;
      Member_Desc_For_Fun_Decl_P_Find_All_Overrides  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 1,
         Name          => Member_Name_For_Fun_Decl_P_Find_All_Overrides'Access,
         Documentation => Member_Doc_For_Fun_Decl_P_Find_All_Overrides'Access,
         Owner         => Type_Index_For_Fun_Decl,
         Member_Type   => Type_Index_For_Fun_Decl_Array,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (7 =>
              (Name          => Arg_Name_7'Access,
               Argument_Type => Type_Index_For_Analysis_Unit_Array,
               Default_Value => (Kind => None)))
        );

      


      

      

      Member_Name_For_Type_Decl_P_Def_Id : aliased constant Text_Type :=
        "P_Def_Id";
      Member_Doc_For_Type_Decl_P_Def_Id  : aliased constant Text_Type := "Return the defining name of this type declaration" & Character_Type'Val (10)
;
      Member_Desc_For_Type_Decl_P_Def_Id  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Type_Decl_P_Def_Id'Access,
         Documentation => Member_Doc_For_Type_Decl_P_Def_Id'Access,
         Owner         => Type_Index_For_Type_Decl,
         Member_Type   => Type_Index_For_Def_Id,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Type_Decl_P_Base_Type : aliased constant Text_Type :=
        "P_Base_Type";
      Member_Doc_For_Type_Decl_P_Base_Type  : aliased constant Text_Type := "Return the base type for this node, if any." & Character_Type'Val (10)
;
      Member_Desc_For_Type_Decl_P_Base_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Type_Decl_P_Base_Type'Access,
         Documentation => Member_Doc_For_Type_Decl_P_Base_Type'Access,
         Owner         => Type_Index_For_Type_Decl,
         Member_Type   => Type_Index_For_Type_Ref,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Type_Decl_P_Base_Type_If_Entity : aliased constant Text_Type :=
        "P_Base_Type_If_Entity";
      Member_Doc_For_Type_Decl_P_Base_Type_If_Entity  : aliased constant Text_Type := "Return the base type for this node, if any." & Character_Type'Val (10)
;
      Member_Desc_For_Type_Decl_P_Base_Type_If_Entity  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Type_Decl_P_Base_Type_If_Entity'Access,
         Documentation => Member_Doc_For_Type_Decl_P_Base_Type_If_Entity'Access,
         Owner         => Type_Index_For_Type_Decl,
         Member_Type   => Type_Index_For_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Excludes_Null_P_As_Bool : aliased constant Text_Type :=
        "P_As_Bool";
      Member_Doc_For_Excludes_Null_P_As_Bool  : aliased constant Text_Type := "Return whether this node is present";
      Member_Desc_For_Excludes_Null_P_As_Bool  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Excludes_Null_P_As_Bool'Access,
         Documentation => Member_Doc_For_Excludes_Null_P_As_Bool'Access,
         Owner         => Type_Index_For_Excludes_Null,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Expr_P_Get_Type : aliased constant Text_Type :=
        "P_Get_Type";
      Member_Doc_For_Expr_P_Get_Type  : aliased constant Text_Type := "Return the type of this expression." & Character_Type'Val (10)
;
      Member_Desc_For_Expr_P_Get_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Expr_P_Get_Type'Access,
         Documentation => Member_Doc_For_Expr_P_Get_Type'Access,
         Owner         => Type_Index_For_Expr,
         Member_Type   => Type_Index_For_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Expr_P_Get_Generic_Type : aliased constant Text_Type :=
        "P_Get_Generic_Type";
      Member_Doc_For_Expr_P_Get_Generic_Type  : aliased constant Text_Type := "Return the expected type of this expression." & Character_Type'Val (10)
;
      Member_Desc_For_Expr_P_Get_Generic_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Expr_P_Get_Generic_Type'Access,
         Documentation => Member_Doc_For_Expr_P_Get_Generic_Type'Access,
         Owner         => Type_Index_For_Expr,
         Member_Type   => Type_Index_For_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Expr_P_Get_Expected_Type : aliased constant Text_Type :=
        "P_Get_Expected_Type";
      Member_Doc_For_Expr_P_Get_Expected_Type  : aliased constant Text_Type := "Return the expected type of this expression." & Character_Type'Val (10)
;
      Member_Desc_For_Expr_P_Get_Expected_Type  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Expr_P_Get_Expected_Type'Access,
         Documentation => Member_Doc_For_Expr_P_Get_Expected_Type'Access,
         Owner         => Type_Index_For_Expr,
         Member_Type   => Type_Index_For_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Expr_P_Referenced_Decl : aliased constant Text_Type :=
        "P_Referenced_Decl";
      Member_Doc_For_Expr_P_Referenced_Decl  : aliased constant Text_Type := "Return the declaration referenced by this expression, if applicable," & Character_Type'Val (10)
 & "else null." & Character_Type'Val (10)
 & Character_Type'Val (10)
 & "The property is memoized in order to avoid use the value inside logic" & Character_Type'Val (10)
 & "variables on every redundent call, causing faulty behavior when used" & Character_Type'Val (10)
 & "with rebindings." & Character_Type'Val (10)
 & "TODO: Do like LAL to avoid memoization for more safety." & Character_Type'Val (10)
;
      Member_Desc_For_Expr_P_Referenced_Decl  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Expr_P_Referenced_Decl'Access,
         Documentation => Member_Doc_For_Expr_P_Referenced_Decl'Access,
         Owner         => Type_Index_For_Expr,
         Member_Type   => Type_Index_For_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Token_Lit_P_Denoted_Value : aliased constant Text_Type :=
        "P_Denoted_Value";
      Member_Doc_For_Token_Lit_P_Denoted_Value  : aliased constant Text_Type := "Return the content of the given token literal node." & Character_Type'Val (10)
;
      Member_Desc_For_Token_Lit_P_Denoted_Value  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Token_Lit_P_Denoted_Value'Access,
         Documentation => Member_Doc_For_Token_Lit_P_Denoted_Value'Access,
         Owner         => Type_Index_For_Token_Lit,
         Member_Type   => Type_Index_For_Decoded_String_Value,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Token_Pattern_Lit_P_Denoted_Value : aliased constant Text_Type :=
        "P_Denoted_Value";
      Member_Doc_For_Token_Pattern_Lit_P_Denoted_Value  : aliased constant Text_Type := "Return the content of the given token pattern literal node." & Character_Type'Val (10)
;
      Member_Desc_For_Token_Pattern_Lit_P_Denoted_Value  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Token_Pattern_Lit_P_Denoted_Value'Access,
         Documentation => Member_Doc_For_Token_Pattern_Lit_P_Denoted_Value'Access,
         Owner         => Type_Index_For_Token_Pattern_Lit,
         Member_Type   => Type_Index_For_Decoded_String_Value,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Id_P_Custom_Image : aliased constant Text_Type :=
        "P_Custom_Image";
      Member_Doc_For_Id_P_Custom_Image  : aliased constant Text_Type := "Returns the image of this RefId using entity information." & Character_Type'Val (10)
;
      Member_Desc_For_Id_P_Custom_Image  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Id_P_Custom_Image'Access,
         Documentation => Member_Doc_For_Id_P_Custom_Image'Access,
         Owner         => Type_Index_For_Id,
         Member_Type   => Type_Index_For_String,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Def_Id_P_Name : aliased constant Text_Type :=
        "P_Name";
      Member_Doc_For_Def_Id_P_Name  : aliased constant Text_Type := "Return the name defined by this DefId." & Character_Type'Val (10)
;
      Member_Desc_For_Def_Id_P_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Def_Id_P_Name'Access,
         Documentation => Member_Doc_For_Def_Id_P_Name'Access,
         Owner         => Type_Index_For_Def_Id,
         Member_Type   => Type_Index_For_String,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      

         Arg_Name_8 : aliased constant Text_Type :=
           "Units";
         

      

      

      Member_Name_For_Def_Id_P_Get_Implementatinons : aliased constant Text_Type :=
        "P_Get_Implementatinons";
      Member_Doc_For_Def_Id_P_Get_Implementatinons  : aliased constant Text_Type := "Return the implementations of this name." & Character_Type'Val (10)
;
      Member_Desc_For_Def_Id_P_Get_Implementatinons  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 1,
         Name          => Member_Name_For_Def_Id_P_Get_Implementatinons'Access,
         Documentation => Member_Doc_For_Def_Id_P_Get_Implementatinons'Access,
         Owner         => Type_Index_For_Def_Id,
         Member_Type   => Type_Index_For_Def_Id_Array,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (8 =>
              (Name          => Arg_Name_8'Access,
               Argument_Type => Type_Index_For_Analysis_Unit_Array,
               Default_Value => (Kind => None)))
        );

      


      

      

      Member_Name_For_Def_Id_P_Decl_Detail : aliased constant Text_Type :=
        "P_Decl_Detail";
      Member_Doc_For_Def_Id_P_Decl_Detail  : aliased constant Text_Type := "Return the details to display in the language server client when it" & Character_Type'Val (10)
 & "requests for completion or hovering information." & Character_Type'Val (10)
;
      Member_Desc_For_Def_Id_P_Decl_Detail  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Def_Id_P_Decl_Detail'Access,
         Documentation => Member_Doc_For_Def_Id_P_Decl_Detail'Access,
         Owner         => Type_Index_For_Def_Id,
         Member_Type   => Type_Index_For_String,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Def_Id_P_Completion_Item_Kind : aliased constant Text_Type :=
        "P_Completion_Item_Kind";
      Member_Doc_For_Def_Id_P_Completion_Item_Kind  : aliased constant Text_Type := "Return the kind of completion item for this DefId." & Character_Type'Val (10)
;
      Member_Desc_For_Def_Id_P_Completion_Item_Kind  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Def_Id_P_Completion_Item_Kind'Access,
         Documentation => Member_Doc_For_Def_Id_P_Completion_Item_Kind'Access,
         Owner         => Type_Index_For_Def_Id,
         Member_Type   => Type_Index_For_Int,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Def_Id_P_Doc : aliased constant Text_Type :=
        "P_Doc";
      Member_Doc_For_Def_Id_P_Doc  : aliased constant Text_Type := "Return the documentation associated to this DefId." & Character_Type'Val (10)
;
      Member_Desc_For_Def_Id_P_Doc  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Def_Id_P_Doc'Access,
         Documentation => Member_Doc_For_Def_Id_P_Doc'Access,
         Owner         => Type_Index_For_Def_Id,
         Member_Type   => Type_Index_For_String,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      

         Arg_Name_9 : aliased constant Text_Type :=
           "Units";
         

      

      

      Member_Name_For_Def_Id_P_Find_All_References : aliased constant Text_Type :=
        "P_Find_All_References";
      Member_Doc_For_Def_Id_P_Find_All_References  : aliased constant Text_Type := "Return the list of all RefId that refer to this DefId." & Character_Type'Val (10)
;
      Member_Desc_For_Def_Id_P_Find_All_References  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 1,
         Name          => Member_Name_For_Def_Id_P_Find_All_References'Access,
         Documentation => Member_Doc_For_Def_Id_P_Find_All_References'Access,
         Owner         => Type_Index_For_Def_Id,
         Member_Type   => Type_Index_For_Ref_Result_Array,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (9 =>
              (Name          => Arg_Name_9'Access,
               Argument_Type => Type_Index_For_Analysis_Unit_Array,
               Default_Value => (Kind => None)))
        );

      


      

      

      Member_Name_For_Ref_Id_P_Referenced_Defining_Name : aliased constant Text_Type :=
        "P_Referenced_Defining_Name";
      Member_Doc_For_Ref_Id_P_Referenced_Defining_Name  : aliased constant Text_Type := "Return the referenced defining name." & Character_Type'Val (10)
;
      Member_Desc_For_Ref_Id_P_Referenced_Defining_Name  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Ref_Id_P_Referenced_Defining_Name'Access,
         Documentation => Member_Doc_For_Ref_Id_P_Referenced_Defining_Name'Access,
         Owner         => Type_Index_For_Ref_Id,
         Member_Type   => Type_Index_For_Def_Id,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Char_Lit_P_Denoted_Value : aliased constant Text_Type :=
        "P_Denoted_Value";
      Member_Doc_For_Char_Lit_P_Denoted_Value  : aliased constant Text_Type := "Return the content of the given character literal node." & Character_Type'Val (10)
;
      Member_Desc_For_Char_Lit_P_Denoted_Value  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Char_Lit_P_Denoted_Value'Access,
         Documentation => Member_Doc_For_Char_Lit_P_Denoted_Value'Access,
         Owner         => Type_Index_For_Char_Lit,
         Member_Type   => Type_Index_For_Decoded_Char_Value,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_String_Lit_P_Denoted_Value : aliased constant Text_Type :=
        "P_Denoted_Value";
      Member_Doc_For_String_Lit_P_Denoted_Value  : aliased constant Text_Type := "Return the content of the given string literal node." & Character_Type'Val (10)
;
      Member_Desc_For_String_Lit_P_Denoted_Value  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_String_Lit_P_Denoted_Value'Access,
         Documentation => Member_Doc_For_String_Lit_P_Denoted_Value'Access,
         Owner         => Type_Index_For_String_Lit,
         Member_Type   => Type_Index_For_Decoded_String_Value,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_String_Lit_P_Is_Prefixed_String : aliased constant Text_Type :=
        "P_Is_Prefixed_String";
      Member_Doc_For_String_Lit_P_Is_Prefixed_String  : aliased constant Text_Type := "Return whether this string is prefixed or not." & Character_Type'Val (10)
;
      Member_Desc_For_String_Lit_P_Is_Prefixed_String  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_String_Lit_P_Is_Prefixed_String'Access,
         Documentation => Member_Doc_For_String_Lit_P_Is_Prefixed_String'Access,
         Owner         => Type_Index_For_String_Lit,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_String_Lit_P_Prefix : aliased constant Text_Type :=
        "P_Prefix";
      Member_Doc_For_String_Lit_P_Prefix  : aliased constant Text_Type := "Return the prefix of this string, or the null character if there is no" & Character_Type'Val (10)
 & "prefix." & Character_Type'Val (10)
;
      Member_Desc_For_String_Lit_P_Prefix  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_String_Lit_P_Prefix'Access,
         Documentation => Member_Doc_For_String_Lit_P_Prefix'Access,
         Owner         => Type_Index_For_String_Lit,
         Member_Type   => Type_Index_For_Char,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_String_Lit_P_Is_Regexp_Literal : aliased constant Text_Type :=
        "P_Is_Regexp_Literal";
      Member_Doc_For_String_Lit_P_Is_Regexp_Literal  : aliased constant Text_Type := "Return whether this string literal is actually a regexp literal, by" & Character_Type'Val (10)
 & "checking that this string is prefixed by 'p'." & Character_Type'Val (10)
;
      Member_Desc_For_String_Lit_P_Is_Regexp_Literal  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_String_Lit_P_Is_Regexp_Literal'Access,
         Documentation => Member_Doc_For_String_Lit_P_Is_Regexp_Literal'Access,
         Owner         => Type_Index_For_String_Lit,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      

         Arg_Name_10 : aliased constant Text_Type :=
           "Name";
         

      

      

      Member_Name_For_Full_Decl_P_Has_Annotation : aliased constant Text_Type :=
        "P_Has_Annotation";
      Member_Doc_For_Full_Decl_P_Has_Annotation  : aliased constant Text_Type := "Return whether this node has an annotation with name ``name``." & Character_Type'Val (10)
;
      Member_Desc_For_Full_Decl_P_Has_Annotation  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 1,
         Name          => Member_Name_For_Full_Decl_P_Has_Annotation'Access,
         Documentation => Member_Doc_For_Full_Decl_P_Has_Annotation'Access,
         Owner         => Type_Index_For_Full_Decl,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (10 =>
              (Name          => Arg_Name_10'Access,
               Argument_Type => Type_Index_For_Symbol,
               Default_Value => (Kind => None)))
        );

      


      

      

      Member_Name_For_Null_Cond_Qualifier_P_As_Bool : aliased constant Text_Type :=
        "P_As_Bool";
      Member_Doc_For_Null_Cond_Qualifier_P_As_Bool  : aliased constant Text_Type := "Return whether this node is present";
      Member_Desc_For_Null_Cond_Qualifier_P_As_Bool  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Null_Cond_Qualifier_P_As_Bool'Access,
         Documentation => Member_Doc_For_Null_Cond_Qualifier_P_As_Bool'Access,
         Owner         => Type_Index_For_Null_Cond_Qualifier,
         Member_Type   => Type_Index_For_Bool,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );

      


      

      

      Member_Name_For_Type_Ref_P_Referenced_Decl : aliased constant Text_Type :=
        "P_Referenced_Decl";
      Member_Doc_For_Type_Ref_P_Referenced_Decl  : aliased constant Text_Type := "Returns the referenced type declaration." & Character_Type'Val (10)
;
      Member_Desc_For_Type_Ref_P_Referenced_Decl  : aliased constant Struct_Member_Descriptor :=
        (Last_Argument => 0,
         Name          => Member_Name_For_Type_Ref_P_Referenced_Decl'Access,
         Documentation => Member_Doc_For_Type_Ref_P_Referenced_Decl'Access,
         Owner         => Type_Index_For_Type_Ref,
         Member_Type   => Type_Index_For_Type_Decl,
         Null_For      => null,
         Indexes       => null,
         Arguments     =>
           (1 .. 0 => <>)
        );


   Struct_Members : aliased constant Struct_Member_Descriptor_Array :=
     (Member_Index_For_Complete_Item_Declaration => Member_Desc_For_Complete_Item_Declaration'Access,
      Member_Index_For_Decoded_Char_Value_Value => Member_Desc_For_Decoded_Char_Value_Value'Access,
      Member_Index_For_Decoded_Char_Value_Has_Error => Member_Desc_For_Decoded_Char_Value_Has_Error'Access,
      Member_Index_For_Decoded_Char_Value_Error_Sloc => Member_Desc_For_Decoded_Char_Value_Error_Sloc'Access,
      Member_Index_For_Decoded_Char_Value_Error_Message => Member_Desc_For_Decoded_Char_Value_Error_Message'Access,
      Member_Index_For_Decoded_String_Value_Value => Member_Desc_For_Decoded_String_Value_Value'Access,
      Member_Index_For_Decoded_String_Value_Has_Error => Member_Desc_For_Decoded_String_Value_Has_Error'Access,
      Member_Index_For_Decoded_String_Value_Error_Sloc => Member_Desc_For_Decoded_String_Value_Error_Sloc'Access,
      Member_Index_For_Decoded_String_Value_Error_Message => Member_Desc_For_Decoded_String_Value_Error_Message'Access,
      Member_Index_For_Logic_Context_Ref_Node => Member_Desc_For_Logic_Context_Ref_Node'Access,
      Member_Index_For_Logic_Context_Decl_Node => Member_Desc_For_Logic_Context_Decl_Node'Access,
      Member_Index_For_Ref_Result_Ref => Member_Desc_For_Ref_Result_Ref'Access,
      Member_Index_For_Solver_Diagnostic_Message_Template => Member_Desc_For_Solver_Diagnostic_Message_Template'Access,
      Member_Index_For_Solver_Diagnostic_Args => Member_Desc_For_Solver_Diagnostic_Args'Access,
      Member_Index_For_Solver_Diagnostic_Location => Member_Desc_For_Solver_Diagnostic_Location'Access,
      Member_Index_For_Solver_Diagnostic_Contexts => Member_Desc_For_Solver_Diagnostic_Contexts'Access,
      Member_Index_For_Solver_Diagnostic_Round => Member_Desc_For_Solver_Diagnostic_Round'Access,
      Member_Index_For_Solver_Result_Success => Member_Desc_For_Solver_Result_Success'Access,
      Member_Index_For_Solver_Result_Diagnostics => Member_Desc_For_Solver_Result_Diagnostics'Access,
      Member_Index_For_Argument_F_Name => Member_Desc_For_Argument_F_Name'Access,
      Member_Index_For_Argument_F_Value => Member_Desc_For_Argument_F_Value'Access,
      Member_Index_For_Import_F_Imported_Names => Member_Desc_For_Import_F_Imported_Names'Access,
      Member_Index_For_Import_All_From_F_Module_Name => Member_Desc_For_Import_All_From_F_Module_Name'Access,
      Member_Index_For_Import_From_F_Module_Name => Member_Desc_For_Import_From_F_Module_Name'Access,
      Member_Index_For_Import_From_F_Imported_Names => Member_Desc_For_Import_From_F_Imported_Names'Access,
      Member_Index_For_Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs => Member_Desc_For_Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs'Access,
      Member_Index_For_Lexer_Case_Rule_Cond_Alt_F_Send => Member_Desc_For_Lexer_Case_Rule_Cond_Alt_F_Send'Access,
      Member_Index_For_Lexer_Case_Rule_Default_Alt_F_Send => Member_Desc_For_Lexer_Case_Rule_Default_Alt_F_Send'Access,
      Member_Index_For_Base_Match_Branch_F_Expr => Member_Desc_For_Base_Match_Branch_F_Expr'Access,
      Member_Index_For_Match_Branch_F_Decl => Member_Desc_For_Match_Branch_F_Decl'Access,
      Member_Index_For_Pattern_Match_Branch_F_Pattern => Member_Desc_For_Pattern_Match_Branch_F_Pattern'Access,
      Member_Index_For_Block_Expr_Clause_F_Clause => Member_Desc_For_Block_Expr_Clause_F_Clause'Access,
      Member_Index_For_Decl_F_Syn_Name => Member_Desc_For_Decl_F_Syn_Name'Access,
      Member_Index_For_Base_Grammar_Rule_Decl_F_Expr => Member_Desc_For_Base_Grammar_Rule_Decl_F_Expr'Access,
      Member_Index_For_Explicitly_Typed_Decl_F_Decl_Type => Member_Desc_For_Explicitly_Typed_Decl_F_Decl_Type'Access,
      Member_Index_For_Component_Decl_F_Default_Val => Member_Desc_For_Component_Decl_F_Default_Val'Access,
      Member_Index_For_Field_Decl_F_Trait_Ref => Member_Desc_For_Field_Decl_F_Trait_Ref'Access,
      Member_Index_For_Fun_Param_Decl_F_Decl_Annotations => Member_Desc_For_Fun_Param_Decl_F_Decl_Annotations'Access,
      Member_Index_For_Val_Decl_F_Expr => Member_Desc_For_Val_Decl_F_Expr'Access,
      Member_Index_For_Fun_Decl_F_Params => Member_Desc_For_Fun_Decl_F_Params'Access,
      Member_Index_For_Fun_Decl_F_Return_Type => Member_Desc_For_Fun_Decl_F_Return_Type'Access,
      Member_Index_For_Fun_Decl_F_Trait_Ref => Member_Desc_For_Fun_Decl_F_Trait_Ref'Access,
      Member_Index_For_Fun_Decl_F_Body => Member_Desc_For_Fun_Decl_F_Body'Access,
      Member_Index_For_Env_Spec_Decl_F_Actions => Member_Desc_For_Env_Spec_Decl_F_Actions'Access,
      Member_Index_For_Generic_Decl_F_Generic_Param_Decls => Member_Desc_For_Generic_Decl_F_Generic_Param_Decls'Access,
      Member_Index_For_Generic_Decl_F_Decl => Member_Desc_For_Generic_Decl_F_Decl'Access,
      Member_Index_For_Grammar_Decl_F_Rules => Member_Desc_For_Grammar_Decl_F_Rules'Access,
      Member_Index_For_Langkit_Root_F_Doc => Member_Desc_For_Langkit_Root_F_Doc'Access,
      Member_Index_For_Langkit_Root_F_Imports => Member_Desc_For_Langkit_Root_F_Imports'Access,
      Member_Index_For_Langkit_Root_F_Decls => Member_Desc_For_Langkit_Root_F_Decls'Access,
      Member_Index_For_Lexer_Decl_F_Rules => Member_Desc_For_Lexer_Decl_F_Rules'Access,
      Member_Index_For_Lexer_Family_Decl_F_Rules => Member_Desc_For_Lexer_Family_Decl_F_Rules'Access,
      Member_Index_For_Type_Decl_F_Traits => Member_Desc_For_Type_Decl_F_Traits'Access,
      Member_Index_For_Type_Decl_F_Syn_Base_Type => Member_Desc_For_Type_Decl_F_Syn_Base_Type'Access,
      Member_Index_For_Generic_Param_Type_Decl_F_Has_Class => Member_Desc_For_Generic_Param_Type_Decl_F_Has_Class'Access,
      Member_Index_For_Named_Type_Decl_F_Decls => Member_Desc_For_Named_Type_Decl_F_Decls'Access,
      Member_Index_For_Enum_Class_Decl_F_Branches => Member_Desc_For_Enum_Class_Decl_F_Branches'Access,
      Member_Index_For_Enum_Type_Decl_F_Literals => Member_Desc_For_Enum_Type_Decl_F_Literals'Access,
      Member_Index_For_Decl_Annotation_F_Name => Member_Desc_For_Decl_Annotation_F_Name'Access,
      Member_Index_For_Decl_Annotation_F_Args => Member_Desc_For_Decl_Annotation_F_Args'Access,
      Member_Index_For_Decl_Annotation_Args_F_Args => Member_Desc_For_Decl_Annotation_Args_F_Args'Access,
      Member_Index_For_Elsif_Branch_F_Cond_Expr => Member_Desc_For_Elsif_Branch_F_Cond_Expr'Access,
      Member_Index_For_Elsif_Branch_F_Then_Expr => Member_Desc_For_Elsif_Branch_F_Then_Expr'Access,
      Member_Index_For_Enum_Class_Case_F_Decls => Member_Desc_For_Enum_Class_Case_F_Decls'Access,
      Member_Index_For_Any_Of_F_Expr => Member_Desc_For_Any_Of_F_Expr'Access,
      Member_Index_For_Any_Of_F_Values => Member_Desc_For_Any_Of_F_Values'Access,
      Member_Index_For_Array_Literal_F_Exprs => Member_Desc_For_Array_Literal_F_Exprs'Access,
      Member_Index_For_Array_Literal_F_Element_Type => Member_Desc_For_Array_Literal_F_Element_Type'Access,
      Member_Index_For_Base_Call_Expr_F_Name => Member_Desc_For_Base_Call_Expr_F_Name'Access,
      Member_Index_For_Base_Call_Expr_F_Args => Member_Desc_For_Base_Call_Expr_F_Args'Access,
      Member_Index_For_Bin_Op_F_Left => Member_Desc_For_Bin_Op_F_Left'Access,
      Member_Index_For_Bin_Op_F_Op => Member_Desc_For_Bin_Op_F_Op'Access,
      Member_Index_For_Bin_Op_F_Right => Member_Desc_For_Bin_Op_F_Right'Access,
      Member_Index_For_Block_Expr_F_Clauses => Member_Desc_For_Block_Expr_F_Clauses'Access,
      Member_Index_For_Cast_Expr_F_Expr => Member_Desc_For_Cast_Expr_F_Expr'Access,
      Member_Index_For_Cast_Expr_F_Null_Cond => Member_Desc_For_Cast_Expr_F_Null_Cond'Access,
      Member_Index_For_Cast_Expr_F_Excludes_Null => Member_Desc_For_Cast_Expr_F_Excludes_Null'Access,
      Member_Index_For_Cast_Expr_F_Dest_Type => Member_Desc_For_Cast_Expr_F_Dest_Type'Access,
      Member_Index_For_Dot_Expr_F_Prefix => Member_Desc_For_Dot_Expr_F_Prefix'Access,
      Member_Index_For_Dot_Expr_F_Null_Cond => Member_Desc_For_Dot_Expr_F_Null_Cond'Access,
      Member_Index_For_Dot_Expr_F_Suffix => Member_Desc_For_Dot_Expr_F_Suffix'Access,
      Member_Index_For_Error_On_Null_F_Expr => Member_Desc_For_Error_On_Null_F_Expr'Access,
      Member_Index_For_Generic_Instantiation_F_Name => Member_Desc_For_Generic_Instantiation_F_Name'Access,
      Member_Index_For_Generic_Instantiation_F_Args => Member_Desc_For_Generic_Instantiation_F_Args'Access,
      Member_Index_For_Grammar_Discard_F_Expr => Member_Desc_For_Grammar_Discard_F_Expr'Access,
      Member_Index_For_Grammar_Dont_Skip_F_Expr => Member_Desc_For_Grammar_Dont_Skip_F_Expr'Access,
      Member_Index_For_Grammar_Dont_Skip_F_Dont_Skip => Member_Desc_For_Grammar_Dont_Skip_F_Dont_Skip'Access,
      Member_Index_For_Grammar_List_F_List_Type => Member_Desc_For_Grammar_List_F_List_Type'Access,
      Member_Index_For_Grammar_List_F_Kind => Member_Desc_For_Grammar_List_F_Kind'Access,
      Member_Index_For_Grammar_List_F_Expr => Member_Desc_For_Grammar_List_F_Expr'Access,
      Member_Index_For_Grammar_List_F_Sep => Member_Desc_For_Grammar_List_F_Sep'Access,
      Member_Index_For_Grammar_Null_F_Name => Member_Desc_For_Grammar_Null_F_Name'Access,
      Member_Index_For_Grammar_Opt_F_Expr => Member_Desc_For_Grammar_Opt_F_Expr'Access,
      Member_Index_For_Grammar_Opt_Error_F_Expr => Member_Desc_For_Grammar_Opt_Error_F_Expr'Access,
      Member_Index_For_Grammar_Opt_Error_Group_F_Expr => Member_Desc_For_Grammar_Opt_Error_Group_F_Expr'Access,
      Member_Index_For_Grammar_Opt_Group_F_Expr => Member_Desc_For_Grammar_Opt_Group_F_Expr'Access,
      Member_Index_For_Grammar_Or_Expr_F_Sub_Exprs => Member_Desc_For_Grammar_Or_Expr_F_Sub_Exprs'Access,
      Member_Index_For_Grammar_Pick_F_Exprs => Member_Desc_For_Grammar_Pick_F_Exprs'Access,
      Member_Index_For_Grammar_Predicate_F_Expr => Member_Desc_For_Grammar_Predicate_F_Expr'Access,
      Member_Index_For_Grammar_Predicate_F_Prop_Ref => Member_Desc_For_Grammar_Predicate_F_Prop_Ref'Access,
      Member_Index_For_Grammar_Rule_Ref_F_Node_Name => Member_Desc_For_Grammar_Rule_Ref_F_Node_Name'Access,
      Member_Index_For_Grammar_Skip_F_Name => Member_Desc_For_Grammar_Skip_F_Name'Access,
      Member_Index_For_Grammar_Stop_Cut_F_Expr => Member_Desc_For_Grammar_Stop_Cut_F_Expr'Access,
      Member_Index_For_Parse_Node_Expr_F_Node_Name => Member_Desc_For_Parse_Node_Expr_F_Node_Name'Access,
      Member_Index_For_Parse_Node_Expr_F_Sub_Exprs => Member_Desc_For_Parse_Node_Expr_F_Sub_Exprs'Access,
      Member_Index_For_Token_No_Case_Lit_F_Lit => Member_Desc_For_Token_No_Case_Lit_F_Lit'Access,
      Member_Index_For_Token_Pattern_Concat_F_Left => Member_Desc_For_Token_Pattern_Concat_F_Left'Access,
      Member_Index_For_Token_Pattern_Concat_F_Right => Member_Desc_For_Token_Pattern_Concat_F_Right'Access,
      Member_Index_For_Token_Ref_F_Token_Name => Member_Desc_For_Token_Ref_F_Token_Name'Access,
      Member_Index_For_Token_Ref_F_Expr => Member_Desc_For_Token_Ref_F_Expr'Access,
      Member_Index_For_If_Expr_F_Cond_Expr => Member_Desc_For_If_Expr_F_Cond_Expr'Access,
      Member_Index_For_If_Expr_F_Then_Expr => Member_Desc_For_If_Expr_F_Then_Expr'Access,
      Member_Index_For_If_Expr_F_Alternatives => Member_Desc_For_If_Expr_F_Alternatives'Access,
      Member_Index_For_If_Expr_F_Else_Expr => Member_Desc_For_If_Expr_F_Else_Expr'Access,
      Member_Index_For_Isa_F_Expr => Member_Desc_For_Isa_F_Expr'Access,
      Member_Index_For_Isa_F_Pattern => Member_Desc_For_Isa_F_Pattern'Access,
      Member_Index_For_Keep_Expr_F_Expr => Member_Desc_For_Keep_Expr_F_Expr'Access,
      Member_Index_For_Keep_Expr_F_Null_Cond => Member_Desc_For_Keep_Expr_F_Null_Cond'Access,
      Member_Index_For_Keep_Expr_F_Keep_Type => Member_Desc_For_Keep_Expr_F_Keep_Type'Access,
      Member_Index_For_Lambda_Expr_F_Params => Member_Desc_For_Lambda_Expr_F_Params'Access,
      Member_Index_For_Lambda_Expr_F_Return_Type => Member_Desc_For_Lambda_Expr_F_Return_Type'Access,
      Member_Index_For_Lambda_Expr_F_Body => Member_Desc_For_Lambda_Expr_F_Body'Access,
      Member_Index_For_Null_Lit_F_Dest_Type => Member_Desc_For_Null_Lit_F_Dest_Type'Access,
      Member_Index_For_Block_String_Lit_F_Lines => Member_Desc_For_Block_String_Lit_F_Lines'Access,
      Member_Index_For_Module_Doc_String_Lit_F_Lines => Member_Desc_For_Module_Doc_String_Lit_F_Lines'Access,
      Member_Index_For_Logic_Assign_F_Dest_Var => Member_Desc_For_Logic_Assign_F_Dest_Var'Access,
      Member_Index_For_Logic_Assign_F_Value => Member_Desc_For_Logic_Assign_F_Value'Access,
      Member_Index_For_Logic_Expr_F_Expr => Member_Desc_For_Logic_Expr_F_Expr'Access,
      Member_Index_For_Logic_Propagate_F_Dest_Var => Member_Desc_For_Logic_Propagate_F_Dest_Var'Access,
      Member_Index_For_Logic_Propagate_F_Call => Member_Desc_For_Logic_Propagate_F_Call'Access,
      Member_Index_For_Logic_Unify_F_Lhs => Member_Desc_For_Logic_Unify_F_Lhs'Access,
      Member_Index_For_Logic_Unify_F_Rhs => Member_Desc_For_Logic_Unify_F_Rhs'Access,
      Member_Index_For_Match_Expr_F_Match_Expr => Member_Desc_For_Match_Expr_F_Match_Expr'Access,
      Member_Index_For_Match_Expr_F_Branches => Member_Desc_For_Match_Expr_F_Branches'Access,
      Member_Index_For_Not_Expr_F_Expr => Member_Desc_For_Not_Expr_F_Expr'Access,
      Member_Index_For_Paren_Expr_F_Expr => Member_Desc_For_Paren_Expr_F_Expr'Access,
      Member_Index_For_Query_F_Source => Member_Desc_For_Query_F_Source'Access,
      Member_Index_For_Query_F_Pattern => Member_Desc_For_Query_F_Pattern'Access,
      Member_Index_For_Query_F_Mapping => Member_Desc_For_Query_F_Mapping'Access,
      Member_Index_For_Query_F_Guard => Member_Desc_For_Query_F_Guard'Access,
      Member_Index_For_Raise_Expr_F_Dest_Type => Member_Desc_For_Raise_Expr_F_Dest_Type'Access,
      Member_Index_For_Raise_Expr_F_Except_Expr => Member_Desc_For_Raise_Expr_F_Except_Expr'Access,
      Member_Index_For_Subscript_Expr_F_Prefix => Member_Desc_For_Subscript_Expr_F_Prefix'Access,
      Member_Index_For_Subscript_Expr_F_Null_Cond => Member_Desc_For_Subscript_Expr_F_Null_Cond'Access,
      Member_Index_For_Subscript_Expr_F_Index => Member_Desc_For_Subscript_Expr_F_Index'Access,
      Member_Index_For_Try_Expr_F_Try_Expr => Member_Desc_For_Try_Expr_F_Try_Expr'Access,
      Member_Index_For_Try_Expr_F_Or_Expr => Member_Desc_For_Try_Expr_F_Or_Expr'Access,
      Member_Index_For_Un_Op_F_Op => Member_Desc_For_Un_Op_F_Op'Access,
      Member_Index_For_Un_Op_F_Expr => Member_Desc_For_Un_Op_F_Expr'Access,
      Member_Index_For_Full_Decl_F_Doc => Member_Desc_For_Full_Decl_F_Doc'Access,
      Member_Index_For_Full_Decl_F_Decl_Annotations => Member_Desc_For_Full_Decl_F_Decl_Annotations'Access,
      Member_Index_For_Full_Decl_F_Decl => Member_Desc_For_Full_Decl_F_Decl'Access,
      Member_Index_For_Grammar_List_Sep_F_Token => Member_Desc_For_Grammar_List_Sep_F_Token'Access,
      Member_Index_For_Grammar_List_Sep_F_Extra => Member_Desc_For_Grammar_List_Sep_F_Extra'Access,
      Member_Index_For_Imported_Name_F_Original_Name => Member_Desc_For_Imported_Name_F_Original_Name'Access,
      Member_Index_For_Imported_Name_F_Renaming => Member_Desc_For_Imported_Name_F_Renaming'Access,
      Member_Index_For_Lexer_Case_Rule_F_Expr => Member_Desc_For_Lexer_Case_Rule_F_Expr'Access,
      Member_Index_For_Lexer_Case_Rule_F_Alts => Member_Desc_For_Lexer_Case_Rule_F_Alts'Access,
      Member_Index_For_Lexer_Case_Rule_Send_F_Sent => Member_Desc_For_Lexer_Case_Rule_Send_F_Sent'Access,
      Member_Index_For_Lexer_Case_Rule_Send_F_Match_Size => Member_Desc_For_Lexer_Case_Rule_Send_F_Match_Size'Access,
      Member_Index_For_Complex_Pattern_F_Decl => Member_Desc_For_Complex_Pattern_F_Decl'Access,
      Member_Index_For_Complex_Pattern_F_Pattern => Member_Desc_For_Complex_Pattern_F_Pattern'Access,
      Member_Index_For_Complex_Pattern_F_Details => Member_Desc_For_Complex_Pattern_F_Details'Access,
      Member_Index_For_Complex_Pattern_F_Predicate => Member_Desc_For_Complex_Pattern_F_Predicate'Access,
      Member_Index_For_Ellipsis_Pattern_F_Binding => Member_Desc_For_Ellipsis_Pattern_F_Binding'Access,
      Member_Index_For_List_Pattern_F_Sub_Patterns => Member_Desc_For_List_Pattern_F_Sub_Patterns'Access,
      Member_Index_For_Not_Pattern_F_Sub_Pattern => Member_Desc_For_Not_Pattern_F_Sub_Pattern'Access,
      Member_Index_For_Or_Pattern_F_Left_Sub_Pattern => Member_Desc_For_Or_Pattern_F_Left_Sub_Pattern'Access,
      Member_Index_For_Or_Pattern_F_Right_Sub_Pattern => Member_Desc_For_Or_Pattern_F_Right_Sub_Pattern'Access,
      Member_Index_For_Paren_Pattern_F_Sub_Pattern => Member_Desc_For_Paren_Pattern_F_Sub_Pattern'Access,
      Member_Index_For_Type_Pattern_F_Type_Name => Member_Desc_For_Type_Pattern_F_Type_Name'Access,
      Member_Index_For_Field_Pattern_Detail_F_Id => Member_Desc_For_Field_Pattern_Detail_F_Id'Access,
      Member_Index_For_Field_Pattern_Detail_F_Expected_Value => Member_Desc_For_Field_Pattern_Detail_F_Expected_Value'Access,
      Member_Index_For_Property_Pattern_Detail_F_Call => Member_Desc_For_Property_Pattern_Detail_F_Call'Access,
      Member_Index_For_Property_Pattern_Detail_F_Expected_Value => Member_Desc_For_Property_Pattern_Detail_F_Expected_Value'Access,
      Member_Index_For_Function_Type_Ref_F_Param_Types => Member_Desc_For_Function_Type_Ref_F_Param_Types'Access,
      Member_Index_For_Function_Type_Ref_F_Return_Type => Member_Desc_For_Function_Type_Ref_F_Return_Type'Access,
      Member_Index_For_Generic_Type_Ref_F_Type_Name => Member_Desc_For_Generic_Type_Ref_F_Type_Name'Access,
      Member_Index_For_Generic_Type_Ref_F_Args => Member_Desc_For_Generic_Type_Ref_F_Args'Access,
      Member_Index_For_Simple_Type_Ref_F_Type_Name => Member_Desc_For_Simple_Type_Ref_F_Type_Name'Access,
      Member_Index_For_Var_Bind_F_Name => Member_Desc_For_Var_Bind_F_Name'Access,
      Member_Index_For_Var_Bind_F_Expr => Member_Desc_For_Var_Bind_F_Expr'Access,
      Member_Index_For_Parent => Member_Desc_For_Parent'Access,
      Member_Index_For_Parents => Member_Desc_For_Parents'Access,
      Member_Index_For_Children => Member_Desc_For_Children'Access,
      Member_Index_For_Token_Start => Member_Desc_For_Token_Start'Access,
      Member_Index_For_Token_End => Member_Desc_For_Token_End'Access,
      Member_Index_For_Child_Index => Member_Desc_For_Child_Index'Access,
      Member_Index_For_Previous_Sibling => Member_Desc_For_Previous_Sibling'Access,
      Member_Index_For_Next_Sibling => Member_Desc_For_Next_Sibling'Access,
      Member_Index_For_Unit => Member_Desc_For_Unit'Access,
      Member_Index_For_Is_Ghost => Member_Desc_For_Is_Ghost'Access,
      Member_Index_For_Full_Sloc_Image => Member_Desc_For_Full_Sloc_Image'Access,
      Member_Index_For_Completion_Item_Kind_To_Int => Member_Desc_For_Completion_Item_Kind_To_Int'Access,
      Member_Index_For_Lkt_Node_P_Set_Solver_Debug_Mode => Member_Desc_For_Lkt_Node_P_Set_Solver_Debug_Mode'Access,
      Member_Index_For_Lkt_Node_P_Prelude_Unit => Member_Desc_For_Lkt_Node_P_Prelude_Unit'Access,
      Member_Index_For_Lkt_Node_P_Basic_Trait_Gen => Member_Desc_For_Lkt_Node_P_Basic_Trait_Gen'Access,
      Member_Index_For_Lkt_Node_P_Basic_Trait => Member_Desc_For_Lkt_Node_P_Basic_Trait'Access,
      Member_Index_For_Lkt_Node_P_Node_Gen_Trait => Member_Desc_For_Lkt_Node_P_Node_Gen_Trait'Access,
      Member_Index_For_Lkt_Node_P_Node_Trait => Member_Desc_For_Lkt_Node_P_Node_Trait'Access,
      Member_Index_For_Lkt_Node_P_Indexable_Gen_Trait => Member_Desc_For_Lkt_Node_P_Indexable_Gen_Trait'Access,
      Member_Index_For_Lkt_Node_P_Indexable_Trait => Member_Desc_For_Lkt_Node_P_Indexable_Trait'Access,
      Member_Index_For_Lkt_Node_P_Token_Node_Trait => Member_Desc_For_Lkt_Node_P_Token_Node_Trait'Access,
      Member_Index_For_Lkt_Node_P_Error_Node_Trait => Member_Desc_For_Lkt_Node_P_Error_Node_Trait'Access,
      Member_Index_For_Lkt_Node_P_Char_Type => Member_Desc_For_Lkt_Node_P_Char_Type'Access,
      Member_Index_For_Lkt_Node_P_Int_Type => Member_Desc_For_Lkt_Node_P_Int_Type'Access,
      Member_Index_For_Lkt_Node_P_Bool_Type => Member_Desc_For_Lkt_Node_P_Bool_Type'Access,
      Member_Index_For_Lkt_Node_P_Bigint_Type => Member_Desc_For_Lkt_Node_P_Bigint_Type'Access,
      Member_Index_For_Lkt_Node_P_String_Type => Member_Desc_For_Lkt_Node_P_String_Type'Access,
      Member_Index_For_Lkt_Node_P_Symbol_Type => Member_Desc_For_Lkt_Node_P_Symbol_Type'Access,
      Member_Index_For_Lkt_Node_P_Property_Error_Type => Member_Desc_For_Lkt_Node_P_Property_Error_Type'Access,
      Member_Index_For_Lkt_Node_P_Regexp_Type => Member_Desc_For_Lkt_Node_P_Regexp_Type'Access,
      Member_Index_For_Lkt_Node_P_Entity_Gen_Type => Member_Desc_For_Lkt_Node_P_Entity_Gen_Type'Access,
      Member_Index_For_Lkt_Node_P_Entity_Type => Member_Desc_For_Lkt_Node_P_Entity_Type'Access,
      Member_Index_For_Lkt_Node_P_Logicvar_Type => Member_Desc_For_Lkt_Node_P_Logicvar_Type'Access,
      Member_Index_For_Lkt_Node_P_Equation_Type => Member_Desc_For_Lkt_Node_P_Equation_Type'Access,
      Member_Index_For_Lkt_Node_P_Array_Gen_Type => Member_Desc_For_Lkt_Node_P_Array_Gen_Type'Access,
      Member_Index_For_Lkt_Node_P_Array_Type => Member_Desc_For_Lkt_Node_P_Array_Type'Access,
      Member_Index_For_Lkt_Node_P_Astlist_Gen_Type => Member_Desc_For_Lkt_Node_P_Astlist_Gen_Type'Access,
      Member_Index_For_Lkt_Node_P_Astlist_Type => Member_Desc_For_Lkt_Node_P_Astlist_Type'Access,
      Member_Index_For_Lkt_Node_P_Node_Builder_Gen_Type => Member_Desc_For_Lkt_Node_P_Node_Builder_Gen_Type'Access,
      Member_Index_For_Lkt_Node_P_Node_Builder_Type => Member_Desc_For_Lkt_Node_P_Node_Builder_Type'Access,
      Member_Index_For_Lkt_Node_P_Iterator_Gen_Trait => Member_Desc_For_Lkt_Node_P_Iterator_Gen_Trait'Access,
      Member_Index_For_Lkt_Node_P_Iterator_Trait => Member_Desc_For_Lkt_Node_P_Iterator_Trait'Access,
      Member_Index_For_Lkt_Node_P_Analysis_Unit_Gen_Trait => Member_Desc_For_Lkt_Node_P_Analysis_Unit_Gen_Trait'Access,
      Member_Index_For_Lkt_Node_P_Analysis_Unit_Trait => Member_Desc_For_Lkt_Node_P_Analysis_Unit_Trait'Access,
      Member_Index_For_Lkt_Node_P_Topmost_Invalid_Decl => Member_Desc_For_Lkt_Node_P_Topmost_Invalid_Decl'Access,
      Member_Index_For_Lkt_Node_P_Nameres_Diagnostics => Member_Desc_For_Lkt_Node_P_Nameres_Diagnostics'Access,
      Member_Index_For_Lkt_Node_P_Solve_Enclosing_Context => Member_Desc_For_Lkt_Node_P_Solve_Enclosing_Context'Access,
      Member_Index_For_Lkt_Node_P_Xref_Entry_Point => Member_Desc_For_Lkt_Node_P_Xref_Entry_Point'Access,
      Member_Index_For_Lkt_Node_P_Complete => Member_Desc_For_Lkt_Node_P_Complete'Access,
      Member_Index_For_Base_Import_P_Referenced_Units => Member_Desc_For_Base_Import_P_Referenced_Units'Access,
      Member_Index_For_Base_Match_Branch_P_Match_Part => Member_Desc_For_Base_Match_Branch_P_Match_Part'Access,
      Member_Index_For_Class_Qualifier_P_As_Bool => Member_Desc_For_Class_Qualifier_P_As_Bool'Access,
      Member_Index_For_Decl_P_Custom_Image => Member_Desc_For_Decl_P_Custom_Image'Access,
      Member_Index_For_Decl_P_Decl_Type_Name => Member_Desc_For_Decl_P_Decl_Type_Name'Access,
      Member_Index_For_Decl_P_Def_Ids => Member_Desc_For_Decl_P_Def_Ids'Access,
      Member_Index_For_Decl_P_As_Bare_Decl => Member_Desc_For_Decl_P_As_Bare_Decl'Access,
      Member_Index_For_Decl_P_Get_Type => Member_Desc_For_Decl_P_Get_Type'Access,
      Member_Index_For_Decl_P_Get_Cast_Type => Member_Desc_For_Decl_P_Get_Cast_Type'Access,
      Member_Index_For_Decl_P_Get_Keep_Type => Member_Desc_For_Decl_P_Get_Keep_Type'Access,
      Member_Index_For_Decl_P_Get_Suffix_Type => Member_Desc_For_Decl_P_Get_Suffix_Type'Access,
      Member_Index_For_Decl_P_Is_Generic => Member_Desc_For_Decl_P_Is_Generic'Access,
      Member_Index_For_Decl_P_Return_Type_Is_Instantiated => Member_Desc_For_Decl_P_Return_Type_Is_Instantiated'Access,
      Member_Index_For_Decl_P_Is_Instantiated => Member_Desc_For_Decl_P_Is_Instantiated'Access,
      Member_Index_For_Decl_P_Name => Member_Desc_For_Decl_P_Name'Access,
      Member_Index_For_Decl_P_Full_Name => Member_Desc_For_Decl_P_Full_Name'Access,
      Member_Index_For_Fun_Decl_P_Is_Dynamic_Combiner => Member_Desc_For_Fun_Decl_P_Is_Dynamic_Combiner'Access,
      Member_Index_For_Fun_Decl_P_Find_All_Overrides => Member_Desc_For_Fun_Decl_P_Find_All_Overrides'Access,
      Member_Index_For_Type_Decl_P_Def_Id => Member_Desc_For_Type_Decl_P_Def_Id'Access,
      Member_Index_For_Type_Decl_P_Base_Type => Member_Desc_For_Type_Decl_P_Base_Type'Access,
      Member_Index_For_Type_Decl_P_Base_Type_If_Entity => Member_Desc_For_Type_Decl_P_Base_Type_If_Entity'Access,
      Member_Index_For_Excludes_Null_P_As_Bool => Member_Desc_For_Excludes_Null_P_As_Bool'Access,
      Member_Index_For_Expr_P_Get_Type => Member_Desc_For_Expr_P_Get_Type'Access,
      Member_Index_For_Expr_P_Get_Generic_Type => Member_Desc_For_Expr_P_Get_Generic_Type'Access,
      Member_Index_For_Expr_P_Get_Expected_Type => Member_Desc_For_Expr_P_Get_Expected_Type'Access,
      Member_Index_For_Expr_P_Referenced_Decl => Member_Desc_For_Expr_P_Referenced_Decl'Access,
      Member_Index_For_Token_Lit_P_Denoted_Value => Member_Desc_For_Token_Lit_P_Denoted_Value'Access,
      Member_Index_For_Token_Pattern_Lit_P_Denoted_Value => Member_Desc_For_Token_Pattern_Lit_P_Denoted_Value'Access,
      Member_Index_For_Id_P_Custom_Image => Member_Desc_For_Id_P_Custom_Image'Access,
      Member_Index_For_Def_Id_P_Name => Member_Desc_For_Def_Id_P_Name'Access,
      Member_Index_For_Def_Id_P_Get_Implementatinons => Member_Desc_For_Def_Id_P_Get_Implementatinons'Access,
      Member_Index_For_Def_Id_P_Decl_Detail => Member_Desc_For_Def_Id_P_Decl_Detail'Access,
      Member_Index_For_Def_Id_P_Completion_Item_Kind => Member_Desc_For_Def_Id_P_Completion_Item_Kind'Access,
      Member_Index_For_Def_Id_P_Doc => Member_Desc_For_Def_Id_P_Doc'Access,
      Member_Index_For_Def_Id_P_Find_All_References => Member_Desc_For_Def_Id_P_Find_All_References'Access,
      Member_Index_For_Ref_Id_P_Referenced_Defining_Name => Member_Desc_For_Ref_Id_P_Referenced_Defining_Name'Access,
      Member_Index_For_Char_Lit_P_Denoted_Value => Member_Desc_For_Char_Lit_P_Denoted_Value'Access,
      Member_Index_For_String_Lit_P_Denoted_Value => Member_Desc_For_String_Lit_P_Denoted_Value'Access,
      Member_Index_For_String_Lit_P_Is_Prefixed_String => Member_Desc_For_String_Lit_P_Is_Prefixed_String'Access,
      Member_Index_For_String_Lit_P_Prefix => Member_Desc_For_String_Lit_P_Prefix'Access,
      Member_Index_For_String_Lit_P_Is_Regexp_Literal => Member_Desc_For_String_Lit_P_Is_Regexp_Literal'Access,
      Member_Index_For_Full_Decl_P_Has_Annotation => Member_Desc_For_Full_Decl_P_Has_Annotation'Access,
      Member_Index_For_Null_Cond_Qualifier_P_As_Bool => Member_Desc_For_Null_Cond_Qualifier_P_As_Bool'Access,
      Member_Index_For_Type_Ref_P_Referenced_Decl => Member_Desc_For_Type_Ref_P_Referenced_Decl'Access);

   -----------------------------
   -- Struct type descriptors --
   -----------------------------

   
      
      Node_Name_For_Complete_Item : aliased constant Text_Type :=
        "Complete_Item";
      Node_Desc_For_Complete_Item : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => No_Type_Index,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Complete_Item'Access,
         Repr_Name         => null,
         Inherited_Members => 1,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Complete_Item_Declaration
         ));
      
      Node_Name_For_Decoded_Char_Value : aliased constant Text_Type :=
        "Decoded_Char_Value";
      Node_Desc_For_Decoded_Char_Value : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => No_Type_Index,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Decoded_Char_Value'Access,
         Repr_Name         => null,
         Inherited_Members => 4,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decoded_Char_Value_Value,
2 => Member_Index_For_Decoded_Char_Value_Has_Error,
3 => Member_Index_For_Decoded_Char_Value_Error_Sloc,
4 => Member_Index_For_Decoded_Char_Value_Error_Message
         ));
      
      Node_Name_For_Decoded_String_Value : aliased constant Text_Type :=
        "Decoded_String_Value";
      Node_Desc_For_Decoded_String_Value : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => No_Type_Index,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Decoded_String_Value'Access,
         Repr_Name         => null,
         Inherited_Members => 4,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decoded_String_Value_Value,
2 => Member_Index_For_Decoded_String_Value_Has_Error,
3 => Member_Index_For_Decoded_String_Value_Error_Sloc,
4 => Member_Index_For_Decoded_String_Value_Error_Message
         ));
      
      Node_Name_For_Logic_Context : aliased constant Text_Type :=
        "Logic_Context";
      Node_Desc_For_Logic_Context : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => No_Type_Index,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Logic_Context'Access,
         Repr_Name         => null,
         Inherited_Members => 2,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Logic_Context_Ref_Node,
2 => Member_Index_For_Logic_Context_Decl_Node
         ));
      
      Node_Name_For_Ref_Result : aliased constant Text_Type :=
        "Ref_Result";
      Node_Desc_For_Ref_Result : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => No_Type_Index,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Ref_Result'Access,
         Repr_Name         => null,
         Inherited_Members => 1,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Ref_Result_Ref
         ));
      
      Node_Name_For_Solver_Diagnostic : aliased constant Text_Type :=
        "Solver_Diagnostic";
      Node_Desc_For_Solver_Diagnostic : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 5,
         Base_Type         => No_Type_Index,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Solver_Diagnostic'Access,
         Repr_Name         => null,
         Inherited_Members => 5,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Solver_Diagnostic_Message_Template,
2 => Member_Index_For_Solver_Diagnostic_Args,
3 => Member_Index_For_Solver_Diagnostic_Location,
4 => Member_Index_For_Solver_Diagnostic_Contexts,
5 => Member_Index_For_Solver_Diagnostic_Round
         ));
      
      Node_Name_For_Solver_Result : aliased constant Text_Type :=
        "Solver_Result";
      Node_Desc_For_Solver_Result : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => No_Type_Index,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Solver_Result'Access,
         Repr_Name         => null,
         Inherited_Members => 2,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Solver_Result_Success,
2 => Member_Index_For_Solver_Result_Diagnostics
         ));
      
      Node_Name_For_Lkt_Node : aliased constant Text_Type :=
        "Lkt_Node";
         Node_Repr_Name_For_Lkt_Node : aliased constant Text_Type :=
           "LktNode";
      Node_Desc_For_Lkt_Node : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 29,
         Member_Count      => 49,
         Base_Type         => No_Type_Index,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Lkt_Node'Access,
         Repr_Name         => Node_Repr_Name_For_Lkt_Node'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 => Type_Index_For_Argument,
2 => Type_Index_For_Base_Import,
3 => Type_Index_For_Base_Lexer_Case_Rule_Alt,
4 => Type_Index_For_Base_Match_Branch,
5 => Type_Index_For_Block_Expr_Clause,
6 => Type_Index_For_Block_String_Line,
7 => Type_Index_For_Class_Qualifier,
8 => Type_Index_For_Decl,
9 => Type_Index_For_Decl_Annotation,
10 => Type_Index_For_Decl_Annotation_Args,
11 => Type_Index_For_Dyn_Env_Wrapper,
12 => Type_Index_For_Elsif_Branch,
13 => Type_Index_For_Enum_Class_Case,
14 => Type_Index_For_Excludes_Null,
15 => Type_Index_For_Expr,
16 => Type_Index_For_Full_Decl,
17 => Type_Index_For_Grammar_List_Sep,
18 => Type_Index_For_Imported_Name,
19 => Type_Index_For_Lexer_Case_Rule,
20 => Type_Index_For_Lexer_Case_Rule_Send,
21 => Type_Index_For_List_Kind,
22 => Type_Index_For_Lkt_Node_Base_List,
23 => Type_Index_For_Module_Doc_String_Line,
24 => Type_Index_For_Null_Cond_Qualifier,
25 => Type_Index_For_Op,
26 => Type_Index_For_Pattern,
27 => Type_Index_For_Pattern_Detail,
28 => Type_Index_For_Type_Ref,
29 => Type_Index_For_Var_Bind
         ),
         Members           => (
              1 => Member_Index_For_Parent,
2 => Member_Index_For_Parents,
3 => Member_Index_For_Children,
4 => Member_Index_For_Token_Start,
5 => Member_Index_For_Token_End,
6 => Member_Index_For_Child_Index,
7 => Member_Index_For_Previous_Sibling,
8 => Member_Index_For_Next_Sibling,
9 => Member_Index_For_Unit,
10 => Member_Index_For_Is_Ghost,
11 => Member_Index_For_Full_Sloc_Image,
12 => Member_Index_For_Completion_Item_Kind_To_Int,
13 => Member_Index_For_Lkt_Node_P_Set_Solver_Debug_Mode,
14 => Member_Index_For_Lkt_Node_P_Prelude_Unit,
15 => Member_Index_For_Lkt_Node_P_Basic_Trait_Gen,
16 => Member_Index_For_Lkt_Node_P_Basic_Trait,
17 => Member_Index_For_Lkt_Node_P_Node_Gen_Trait,
18 => Member_Index_For_Lkt_Node_P_Node_Trait,
19 => Member_Index_For_Lkt_Node_P_Indexable_Gen_Trait,
20 => Member_Index_For_Lkt_Node_P_Indexable_Trait,
21 => Member_Index_For_Lkt_Node_P_Token_Node_Trait,
22 => Member_Index_For_Lkt_Node_P_Error_Node_Trait,
23 => Member_Index_For_Lkt_Node_P_Char_Type,
24 => Member_Index_For_Lkt_Node_P_Int_Type,
25 => Member_Index_For_Lkt_Node_P_Bool_Type,
26 => Member_Index_For_Lkt_Node_P_Bigint_Type,
27 => Member_Index_For_Lkt_Node_P_String_Type,
28 => Member_Index_For_Lkt_Node_P_Symbol_Type,
29 => Member_Index_For_Lkt_Node_P_Property_Error_Type,
30 => Member_Index_For_Lkt_Node_P_Regexp_Type,
31 => Member_Index_For_Lkt_Node_P_Entity_Gen_Type,
32 => Member_Index_For_Lkt_Node_P_Entity_Type,
33 => Member_Index_For_Lkt_Node_P_Logicvar_Type,
34 => Member_Index_For_Lkt_Node_P_Equation_Type,
35 => Member_Index_For_Lkt_Node_P_Array_Gen_Type,
36 => Member_Index_For_Lkt_Node_P_Array_Type,
37 => Member_Index_For_Lkt_Node_P_Astlist_Gen_Type,
38 => Member_Index_For_Lkt_Node_P_Astlist_Type,
39 => Member_Index_For_Lkt_Node_P_Node_Builder_Gen_Type,
40 => Member_Index_For_Lkt_Node_P_Node_Builder_Type,
41 => Member_Index_For_Lkt_Node_P_Iterator_Gen_Trait,
42 => Member_Index_For_Lkt_Node_P_Iterator_Trait,
43 => Member_Index_For_Lkt_Node_P_Analysis_Unit_Gen_Trait,
44 => Member_Index_For_Lkt_Node_P_Analysis_Unit_Trait,
45 => Member_Index_For_Lkt_Node_P_Topmost_Invalid_Decl,
46 => Member_Index_For_Lkt_Node_P_Nameres_Diagnostics,
47 => Member_Index_For_Lkt_Node_P_Solve_Enclosing_Context,
48 => Member_Index_For_Lkt_Node_P_Xref_Entry_Point,
49 => Member_Index_For_Lkt_Node_P_Complete
         ));
      
      Node_Name_For_Argument : aliased constant Text_Type :=
        "Argument";
         Node_Repr_Name_For_Argument : aliased constant Text_Type :=
           "Argument";
      Node_Desc_For_Argument : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Argument'Access,
         Repr_Name         => Node_Repr_Name_For_Argument'Access,
         Inherited_Members => 51,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Argument_F_Name,
2 => Member_Index_For_Argument_F_Value
         ));
      
      Node_Name_For_Base_Import : aliased constant Text_Type :=
        "Base_Import";
         Node_Repr_Name_For_Base_Import : aliased constant Text_Type :=
           "BaseImport";
      Node_Desc_For_Base_Import : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 3,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Base_Import'Access,
         Repr_Name         => Node_Repr_Name_For_Base_Import'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 => Type_Index_For_Import,
2 => Type_Index_For_Import_All_From,
3 => Type_Index_For_Import_From
         ),
         Members           => (
              1 => Member_Index_For_Base_Import_P_Referenced_Units
         ));
      
      Node_Name_For_Import : aliased constant Text_Type :=
        "Import";
         Node_Repr_Name_For_Import : aliased constant Text_Type :=
           "Import";
      Node_Desc_For_Import : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Base_Import,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Import'Access,
         Repr_Name         => Node_Repr_Name_For_Import'Access,
         Inherited_Members => 51,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Import_F_Imported_Names
         ));
      
      Node_Name_For_Import_All_From : aliased constant Text_Type :=
        "Import_All_From";
         Node_Repr_Name_For_Import_All_From : aliased constant Text_Type :=
           "ImportAllFrom";
      Node_Desc_For_Import_All_From : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Base_Import,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Import_All_From'Access,
         Repr_Name         => Node_Repr_Name_For_Import_All_From'Access,
         Inherited_Members => 51,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Import_All_From_F_Module_Name
         ));
      
      Node_Name_For_Import_From : aliased constant Text_Type :=
        "Import_From";
         Node_Repr_Name_For_Import_From : aliased constant Text_Type :=
           "ImportFrom";
      Node_Desc_For_Import_From : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Base_Import,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Import_From'Access,
         Repr_Name         => Node_Repr_Name_For_Import_From'Access,
         Inherited_Members => 52,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Import_From_F_Module_Name,
2 => Member_Index_For_Import_From_F_Imported_Names
         ));
      
      Node_Name_For_Base_Lexer_Case_Rule_Alt : aliased constant Text_Type :=
        "Base_Lexer_Case_Rule_Alt";
         Node_Repr_Name_For_Base_Lexer_Case_Rule_Alt : aliased constant Text_Type :=
           "BaseLexerCaseRuleAlt";
      Node_Desc_For_Base_Lexer_Case_Rule_Alt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 3,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Base_Lexer_Case_Rule_Alt'Access,
         Repr_Name         => Node_Repr_Name_For_Base_Lexer_Case_Rule_Alt'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 => Type_Index_For_Error_Lexer_Case_Rule_Alt,
2 => Type_Index_For_Lexer_Case_Rule_Cond_Alt,
3 => Type_Index_For_Lexer_Case_Rule_Default_Alt
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Error_Lexer_Case_Rule_Alt : aliased constant Text_Type :=
        "Error_Lexer_Case_Rule_Alt";
         Node_Repr_Name_For_Error_Lexer_Case_Rule_Alt : aliased constant Text_Type :=
           "ErrorLexerCaseRuleAlt";
      Node_Desc_For_Error_Lexer_Case_Rule_Alt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Base_Lexer_Case_Rule_Alt,
         Is_Abstract       => False,
         Is_Error_Node     => True,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Error_Lexer_Case_Rule_Alt'Access,
         Repr_Name         => Node_Repr_Name_For_Error_Lexer_Case_Rule_Alt'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Lexer_Case_Rule_Cond_Alt : aliased constant Text_Type :=
        "Lexer_Case_Rule_Cond_Alt";
         Node_Repr_Name_For_Lexer_Case_Rule_Cond_Alt : aliased constant Text_Type :=
           "LexerCaseRuleCondAlt";
      Node_Desc_For_Lexer_Case_Rule_Cond_Alt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Base_Lexer_Case_Rule_Alt,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Lexer_Case_Rule_Cond_Alt'Access,
         Repr_Name         => Node_Repr_Name_For_Lexer_Case_Rule_Cond_Alt'Access,
         Inherited_Members => 51,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs,
2 => Member_Index_For_Lexer_Case_Rule_Cond_Alt_F_Send
         ));
      
      Node_Name_For_Lexer_Case_Rule_Default_Alt : aliased constant Text_Type :=
        "Lexer_Case_Rule_Default_Alt";
         Node_Repr_Name_For_Lexer_Case_Rule_Default_Alt : aliased constant Text_Type :=
           "LexerCaseRuleDefaultAlt";
      Node_Desc_For_Lexer_Case_Rule_Default_Alt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Base_Lexer_Case_Rule_Alt,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Lexer_Case_Rule_Default_Alt'Access,
         Repr_Name         => Node_Repr_Name_For_Lexer_Case_Rule_Default_Alt'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Lexer_Case_Rule_Default_Alt_F_Send
         ));
      
      Node_Name_For_Base_Match_Branch : aliased constant Text_Type :=
        "Base_Match_Branch";
         Node_Repr_Name_For_Base_Match_Branch : aliased constant Text_Type :=
           "BaseMatchBranch";
      Node_Desc_For_Base_Match_Branch : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Base_Match_Branch'Access,
         Repr_Name         => Node_Repr_Name_For_Base_Match_Branch'Access,
         Inherited_Members => 51,
         Derivations       => (
             1 => Type_Index_For_Match_Branch,
2 => Type_Index_For_Pattern_Match_Branch
         ),
         Members           => (
              1 => Member_Index_For_Base_Match_Branch_F_Expr,
2 => Member_Index_For_Base_Match_Branch_P_Match_Part
         ));
      
      Node_Name_For_Match_Branch : aliased constant Text_Type :=
        "Match_Branch";
         Node_Repr_Name_For_Match_Branch : aliased constant Text_Type :=
           "MatchBranch";
      Node_Desc_For_Match_Branch : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Base_Match_Branch,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Match_Branch'Access,
         Repr_Name         => Node_Repr_Name_For_Match_Branch'Access,
         Inherited_Members => 52,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Match_Branch_F_Decl,
2 => Member_Index_For_Base_Match_Branch_F_Expr
         ));
      
      Node_Name_For_Pattern_Match_Branch : aliased constant Text_Type :=
        "Pattern_Match_Branch";
         Node_Repr_Name_For_Pattern_Match_Branch : aliased constant Text_Type :=
           "PatternMatchBranch";
      Node_Desc_For_Pattern_Match_Branch : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Base_Match_Branch,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Pattern_Match_Branch'Access,
         Repr_Name         => Node_Repr_Name_For_Pattern_Match_Branch'Access,
         Inherited_Members => 52,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Pattern_Match_Branch_F_Pattern,
2 => Member_Index_For_Base_Match_Branch_F_Expr
         ));
      
      Node_Name_For_Block_Expr_Clause : aliased constant Text_Type :=
        "Block_Expr_Clause";
         Node_Repr_Name_For_Block_Expr_Clause : aliased constant Text_Type :=
           "BlockExprClause";
      Node_Desc_For_Block_Expr_Clause : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Block_Expr_Clause'Access,
         Repr_Name         => Node_Repr_Name_For_Block_Expr_Clause'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Block_Expr_Clause_F_Clause
         ));
      
      Node_Name_For_Block_String_Line : aliased constant Text_Type :=
        "Block_String_Line";
         Node_Repr_Name_For_Block_String_Line : aliased constant Text_Type :=
           "BlockStringLine";
      Node_Desc_For_Block_String_Line : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_Block_String_Line,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Block_String_Line'Access,
         Repr_Name         => Node_Repr_Name_For_Block_String_Line'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Class_Qualifier : aliased constant Text_Type :=
        "Class_Qualifier";
         Node_Repr_Name_For_Class_Qualifier : aliased constant Text_Type :=
           "ClassQualifier";
      Node_Desc_For_Class_Qualifier : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Class_Qualifier'Access,
         Repr_Name         => Node_Repr_Name_For_Class_Qualifier'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 => Type_Index_For_Class_Qualifier_Absent,
2 => Type_Index_For_Class_Qualifier_Present
         ),
         Members           => (
              1 => Member_Index_For_Class_Qualifier_P_As_Bool
         ));
      
      Node_Name_For_Class_Qualifier_Absent : aliased constant Text_Type :=
        "Class_Qualifier_Absent";
         Node_Repr_Name_For_Class_Qualifier_Absent : aliased constant Text_Type :=
           "ClassQualifierAbsent";
      Node_Desc_For_Class_Qualifier_Absent : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Class_Qualifier,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Class_Qualifier_Absent'Access,
         Repr_Name         => Node_Repr_Name_For_Class_Qualifier_Absent'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Class_Qualifier_Present : aliased constant Text_Type :=
        "Class_Qualifier_Present";
         Node_Repr_Name_For_Class_Qualifier_Present : aliased constant Text_Type :=
           "ClassQualifierPresent";
      Node_Desc_For_Class_Qualifier_Present : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Class_Qualifier,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Class_Qualifier_Present'Access,
         Repr_Name         => Node_Repr_Name_For_Class_Qualifier_Present'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Decl : aliased constant Text_Type :=
        "Decl";
         Node_Repr_Name_For_Decl : aliased constant Text_Type :=
           "Decl";
      Node_Desc_For_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 12,
         Member_Count      => 14,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Decl'Access,
         Inherited_Members => 63,
         Derivations       => (
             1 => Type_Index_For_Base_Grammar_Rule_Decl,
2 => Type_Index_For_Base_Val_Decl,
3 => Type_Index_For_Env_Spec_Decl,
4 => Type_Index_For_Error_Decl,
5 => Type_Index_For_Generic_Decl,
6 => Type_Index_For_Grammar_Decl,
7 => Type_Index_For_Langkit_Root,
8 => Type_Index_For_Lexer_Decl,
9 => Type_Index_For_Lexer_Family_Decl,
10 => Type_Index_For_Synth_Fun_Decl,
11 => Type_Index_For_Synth_Param_Decl,
12 => Type_Index_For_Type_Decl
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Decl_P_Custom_Image,
3 => Member_Index_For_Decl_P_Decl_Type_Name,
4 => Member_Index_For_Decl_P_Def_Ids,
5 => Member_Index_For_Decl_P_As_Bare_Decl,
6 => Member_Index_For_Decl_P_Get_Type,
7 => Member_Index_For_Decl_P_Get_Cast_Type,
8 => Member_Index_For_Decl_P_Get_Keep_Type,
9 => Member_Index_For_Decl_P_Get_Suffix_Type,
10 => Member_Index_For_Decl_P_Is_Generic,
11 => Member_Index_For_Decl_P_Return_Type_Is_Instantiated,
12 => Member_Index_For_Decl_P_Is_Instantiated,
13 => Member_Index_For_Decl_P_Name,
14 => Member_Index_For_Decl_P_Full_Name
         ));
      
      Node_Name_For_Base_Grammar_Rule_Decl : aliased constant Text_Type :=
        "Base_Grammar_Rule_Decl";
         Node_Repr_Name_For_Base_Grammar_Rule_Decl : aliased constant Text_Type :=
           "BaseGrammarRuleDecl";
      Node_Desc_For_Base_Grammar_Rule_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Decl,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Base_Grammar_Rule_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Base_Grammar_Rule_Decl'Access,
         Inherited_Members => 64,
         Derivations       => (
             1 => Type_Index_For_Grammar_Rule_Decl,
2 => Type_Index_For_Synthetic_Lexer_Decl
         ),
         Members           => (
              1 => Member_Index_For_Base_Grammar_Rule_Decl_F_Expr
         ));
      
      Node_Name_For_Grammar_Rule_Decl : aliased constant Text_Type :=
        "Grammar_Rule_Decl";
         Node_Repr_Name_For_Grammar_Rule_Decl : aliased constant Text_Type :=
           "GrammarRuleDecl";
      Node_Desc_For_Grammar_Rule_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Base_Grammar_Rule_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Rule_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Rule_Decl'Access,
         Inherited_Members => 64,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Base_Grammar_Rule_Decl_F_Expr
         ));
      
      Node_Name_For_Synthetic_Lexer_Decl : aliased constant Text_Type :=
        "Synthetic_Lexer_Decl";
         Node_Repr_Name_For_Synthetic_Lexer_Decl : aliased constant Text_Type :=
           "SyntheticLexerDecl";
      Node_Desc_For_Synthetic_Lexer_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Base_Grammar_Rule_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => True,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Synthetic_Lexer_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Synthetic_Lexer_Decl'Access,
         Inherited_Members => 64,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Base_Grammar_Rule_Decl_F_Expr
         ));
      
      Node_Name_For_Base_Val_Decl : aliased constant Text_Type :=
        "Base_Val_Decl";
         Node_Repr_Name_For_Base_Val_Decl : aliased constant Text_Type :=
           "BaseValDecl";
      Node_Desc_For_Base_Val_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 3,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Decl,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Base_Val_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Base_Val_Decl'Access,
         Inherited_Members => 63,
         Derivations       => (
             1 => Type_Index_For_Node_Decl,
2 => Type_Index_For_Self_Decl,
3 => Type_Index_For_User_Val_Decl
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Node_Decl : aliased constant Text_Type :=
        "Node_Decl";
         Node_Repr_Name_For_Node_Decl : aliased constant Text_Type :=
           "NodeDecl";
      Node_Desc_For_Node_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Base_Val_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => True,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Node_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Node_Decl'Access,
         Inherited_Members => 63,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name
         ));
      
      Node_Name_For_Self_Decl : aliased constant Text_Type :=
        "Self_Decl";
         Node_Repr_Name_For_Self_Decl : aliased constant Text_Type :=
           "SelfDecl";
      Node_Desc_For_Self_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Base_Val_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => True,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Self_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Self_Decl'Access,
         Inherited_Members => 63,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name
         ));
      
      Node_Name_For_User_Val_Decl : aliased constant Text_Type :=
        "User_Val_Decl";
         Node_Repr_Name_For_User_Val_Decl : aliased constant Text_Type :=
           "UserValDecl";
      Node_Desc_For_User_Val_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 4,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Base_Val_Decl,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_User_Val_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_User_Val_Decl'Access,
         Inherited_Members => 63,
         Derivations       => (
             1 => Type_Index_For_Binding_Val_Decl,
2 => Type_Index_For_Enum_Lit_Decl,
3 => Type_Index_For_Explicitly_Typed_Decl,
4 => Type_Index_For_Fun_Decl
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Binding_Val_Decl : aliased constant Text_Type :=
        "Binding_Val_Decl";
         Node_Repr_Name_For_Binding_Val_Decl : aliased constant Text_Type :=
           "BindingValDecl";
      Node_Desc_For_Binding_Val_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_User_Val_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Binding_Val_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Binding_Val_Decl'Access,
         Inherited_Members => 63,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name
         ));
      
      Node_Name_For_Enum_Lit_Decl : aliased constant Text_Type :=
        "Enum_Lit_Decl";
         Node_Repr_Name_For_Enum_Lit_Decl : aliased constant Text_Type :=
           "EnumLitDecl";
      Node_Desc_For_Enum_Lit_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_User_Val_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Enum_Lit_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Enum_Lit_Decl'Access,
         Inherited_Members => 63,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name
         ));
      
      Node_Name_For_Explicitly_Typed_Decl : aliased constant Text_Type :=
        "Explicitly_Typed_Decl";
         Node_Repr_Name_For_Explicitly_Typed_Decl : aliased constant Text_Type :=
           "ExplicitlyTypedDecl";
      Node_Desc_For_Explicitly_Typed_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 4,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_User_Val_Decl,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Explicitly_Typed_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Explicitly_Typed_Decl'Access,
         Inherited_Members => 64,
         Derivations       => (
             1 => Type_Index_For_Component_Decl,
2 => Type_Index_For_Dyn_Var_Decl,
3 => Type_Index_For_Match_Val_Decl,
4 => Type_Index_For_Val_Decl
         ),
         Members           => (
              1 => Member_Index_For_Explicitly_Typed_Decl_F_Decl_Type
         ));
      
      Node_Name_For_Component_Decl : aliased constant Text_Type :=
        "Component_Decl";
         Node_Repr_Name_For_Component_Decl : aliased constant Text_Type :=
           "ComponentDecl";
      Node_Desc_For_Component_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 3,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Explicitly_Typed_Decl,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Component_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Component_Decl'Access,
         Inherited_Members => 65,
         Derivations       => (
             1 => Type_Index_For_Field_Decl,
2 => Type_Index_For_Fun_Param_Decl,
3 => Type_Index_For_Lambda_Param_Decl
         ),
         Members           => (
              1 => Member_Index_For_Component_Decl_F_Default_Val
         ));
      
      Node_Name_For_Field_Decl : aliased constant Text_Type :=
        "Field_Decl";
         Node_Repr_Name_For_Field_Decl : aliased constant Text_Type :=
           "FieldDecl";
      Node_Desc_For_Field_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Component_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Field_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Field_Decl'Access,
         Inherited_Members => 66,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Explicitly_Typed_Decl_F_Decl_Type,
3 => Member_Index_For_Field_Decl_F_Trait_Ref,
4 => Member_Index_For_Component_Decl_F_Default_Val
         ));
      
      Node_Name_For_Fun_Param_Decl : aliased constant Text_Type :=
        "Fun_Param_Decl";
         Node_Repr_Name_For_Fun_Param_Decl : aliased constant Text_Type :=
           "FunParamDecl";
      Node_Desc_For_Fun_Param_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Component_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Fun_Param_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Fun_Param_Decl'Access,
         Inherited_Members => 66,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Fun_Param_Decl_F_Decl_Annotations,
2 => Member_Index_For_Decl_F_Syn_Name,
3 => Member_Index_For_Explicitly_Typed_Decl_F_Decl_Type,
4 => Member_Index_For_Component_Decl_F_Default_Val
         ));
      
      Node_Name_For_Lambda_Param_Decl : aliased constant Text_Type :=
        "Lambda_Param_Decl";
         Node_Repr_Name_For_Lambda_Param_Decl : aliased constant Text_Type :=
           "LambdaParamDecl";
      Node_Desc_For_Lambda_Param_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Component_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Lambda_Param_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Lambda_Param_Decl'Access,
         Inherited_Members => 65,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Explicitly_Typed_Decl_F_Decl_Type,
3 => Member_Index_For_Component_Decl_F_Default_Val
         ));
      
      Node_Name_For_Dyn_Var_Decl : aliased constant Text_Type :=
        "Dyn_Var_Decl";
         Node_Repr_Name_For_Dyn_Var_Decl : aliased constant Text_Type :=
           "DynVarDecl";
      Node_Desc_For_Dyn_Var_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Explicitly_Typed_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Dyn_Var_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Dyn_Var_Decl'Access,
         Inherited_Members => 64,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Explicitly_Typed_Decl_F_Decl_Type
         ));
      
      Node_Name_For_Match_Val_Decl : aliased constant Text_Type :=
        "Match_Val_Decl";
         Node_Repr_Name_For_Match_Val_Decl : aliased constant Text_Type :=
           "MatchValDecl";
      Node_Desc_For_Match_Val_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Explicitly_Typed_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Match_Val_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Match_Val_Decl'Access,
         Inherited_Members => 64,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Explicitly_Typed_Decl_F_Decl_Type
         ));
      
      Node_Name_For_Val_Decl : aliased constant Text_Type :=
        "Val_Decl";
         Node_Repr_Name_For_Val_Decl : aliased constant Text_Type :=
           "ValDecl";
      Node_Desc_For_Val_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Explicitly_Typed_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Val_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Val_Decl'Access,
         Inherited_Members => 65,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Explicitly_Typed_Decl_F_Decl_Type,
3 => Member_Index_For_Val_Decl_F_Expr
         ));
      
      Node_Name_For_Fun_Decl : aliased constant Text_Type :=
        "Fun_Decl";
         Node_Repr_Name_For_Fun_Decl : aliased constant Text_Type :=
           "FunDecl";
      Node_Desc_For_Fun_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 7,
         Base_Type         => Type_Index_For_User_Val_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Fun_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Fun_Decl'Access,
         Inherited_Members => 69,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Fun_Decl_F_Params,
3 => Member_Index_For_Fun_Decl_F_Return_Type,
4 => Member_Index_For_Fun_Decl_F_Trait_Ref,
5 => Member_Index_For_Fun_Decl_F_Body,
6 => Member_Index_For_Fun_Decl_P_Is_Dynamic_Combiner,
7 => Member_Index_For_Fun_Decl_P_Find_All_Overrides
         ));
      
      Node_Name_For_Env_Spec_Decl : aliased constant Text_Type :=
        "Env_Spec_Decl";
         Node_Repr_Name_For_Env_Spec_Decl : aliased constant Text_Type :=
           "EnvSpecDecl";
      Node_Desc_For_Env_Spec_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Env_Spec_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Env_Spec_Decl'Access,
         Inherited_Members => 64,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Env_Spec_Decl_F_Actions
         ));
      
      Node_Name_For_Error_Decl : aliased constant Text_Type :=
        "Error_Decl";
         Node_Repr_Name_For_Error_Decl : aliased constant Text_Type :=
           "ErrorDecl";
      Node_Desc_For_Error_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => True,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Error_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Error_Decl'Access,
         Inherited_Members => 63,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name
         ));
      
      Node_Name_For_Generic_Decl : aliased constant Text_Type :=
        "Generic_Decl";
         Node_Repr_Name_For_Generic_Decl : aliased constant Text_Type :=
           "GenericDecl";
      Node_Desc_For_Generic_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Generic_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Generic_Decl'Access,
         Inherited_Members => 65,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Generic_Decl_F_Generic_Param_Decls,
2 => Member_Index_For_Generic_Decl_F_Decl,
3 => Member_Index_For_Decl_F_Syn_Name
         ));
      
      Node_Name_For_Grammar_Decl : aliased constant Text_Type :=
        "Grammar_Decl";
         Node_Repr_Name_For_Grammar_Decl : aliased constant Text_Type :=
           "GrammarDecl";
      Node_Desc_For_Grammar_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Decl'Access,
         Inherited_Members => 64,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Grammar_Decl_F_Rules
         ));
      
      Node_Name_For_Langkit_Root : aliased constant Text_Type :=
        "Langkit_Root";
         Node_Repr_Name_For_Langkit_Root : aliased constant Text_Type :=
           "LangkitRoot";
      Node_Desc_For_Langkit_Root : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Langkit_Root'Access,
         Repr_Name         => Node_Repr_Name_For_Langkit_Root'Access,
         Inherited_Members => 66,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Langkit_Root_F_Doc,
3 => Member_Index_For_Langkit_Root_F_Imports,
4 => Member_Index_For_Langkit_Root_F_Decls
         ));
      
      Node_Name_For_Lexer_Decl : aliased constant Text_Type :=
        "Lexer_Decl";
         Node_Repr_Name_For_Lexer_Decl : aliased constant Text_Type :=
           "LexerDecl";
      Node_Desc_For_Lexer_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Lexer_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Lexer_Decl'Access,
         Inherited_Members => 64,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Lexer_Decl_F_Rules
         ));
      
      Node_Name_For_Lexer_Family_Decl : aliased constant Text_Type :=
        "Lexer_Family_Decl";
         Node_Repr_Name_For_Lexer_Family_Decl : aliased constant Text_Type :=
           "LexerFamilyDecl";
      Node_Desc_For_Lexer_Family_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Lexer_Family_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Lexer_Family_Decl'Access,
         Inherited_Members => 64,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Lexer_Family_Decl_F_Rules
         ));
      
      Node_Name_For_Synth_Fun_Decl : aliased constant Text_Type :=
        "Synth_Fun_Decl";
         Node_Repr_Name_For_Synth_Fun_Decl : aliased constant Text_Type :=
           "SynthFunDecl";
      Node_Desc_For_Synth_Fun_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => True,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Synth_Fun_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Synth_Fun_Decl'Access,
         Inherited_Members => 63,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name
         ));
      
      Node_Name_For_Synth_Param_Decl : aliased constant Text_Type :=
        "Synth_Param_Decl";
         Node_Repr_Name_For_Synth_Param_Decl : aliased constant Text_Type :=
           "SynthParamDecl";
      Node_Desc_For_Synth_Param_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => True,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Synth_Param_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Synth_Param_Decl'Access,
         Inherited_Members => 63,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name
         ));
      
      Node_Name_For_Type_Decl : aliased constant Text_Type :=
        "Type_Decl";
         Node_Repr_Name_For_Type_Decl : aliased constant Text_Type :=
           "TypeDecl";
      Node_Desc_For_Type_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 5,
         Member_Count      => 5,
         Base_Type         => Type_Index_For_Decl,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Type_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Type_Decl'Access,
         Inherited_Members => 68,
         Derivations       => (
             1 => Type_Index_For_Any_Type_Decl,
2 => Type_Index_For_Enum_Class_Alt_Decl,
3 => Type_Index_For_Function_Type,
4 => Type_Index_For_Generic_Param_Type_Decl,
5 => Type_Index_For_Named_Type_Decl
         ),
         Members           => (
              1 => Member_Index_For_Type_Decl_F_Traits,
2 => Member_Index_For_Type_Decl_F_Syn_Base_Type,
3 => Member_Index_For_Type_Decl_P_Def_Id,
4 => Member_Index_For_Type_Decl_P_Base_Type,
5 => Member_Index_For_Type_Decl_P_Base_Type_If_Entity
         ));
      
      Node_Name_For_Any_Type_Decl : aliased constant Text_Type :=
        "Any_Type_Decl";
         Node_Repr_Name_For_Any_Type_Decl : aliased constant Text_Type :=
           "AnyTypeDecl";
      Node_Desc_For_Any_Type_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Type_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => True,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Any_Type_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Any_Type_Decl'Access,
         Inherited_Members => 68,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Type_Decl_F_Traits,
2 => Member_Index_For_Decl_F_Syn_Name,
3 => Member_Index_For_Type_Decl_F_Syn_Base_Type
         ));
      
      Node_Name_For_Enum_Class_Alt_Decl : aliased constant Text_Type :=
        "Enum_Class_Alt_Decl";
         Node_Repr_Name_For_Enum_Class_Alt_Decl : aliased constant Text_Type :=
           "EnumClassAltDecl";
      Node_Desc_For_Enum_Class_Alt_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Type_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Enum_Class_Alt_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Enum_Class_Alt_Decl'Access,
         Inherited_Members => 68,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Type_Decl_F_Traits,
3 => Member_Index_For_Type_Decl_F_Syn_Base_Type
         ));
      
      Node_Name_For_Function_Type : aliased constant Text_Type :=
        "Function_Type";
         Node_Repr_Name_For_Function_Type : aliased constant Text_Type :=
           "FunctionType";
      Node_Desc_For_Function_Type : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Type_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => True,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Function_Type'Access,
         Repr_Name         => Node_Repr_Name_For_Function_Type'Access,
         Inherited_Members => 68,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Type_Decl_F_Traits,
3 => Member_Index_For_Type_Decl_F_Syn_Base_Type
         ));
      
      Node_Name_For_Generic_Param_Type_Decl : aliased constant Text_Type :=
        "Generic_Param_Type_Decl";
         Node_Repr_Name_For_Generic_Param_Type_Decl : aliased constant Text_Type :=
           "GenericParamTypeDecl";
      Node_Desc_For_Generic_Param_Type_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Type_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Generic_Param_Type_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Generic_Param_Type_Decl'Access,
         Inherited_Members => 69,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Generic_Param_Type_Decl_F_Has_Class,
2 => Member_Index_For_Decl_F_Syn_Name,
3 => Member_Index_For_Type_Decl_F_Traits,
4 => Member_Index_For_Type_Decl_F_Syn_Base_Type
         ));
      
      Node_Name_For_Named_Type_Decl : aliased constant Text_Type :=
        "Named_Type_Decl";
         Node_Repr_Name_For_Named_Type_Decl : aliased constant Text_Type :=
           "NamedTypeDecl";
      Node_Desc_For_Named_Type_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 4,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Type_Decl,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Named_Type_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Named_Type_Decl'Access,
         Inherited_Members => 69,
         Derivations       => (
             1 => Type_Index_For_Basic_Class_Decl,
2 => Type_Index_For_Enum_Type_Decl,
3 => Type_Index_For_Struct_Decl,
4 => Type_Index_For_Trait_Decl
         ),
         Members           => (
              1 => Member_Index_For_Named_Type_Decl_F_Decls
         ));
      
      Node_Name_For_Basic_Class_Decl : aliased constant Text_Type :=
        "Basic_Class_Decl";
         Node_Repr_Name_For_Basic_Class_Decl : aliased constant Text_Type :=
           "BasicClassDecl";
      Node_Desc_For_Basic_Class_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Named_Type_Decl,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Basic_Class_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Basic_Class_Decl'Access,
         Inherited_Members => 69,
         Derivations       => (
             1 => Type_Index_For_Class_Decl,
2 => Type_Index_For_Enum_Class_Decl
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Type_Decl_F_Syn_Base_Type,
3 => Member_Index_For_Type_Decl_F_Traits
         ));
      
      Node_Name_For_Class_Decl : aliased constant Text_Type :=
        "Class_Decl";
         Node_Repr_Name_For_Class_Decl : aliased constant Text_Type :=
           "ClassDecl";
      Node_Desc_For_Class_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Basic_Class_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Class_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Class_Decl'Access,
         Inherited_Members => 69,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Named_Type_Decl_F_Decls
         ));
      
      Node_Name_For_Enum_Class_Decl : aliased constant Text_Type :=
        "Enum_Class_Decl";
         Node_Repr_Name_For_Enum_Class_Decl : aliased constant Text_Type :=
           "EnumClassDecl";
      Node_Desc_For_Enum_Class_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Basic_Class_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Enum_Class_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Enum_Class_Decl'Access,
         Inherited_Members => 70,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Enum_Class_Decl_F_Branches,
2 => Member_Index_For_Named_Type_Decl_F_Decls
         ));
      
      Node_Name_For_Enum_Type_Decl : aliased constant Text_Type :=
        "Enum_Type_Decl";
         Node_Repr_Name_For_Enum_Type_Decl : aliased constant Text_Type :=
           "EnumTypeDecl";
      Node_Desc_For_Enum_Type_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 5,
         Base_Type         => Type_Index_For_Named_Type_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Enum_Type_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Enum_Type_Decl'Access,
         Inherited_Members => 70,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Type_Decl_F_Traits,
3 => Member_Index_For_Type_Decl_F_Syn_Base_Type,
4 => Member_Index_For_Enum_Type_Decl_F_Literals,
5 => Member_Index_For_Named_Type_Decl_F_Decls
         ));
      
      Node_Name_For_Struct_Decl : aliased constant Text_Type :=
        "Struct_Decl";
         Node_Repr_Name_For_Struct_Decl : aliased constant Text_Type :=
           "StructDecl";
      Node_Desc_For_Struct_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Named_Type_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Struct_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Struct_Decl'Access,
         Inherited_Members => 69,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Type_Decl_F_Traits,
3 => Member_Index_For_Type_Decl_F_Syn_Base_Type,
4 => Member_Index_For_Named_Type_Decl_F_Decls
         ));
      
      Node_Name_For_Trait_Decl : aliased constant Text_Type :=
        "Trait_Decl";
         Node_Repr_Name_For_Trait_Decl : aliased constant Text_Type :=
           "TraitDecl";
      Node_Desc_For_Trait_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Named_Type_Decl,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Trait_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Trait_Decl'Access,
         Inherited_Members => 69,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_F_Syn_Name,
2 => Member_Index_For_Type_Decl_F_Traits,
3 => Member_Index_For_Type_Decl_F_Syn_Base_Type,
4 => Member_Index_For_Named_Type_Decl_F_Decls
         ));
      
      Node_Name_For_Decl_Annotation : aliased constant Text_Type :=
        "Decl_Annotation";
         Node_Repr_Name_For_Decl_Annotation : aliased constant Text_Type :=
           "DeclAnnotation";
      Node_Desc_For_Decl_Annotation : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Decl_Annotation'Access,
         Repr_Name         => Node_Repr_Name_For_Decl_Annotation'Access,
         Inherited_Members => 51,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_Annotation_F_Name,
2 => Member_Index_For_Decl_Annotation_F_Args
         ));
      
      Node_Name_For_Decl_Annotation_Args : aliased constant Text_Type :=
        "Decl_Annotation_Args";
         Node_Repr_Name_For_Decl_Annotation_Args : aliased constant Text_Type :=
           "DeclAnnotationArgs";
      Node_Desc_For_Decl_Annotation_Args : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Decl_Annotation_Args'Access,
         Repr_Name         => Node_Repr_Name_For_Decl_Annotation_Args'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Decl_Annotation_Args_F_Args
         ));
      
      Node_Name_For_Dyn_Env_Wrapper : aliased constant Text_Type :=
        "Dyn_Env_Wrapper";
         Node_Repr_Name_For_Dyn_Env_Wrapper : aliased constant Text_Type :=
           "DynEnvWrapper";
      Node_Desc_For_Dyn_Env_Wrapper : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => True,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Dyn_Env_Wrapper'Access,
         Repr_Name         => Node_Repr_Name_For_Dyn_Env_Wrapper'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Elsif_Branch : aliased constant Text_Type :=
        "Elsif_Branch";
         Node_Repr_Name_For_Elsif_Branch : aliased constant Text_Type :=
           "ElsifBranch";
      Node_Desc_For_Elsif_Branch : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Elsif_Branch'Access,
         Repr_Name         => Node_Repr_Name_For_Elsif_Branch'Access,
         Inherited_Members => 51,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Elsif_Branch_F_Cond_Expr,
2 => Member_Index_For_Elsif_Branch_F_Then_Expr
         ));
      
      Node_Name_For_Enum_Class_Case : aliased constant Text_Type :=
        "Enum_Class_Case";
         Node_Repr_Name_For_Enum_Class_Case : aliased constant Text_Type :=
           "EnumClassCase";
      Node_Desc_For_Enum_Class_Case : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Enum_Class_Case'Access,
         Repr_Name         => Node_Repr_Name_For_Enum_Class_Case'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Enum_Class_Case_F_Decls
         ));
      
      Node_Name_For_Excludes_Null : aliased constant Text_Type :=
        "Excludes_Null";
         Node_Repr_Name_For_Excludes_Null : aliased constant Text_Type :=
           "ExcludesNull";
      Node_Desc_For_Excludes_Null : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Excludes_Null'Access,
         Repr_Name         => Node_Repr_Name_For_Excludes_Null'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 => Type_Index_For_Excludes_Null_Absent,
2 => Type_Index_For_Excludes_Null_Present
         ),
         Members           => (
              1 => Member_Index_For_Excludes_Null_P_As_Bool
         ));
      
      Node_Name_For_Excludes_Null_Absent : aliased constant Text_Type :=
        "Excludes_Null_Absent";
         Node_Repr_Name_For_Excludes_Null_Absent : aliased constant Text_Type :=
           "ExcludesNullAbsent";
      Node_Desc_For_Excludes_Null_Absent : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Excludes_Null,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Excludes_Null_Absent'Access,
         Repr_Name         => Node_Repr_Name_For_Excludes_Null_Absent'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Excludes_Null_Present : aliased constant Text_Type :=
        "Excludes_Null_Present";
         Node_Repr_Name_For_Excludes_Null_Present : aliased constant Text_Type :=
           "ExcludesNullPresent";
      Node_Desc_For_Excludes_Null_Present : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Excludes_Null,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Excludes_Null_Present'Access,
         Repr_Name         => Node_Repr_Name_For_Excludes_Null_Present'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Expr : aliased constant Text_Type :=
        "Expr";
         Node_Repr_Name_For_Expr : aliased constant Text_Type :=
           "Expr";
      Node_Desc_For_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 28,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Expr'Access,
         Inherited_Members => 53,
         Derivations       => (
             1 => Type_Index_For_Any_Of,
2 => Type_Index_For_Array_Literal,
3 => Type_Index_For_Base_Call_Expr,
4 => Type_Index_For_Bin_Op,
5 => Type_Index_For_Block_Expr,
6 => Type_Index_For_Cast_Expr,
7 => Type_Index_For_Dot_Expr,
8 => Type_Index_For_Error_On_Null,
9 => Type_Index_For_Generic_Instantiation,
10 => Type_Index_For_Grammar_Expr,
11 => Type_Index_For_Id,
12 => Type_Index_For_If_Expr,
13 => Type_Index_For_Isa,
14 => Type_Index_For_Keep_Expr,
15 => Type_Index_For_Lambda_Expr,
16 => Type_Index_For_Lit,
17 => Type_Index_For_Logic_Assign,
18 => Type_Index_For_Logic_Expr,
19 => Type_Index_For_Logic_Propagate,
20 => Type_Index_For_Logic_Unify,
21 => Type_Index_For_Match_Expr,
22 => Type_Index_For_Not_Expr,
23 => Type_Index_For_Paren_Expr,
24 => Type_Index_For_Query,
25 => Type_Index_For_Raise_Expr,
26 => Type_Index_For_Subscript_Expr,
27 => Type_Index_For_Try_Expr,
28 => Type_Index_For_Un_Op
         ),
         Members           => (
              1 => Member_Index_For_Expr_P_Get_Type,
2 => Member_Index_For_Expr_P_Get_Generic_Type,
3 => Member_Index_For_Expr_P_Get_Expected_Type,
4 => Member_Index_For_Expr_P_Referenced_Decl
         ));
      
      Node_Name_For_Any_Of : aliased constant Text_Type :=
        "Any_Of";
         Node_Repr_Name_For_Any_Of : aliased constant Text_Type :=
           "AnyOf";
      Node_Desc_For_Any_Of : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Any_Of'Access,
         Repr_Name         => Node_Repr_Name_For_Any_Of'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Any_Of_F_Expr,
2 => Member_Index_For_Any_Of_F_Values
         ));
      
      Node_Name_For_Array_Literal : aliased constant Text_Type :=
        "Array_Literal";
         Node_Repr_Name_For_Array_Literal : aliased constant Text_Type :=
           "ArrayLiteral";
      Node_Desc_For_Array_Literal : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Array_Literal'Access,
         Repr_Name         => Node_Repr_Name_For_Array_Literal'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Array_Literal_F_Exprs,
2 => Member_Index_For_Array_Literal_F_Element_Type
         ));
      
      Node_Name_For_Base_Call_Expr : aliased constant Text_Type :=
        "Base_Call_Expr";
         Node_Repr_Name_For_Base_Call_Expr : aliased constant Text_Type :=
           "BaseCallExpr";
      Node_Desc_For_Base_Call_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Base_Call_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Base_Call_Expr'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 => Type_Index_For_Call_Expr,
2 => Type_Index_For_Logic_Call_Expr
         ),
         Members           => (
              1 => Member_Index_For_Base_Call_Expr_F_Name,
2 => Member_Index_For_Base_Call_Expr_F_Args
         ));
      
      Node_Name_For_Call_Expr : aliased constant Text_Type :=
        "Call_Expr";
         Node_Repr_Name_For_Call_Expr : aliased constant Text_Type :=
           "CallExpr";
      Node_Desc_For_Call_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Base_Call_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Call_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Call_Expr'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Logic_Call_Expr : aliased constant Text_Type :=
        "Logic_Call_Expr";
         Node_Repr_Name_For_Logic_Call_Expr : aliased constant Text_Type :=
           "LogicCallExpr";
      Node_Desc_For_Logic_Call_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Base_Call_Expr,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Logic_Call_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Logic_Call_Expr'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 => Type_Index_For_Logic_Predicate,
2 => Type_Index_For_Logic_Propagate_Call
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Logic_Predicate : aliased constant Text_Type :=
        "Logic_Predicate";
         Node_Repr_Name_For_Logic_Predicate : aliased constant Text_Type :=
           "LogicPredicate";
      Node_Desc_For_Logic_Predicate : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Logic_Call_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Logic_Predicate'Access,
         Repr_Name         => Node_Repr_Name_For_Logic_Predicate'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Logic_Propagate_Call : aliased constant Text_Type :=
        "Logic_Propagate_Call";
         Node_Repr_Name_For_Logic_Propagate_Call : aliased constant Text_Type :=
           "LogicPropagateCall";
      Node_Desc_For_Logic_Propagate_Call : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Logic_Call_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Logic_Propagate_Call'Access,
         Repr_Name         => Node_Repr_Name_For_Logic_Propagate_Call'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Bin_Op : aliased constant Text_Type :=
        "Bin_Op";
         Node_Repr_Name_For_Bin_Op : aliased constant Text_Type :=
           "BinOp";
      Node_Desc_For_Bin_Op : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Bin_Op'Access,
         Repr_Name         => Node_Repr_Name_For_Bin_Op'Access,
         Inherited_Members => 56,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Bin_Op_F_Left,
2 => Member_Index_For_Bin_Op_F_Op,
3 => Member_Index_For_Bin_Op_F_Right
         ));
      
      Node_Name_For_Block_Expr : aliased constant Text_Type :=
        "Block_Expr";
         Node_Repr_Name_For_Block_Expr : aliased constant Text_Type :=
           "BlockExpr";
      Node_Desc_For_Block_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Block_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Block_Expr'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Block_Expr_F_Clauses
         ));
      
      Node_Name_For_Cast_Expr : aliased constant Text_Type :=
        "Cast_Expr";
         Node_Repr_Name_For_Cast_Expr : aliased constant Text_Type :=
           "CastExpr";
      Node_Desc_For_Cast_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Cast_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Cast_Expr'Access,
         Inherited_Members => 57,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Cast_Expr_F_Expr,
2 => Member_Index_For_Cast_Expr_F_Null_Cond,
3 => Member_Index_For_Cast_Expr_F_Excludes_Null,
4 => Member_Index_For_Cast_Expr_F_Dest_Type
         ));
      
      Node_Name_For_Dot_Expr : aliased constant Text_Type :=
        "Dot_Expr";
         Node_Repr_Name_For_Dot_Expr : aliased constant Text_Type :=
           "DotExpr";
      Node_Desc_For_Dot_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Dot_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Dot_Expr'Access,
         Inherited_Members => 56,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Dot_Expr_F_Prefix,
2 => Member_Index_For_Dot_Expr_F_Null_Cond,
3 => Member_Index_For_Dot_Expr_F_Suffix
         ));
      
      Node_Name_For_Error_On_Null : aliased constant Text_Type :=
        "Error_On_Null";
         Node_Repr_Name_For_Error_On_Null : aliased constant Text_Type :=
           "ErrorOnNull";
      Node_Desc_For_Error_On_Null : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Error_On_Null'Access,
         Repr_Name         => Node_Repr_Name_For_Error_On_Null'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Error_On_Null_F_Expr
         ));
      
      Node_Name_For_Generic_Instantiation : aliased constant Text_Type :=
        "Generic_Instantiation";
         Node_Repr_Name_For_Generic_Instantiation : aliased constant Text_Type :=
           "GenericInstantiation";
      Node_Desc_For_Generic_Instantiation : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Generic_Instantiation'Access,
         Repr_Name         => Node_Repr_Name_For_Generic_Instantiation'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Generic_Instantiation_F_Name,
2 => Member_Index_For_Generic_Instantiation_F_Args
         ));
      
      Node_Name_For_Grammar_Expr : aliased constant Text_Type :=
        "Grammar_Expr";
         Node_Repr_Name_For_Grammar_Expr : aliased constant Text_Type :=
           "GrammarExpr";
      Node_Desc_For_Grammar_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 22,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Expr'Access,
         Inherited_Members => 53,
         Derivations       => (
             1 => Type_Index_For_Error_Grammar_Expr,
2 => Type_Index_For_Grammar_Cut,
3 => Type_Index_For_Grammar_Discard,
4 => Type_Index_For_Grammar_Dont_Skip,
5 => Type_Index_For_Grammar_List,
6 => Type_Index_For_Grammar_Null,
7 => Type_Index_For_Grammar_Opt,
8 => Type_Index_For_Grammar_Opt_Error,
9 => Type_Index_For_Grammar_Opt_Error_Group,
10 => Type_Index_For_Grammar_Opt_Group,
11 => Type_Index_For_Grammar_Or_Expr,
12 => Type_Index_For_Grammar_Pick,
13 => Type_Index_For_Grammar_Predicate,
14 => Type_Index_For_Grammar_Rule_Ref,
15 => Type_Index_For_Grammar_Skip,
16 => Type_Index_For_Grammar_Stop_Cut,
17 => Type_Index_For_Parse_Node_Expr,
18 => Type_Index_For_Token_Lit,
19 => Type_Index_For_Token_No_Case_Lit,
20 => Type_Index_For_Token_Pattern_Concat,
21 => Type_Index_For_Token_Pattern_Lit,
22 => Type_Index_For_Token_Ref
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Error_Grammar_Expr : aliased constant Text_Type :=
        "Error_Grammar_Expr";
         Node_Repr_Name_For_Error_Grammar_Expr : aliased constant Text_Type :=
           "ErrorGrammarExpr";
      Node_Desc_For_Error_Grammar_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => True,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Error_Grammar_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Error_Grammar_Expr'Access,
         Inherited_Members => 53,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Grammar_Cut : aliased constant Text_Type :=
        "Grammar_Cut";
         Node_Repr_Name_For_Grammar_Cut : aliased constant Text_Type :=
           "GrammarCut";
      Node_Desc_For_Grammar_Cut : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Cut'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Cut'Access,
         Inherited_Members => 53,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Grammar_Discard : aliased constant Text_Type :=
        "Grammar_Discard";
         Node_Repr_Name_For_Grammar_Discard : aliased constant Text_Type :=
           "GrammarDiscard";
      Node_Desc_For_Grammar_Discard : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Discard'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Discard'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Grammar_Discard_F_Expr
         ));
      
      Node_Name_For_Grammar_Dont_Skip : aliased constant Text_Type :=
        "Grammar_Dont_Skip";
         Node_Repr_Name_For_Grammar_Dont_Skip : aliased constant Text_Type :=
           "GrammarDontSkip";
      Node_Desc_For_Grammar_Dont_Skip : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Dont_Skip'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Dont_Skip'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Grammar_Dont_Skip_F_Expr,
2 => Member_Index_For_Grammar_Dont_Skip_F_Dont_Skip
         ));
      
      Node_Name_For_Grammar_List : aliased constant Text_Type :=
        "Grammar_List";
         Node_Repr_Name_For_Grammar_List : aliased constant Text_Type :=
           "GrammarList";
      Node_Desc_For_Grammar_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_List'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_List'Access,
         Inherited_Members => 57,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Grammar_List_F_List_Type,
2 => Member_Index_For_Grammar_List_F_Kind,
3 => Member_Index_For_Grammar_List_F_Expr,
4 => Member_Index_For_Grammar_List_F_Sep
         ));
      
      Node_Name_For_Grammar_Null : aliased constant Text_Type :=
        "Grammar_Null";
         Node_Repr_Name_For_Grammar_Null : aliased constant Text_Type :=
           "GrammarNull";
      Node_Desc_For_Grammar_Null : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Null'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Null'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Grammar_Null_F_Name
         ));
      
      Node_Name_For_Grammar_Opt : aliased constant Text_Type :=
        "Grammar_Opt";
         Node_Repr_Name_For_Grammar_Opt : aliased constant Text_Type :=
           "GrammarOpt";
      Node_Desc_For_Grammar_Opt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Opt'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Opt'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Grammar_Opt_F_Expr
         ));
      
      Node_Name_For_Grammar_Opt_Error : aliased constant Text_Type :=
        "Grammar_Opt_Error";
         Node_Repr_Name_For_Grammar_Opt_Error : aliased constant Text_Type :=
           "GrammarOptError";
      Node_Desc_For_Grammar_Opt_Error : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Opt_Error'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Opt_Error'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Grammar_Opt_Error_F_Expr
         ));
      
      Node_Name_For_Grammar_Opt_Error_Group : aliased constant Text_Type :=
        "Grammar_Opt_Error_Group";
         Node_Repr_Name_For_Grammar_Opt_Error_Group : aliased constant Text_Type :=
           "GrammarOptErrorGroup";
      Node_Desc_For_Grammar_Opt_Error_Group : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Opt_Error_Group'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Opt_Error_Group'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Grammar_Opt_Error_Group_F_Expr
         ));
      
      Node_Name_For_Grammar_Opt_Group : aliased constant Text_Type :=
        "Grammar_Opt_Group";
         Node_Repr_Name_For_Grammar_Opt_Group : aliased constant Text_Type :=
           "GrammarOptGroup";
      Node_Desc_For_Grammar_Opt_Group : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Opt_Group'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Opt_Group'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Grammar_Opt_Group_F_Expr
         ));
      
      Node_Name_For_Grammar_Or_Expr : aliased constant Text_Type :=
        "Grammar_Or_Expr";
         Node_Repr_Name_For_Grammar_Or_Expr : aliased constant Text_Type :=
           "GrammarOrExpr";
      Node_Desc_For_Grammar_Or_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Or_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Or_Expr'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Grammar_Or_Expr_F_Sub_Exprs
         ));
      
      Node_Name_For_Grammar_Pick : aliased constant Text_Type :=
        "Grammar_Pick";
         Node_Repr_Name_For_Grammar_Pick : aliased constant Text_Type :=
           "GrammarPick";
      Node_Desc_For_Grammar_Pick : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 1,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Pick'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Pick'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 => Type_Index_For_Grammar_Implicit_Pick
         ),
         Members           => (
              1 => Member_Index_For_Grammar_Pick_F_Exprs
         ));
      
      Node_Name_For_Grammar_Implicit_Pick : aliased constant Text_Type :=
        "Grammar_Implicit_Pick";
         Node_Repr_Name_For_Grammar_Implicit_Pick : aliased constant Text_Type :=
           "GrammarImplicitPick";
      Node_Desc_For_Grammar_Implicit_Pick : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Grammar_Pick,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Implicit_Pick'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Implicit_Pick'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Grammar_Predicate : aliased constant Text_Type :=
        "Grammar_Predicate";
         Node_Repr_Name_For_Grammar_Predicate : aliased constant Text_Type :=
           "GrammarPredicate";
      Node_Desc_For_Grammar_Predicate : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Predicate'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Predicate'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Grammar_Predicate_F_Expr,
2 => Member_Index_For_Grammar_Predicate_F_Prop_Ref
         ));
      
      Node_Name_For_Grammar_Rule_Ref : aliased constant Text_Type :=
        "Grammar_Rule_Ref";
         Node_Repr_Name_For_Grammar_Rule_Ref : aliased constant Text_Type :=
           "GrammarRuleRef";
      Node_Desc_For_Grammar_Rule_Ref : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Rule_Ref'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Rule_Ref'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Grammar_Rule_Ref_F_Node_Name
         ));
      
      Node_Name_For_Grammar_Skip : aliased constant Text_Type :=
        "Grammar_Skip";
         Node_Repr_Name_For_Grammar_Skip : aliased constant Text_Type :=
           "GrammarSkip";
      Node_Desc_For_Grammar_Skip : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Skip'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Skip'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Grammar_Skip_F_Name
         ));
      
      Node_Name_For_Grammar_Stop_Cut : aliased constant Text_Type :=
        "Grammar_Stop_Cut";
         Node_Repr_Name_For_Grammar_Stop_Cut : aliased constant Text_Type :=
           "GrammarStopCut";
      Node_Desc_For_Grammar_Stop_Cut : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_Stop_Cut'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Stop_Cut'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Grammar_Stop_Cut_F_Expr
         ));
      
      Node_Name_For_Parse_Node_Expr : aliased constant Text_Type :=
        "Parse_Node_Expr";
         Node_Repr_Name_For_Parse_Node_Expr : aliased constant Text_Type :=
           "ParseNodeExpr";
      Node_Desc_For_Parse_Node_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Parse_Node_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Parse_Node_Expr'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Parse_Node_Expr_F_Node_Name,
2 => Member_Index_For_Parse_Node_Expr_F_Sub_Exprs
         ));
      
      Node_Name_For_Token_Lit : aliased constant Text_Type :=
        "Token_Lit";
         Node_Repr_Name_For_Token_Lit : aliased constant Text_Type :=
           "TokenLit";
      Node_Desc_For_Token_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_String,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Token_Lit'Access,
         Repr_Name         => Node_Repr_Name_For_Token_Lit'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Token_Lit_P_Denoted_Value
         ));
      
      Node_Name_For_Token_No_Case_Lit : aliased constant Text_Type :=
        "Token_No_Case_Lit";
         Node_Repr_Name_For_Token_No_Case_Lit : aliased constant Text_Type :=
           "TokenNoCaseLit";
      Node_Desc_For_Token_No_Case_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Token_No_Case_Lit'Access,
         Repr_Name         => Node_Repr_Name_For_Token_No_Case_Lit'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Token_No_Case_Lit_F_Lit
         ));
      
      Node_Name_For_Token_Pattern_Concat : aliased constant Text_Type :=
        "Token_Pattern_Concat";
         Node_Repr_Name_For_Token_Pattern_Concat : aliased constant Text_Type :=
           "TokenPatternConcat";
      Node_Desc_For_Token_Pattern_Concat : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Token_Pattern_Concat'Access,
         Repr_Name         => Node_Repr_Name_For_Token_Pattern_Concat'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Token_Pattern_Concat_F_Left,
2 => Member_Index_For_Token_Pattern_Concat_F_Right
         ));
      
      Node_Name_For_Token_Pattern_Lit : aliased constant Text_Type :=
        "Token_Pattern_Lit";
         Node_Repr_Name_For_Token_Pattern_Lit : aliased constant Text_Type :=
           "TokenPatternLit";
      Node_Desc_For_Token_Pattern_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_P_String,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Token_Pattern_Lit'Access,
         Repr_Name         => Node_Repr_Name_For_Token_Pattern_Lit'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Token_Pattern_Lit_P_Denoted_Value
         ));
      
      Node_Name_For_Token_Ref : aliased constant Text_Type :=
        "Token_Ref";
         Node_Repr_Name_For_Token_Ref : aliased constant Text_Type :=
           "TokenRef";
      Node_Desc_For_Token_Ref : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Grammar_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Token_Ref'Access,
         Repr_Name         => Node_Repr_Name_For_Token_Ref'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Token_Ref_F_Token_Name,
2 => Member_Index_For_Token_Ref_F_Expr
         ));
      
      Node_Name_For_Id : aliased constant Text_Type :=
        "Id";
         Node_Repr_Name_For_Id : aliased constant Text_Type :=
           "Id";
      Node_Desc_For_Id : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 4,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_Identifier,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Id'Access,
         Repr_Name         => Node_Repr_Name_For_Id'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 => Type_Index_For_Def_Id,
2 => Type_Index_For_Imported_Id,
3 => Type_Index_For_Module_Id,
4 => Type_Index_For_Ref_Id
         ),
         Members           => (
              1 => Member_Index_For_Id_P_Custom_Image
         ));
      
      Node_Name_For_Def_Id : aliased constant Text_Type :=
        "Def_Id";
         Node_Repr_Name_For_Def_Id : aliased constant Text_Type :=
           "DefId";
      Node_Desc_For_Def_Id : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 6,
         Base_Type         => Type_Index_For_Id,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_Identifier,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Def_Id'Access,
         Repr_Name         => Node_Repr_Name_For_Def_Id'Access,
         Inherited_Members => 60,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Def_Id_P_Name,
2 => Member_Index_For_Def_Id_P_Get_Implementatinons,
3 => Member_Index_For_Def_Id_P_Decl_Detail,
4 => Member_Index_For_Def_Id_P_Completion_Item_Kind,
5 => Member_Index_For_Def_Id_P_Doc,
6 => Member_Index_For_Def_Id_P_Find_All_References
         ));
      
      Node_Name_For_Imported_Id : aliased constant Text_Type :=
        "Imported_Id";
         Node_Repr_Name_For_Imported_Id : aliased constant Text_Type :=
           "ImportedId";
      Node_Desc_For_Imported_Id : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Id,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_Identifier,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Imported_Id'Access,
         Repr_Name         => Node_Repr_Name_For_Imported_Id'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Module_Id : aliased constant Text_Type :=
        "Module_Id";
         Node_Repr_Name_For_Module_Id : aliased constant Text_Type :=
           "ModuleId";
      Node_Desc_For_Module_Id : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Id,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_Identifier,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Module_Id'Access,
         Repr_Name         => Node_Repr_Name_For_Module_Id'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Ref_Id : aliased constant Text_Type :=
        "Ref_Id";
         Node_Repr_Name_For_Ref_Id : aliased constant Text_Type :=
           "RefId";
      Node_Desc_For_Ref_Id : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Id,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_Identifier,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Ref_Id'Access,
         Repr_Name         => Node_Repr_Name_For_Ref_Id'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Ref_Id_P_Referenced_Defining_Name
         ));
      
      Node_Name_For_If_Expr : aliased constant Text_Type :=
        "If_Expr";
         Node_Repr_Name_For_If_Expr : aliased constant Text_Type :=
           "IfExpr";
      Node_Desc_For_If_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_If_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_If_Expr'Access,
         Inherited_Members => 57,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_If_Expr_F_Cond_Expr,
2 => Member_Index_For_If_Expr_F_Then_Expr,
3 => Member_Index_For_If_Expr_F_Alternatives,
4 => Member_Index_For_If_Expr_F_Else_Expr
         ));
      
      Node_Name_For_Isa : aliased constant Text_Type :=
        "Isa";
         Node_Repr_Name_For_Isa : aliased constant Text_Type :=
           "Isa";
      Node_Desc_For_Isa : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Isa'Access,
         Repr_Name         => Node_Repr_Name_For_Isa'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Isa_F_Expr,
2 => Member_Index_For_Isa_F_Pattern
         ));
      
      Node_Name_For_Keep_Expr : aliased constant Text_Type :=
        "Keep_Expr";
         Node_Repr_Name_For_Keep_Expr : aliased constant Text_Type :=
           "KeepExpr";
      Node_Desc_For_Keep_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Keep_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Keep_Expr'Access,
         Inherited_Members => 56,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Keep_Expr_F_Expr,
2 => Member_Index_For_Keep_Expr_F_Null_Cond,
3 => Member_Index_For_Keep_Expr_F_Keep_Type
         ));
      
      Node_Name_For_Lambda_Expr : aliased constant Text_Type :=
        "Lambda_Expr";
         Node_Repr_Name_For_Lambda_Expr : aliased constant Text_Type :=
           "LambdaExpr";
      Node_Desc_For_Lambda_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Lambda_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Lambda_Expr'Access,
         Inherited_Members => 56,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Lambda_Expr_F_Params,
2 => Member_Index_For_Lambda_Expr_F_Return_Type,
3 => Member_Index_For_Lambda_Expr_F_Body
         ));
      
      Node_Name_For_Lit : aliased constant Text_Type :=
        "Lit";
         Node_Repr_Name_For_Lit : aliased constant Text_Type :=
           "Lit";
      Node_Desc_For_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 5,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Lit'Access,
         Repr_Name         => Node_Repr_Name_For_Lit'Access,
         Inherited_Members => 53,
         Derivations       => (
             1 => Type_Index_For_Big_Num_Lit,
2 => Type_Index_For_Char_Lit,
3 => Type_Index_For_Null_Lit,
4 => Type_Index_For_Num_Lit,
5 => Type_Index_For_String_Lit
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Big_Num_Lit : aliased constant Text_Type :=
        "Big_Num_Lit";
         Node_Repr_Name_For_Big_Num_Lit : aliased constant Text_Type :=
           "BigNumLit";
      Node_Desc_For_Big_Num_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lit,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_Big_Number,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Big_Num_Lit'Access,
         Repr_Name         => Node_Repr_Name_For_Big_Num_Lit'Access,
         Inherited_Members => 53,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Char_Lit : aliased constant Text_Type :=
        "Char_Lit";
         Node_Repr_Name_For_Char_Lit : aliased constant Text_Type :=
           "CharLit";
      Node_Desc_For_Char_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Lit,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_Char,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Char_Lit'Access,
         Repr_Name         => Node_Repr_Name_For_Char_Lit'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Char_Lit_P_Denoted_Value
         ));
      
      Node_Name_For_Null_Lit : aliased constant Text_Type :=
        "Null_Lit";
         Node_Repr_Name_For_Null_Lit : aliased constant Text_Type :=
           "NullLit";
      Node_Desc_For_Null_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Lit,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Null_Lit'Access,
         Repr_Name         => Node_Repr_Name_For_Null_Lit'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Null_Lit_F_Dest_Type
         ));
      
      Node_Name_For_Num_Lit : aliased constant Text_Type :=
        "Num_Lit";
         Node_Repr_Name_For_Num_Lit : aliased constant Text_Type :=
           "NumLit";
      Node_Desc_For_Num_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lit,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_Number,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Num_Lit'Access,
         Repr_Name         => Node_Repr_Name_For_Num_Lit'Access,
         Inherited_Members => 53,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_String_Lit : aliased constant Text_Type :=
        "String_Lit";
         Node_Repr_Name_For_String_Lit : aliased constant Text_Type :=
           "StringLit";
      Node_Desc_For_String_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 3,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Lit,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_String_Lit'Access,
         Repr_Name         => Node_Repr_Name_For_String_Lit'Access,
         Inherited_Members => 57,
         Derivations       => (
             1 => Type_Index_For_Block_String_Lit,
2 => Type_Index_For_Module_Doc_String_Lit,
3 => Type_Index_For_Single_Line_String_Lit
         ),
         Members           => (
              1 => Member_Index_For_String_Lit_P_Denoted_Value,
2 => Member_Index_For_String_Lit_P_Is_Prefixed_String,
3 => Member_Index_For_String_Lit_P_Prefix,
4 => Member_Index_For_String_Lit_P_Is_Regexp_Literal
         ));
      
      Node_Name_For_Block_String_Lit : aliased constant Text_Type :=
        "Block_String_Lit";
         Node_Repr_Name_For_Block_String_Lit : aliased constant Text_Type :=
           "BlockStringLit";
      Node_Desc_For_Block_String_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_String_Lit,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Block_String_Lit'Access,
         Repr_Name         => Node_Repr_Name_For_Block_String_Lit'Access,
         Inherited_Members => 58,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Block_String_Lit_F_Lines
         ));
      
      Node_Name_For_Module_Doc_String_Lit : aliased constant Text_Type :=
        "Module_Doc_String_Lit";
         Node_Repr_Name_For_Module_Doc_String_Lit : aliased constant Text_Type :=
           "ModuleDocStringLit";
      Node_Desc_For_Module_Doc_String_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_String_Lit,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Module_Doc_String_Lit'Access,
         Repr_Name         => Node_Repr_Name_For_Module_Doc_String_Lit'Access,
         Inherited_Members => 58,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Module_Doc_String_Lit_F_Lines
         ));
      
      Node_Name_For_Single_Line_String_Lit : aliased constant Text_Type :=
        "Single_Line_String_Lit";
         Node_Repr_Name_For_Single_Line_String_Lit : aliased constant Text_Type :=
           "SingleLineStringLit";
      Node_Desc_For_Single_Line_String_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 1,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_String_Lit,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_String,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Single_Line_String_Lit'Access,
         Repr_Name         => Node_Repr_Name_For_Single_Line_String_Lit'Access,
         Inherited_Members => 57,
         Derivations       => (
             1 => Type_Index_For_Pattern_Single_Line_String_Lit
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Pattern_Single_Line_String_Lit : aliased constant Text_Type :=
        "Pattern_Single_Line_String_Lit";
         Node_Repr_Name_For_Pattern_Single_Line_String_Lit : aliased constant Text_Type :=
           "PatternSingleLineStringLit";
      Node_Desc_For_Pattern_Single_Line_String_Lit : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Single_Line_String_Lit,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_P_String,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Pattern_Single_Line_String_Lit'Access,
         Repr_Name         => Node_Repr_Name_For_Pattern_Single_Line_String_Lit'Access,
         Inherited_Members => 57,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Logic_Assign : aliased constant Text_Type :=
        "Logic_Assign";
         Node_Repr_Name_For_Logic_Assign : aliased constant Text_Type :=
           "LogicAssign";
      Node_Desc_For_Logic_Assign : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Logic_Assign'Access,
         Repr_Name         => Node_Repr_Name_For_Logic_Assign'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Logic_Assign_F_Dest_Var,
2 => Member_Index_For_Logic_Assign_F_Value
         ));
      
      Node_Name_For_Logic_Expr : aliased constant Text_Type :=
        "Logic_Expr";
         Node_Repr_Name_For_Logic_Expr : aliased constant Text_Type :=
           "LogicExpr";
      Node_Desc_For_Logic_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Logic_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Logic_Expr'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Logic_Expr_F_Expr
         ));
      
      Node_Name_For_Logic_Propagate : aliased constant Text_Type :=
        "Logic_Propagate";
         Node_Repr_Name_For_Logic_Propagate : aliased constant Text_Type :=
           "LogicPropagate";
      Node_Desc_For_Logic_Propagate : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Logic_Propagate'Access,
         Repr_Name         => Node_Repr_Name_For_Logic_Propagate'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Logic_Propagate_F_Dest_Var,
2 => Member_Index_For_Logic_Propagate_F_Call
         ));
      
      Node_Name_For_Logic_Unify : aliased constant Text_Type :=
        "Logic_Unify";
         Node_Repr_Name_For_Logic_Unify : aliased constant Text_Type :=
           "LogicUnify";
      Node_Desc_For_Logic_Unify : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Logic_Unify'Access,
         Repr_Name         => Node_Repr_Name_For_Logic_Unify'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Logic_Unify_F_Lhs,
2 => Member_Index_For_Logic_Unify_F_Rhs
         ));
      
      Node_Name_For_Match_Expr : aliased constant Text_Type :=
        "Match_Expr";
         Node_Repr_Name_For_Match_Expr : aliased constant Text_Type :=
           "MatchExpr";
      Node_Desc_For_Match_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Match_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Match_Expr'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Match_Expr_F_Match_Expr,
2 => Member_Index_For_Match_Expr_F_Branches
         ));
      
      Node_Name_For_Not_Expr : aliased constant Text_Type :=
        "Not_Expr";
         Node_Repr_Name_For_Not_Expr : aliased constant Text_Type :=
           "NotExpr";
      Node_Desc_For_Not_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Not_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Not_Expr'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Not_Expr_F_Expr
         ));
      
      Node_Name_For_Paren_Expr : aliased constant Text_Type :=
        "Paren_Expr";
         Node_Repr_Name_For_Paren_Expr : aliased constant Text_Type :=
           "ParenExpr";
      Node_Desc_For_Paren_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Paren_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Paren_Expr'Access,
         Inherited_Members => 54,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Paren_Expr_F_Expr
         ));
      
      Node_Name_For_Query : aliased constant Text_Type :=
        "Query";
         Node_Repr_Name_For_Query : aliased constant Text_Type :=
           "Query";
      Node_Desc_For_Query : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Query'Access,
         Repr_Name         => Node_Repr_Name_For_Query'Access,
         Inherited_Members => 57,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Query_F_Source,
2 => Member_Index_For_Query_F_Pattern,
3 => Member_Index_For_Query_F_Mapping,
4 => Member_Index_For_Query_F_Guard
         ));
      
      Node_Name_For_Raise_Expr : aliased constant Text_Type :=
        "Raise_Expr";
         Node_Repr_Name_For_Raise_Expr : aliased constant Text_Type :=
           "RaiseExpr";
      Node_Desc_For_Raise_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Raise_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Raise_Expr'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Raise_Expr_F_Dest_Type,
2 => Member_Index_For_Raise_Expr_F_Except_Expr
         ));
      
      Node_Name_For_Subscript_Expr : aliased constant Text_Type :=
        "Subscript_Expr";
         Node_Repr_Name_For_Subscript_Expr : aliased constant Text_Type :=
           "SubscriptExpr";
      Node_Desc_For_Subscript_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 3,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Subscript_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Subscript_Expr'Access,
         Inherited_Members => 56,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Subscript_Expr_F_Prefix,
2 => Member_Index_For_Subscript_Expr_F_Null_Cond,
3 => Member_Index_For_Subscript_Expr_F_Index
         ));
      
      Node_Name_For_Try_Expr : aliased constant Text_Type :=
        "Try_Expr";
         Node_Repr_Name_For_Try_Expr : aliased constant Text_Type :=
           "TryExpr";
      Node_Desc_For_Try_Expr : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Try_Expr'Access,
         Repr_Name         => Node_Repr_Name_For_Try_Expr'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Try_Expr_F_Try_Expr,
2 => Member_Index_For_Try_Expr_F_Or_Expr
         ));
      
      Node_Name_For_Un_Op : aliased constant Text_Type :=
        "Un_Op";
         Node_Repr_Name_For_Un_Op : aliased constant Text_Type :=
           "UnOp";
      Node_Desc_For_Un_Op : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Expr,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Un_Op'Access,
         Repr_Name         => Node_Repr_Name_For_Un_Op'Access,
         Inherited_Members => 55,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Un_Op_F_Op,
2 => Member_Index_For_Un_Op_F_Expr
         ));
      
      Node_Name_For_Full_Decl : aliased constant Text_Type :=
        "Full_Decl";
         Node_Repr_Name_For_Full_Decl : aliased constant Text_Type :=
           "FullDecl";
      Node_Desc_For_Full_Decl : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Full_Decl'Access,
         Repr_Name         => Node_Repr_Name_For_Full_Decl'Access,
         Inherited_Members => 53,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Full_Decl_F_Doc,
2 => Member_Index_For_Full_Decl_F_Decl_Annotations,
3 => Member_Index_For_Full_Decl_F_Decl,
4 => Member_Index_For_Full_Decl_P_Has_Annotation
         ));
      
      Node_Name_For_Grammar_List_Sep : aliased constant Text_Type :=
        "Grammar_List_Sep";
         Node_Repr_Name_For_Grammar_List_Sep : aliased constant Text_Type :=
           "GrammarListSep";
      Node_Desc_For_Grammar_List_Sep : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Grammar_List_Sep'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_List_Sep'Access,
         Inherited_Members => 51,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Grammar_List_Sep_F_Token,
2 => Member_Index_For_Grammar_List_Sep_F_Extra
         ));
      
      Node_Name_For_Imported_Name : aliased constant Text_Type :=
        "Imported_Name";
         Node_Repr_Name_For_Imported_Name : aliased constant Text_Type :=
           "ImportedName";
      Node_Desc_For_Imported_Name : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Imported_Name'Access,
         Repr_Name         => Node_Repr_Name_For_Imported_Name'Access,
         Inherited_Members => 51,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Imported_Name_F_Original_Name,
2 => Member_Index_For_Imported_Name_F_Renaming
         ));
      
      Node_Name_For_Lexer_Case_Rule : aliased constant Text_Type :=
        "Lexer_Case_Rule";
         Node_Repr_Name_For_Lexer_Case_Rule : aliased constant Text_Type :=
           "LexerCaseRule";
      Node_Desc_For_Lexer_Case_Rule : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Lexer_Case_Rule'Access,
         Repr_Name         => Node_Repr_Name_For_Lexer_Case_Rule'Access,
         Inherited_Members => 51,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Lexer_Case_Rule_F_Expr,
2 => Member_Index_For_Lexer_Case_Rule_F_Alts
         ));
      
      Node_Name_For_Lexer_Case_Rule_Send : aliased constant Text_Type :=
        "Lexer_Case_Rule_Send";
         Node_Repr_Name_For_Lexer_Case_Rule_Send : aliased constant Text_Type :=
           "LexerCaseRuleSend";
      Node_Desc_For_Lexer_Case_Rule_Send : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Lexer_Case_Rule_Send'Access,
         Repr_Name         => Node_Repr_Name_For_Lexer_Case_Rule_Send'Access,
         Inherited_Members => 51,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Lexer_Case_Rule_Send_F_Sent,
2 => Member_Index_For_Lexer_Case_Rule_Send_F_Match_Size
         ));
      
      Node_Name_For_List_Kind : aliased constant Text_Type :=
        "List_Kind";
         Node_Repr_Name_For_List_Kind : aliased constant Text_Type :=
           "ListKind";
      Node_Desc_For_List_Kind : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_List_Kind'Access,
         Repr_Name         => Node_Repr_Name_For_List_Kind'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 => Type_Index_For_List_Kind_One,
2 => Type_Index_For_List_Kind_Zero
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_List_Kind_One : aliased constant Text_Type :=
        "List_Kind_One";
         Node_Repr_Name_For_List_Kind_One : aliased constant Text_Type :=
           "ListKindOne";
      Node_Desc_For_List_Kind_One : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_List_Kind,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_List_Kind_One'Access,
         Repr_Name         => Node_Repr_Name_For_List_Kind_One'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_List_Kind_Zero : aliased constant Text_Type :=
        "List_Kind_Zero";
         Node_Repr_Name_For_List_Kind_Zero : aliased constant Text_Type :=
           "ListKindZero";
      Node_Desc_For_List_Kind_Zero : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_List_Kind,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_List_Kind_Zero'Access,
         Repr_Name         => Node_Repr_Name_For_List_Kind_Zero'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Lkt_Node_Base_List : aliased constant Text_Type :=
        "Lkt_Node_Base_List";
         Node_Repr_Name_For_Lkt_Node_Base_List : aliased constant Text_Type :=
           "LktNodeBaseList";
      Node_Desc_For_Lkt_Node_Base_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 24,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Lkt_Node_Base_List'Access,
         Repr_Name         => Node_Repr_Name_For_Lkt_Node_Base_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 => Type_Index_For_Argument_List,
2 => Type_Index_For_Base_Import_List,
3 => Type_Index_For_Base_Lexer_Case_Rule_Alt_List,
4 => Type_Index_For_Base_Match_Branch_List,
5 => Type_Index_For_Block_String_Line_List,
6 => Type_Index_For_Call_Expr_List,
7 => Type_Index_For_Decl_Annotation_List,
8 => Type_Index_For_Elsif_Branch_List,
9 => Type_Index_For_Enum_Class_Alt_Decl_List,
10 => Type_Index_For_Enum_Class_Case_List,
11 => Type_Index_For_Enum_Lit_Decl_List,
12 => Type_Index_For_Expr_List,
13 => Type_Index_For_Full_Decl_List,
14 => Type_Index_For_Fun_Param_Decl_List,
15 => Type_Index_For_Grammar_Expr_List,
16 => Type_Index_For_Grammar_Expr_List_List,
17 => Type_Index_For_Imported_Name_List,
18 => Type_Index_For_Lambda_Param_Decl_List,
19 => Type_Index_For_Lkt_Node_List,
20 => Type_Index_For_Module_Doc_String_Line_List,
21 => Type_Index_For_Pattern_Detail_List,
22 => Type_Index_For_Pattern_List,
23 => Type_Index_For_Ref_Id_List,
24 => Type_Index_For_Type_Ref_List
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Argument_List : aliased constant Text_Type :=
        "Argument_List";
         Node_Repr_Name_For_Argument_List : aliased constant Text_Type :=
           "ArgumentList";
      Node_Desc_For_Argument_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Argument,
         Name              => Node_Name_For_Argument_List'Access,
         Repr_Name         => Node_Repr_Name_For_Argument_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Base_Import_List : aliased constant Text_Type :=
        "Base_Import_List";
         Node_Repr_Name_For_Base_Import_List : aliased constant Text_Type :=
           "BaseImportList";
      Node_Desc_For_Base_Import_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Base_Import,
         Name              => Node_Name_For_Base_Import_List'Access,
         Repr_Name         => Node_Repr_Name_For_Base_Import_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Base_Lexer_Case_Rule_Alt_List : aliased constant Text_Type :=
        "Base_Lexer_Case_Rule_Alt_List";
         Node_Repr_Name_For_Base_Lexer_Case_Rule_Alt_List : aliased constant Text_Type :=
           "BaseLexerCaseRuleAltList";
      Node_Desc_For_Base_Lexer_Case_Rule_Alt_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Base_Lexer_Case_Rule_Alt,
         Name              => Node_Name_For_Base_Lexer_Case_Rule_Alt_List'Access,
         Repr_Name         => Node_Repr_Name_For_Base_Lexer_Case_Rule_Alt_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Base_Match_Branch_List : aliased constant Text_Type :=
        "Base_Match_Branch_List";
         Node_Repr_Name_For_Base_Match_Branch_List : aliased constant Text_Type :=
           "BaseMatchBranchList";
      Node_Desc_For_Base_Match_Branch_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Base_Match_Branch,
         Name              => Node_Name_For_Base_Match_Branch_List'Access,
         Repr_Name         => Node_Repr_Name_For_Base_Match_Branch_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Block_String_Line_List : aliased constant Text_Type :=
        "Block_String_Line_List";
         Node_Repr_Name_For_Block_String_Line_List : aliased constant Text_Type :=
           "BlockStringLineList";
      Node_Desc_For_Block_String_Line_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Block_String_Line,
         Name              => Node_Name_For_Block_String_Line_List'Access,
         Repr_Name         => Node_Repr_Name_For_Block_String_Line_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Call_Expr_List : aliased constant Text_Type :=
        "Call_Expr_List";
         Node_Repr_Name_For_Call_Expr_List : aliased constant Text_Type :=
           "CallExprList";
      Node_Desc_For_Call_Expr_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Call_Expr,
         Name              => Node_Name_For_Call_Expr_List'Access,
         Repr_Name         => Node_Repr_Name_For_Call_Expr_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Decl_Annotation_List : aliased constant Text_Type :=
        "Decl_Annotation_List";
         Node_Repr_Name_For_Decl_Annotation_List : aliased constant Text_Type :=
           "DeclAnnotationList";
      Node_Desc_For_Decl_Annotation_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Decl_Annotation,
         Name              => Node_Name_For_Decl_Annotation_List'Access,
         Repr_Name         => Node_Repr_Name_For_Decl_Annotation_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Elsif_Branch_List : aliased constant Text_Type :=
        "Elsif_Branch_List";
         Node_Repr_Name_For_Elsif_Branch_List : aliased constant Text_Type :=
           "ElsifBranchList";
      Node_Desc_For_Elsif_Branch_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Elsif_Branch,
         Name              => Node_Name_For_Elsif_Branch_List'Access,
         Repr_Name         => Node_Repr_Name_For_Elsif_Branch_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Enum_Class_Alt_Decl_List : aliased constant Text_Type :=
        "Enum_Class_Alt_Decl_List";
         Node_Repr_Name_For_Enum_Class_Alt_Decl_List : aliased constant Text_Type :=
           "EnumClassAltDeclList";
      Node_Desc_For_Enum_Class_Alt_Decl_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Enum_Class_Alt_Decl,
         Name              => Node_Name_For_Enum_Class_Alt_Decl_List'Access,
         Repr_Name         => Node_Repr_Name_For_Enum_Class_Alt_Decl_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Enum_Class_Case_List : aliased constant Text_Type :=
        "Enum_Class_Case_List";
         Node_Repr_Name_For_Enum_Class_Case_List : aliased constant Text_Type :=
           "EnumClassCaseList";
      Node_Desc_For_Enum_Class_Case_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Enum_Class_Case,
         Name              => Node_Name_For_Enum_Class_Case_List'Access,
         Repr_Name         => Node_Repr_Name_For_Enum_Class_Case_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Enum_Lit_Decl_List : aliased constant Text_Type :=
        "Enum_Lit_Decl_List";
         Node_Repr_Name_For_Enum_Lit_Decl_List : aliased constant Text_Type :=
           "EnumLitDeclList";
      Node_Desc_For_Enum_Lit_Decl_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Enum_Lit_Decl,
         Name              => Node_Name_For_Enum_Lit_Decl_List'Access,
         Repr_Name         => Node_Repr_Name_For_Enum_Lit_Decl_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Expr_List : aliased constant Text_Type :=
        "Expr_List";
         Node_Repr_Name_For_Expr_List : aliased constant Text_Type :=
           "ExprList";
      Node_Desc_For_Expr_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 1,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Expr,
         Name              => Node_Name_For_Expr_List'Access,
         Repr_Name         => Node_Repr_Name_For_Expr_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 => Type_Index_For_Any_Of_List
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Any_Of_List : aliased constant Text_Type :=
        "Any_Of_List";
         Node_Repr_Name_For_Any_Of_List : aliased constant Text_Type :=
           "AnyOfList";
      Node_Desc_For_Any_Of_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Expr_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Expr,
         Name              => Node_Name_For_Any_Of_List'Access,
         Repr_Name         => Node_Repr_Name_For_Any_Of_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Full_Decl_List : aliased constant Text_Type :=
        "Full_Decl_List";
         Node_Repr_Name_For_Full_Decl_List : aliased constant Text_Type :=
           "FullDeclList";
      Node_Desc_For_Full_Decl_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Full_Decl,
         Name              => Node_Name_For_Full_Decl_List'Access,
         Repr_Name         => Node_Repr_Name_For_Full_Decl_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 => Type_Index_For_Decl_Block,
2 => Type_Index_For_Generic_Param_Decl_List
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Decl_Block : aliased constant Text_Type :=
        "Decl_Block";
         Node_Repr_Name_For_Decl_Block : aliased constant Text_Type :=
           "DeclBlock";
      Node_Desc_For_Decl_Block : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Full_Decl_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Full_Decl,
         Name              => Node_Name_For_Decl_Block'Access,
         Repr_Name         => Node_Repr_Name_For_Decl_Block'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Generic_Param_Decl_List : aliased constant Text_Type :=
        "Generic_Param_Decl_List";
         Node_Repr_Name_For_Generic_Param_Decl_List : aliased constant Text_Type :=
           "GenericParamDeclList";
      Node_Desc_For_Generic_Param_Decl_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Full_Decl_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Full_Decl,
         Name              => Node_Name_For_Generic_Param_Decl_List'Access,
         Repr_Name         => Node_Repr_Name_For_Generic_Param_Decl_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Fun_Param_Decl_List : aliased constant Text_Type :=
        "Fun_Param_Decl_List";
         Node_Repr_Name_For_Fun_Param_Decl_List : aliased constant Text_Type :=
           "FunParamDeclList";
      Node_Desc_For_Fun_Param_Decl_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Fun_Param_Decl,
         Name              => Node_Name_For_Fun_Param_Decl_List'Access,
         Repr_Name         => Node_Repr_Name_For_Fun_Param_Decl_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Grammar_Expr_List : aliased constant Text_Type :=
        "Grammar_Expr_List";
         Node_Repr_Name_For_Grammar_Expr_List : aliased constant Text_Type :=
           "GrammarExprList";
      Node_Desc_For_Grammar_Expr_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Grammar_Expr,
         Name              => Node_Name_For_Grammar_Expr_List'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Expr_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Grammar_Expr_List_List : aliased constant Text_Type :=
        "Grammar_Expr_List_List";
         Node_Repr_Name_For_Grammar_Expr_List_List : aliased constant Text_Type :=
           "GrammarExprListList";
      Node_Desc_For_Grammar_Expr_List_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Grammar_Expr_List,
         Name              => Node_Name_For_Grammar_Expr_List_List'Access,
         Repr_Name         => Node_Repr_Name_For_Grammar_Expr_List_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Imported_Name_List : aliased constant Text_Type :=
        "Imported_Name_List";
         Node_Repr_Name_For_Imported_Name_List : aliased constant Text_Type :=
           "ImportedNameList";
      Node_Desc_For_Imported_Name_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Imported_Name,
         Name              => Node_Name_For_Imported_Name_List'Access,
         Repr_Name         => Node_Repr_Name_For_Imported_Name_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Lambda_Param_Decl_List : aliased constant Text_Type :=
        "Lambda_Param_Decl_List";
         Node_Repr_Name_For_Lambda_Param_Decl_List : aliased constant Text_Type :=
           "LambdaParamDeclList";
      Node_Desc_For_Lambda_Param_Decl_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Lambda_Param_Decl,
         Name              => Node_Name_For_Lambda_Param_Decl_List'Access,
         Repr_Name         => Node_Repr_Name_For_Lambda_Param_Decl_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Lkt_Node_List : aliased constant Text_Type :=
        "Lkt_Node_List";
         Node_Repr_Name_For_Lkt_Node_List : aliased constant Text_Type :=
           "LktNodeList";
      Node_Desc_For_Lkt_Node_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Lkt_Node,
         Name              => Node_Name_For_Lkt_Node_List'Access,
         Repr_Name         => Node_Repr_Name_For_Lkt_Node_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Module_Doc_String_Line_List : aliased constant Text_Type :=
        "Module_Doc_String_Line_List";
         Node_Repr_Name_For_Module_Doc_String_Line_List : aliased constant Text_Type :=
           "ModuleDocStringLineList";
      Node_Desc_For_Module_Doc_String_Line_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Module_Doc_String_Line,
         Name              => Node_Name_For_Module_Doc_String_Line_List'Access,
         Repr_Name         => Node_Repr_Name_For_Module_Doc_String_Line_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Pattern_Detail_List : aliased constant Text_Type :=
        "Pattern_Detail_List";
         Node_Repr_Name_For_Pattern_Detail_List : aliased constant Text_Type :=
           "PatternDetailList";
      Node_Desc_For_Pattern_Detail_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Pattern_Detail,
         Name              => Node_Name_For_Pattern_Detail_List'Access,
         Repr_Name         => Node_Repr_Name_For_Pattern_Detail_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Pattern_List : aliased constant Text_Type :=
        "Pattern_List";
         Node_Repr_Name_For_Pattern_List : aliased constant Text_Type :=
           "PatternList";
      Node_Desc_For_Pattern_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Pattern,
         Name              => Node_Name_For_Pattern_List'Access,
         Repr_Name         => Node_Repr_Name_For_Pattern_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Ref_Id_List : aliased constant Text_Type :=
        "Ref_Id_List";
         Node_Repr_Name_For_Ref_Id_List : aliased constant Text_Type :=
           "RefIdList";
      Node_Desc_For_Ref_Id_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Ref_Id,
         Name              => Node_Name_For_Ref_Id_List'Access,
         Repr_Name         => Node_Repr_Name_For_Ref_Id_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Type_Ref_List : aliased constant Text_Type :=
        "Type_Ref_List";
         Node_Repr_Name_For_Type_Ref_List : aliased constant Text_Type :=
           "TypeRefList";
      Node_Desc_For_Type_Ref_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 1,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node_Base_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Type_Ref,
         Name              => Node_Name_For_Type_Ref_List'Access,
         Repr_Name         => Node_Repr_Name_For_Type_Ref_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 => Type_Index_For_Synthetic_Type_Ref_List
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Synthetic_Type_Ref_List : aliased constant Text_Type :=
        "Synthetic_Type_Ref_List";
         Node_Repr_Name_For_Synthetic_Type_Ref_List : aliased constant Text_Type :=
           "SyntheticTypeRefList";
      Node_Desc_For_Synthetic_Type_Ref_List : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Type_Ref_List,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => True,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => Type_Index_For_Type_Ref,
         Name              => Node_Name_For_Synthetic_Type_Ref_List'Access,
         Repr_Name         => Node_Repr_Name_For_Synthetic_Type_Ref_List'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Module_Doc_String_Line : aliased constant Text_Type :=
        "Module_Doc_String_Line";
         Node_Repr_Name_For_Module_Doc_String_Line : aliased constant Text_Type :=
           "ModuleDocStringLine";
      Node_Desc_For_Module_Doc_String_Line : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_Module_Doc_String_Line,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Module_Doc_String_Line'Access,
         Repr_Name         => Node_Repr_Name_For_Module_Doc_String_Line'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Null_Cond_Qualifier : aliased constant Text_Type :=
        "Null_Cond_Qualifier";
         Node_Repr_Name_For_Null_Cond_Qualifier : aliased constant Text_Type :=
           "NullCondQualifier";
      Node_Desc_For_Null_Cond_Qualifier : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Null_Cond_Qualifier'Access,
         Repr_Name         => Node_Repr_Name_For_Null_Cond_Qualifier'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 => Type_Index_For_Null_Cond_Qualifier_Absent,
2 => Type_Index_For_Null_Cond_Qualifier_Present
         ),
         Members           => (
              1 => Member_Index_For_Null_Cond_Qualifier_P_As_Bool
         ));
      
      Node_Name_For_Null_Cond_Qualifier_Absent : aliased constant Text_Type :=
        "Null_Cond_Qualifier_Absent";
         Node_Repr_Name_For_Null_Cond_Qualifier_Absent : aliased constant Text_Type :=
           "NullCondQualifierAbsent";
      Node_Desc_For_Null_Cond_Qualifier_Absent : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Null_Cond_Qualifier,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Null_Cond_Qualifier_Absent'Access,
         Repr_Name         => Node_Repr_Name_For_Null_Cond_Qualifier_Absent'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Null_Cond_Qualifier_Present : aliased constant Text_Type :=
        "Null_Cond_Qualifier_Present";
         Node_Repr_Name_For_Null_Cond_Qualifier_Present : aliased constant Text_Type :=
           "NullCondQualifierPresent";
      Node_Desc_For_Null_Cond_Qualifier_Present : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Null_Cond_Qualifier,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Null_Cond_Qualifier_Present'Access,
         Repr_Name         => Node_Repr_Name_For_Null_Cond_Qualifier_Present'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op : aliased constant Text_Type :=
        "Op";
         Node_Repr_Name_For_Op : aliased constant Text_Type :=
           "Op";
      Node_Desc_For_Op : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 18,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op'Access,
         Repr_Name         => Node_Repr_Name_For_Op'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 => Type_Index_For_Op_Amp,
2 => Type_Index_For_Op_And,
3 => Type_Index_For_Op_Div,
4 => Type_Index_For_Op_Eq,
5 => Type_Index_For_Op_Gt,
6 => Type_Index_For_Op_Gte,
7 => Type_Index_For_Op_Logic_And,
8 => Type_Index_For_Op_Logic_Or,
9 => Type_Index_For_Op_Lt,
10 => Type_Index_For_Op_Lte,
11 => Type_Index_For_Op_Minus,
12 => Type_Index_For_Op_Mult,
13 => Type_Index_For_Op_Ne,
14 => Type_Index_For_Op_Or,
15 => Type_Index_For_Op_Or_Int,
16 => Type_Index_For_Op_Plus,
17 => Type_Index_For_Op_Stream_Concat,
18 => Type_Index_For_Op_Stream_Cons
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Amp : aliased constant Text_Type :=
        "Op_Amp";
         Node_Repr_Name_For_Op_Amp : aliased constant Text_Type :=
           "OpAmp";
      Node_Desc_For_Op_Amp : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Amp'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Amp'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_And : aliased constant Text_Type :=
        "Op_And";
         Node_Repr_Name_For_Op_And : aliased constant Text_Type :=
           "OpAnd";
      Node_Desc_For_Op_And : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_And'Access,
         Repr_Name         => Node_Repr_Name_For_Op_And'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Div : aliased constant Text_Type :=
        "Op_Div";
         Node_Repr_Name_For_Op_Div : aliased constant Text_Type :=
           "OpDiv";
      Node_Desc_For_Op_Div : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Div'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Div'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Eq : aliased constant Text_Type :=
        "Op_Eq";
         Node_Repr_Name_For_Op_Eq : aliased constant Text_Type :=
           "OpEq";
      Node_Desc_For_Op_Eq : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Eq'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Eq'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Gt : aliased constant Text_Type :=
        "Op_Gt";
         Node_Repr_Name_For_Op_Gt : aliased constant Text_Type :=
           "OpGt";
      Node_Desc_For_Op_Gt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Gt'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Gt'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Gte : aliased constant Text_Type :=
        "Op_Gte";
         Node_Repr_Name_For_Op_Gte : aliased constant Text_Type :=
           "OpGte";
      Node_Desc_For_Op_Gte : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Gte'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Gte'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Logic_And : aliased constant Text_Type :=
        "Op_Logic_And";
         Node_Repr_Name_For_Op_Logic_And : aliased constant Text_Type :=
           "OpLogicAnd";
      Node_Desc_For_Op_Logic_And : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Logic_And'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Logic_And'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Logic_Or : aliased constant Text_Type :=
        "Op_Logic_Or";
         Node_Repr_Name_For_Op_Logic_Or : aliased constant Text_Type :=
           "OpLogicOr";
      Node_Desc_For_Op_Logic_Or : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Logic_Or'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Logic_Or'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Lt : aliased constant Text_Type :=
        "Op_Lt";
         Node_Repr_Name_For_Op_Lt : aliased constant Text_Type :=
           "OpLt";
      Node_Desc_For_Op_Lt : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Lt'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Lt'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Lte : aliased constant Text_Type :=
        "Op_Lte";
         Node_Repr_Name_For_Op_Lte : aliased constant Text_Type :=
           "OpLte";
      Node_Desc_For_Op_Lte : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Lte'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Lte'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Minus : aliased constant Text_Type :=
        "Op_Minus";
         Node_Repr_Name_For_Op_Minus : aliased constant Text_Type :=
           "OpMinus";
      Node_Desc_For_Op_Minus : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Minus'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Minus'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Mult : aliased constant Text_Type :=
        "Op_Mult";
         Node_Repr_Name_For_Op_Mult : aliased constant Text_Type :=
           "OpMult";
      Node_Desc_For_Op_Mult : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Mult'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Mult'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Ne : aliased constant Text_Type :=
        "Op_Ne";
         Node_Repr_Name_For_Op_Ne : aliased constant Text_Type :=
           "OpNe";
      Node_Desc_For_Op_Ne : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Ne'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Ne'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Or : aliased constant Text_Type :=
        "Op_Or";
         Node_Repr_Name_For_Op_Or : aliased constant Text_Type :=
           "OpOr";
      Node_Desc_For_Op_Or : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Or'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Or'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Or_Int : aliased constant Text_Type :=
        "Op_Or_Int";
         Node_Repr_Name_For_Op_Or_Int : aliased constant Text_Type :=
           "OpOrInt";
      Node_Desc_For_Op_Or_Int : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Or_Int'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Or_Int'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Plus : aliased constant Text_Type :=
        "Op_Plus";
         Node_Repr_Name_For_Op_Plus : aliased constant Text_Type :=
           "OpPlus";
      Node_Desc_For_Op_Plus : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Plus'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Plus'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Stream_Concat : aliased constant Text_Type :=
        "Op_Stream_Concat";
         Node_Repr_Name_For_Op_Stream_Concat : aliased constant Text_Type :=
           "OpStreamConcat";
      Node_Desc_For_Op_Stream_Concat : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Stream_Concat'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Stream_Concat'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Op_Stream_Cons : aliased constant Text_Type :=
        "Op_Stream_Cons";
         Node_Repr_Name_For_Op_Stream_Cons : aliased constant Text_Type :=
           "OpStreamCons";
      Node_Desc_For_Op_Stream_Cons : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Op,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Op_Stream_Cons'Access,
         Repr_Name         => Node_Repr_Name_For_Op_Stream_Cons'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Pattern : aliased constant Text_Type :=
        "Pattern";
         Node_Repr_Name_For_Pattern : aliased constant Text_Type :=
           "Pattern";
      Node_Desc_For_Pattern : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 12,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Pattern'Access,
         Repr_Name         => Node_Repr_Name_For_Pattern'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 => Type_Index_For_Any_Type_Pattern,
2 => Type_Index_For_Bool_Pattern,
3 => Type_Index_For_Complex_Pattern,
4 => Type_Index_For_Ellipsis_Pattern,
5 => Type_Index_For_Integer_Pattern,
6 => Type_Index_For_List_Pattern,
7 => Type_Index_For_Not_Pattern,
8 => Type_Index_For_Null_Pattern,
9 => Type_Index_For_Or_Pattern,
10 => Type_Index_For_Paren_Pattern,
11 => Type_Index_For_Regex_Pattern,
12 => Type_Index_For_Type_Pattern
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Any_Type_Pattern : aliased constant Text_Type :=
        "Any_Type_Pattern";
         Node_Repr_Name_For_Any_Type_Pattern : aliased constant Text_Type :=
           "AnyTypePattern";
      Node_Desc_For_Any_Type_Pattern : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Pattern,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Any_Type_Pattern'Access,
         Repr_Name         => Node_Repr_Name_For_Any_Type_Pattern'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Bool_Pattern : aliased constant Text_Type :=
        "Bool_Pattern";
         Node_Repr_Name_For_Bool_Pattern : aliased constant Text_Type :=
           "BoolPattern";
      Node_Desc_For_Bool_Pattern : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Pattern,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Bool_Pattern'Access,
         Repr_Name         => Node_Repr_Name_For_Bool_Pattern'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 => Type_Index_For_Bool_Pattern_False,
2 => Type_Index_For_Bool_Pattern_True
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Bool_Pattern_False : aliased constant Text_Type :=
        "Bool_Pattern_False";
         Node_Repr_Name_For_Bool_Pattern_False : aliased constant Text_Type :=
           "BoolPatternFalse";
      Node_Desc_For_Bool_Pattern_False : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Bool_Pattern,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Bool_Pattern_False'Access,
         Repr_Name         => Node_Repr_Name_For_Bool_Pattern_False'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Bool_Pattern_True : aliased constant Text_Type :=
        "Bool_Pattern_True";
         Node_Repr_Name_For_Bool_Pattern_True : aliased constant Text_Type :=
           "BoolPatternTrue";
      Node_Desc_For_Bool_Pattern_True : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Bool_Pattern,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Bool_Pattern_True'Access,
         Repr_Name         => Node_Repr_Name_For_Bool_Pattern_True'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Complex_Pattern : aliased constant Text_Type :=
        "Complex_Pattern";
         Node_Repr_Name_For_Complex_Pattern : aliased constant Text_Type :=
           "ComplexPattern";
      Node_Desc_For_Complex_Pattern : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 1,
         Member_Count      => 4,
         Base_Type         => Type_Index_For_Pattern,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Complex_Pattern'Access,
         Repr_Name         => Node_Repr_Name_For_Complex_Pattern'Access,
         Inherited_Members => 53,
         Derivations       => (
             1 => Type_Index_For_Renaming_Complex_Pattern
         ),
         Members           => (
              1 => Member_Index_For_Complex_Pattern_F_Decl,
2 => Member_Index_For_Complex_Pattern_F_Pattern,
3 => Member_Index_For_Complex_Pattern_F_Details,
4 => Member_Index_For_Complex_Pattern_F_Predicate
         ));
      
      Node_Name_For_Renaming_Complex_Pattern : aliased constant Text_Type :=
        "Renaming_Complex_Pattern";
         Node_Repr_Name_For_Renaming_Complex_Pattern : aliased constant Text_Type :=
           "RenamingComplexPattern";
      Node_Desc_For_Renaming_Complex_Pattern : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Complex_Pattern,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Renaming_Complex_Pattern'Access,
         Repr_Name         => Node_Repr_Name_For_Renaming_Complex_Pattern'Access,
         Inherited_Members => 53,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Ellipsis_Pattern : aliased constant Text_Type :=
        "Ellipsis_Pattern";
         Node_Repr_Name_For_Ellipsis_Pattern : aliased constant Text_Type :=
           "EllipsisPattern";
      Node_Desc_For_Ellipsis_Pattern : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Pattern,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Ellipsis_Pattern'Access,
         Repr_Name         => Node_Repr_Name_For_Ellipsis_Pattern'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Ellipsis_Pattern_F_Binding
         ));
      
      Node_Name_For_Integer_Pattern : aliased constant Text_Type :=
        "Integer_Pattern";
         Node_Repr_Name_For_Integer_Pattern : aliased constant Text_Type :=
           "IntegerPattern";
      Node_Desc_For_Integer_Pattern : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Pattern,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_Number,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Integer_Pattern'Access,
         Repr_Name         => Node_Repr_Name_For_Integer_Pattern'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_List_Pattern : aliased constant Text_Type :=
        "List_Pattern";
         Node_Repr_Name_For_List_Pattern : aliased constant Text_Type :=
           "ListPattern";
      Node_Desc_For_List_Pattern : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Pattern,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_List_Pattern'Access,
         Repr_Name         => Node_Repr_Name_For_List_Pattern'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_List_Pattern_F_Sub_Patterns
         ));
      
      Node_Name_For_Not_Pattern : aliased constant Text_Type :=
        "Not_Pattern";
         Node_Repr_Name_For_Not_Pattern : aliased constant Text_Type :=
           "NotPattern";
      Node_Desc_For_Not_Pattern : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Pattern,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Not_Pattern'Access,
         Repr_Name         => Node_Repr_Name_For_Not_Pattern'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Not_Pattern_F_Sub_Pattern
         ));
      
      Node_Name_For_Null_Pattern : aliased constant Text_Type :=
        "Null_Pattern";
         Node_Repr_Name_For_Null_Pattern : aliased constant Text_Type :=
           "NullPattern";
      Node_Desc_For_Null_Pattern : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Pattern,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Null_Pattern'Access,
         Repr_Name         => Node_Repr_Name_For_Null_Pattern'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Or_Pattern : aliased constant Text_Type :=
        "Or_Pattern";
         Node_Repr_Name_For_Or_Pattern : aliased constant Text_Type :=
           "OrPattern";
      Node_Desc_For_Or_Pattern : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Pattern,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Or_Pattern'Access,
         Repr_Name         => Node_Repr_Name_For_Or_Pattern'Access,
         Inherited_Members => 51,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Or_Pattern_F_Left_Sub_Pattern,
2 => Member_Index_For_Or_Pattern_F_Right_Sub_Pattern
         ));
      
      Node_Name_For_Paren_Pattern : aliased constant Text_Type :=
        "Paren_Pattern";
         Node_Repr_Name_For_Paren_Pattern : aliased constant Text_Type :=
           "ParenPattern";
      Node_Desc_For_Paren_Pattern : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Pattern,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Paren_Pattern'Access,
         Repr_Name         => Node_Repr_Name_For_Paren_Pattern'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Paren_Pattern_F_Sub_Pattern
         ));
      
      Node_Name_For_Regex_Pattern : aliased constant Text_Type :=
        "Regex_Pattern";
         Node_Repr_Name_For_Regex_Pattern : aliased constant Text_Type :=
           "RegexPattern";
      Node_Desc_For_Regex_Pattern : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Pattern,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_String,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Regex_Pattern'Access,
         Repr_Name         => Node_Repr_Name_For_Regex_Pattern'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Type_Pattern : aliased constant Text_Type :=
        "Type_Pattern";
         Node_Repr_Name_For_Type_Pattern : aliased constant Text_Type :=
           "TypePattern";
      Node_Desc_For_Type_Pattern : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Pattern,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Type_Pattern'Access,
         Repr_Name         => Node_Repr_Name_For_Type_Pattern'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Type_Pattern_F_Type_Name
         ));
      
      Node_Name_For_Pattern_Detail : aliased constant Text_Type :=
        "Pattern_Detail";
         Node_Repr_Name_For_Pattern_Detail : aliased constant Text_Type :=
           "PatternDetail";
      Node_Desc_For_Pattern_Detail : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 2,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Pattern_Detail'Access,
         Repr_Name         => Node_Repr_Name_For_Pattern_Detail'Access,
         Inherited_Members => 49,
         Derivations       => (
             1 => Type_Index_For_Field_Pattern_Detail,
2 => Type_Index_For_Property_Pattern_Detail
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Field_Pattern_Detail : aliased constant Text_Type :=
        "Field_Pattern_Detail";
         Node_Repr_Name_For_Field_Pattern_Detail : aliased constant Text_Type :=
           "FieldPatternDetail";
      Node_Desc_For_Field_Pattern_Detail : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Pattern_Detail,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Field_Pattern_Detail'Access,
         Repr_Name         => Node_Repr_Name_For_Field_Pattern_Detail'Access,
         Inherited_Members => 51,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Field_Pattern_Detail_F_Id,
2 => Member_Index_For_Field_Pattern_Detail_F_Expected_Value
         ));
      
      Node_Name_For_Property_Pattern_Detail : aliased constant Text_Type :=
        "Property_Pattern_Detail";
         Node_Repr_Name_For_Property_Pattern_Detail : aliased constant Text_Type :=
           "PropertyPatternDetail";
      Node_Desc_For_Property_Pattern_Detail : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Pattern_Detail,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Property_Pattern_Detail'Access,
         Repr_Name         => Node_Repr_Name_For_Property_Pattern_Detail'Access,
         Inherited_Members => 51,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Property_Pattern_Detail_F_Call,
2 => Member_Index_For_Property_Pattern_Detail_F_Expected_Value
         ));
      
      Node_Name_For_Type_Ref : aliased constant Text_Type :=
        "Type_Ref";
         Node_Repr_Name_For_Type_Ref : aliased constant Text_Type :=
           "TypeRef";
      Node_Desc_For_Type_Ref : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 4,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => True,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Type_Ref'Access,
         Repr_Name         => Node_Repr_Name_For_Type_Ref'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 => Type_Index_For_Default_List_Type_Ref,
2 => Type_Index_For_Function_Type_Ref,
3 => Type_Index_For_Generic_Type_Ref,
4 => Type_Index_For_Simple_Type_Ref
         ),
         Members           => (
              1 => Member_Index_For_Type_Ref_P_Referenced_Decl
         ));
      
      Node_Name_For_Default_List_Type_Ref : aliased constant Text_Type :=
        "Default_List_Type_Ref";
         Node_Repr_Name_For_Default_List_Type_Ref : aliased constant Text_Type :=
           "DefaultListTypeRef";
      Node_Desc_For_Default_List_Type_Ref : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 0,
         Base_Type         => Type_Index_For_Type_Ref,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => True,
         Token_Node_Kind   => Token_Index_For_Lkt_Identifier,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Default_List_Type_Ref'Access,
         Repr_Name         => Node_Repr_Name_For_Default_List_Type_Ref'Access,
         Inherited_Members => 50,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 .. 0 => <>
         ));
      
      Node_Name_For_Function_Type_Ref : aliased constant Text_Type :=
        "Function_Type_Ref";
         Node_Repr_Name_For_Function_Type_Ref : aliased constant Text_Type :=
           "FunctionTypeRef";
      Node_Desc_For_Function_Type_Ref : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Type_Ref,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Function_Type_Ref'Access,
         Repr_Name         => Node_Repr_Name_For_Function_Type_Ref'Access,
         Inherited_Members => 52,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Function_Type_Ref_F_Param_Types,
2 => Member_Index_For_Function_Type_Ref_F_Return_Type
         ));
      
      Node_Name_For_Generic_Type_Ref : aliased constant Text_Type :=
        "Generic_Type_Ref";
         Node_Repr_Name_For_Generic_Type_Ref : aliased constant Text_Type :=
           "GenericTypeRef";
      Node_Desc_For_Generic_Type_Ref : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Type_Ref,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Generic_Type_Ref'Access,
         Repr_Name         => Node_Repr_Name_For_Generic_Type_Ref'Access,
         Inherited_Members => 52,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Generic_Type_Ref_F_Type_Name,
2 => Member_Index_For_Generic_Type_Ref_F_Args
         ));
      
      Node_Name_For_Simple_Type_Ref : aliased constant Text_Type :=
        "Simple_Type_Ref";
         Node_Repr_Name_For_Simple_Type_Ref : aliased constant Text_Type :=
           "SimpleTypeRef";
      Node_Desc_For_Simple_Type_Ref : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 1,
         Base_Type         => Type_Index_For_Type_Ref,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Simple_Type_Ref'Access,
         Repr_Name         => Node_Repr_Name_For_Simple_Type_Ref'Access,
         Inherited_Members => 51,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Simple_Type_Ref_F_Type_Name
         ));
      
      Node_Name_For_Var_Bind : aliased constant Text_Type :=
        "Var_Bind";
         Node_Repr_Name_For_Var_Bind : aliased constant Text_Type :=
           "VarBind";
      Node_Desc_For_Var_Bind : aliased constant Struct_Type_Descriptor :=
        (Derivations_Count => 0,
         Member_Count      => 2,
         Base_Type         => Type_Index_For_Lkt_Node,
         Is_Abstract       => False,
         Is_Error_Node     => False,
         Is_Synthetic      => False,
         Is_Token_Node     => False,
         Token_Node_Kind   => No_Token_Kind_Index,
         List_Element_Type => No_Type_Index,
         Name              => Node_Name_For_Var_Bind'Access,
         Repr_Name         => Node_Repr_Name_For_Var_Bind'Access,
         Inherited_Members => 51,
         Derivations       => (
             1 .. 0 => <>
         ),
         Members           => (
              1 => Member_Index_For_Var_Bind_F_Name,
2 => Member_Index_For_Var_Bind_F_Expr
         ));

   Struct_Types : aliased constant Struct_Type_Descriptor_Array := (
      Type_Index_For_Complete_Item => Node_Desc_For_Complete_Item'Access,
Type_Index_For_Decoded_Char_Value => Node_Desc_For_Decoded_Char_Value'Access,
Type_Index_For_Decoded_String_Value => Node_Desc_For_Decoded_String_Value'Access,
Type_Index_For_Logic_Context => Node_Desc_For_Logic_Context'Access,
Type_Index_For_Ref_Result => Node_Desc_For_Ref_Result'Access,
Type_Index_For_Solver_Diagnostic => Node_Desc_For_Solver_Diagnostic'Access,
Type_Index_For_Solver_Result => Node_Desc_For_Solver_Result'Access,
Type_Index_For_Lkt_Node => Node_Desc_For_Lkt_Node'Access,
Type_Index_For_Argument => Node_Desc_For_Argument'Access,
Type_Index_For_Base_Import => Node_Desc_For_Base_Import'Access,
Type_Index_For_Import => Node_Desc_For_Import'Access,
Type_Index_For_Import_All_From => Node_Desc_For_Import_All_From'Access,
Type_Index_For_Import_From => Node_Desc_For_Import_From'Access,
Type_Index_For_Base_Lexer_Case_Rule_Alt => Node_Desc_For_Base_Lexer_Case_Rule_Alt'Access,
Type_Index_For_Error_Lexer_Case_Rule_Alt => Node_Desc_For_Error_Lexer_Case_Rule_Alt'Access,
Type_Index_For_Lexer_Case_Rule_Cond_Alt => Node_Desc_For_Lexer_Case_Rule_Cond_Alt'Access,
Type_Index_For_Lexer_Case_Rule_Default_Alt => Node_Desc_For_Lexer_Case_Rule_Default_Alt'Access,
Type_Index_For_Base_Match_Branch => Node_Desc_For_Base_Match_Branch'Access,
Type_Index_For_Match_Branch => Node_Desc_For_Match_Branch'Access,
Type_Index_For_Pattern_Match_Branch => Node_Desc_For_Pattern_Match_Branch'Access,
Type_Index_For_Block_Expr_Clause => Node_Desc_For_Block_Expr_Clause'Access,
Type_Index_For_Block_String_Line => Node_Desc_For_Block_String_Line'Access,
Type_Index_For_Class_Qualifier => Node_Desc_For_Class_Qualifier'Access,
Type_Index_For_Class_Qualifier_Absent => Node_Desc_For_Class_Qualifier_Absent'Access,
Type_Index_For_Class_Qualifier_Present => Node_Desc_For_Class_Qualifier_Present'Access,
Type_Index_For_Decl => Node_Desc_For_Decl'Access,
Type_Index_For_Base_Grammar_Rule_Decl => Node_Desc_For_Base_Grammar_Rule_Decl'Access,
Type_Index_For_Grammar_Rule_Decl => Node_Desc_For_Grammar_Rule_Decl'Access,
Type_Index_For_Synthetic_Lexer_Decl => Node_Desc_For_Synthetic_Lexer_Decl'Access,
Type_Index_For_Base_Val_Decl => Node_Desc_For_Base_Val_Decl'Access,
Type_Index_For_Node_Decl => Node_Desc_For_Node_Decl'Access,
Type_Index_For_Self_Decl => Node_Desc_For_Self_Decl'Access,
Type_Index_For_User_Val_Decl => Node_Desc_For_User_Val_Decl'Access,
Type_Index_For_Binding_Val_Decl => Node_Desc_For_Binding_Val_Decl'Access,
Type_Index_For_Enum_Lit_Decl => Node_Desc_For_Enum_Lit_Decl'Access,
Type_Index_For_Explicitly_Typed_Decl => Node_Desc_For_Explicitly_Typed_Decl'Access,
Type_Index_For_Component_Decl => Node_Desc_For_Component_Decl'Access,
Type_Index_For_Field_Decl => Node_Desc_For_Field_Decl'Access,
Type_Index_For_Fun_Param_Decl => Node_Desc_For_Fun_Param_Decl'Access,
Type_Index_For_Lambda_Param_Decl => Node_Desc_For_Lambda_Param_Decl'Access,
Type_Index_For_Dyn_Var_Decl => Node_Desc_For_Dyn_Var_Decl'Access,
Type_Index_For_Match_Val_Decl => Node_Desc_For_Match_Val_Decl'Access,
Type_Index_For_Val_Decl => Node_Desc_For_Val_Decl'Access,
Type_Index_For_Fun_Decl => Node_Desc_For_Fun_Decl'Access,
Type_Index_For_Env_Spec_Decl => Node_Desc_For_Env_Spec_Decl'Access,
Type_Index_For_Error_Decl => Node_Desc_For_Error_Decl'Access,
Type_Index_For_Generic_Decl => Node_Desc_For_Generic_Decl'Access,
Type_Index_For_Grammar_Decl => Node_Desc_For_Grammar_Decl'Access,
Type_Index_For_Langkit_Root => Node_Desc_For_Langkit_Root'Access,
Type_Index_For_Lexer_Decl => Node_Desc_For_Lexer_Decl'Access,
Type_Index_For_Lexer_Family_Decl => Node_Desc_For_Lexer_Family_Decl'Access,
Type_Index_For_Synth_Fun_Decl => Node_Desc_For_Synth_Fun_Decl'Access,
Type_Index_For_Synth_Param_Decl => Node_Desc_For_Synth_Param_Decl'Access,
Type_Index_For_Type_Decl => Node_Desc_For_Type_Decl'Access,
Type_Index_For_Any_Type_Decl => Node_Desc_For_Any_Type_Decl'Access,
Type_Index_For_Enum_Class_Alt_Decl => Node_Desc_For_Enum_Class_Alt_Decl'Access,
Type_Index_For_Function_Type => Node_Desc_For_Function_Type'Access,
Type_Index_For_Generic_Param_Type_Decl => Node_Desc_For_Generic_Param_Type_Decl'Access,
Type_Index_For_Named_Type_Decl => Node_Desc_For_Named_Type_Decl'Access,
Type_Index_For_Basic_Class_Decl => Node_Desc_For_Basic_Class_Decl'Access,
Type_Index_For_Class_Decl => Node_Desc_For_Class_Decl'Access,
Type_Index_For_Enum_Class_Decl => Node_Desc_For_Enum_Class_Decl'Access,
Type_Index_For_Enum_Type_Decl => Node_Desc_For_Enum_Type_Decl'Access,
Type_Index_For_Struct_Decl => Node_Desc_For_Struct_Decl'Access,
Type_Index_For_Trait_Decl => Node_Desc_For_Trait_Decl'Access,
Type_Index_For_Decl_Annotation => Node_Desc_For_Decl_Annotation'Access,
Type_Index_For_Decl_Annotation_Args => Node_Desc_For_Decl_Annotation_Args'Access,
Type_Index_For_Dyn_Env_Wrapper => Node_Desc_For_Dyn_Env_Wrapper'Access,
Type_Index_For_Elsif_Branch => Node_Desc_For_Elsif_Branch'Access,
Type_Index_For_Enum_Class_Case => Node_Desc_For_Enum_Class_Case'Access,
Type_Index_For_Excludes_Null => Node_Desc_For_Excludes_Null'Access,
Type_Index_For_Excludes_Null_Absent => Node_Desc_For_Excludes_Null_Absent'Access,
Type_Index_For_Excludes_Null_Present => Node_Desc_For_Excludes_Null_Present'Access,
Type_Index_For_Expr => Node_Desc_For_Expr'Access,
Type_Index_For_Any_Of => Node_Desc_For_Any_Of'Access,
Type_Index_For_Array_Literal => Node_Desc_For_Array_Literal'Access,
Type_Index_For_Base_Call_Expr => Node_Desc_For_Base_Call_Expr'Access,
Type_Index_For_Call_Expr => Node_Desc_For_Call_Expr'Access,
Type_Index_For_Logic_Call_Expr => Node_Desc_For_Logic_Call_Expr'Access,
Type_Index_For_Logic_Predicate => Node_Desc_For_Logic_Predicate'Access,
Type_Index_For_Logic_Propagate_Call => Node_Desc_For_Logic_Propagate_Call'Access,
Type_Index_For_Bin_Op => Node_Desc_For_Bin_Op'Access,
Type_Index_For_Block_Expr => Node_Desc_For_Block_Expr'Access,
Type_Index_For_Cast_Expr => Node_Desc_For_Cast_Expr'Access,
Type_Index_For_Dot_Expr => Node_Desc_For_Dot_Expr'Access,
Type_Index_For_Error_On_Null => Node_Desc_For_Error_On_Null'Access,
Type_Index_For_Generic_Instantiation => Node_Desc_For_Generic_Instantiation'Access,
Type_Index_For_Grammar_Expr => Node_Desc_For_Grammar_Expr'Access,
Type_Index_For_Error_Grammar_Expr => Node_Desc_For_Error_Grammar_Expr'Access,
Type_Index_For_Grammar_Cut => Node_Desc_For_Grammar_Cut'Access,
Type_Index_For_Grammar_Discard => Node_Desc_For_Grammar_Discard'Access,
Type_Index_For_Grammar_Dont_Skip => Node_Desc_For_Grammar_Dont_Skip'Access,
Type_Index_For_Grammar_List => Node_Desc_For_Grammar_List'Access,
Type_Index_For_Grammar_Null => Node_Desc_For_Grammar_Null'Access,
Type_Index_For_Grammar_Opt => Node_Desc_For_Grammar_Opt'Access,
Type_Index_For_Grammar_Opt_Error => Node_Desc_For_Grammar_Opt_Error'Access,
Type_Index_For_Grammar_Opt_Error_Group => Node_Desc_For_Grammar_Opt_Error_Group'Access,
Type_Index_For_Grammar_Opt_Group => Node_Desc_For_Grammar_Opt_Group'Access,
Type_Index_For_Grammar_Or_Expr => Node_Desc_For_Grammar_Or_Expr'Access,
Type_Index_For_Grammar_Pick => Node_Desc_For_Grammar_Pick'Access,
Type_Index_For_Grammar_Implicit_Pick => Node_Desc_For_Grammar_Implicit_Pick'Access,
Type_Index_For_Grammar_Predicate => Node_Desc_For_Grammar_Predicate'Access,
Type_Index_For_Grammar_Rule_Ref => Node_Desc_For_Grammar_Rule_Ref'Access,
Type_Index_For_Grammar_Skip => Node_Desc_For_Grammar_Skip'Access,
Type_Index_For_Grammar_Stop_Cut => Node_Desc_For_Grammar_Stop_Cut'Access,
Type_Index_For_Parse_Node_Expr => Node_Desc_For_Parse_Node_Expr'Access,
Type_Index_For_Token_Lit => Node_Desc_For_Token_Lit'Access,
Type_Index_For_Token_No_Case_Lit => Node_Desc_For_Token_No_Case_Lit'Access,
Type_Index_For_Token_Pattern_Concat => Node_Desc_For_Token_Pattern_Concat'Access,
Type_Index_For_Token_Pattern_Lit => Node_Desc_For_Token_Pattern_Lit'Access,
Type_Index_For_Token_Ref => Node_Desc_For_Token_Ref'Access,
Type_Index_For_Id => Node_Desc_For_Id'Access,
Type_Index_For_Def_Id => Node_Desc_For_Def_Id'Access,
Type_Index_For_Imported_Id => Node_Desc_For_Imported_Id'Access,
Type_Index_For_Module_Id => Node_Desc_For_Module_Id'Access,
Type_Index_For_Ref_Id => Node_Desc_For_Ref_Id'Access,
Type_Index_For_If_Expr => Node_Desc_For_If_Expr'Access,
Type_Index_For_Isa => Node_Desc_For_Isa'Access,
Type_Index_For_Keep_Expr => Node_Desc_For_Keep_Expr'Access,
Type_Index_For_Lambda_Expr => Node_Desc_For_Lambda_Expr'Access,
Type_Index_For_Lit => Node_Desc_For_Lit'Access,
Type_Index_For_Big_Num_Lit => Node_Desc_For_Big_Num_Lit'Access,
Type_Index_For_Char_Lit => Node_Desc_For_Char_Lit'Access,
Type_Index_For_Null_Lit => Node_Desc_For_Null_Lit'Access,
Type_Index_For_Num_Lit => Node_Desc_For_Num_Lit'Access,
Type_Index_For_String_Lit => Node_Desc_For_String_Lit'Access,
Type_Index_For_Block_String_Lit => Node_Desc_For_Block_String_Lit'Access,
Type_Index_For_Module_Doc_String_Lit => Node_Desc_For_Module_Doc_String_Lit'Access,
Type_Index_For_Single_Line_String_Lit => Node_Desc_For_Single_Line_String_Lit'Access,
Type_Index_For_Pattern_Single_Line_String_Lit => Node_Desc_For_Pattern_Single_Line_String_Lit'Access,
Type_Index_For_Logic_Assign => Node_Desc_For_Logic_Assign'Access,
Type_Index_For_Logic_Expr => Node_Desc_For_Logic_Expr'Access,
Type_Index_For_Logic_Propagate => Node_Desc_For_Logic_Propagate'Access,
Type_Index_For_Logic_Unify => Node_Desc_For_Logic_Unify'Access,
Type_Index_For_Match_Expr => Node_Desc_For_Match_Expr'Access,
Type_Index_For_Not_Expr => Node_Desc_For_Not_Expr'Access,
Type_Index_For_Paren_Expr => Node_Desc_For_Paren_Expr'Access,
Type_Index_For_Query => Node_Desc_For_Query'Access,
Type_Index_For_Raise_Expr => Node_Desc_For_Raise_Expr'Access,
Type_Index_For_Subscript_Expr => Node_Desc_For_Subscript_Expr'Access,
Type_Index_For_Try_Expr => Node_Desc_For_Try_Expr'Access,
Type_Index_For_Un_Op => Node_Desc_For_Un_Op'Access,
Type_Index_For_Full_Decl => Node_Desc_For_Full_Decl'Access,
Type_Index_For_Grammar_List_Sep => Node_Desc_For_Grammar_List_Sep'Access,
Type_Index_For_Imported_Name => Node_Desc_For_Imported_Name'Access,
Type_Index_For_Lexer_Case_Rule => Node_Desc_For_Lexer_Case_Rule'Access,
Type_Index_For_Lexer_Case_Rule_Send => Node_Desc_For_Lexer_Case_Rule_Send'Access,
Type_Index_For_List_Kind => Node_Desc_For_List_Kind'Access,
Type_Index_For_List_Kind_One => Node_Desc_For_List_Kind_One'Access,
Type_Index_For_List_Kind_Zero => Node_Desc_For_List_Kind_Zero'Access,
Type_Index_For_Lkt_Node_Base_List => Node_Desc_For_Lkt_Node_Base_List'Access,
Type_Index_For_Argument_List => Node_Desc_For_Argument_List'Access,
Type_Index_For_Base_Import_List => Node_Desc_For_Base_Import_List'Access,
Type_Index_For_Base_Lexer_Case_Rule_Alt_List => Node_Desc_For_Base_Lexer_Case_Rule_Alt_List'Access,
Type_Index_For_Base_Match_Branch_List => Node_Desc_For_Base_Match_Branch_List'Access,
Type_Index_For_Block_String_Line_List => Node_Desc_For_Block_String_Line_List'Access,
Type_Index_For_Call_Expr_List => Node_Desc_For_Call_Expr_List'Access,
Type_Index_For_Decl_Annotation_List => Node_Desc_For_Decl_Annotation_List'Access,
Type_Index_For_Elsif_Branch_List => Node_Desc_For_Elsif_Branch_List'Access,
Type_Index_For_Enum_Class_Alt_Decl_List => Node_Desc_For_Enum_Class_Alt_Decl_List'Access,
Type_Index_For_Enum_Class_Case_List => Node_Desc_For_Enum_Class_Case_List'Access,
Type_Index_For_Enum_Lit_Decl_List => Node_Desc_For_Enum_Lit_Decl_List'Access,
Type_Index_For_Expr_List => Node_Desc_For_Expr_List'Access,
Type_Index_For_Any_Of_List => Node_Desc_For_Any_Of_List'Access,
Type_Index_For_Full_Decl_List => Node_Desc_For_Full_Decl_List'Access,
Type_Index_For_Decl_Block => Node_Desc_For_Decl_Block'Access,
Type_Index_For_Generic_Param_Decl_List => Node_Desc_For_Generic_Param_Decl_List'Access,
Type_Index_For_Fun_Param_Decl_List => Node_Desc_For_Fun_Param_Decl_List'Access,
Type_Index_For_Grammar_Expr_List => Node_Desc_For_Grammar_Expr_List'Access,
Type_Index_For_Grammar_Expr_List_List => Node_Desc_For_Grammar_Expr_List_List'Access,
Type_Index_For_Imported_Name_List => Node_Desc_For_Imported_Name_List'Access,
Type_Index_For_Lambda_Param_Decl_List => Node_Desc_For_Lambda_Param_Decl_List'Access,
Type_Index_For_Lkt_Node_List => Node_Desc_For_Lkt_Node_List'Access,
Type_Index_For_Module_Doc_String_Line_List => Node_Desc_For_Module_Doc_String_Line_List'Access,
Type_Index_For_Pattern_Detail_List => Node_Desc_For_Pattern_Detail_List'Access,
Type_Index_For_Pattern_List => Node_Desc_For_Pattern_List'Access,
Type_Index_For_Ref_Id_List => Node_Desc_For_Ref_Id_List'Access,
Type_Index_For_Type_Ref_List => Node_Desc_For_Type_Ref_List'Access,
Type_Index_For_Synthetic_Type_Ref_List => Node_Desc_For_Synthetic_Type_Ref_List'Access,
Type_Index_For_Module_Doc_String_Line => Node_Desc_For_Module_Doc_String_Line'Access,
Type_Index_For_Null_Cond_Qualifier => Node_Desc_For_Null_Cond_Qualifier'Access,
Type_Index_For_Null_Cond_Qualifier_Absent => Node_Desc_For_Null_Cond_Qualifier_Absent'Access,
Type_Index_For_Null_Cond_Qualifier_Present => Node_Desc_For_Null_Cond_Qualifier_Present'Access,
Type_Index_For_Op => Node_Desc_For_Op'Access,
Type_Index_For_Op_Amp => Node_Desc_For_Op_Amp'Access,
Type_Index_For_Op_And => Node_Desc_For_Op_And'Access,
Type_Index_For_Op_Div => Node_Desc_For_Op_Div'Access,
Type_Index_For_Op_Eq => Node_Desc_For_Op_Eq'Access,
Type_Index_For_Op_Gt => Node_Desc_For_Op_Gt'Access,
Type_Index_For_Op_Gte => Node_Desc_For_Op_Gte'Access,
Type_Index_For_Op_Logic_And => Node_Desc_For_Op_Logic_And'Access,
Type_Index_For_Op_Logic_Or => Node_Desc_For_Op_Logic_Or'Access,
Type_Index_For_Op_Lt => Node_Desc_For_Op_Lt'Access,
Type_Index_For_Op_Lte => Node_Desc_For_Op_Lte'Access,
Type_Index_For_Op_Minus => Node_Desc_For_Op_Minus'Access,
Type_Index_For_Op_Mult => Node_Desc_For_Op_Mult'Access,
Type_Index_For_Op_Ne => Node_Desc_For_Op_Ne'Access,
Type_Index_For_Op_Or => Node_Desc_For_Op_Or'Access,
Type_Index_For_Op_Or_Int => Node_Desc_For_Op_Or_Int'Access,
Type_Index_For_Op_Plus => Node_Desc_For_Op_Plus'Access,
Type_Index_For_Op_Stream_Concat => Node_Desc_For_Op_Stream_Concat'Access,
Type_Index_For_Op_Stream_Cons => Node_Desc_For_Op_Stream_Cons'Access,
Type_Index_For_Pattern => Node_Desc_For_Pattern'Access,
Type_Index_For_Any_Type_Pattern => Node_Desc_For_Any_Type_Pattern'Access,
Type_Index_For_Bool_Pattern => Node_Desc_For_Bool_Pattern'Access,
Type_Index_For_Bool_Pattern_False => Node_Desc_For_Bool_Pattern_False'Access,
Type_Index_For_Bool_Pattern_True => Node_Desc_For_Bool_Pattern_True'Access,
Type_Index_For_Complex_Pattern => Node_Desc_For_Complex_Pattern'Access,
Type_Index_For_Renaming_Complex_Pattern => Node_Desc_For_Renaming_Complex_Pattern'Access,
Type_Index_For_Ellipsis_Pattern => Node_Desc_For_Ellipsis_Pattern'Access,
Type_Index_For_Integer_Pattern => Node_Desc_For_Integer_Pattern'Access,
Type_Index_For_List_Pattern => Node_Desc_For_List_Pattern'Access,
Type_Index_For_Not_Pattern => Node_Desc_For_Not_Pattern'Access,
Type_Index_For_Null_Pattern => Node_Desc_For_Null_Pattern'Access,
Type_Index_For_Or_Pattern => Node_Desc_For_Or_Pattern'Access,
Type_Index_For_Paren_Pattern => Node_Desc_For_Paren_Pattern'Access,
Type_Index_For_Regex_Pattern => Node_Desc_For_Regex_Pattern'Access,
Type_Index_For_Type_Pattern => Node_Desc_For_Type_Pattern'Access,
Type_Index_For_Pattern_Detail => Node_Desc_For_Pattern_Detail'Access,
Type_Index_For_Field_Pattern_Detail => Node_Desc_For_Field_Pattern_Detail'Access,
Type_Index_For_Property_Pattern_Detail => Node_Desc_For_Property_Pattern_Detail'Access,
Type_Index_For_Type_Ref => Node_Desc_For_Type_Ref'Access,
Type_Index_For_Default_List_Type_Ref => Node_Desc_For_Default_List_Type_Ref'Access,
Type_Index_For_Function_Type_Ref => Node_Desc_For_Function_Type_Ref'Access,
Type_Index_For_Generic_Type_Ref => Node_Desc_For_Generic_Type_Ref'Access,
Type_Index_For_Simple_Type_Ref => Node_Desc_For_Simple_Type_Ref'Access,
Type_Index_For_Var_Bind => Node_Desc_For_Var_Bind'Access
   );

   First_Node     : constant Type_Index := Type_Index_For_Lkt_Node;
   First_Property : constant Struct_Member_Index :=
     Member_Index_For_Parent;

   function Eval_Node_Member
     (Node      : Internal_Acc_Node;
      Member    : Struct_Member_Index;
      Arguments : Internal_Value_Array) return Internal_Value_Access;
   --  Implementation for the Eval_Node_Member operation in the language
   --  descriptor.

   Builtin_Types : aliased constant Builtin_Types_Record :=
     (Analysis_Unit         => Type_Index_For_Analysis_Unit,
      Big_Int               => Type_Index_For_Big_Int,
      Bool                  => Type_Index_For_Bool,
      Char                  => Type_Index_For_Char,
      Int                   => Type_Index_For_Int,
      Source_Location       => Type_Index_For_Source_Location,
      Source_Location_Range => Type_Index_For_Source_Location_Range,
      String                => Type_Index_For_String,
      Token                 => Type_Index_For_Token,
      Symbol                => Type_Index_For_Symbol);

   Node_Kinds : constant array (Lkt_Node_Kind_Type) of Type_Index :=
     (Lkt_Argument => Type_Index_For_Argument,
      Lkt_Import => Type_Index_For_Import,
      Lkt_Import_All_From => Type_Index_For_Import_All_From,
      Lkt_Import_From => Type_Index_For_Import_From,
      Lkt_Error_Lexer_Case_Rule_Alt => Type_Index_For_Error_Lexer_Case_Rule_Alt,
      Lkt_Lexer_Case_Rule_Cond_Alt => Type_Index_For_Lexer_Case_Rule_Cond_Alt,
      Lkt_Lexer_Case_Rule_Default_Alt => Type_Index_For_Lexer_Case_Rule_Default_Alt,
      Lkt_Match_Branch => Type_Index_For_Match_Branch,
      Lkt_Pattern_Match_Branch => Type_Index_For_Pattern_Match_Branch,
      Lkt_Block_Expr_Clause => Type_Index_For_Block_Expr_Clause,
      Lkt_Block_String_Line => Type_Index_For_Block_String_Line,
      Lkt_Class_Qualifier_Absent => Type_Index_For_Class_Qualifier_Absent,
      Lkt_Class_Qualifier_Present => Type_Index_For_Class_Qualifier_Present,
      Lkt_Grammar_Rule_Decl => Type_Index_For_Grammar_Rule_Decl,
      Lkt_Synthetic_Lexer_Decl => Type_Index_For_Synthetic_Lexer_Decl,
      Lkt_Node_Decl => Type_Index_For_Node_Decl,
      Lkt_Self_Decl => Type_Index_For_Self_Decl,
      Lkt_Binding_Val_Decl => Type_Index_For_Binding_Val_Decl,
      Lkt_Enum_Lit_Decl => Type_Index_For_Enum_Lit_Decl,
      Lkt_Field_Decl => Type_Index_For_Field_Decl,
      Lkt_Fun_Param_Decl => Type_Index_For_Fun_Param_Decl,
      Lkt_Lambda_Param_Decl => Type_Index_For_Lambda_Param_Decl,
      Lkt_Dyn_Var_Decl => Type_Index_For_Dyn_Var_Decl,
      Lkt_Match_Val_Decl => Type_Index_For_Match_Val_Decl,
      Lkt_Val_Decl => Type_Index_For_Val_Decl,
      Lkt_Fun_Decl => Type_Index_For_Fun_Decl,
      Lkt_Env_Spec_Decl => Type_Index_For_Env_Spec_Decl,
      Lkt_Error_Decl => Type_Index_For_Error_Decl,
      Lkt_Generic_Decl => Type_Index_For_Generic_Decl,
      Lkt_Grammar_Decl => Type_Index_For_Grammar_Decl,
      Lkt_Langkit_Root => Type_Index_For_Langkit_Root,
      Lkt_Lexer_Decl => Type_Index_For_Lexer_Decl,
      Lkt_Lexer_Family_Decl => Type_Index_For_Lexer_Family_Decl,
      Lkt_Synth_Fun_Decl => Type_Index_For_Synth_Fun_Decl,
      Lkt_Synth_Param_Decl => Type_Index_For_Synth_Param_Decl,
      Lkt_Any_Type_Decl => Type_Index_For_Any_Type_Decl,
      Lkt_Enum_Class_Alt_Decl => Type_Index_For_Enum_Class_Alt_Decl,
      Lkt_Function_Type => Type_Index_For_Function_Type,
      Lkt_Generic_Param_Type_Decl => Type_Index_For_Generic_Param_Type_Decl,
      Lkt_Class_Decl => Type_Index_For_Class_Decl,
      Lkt_Enum_Class_Decl => Type_Index_For_Enum_Class_Decl,
      Lkt_Enum_Type_Decl => Type_Index_For_Enum_Type_Decl,
      Lkt_Struct_Decl => Type_Index_For_Struct_Decl,
      Lkt_Trait_Decl => Type_Index_For_Trait_Decl,
      Lkt_Decl_Annotation => Type_Index_For_Decl_Annotation,
      Lkt_Decl_Annotation_Args => Type_Index_For_Decl_Annotation_Args,
      Lkt_Dyn_Env_Wrapper => Type_Index_For_Dyn_Env_Wrapper,
      Lkt_Elsif_Branch => Type_Index_For_Elsif_Branch,
      Lkt_Enum_Class_Case => Type_Index_For_Enum_Class_Case,
      Lkt_Excludes_Null_Absent => Type_Index_For_Excludes_Null_Absent,
      Lkt_Excludes_Null_Present => Type_Index_For_Excludes_Null_Present,
      Lkt_Any_Of => Type_Index_For_Any_Of,
      Lkt_Array_Literal => Type_Index_For_Array_Literal,
      Lkt_Call_Expr => Type_Index_For_Call_Expr,
      Lkt_Logic_Predicate => Type_Index_For_Logic_Predicate,
      Lkt_Logic_Propagate_Call => Type_Index_For_Logic_Propagate_Call,
      Lkt_Bin_Op => Type_Index_For_Bin_Op,
      Lkt_Block_Expr => Type_Index_For_Block_Expr,
      Lkt_Cast_Expr => Type_Index_For_Cast_Expr,
      Lkt_Dot_Expr => Type_Index_For_Dot_Expr,
      Lkt_Error_On_Null => Type_Index_For_Error_On_Null,
      Lkt_Generic_Instantiation => Type_Index_For_Generic_Instantiation,
      Lkt_Error_Grammar_Expr => Type_Index_For_Error_Grammar_Expr,
      Lkt_Grammar_Cut => Type_Index_For_Grammar_Cut,
      Lkt_Grammar_Discard => Type_Index_For_Grammar_Discard,
      Lkt_Grammar_Dont_Skip => Type_Index_For_Grammar_Dont_Skip,
      Lkt_Grammar_List => Type_Index_For_Grammar_List,
      Lkt_Grammar_Null => Type_Index_For_Grammar_Null,
      Lkt_Grammar_Opt => Type_Index_For_Grammar_Opt,
      Lkt_Grammar_Opt_Error => Type_Index_For_Grammar_Opt_Error,
      Lkt_Grammar_Opt_Error_Group => Type_Index_For_Grammar_Opt_Error_Group,
      Lkt_Grammar_Opt_Group => Type_Index_For_Grammar_Opt_Group,
      Lkt_Grammar_Or_Expr => Type_Index_For_Grammar_Or_Expr,
      Lkt_Grammar_Pick => Type_Index_For_Grammar_Pick,
      Lkt_Grammar_Implicit_Pick => Type_Index_For_Grammar_Implicit_Pick,
      Lkt_Grammar_Predicate => Type_Index_For_Grammar_Predicate,
      Lkt_Grammar_Rule_Ref => Type_Index_For_Grammar_Rule_Ref,
      Lkt_Grammar_Skip => Type_Index_For_Grammar_Skip,
      Lkt_Grammar_Stop_Cut => Type_Index_For_Grammar_Stop_Cut,
      Lkt_Parse_Node_Expr => Type_Index_For_Parse_Node_Expr,
      Lkt_Token_Lit => Type_Index_For_Token_Lit,
      Lkt_Token_No_Case_Lit => Type_Index_For_Token_No_Case_Lit,
      Lkt_Token_Pattern_Concat => Type_Index_For_Token_Pattern_Concat,
      Lkt_Token_Pattern_Lit => Type_Index_For_Token_Pattern_Lit,
      Lkt_Token_Ref => Type_Index_For_Token_Ref,
      Lkt_Id => Type_Index_For_Id,
      Lkt_Def_Id => Type_Index_For_Def_Id,
      Lkt_Imported_Id => Type_Index_For_Imported_Id,
      Lkt_Module_Id => Type_Index_For_Module_Id,
      Lkt_Ref_Id => Type_Index_For_Ref_Id,
      Lkt_If_Expr => Type_Index_For_If_Expr,
      Lkt_Isa => Type_Index_For_Isa,
      Lkt_Keep_Expr => Type_Index_For_Keep_Expr,
      Lkt_Lambda_Expr => Type_Index_For_Lambda_Expr,
      Lkt_Big_Num_Lit => Type_Index_For_Big_Num_Lit,
      Lkt_Char_Lit => Type_Index_For_Char_Lit,
      Lkt_Null_Lit => Type_Index_For_Null_Lit,
      Lkt_Num_Lit => Type_Index_For_Num_Lit,
      Lkt_Block_String_Lit => Type_Index_For_Block_String_Lit,
      Lkt_Module_Doc_String_Lit => Type_Index_For_Module_Doc_String_Lit,
      Lkt_Single_Line_String_Lit => Type_Index_For_Single_Line_String_Lit,
      Lkt_Pattern_Single_Line_String_Lit => Type_Index_For_Pattern_Single_Line_String_Lit,
      Lkt_Logic_Assign => Type_Index_For_Logic_Assign,
      Lkt_Logic_Expr => Type_Index_For_Logic_Expr,
      Lkt_Logic_Propagate => Type_Index_For_Logic_Propagate,
      Lkt_Logic_Unify => Type_Index_For_Logic_Unify,
      Lkt_Match_Expr => Type_Index_For_Match_Expr,
      Lkt_Not_Expr => Type_Index_For_Not_Expr,
      Lkt_Paren_Expr => Type_Index_For_Paren_Expr,
      Lkt_Query => Type_Index_For_Query,
      Lkt_Raise_Expr => Type_Index_For_Raise_Expr,
      Lkt_Subscript_Expr => Type_Index_For_Subscript_Expr,
      Lkt_Try_Expr => Type_Index_For_Try_Expr,
      Lkt_Un_Op => Type_Index_For_Un_Op,
      Lkt_Full_Decl => Type_Index_For_Full_Decl,
      Lkt_Grammar_List_Sep => Type_Index_For_Grammar_List_Sep,
      Lkt_Imported_Name => Type_Index_For_Imported_Name,
      Lkt_Lexer_Case_Rule => Type_Index_For_Lexer_Case_Rule,
      Lkt_Lexer_Case_Rule_Send => Type_Index_For_Lexer_Case_Rule_Send,
      Lkt_List_Kind_One => Type_Index_For_List_Kind_One,
      Lkt_List_Kind_Zero => Type_Index_For_List_Kind_Zero,
      Lkt_Argument_List => Type_Index_For_Argument_List,
      Lkt_Base_Import_List => Type_Index_For_Base_Import_List,
      Lkt_Base_Lexer_Case_Rule_Alt_List => Type_Index_For_Base_Lexer_Case_Rule_Alt_List,
      Lkt_Base_Match_Branch_List => Type_Index_For_Base_Match_Branch_List,
      Lkt_Block_String_Line_List => Type_Index_For_Block_String_Line_List,
      Lkt_Call_Expr_List => Type_Index_For_Call_Expr_List,
      Lkt_Decl_Annotation_List => Type_Index_For_Decl_Annotation_List,
      Lkt_Elsif_Branch_List => Type_Index_For_Elsif_Branch_List,
      Lkt_Enum_Class_Alt_Decl_List => Type_Index_For_Enum_Class_Alt_Decl_List,
      Lkt_Enum_Class_Case_List => Type_Index_For_Enum_Class_Case_List,
      Lkt_Enum_Lit_Decl_List => Type_Index_For_Enum_Lit_Decl_List,
      Lkt_Expr_List => Type_Index_For_Expr_List,
      Lkt_Any_Of_List => Type_Index_For_Any_Of_List,
      Lkt_Full_Decl_List => Type_Index_For_Full_Decl_List,
      Lkt_Decl_Block => Type_Index_For_Decl_Block,
      Lkt_Generic_Param_Decl_List => Type_Index_For_Generic_Param_Decl_List,
      Lkt_Fun_Param_Decl_List => Type_Index_For_Fun_Param_Decl_List,
      Lkt_Grammar_Expr_List => Type_Index_For_Grammar_Expr_List,
      Lkt_Grammar_Expr_List_List => Type_Index_For_Grammar_Expr_List_List,
      Lkt_Imported_Name_List => Type_Index_For_Imported_Name_List,
      Lkt_Lambda_Param_Decl_List => Type_Index_For_Lambda_Param_Decl_List,
      Lkt_Lkt_Node_List => Type_Index_For_Lkt_Node_List,
      Lkt_Module_Doc_String_Line_List => Type_Index_For_Module_Doc_String_Line_List,
      Lkt_Pattern_Detail_List => Type_Index_For_Pattern_Detail_List,
      Lkt_Pattern_List => Type_Index_For_Pattern_List,
      Lkt_Ref_Id_List => Type_Index_For_Ref_Id_List,
      Lkt_Type_Ref_List => Type_Index_For_Type_Ref_List,
      Lkt_Synthetic_Type_Ref_List => Type_Index_For_Synthetic_Type_Ref_List,
      Lkt_Module_Doc_String_Line => Type_Index_For_Module_Doc_String_Line,
      Lkt_Null_Cond_Qualifier_Absent => Type_Index_For_Null_Cond_Qualifier_Absent,
      Lkt_Null_Cond_Qualifier_Present => Type_Index_For_Null_Cond_Qualifier_Present,
      Lkt_Op_Amp => Type_Index_For_Op_Amp,
      Lkt_Op_And => Type_Index_For_Op_And,
      Lkt_Op_Div => Type_Index_For_Op_Div,
      Lkt_Op_Eq => Type_Index_For_Op_Eq,
      Lkt_Op_Gt => Type_Index_For_Op_Gt,
      Lkt_Op_Gte => Type_Index_For_Op_Gte,
      Lkt_Op_Logic_And => Type_Index_For_Op_Logic_And,
      Lkt_Op_Logic_Or => Type_Index_For_Op_Logic_Or,
      Lkt_Op_Lt => Type_Index_For_Op_Lt,
      Lkt_Op_Lte => Type_Index_For_Op_Lte,
      Lkt_Op_Minus => Type_Index_For_Op_Minus,
      Lkt_Op_Mult => Type_Index_For_Op_Mult,
      Lkt_Op_Ne => Type_Index_For_Op_Ne,
      Lkt_Op_Or => Type_Index_For_Op_Or,
      Lkt_Op_Or_Int => Type_Index_For_Op_Or_Int,
      Lkt_Op_Plus => Type_Index_For_Op_Plus,
      Lkt_Op_Stream_Concat => Type_Index_For_Op_Stream_Concat,
      Lkt_Op_Stream_Cons => Type_Index_For_Op_Stream_Cons,
      Lkt_Any_Type_Pattern => Type_Index_For_Any_Type_Pattern,
      Lkt_Bool_Pattern_False => Type_Index_For_Bool_Pattern_False,
      Lkt_Bool_Pattern_True => Type_Index_For_Bool_Pattern_True,
      Lkt_Complex_Pattern => Type_Index_For_Complex_Pattern,
      Lkt_Renaming_Complex_Pattern => Type_Index_For_Renaming_Complex_Pattern,
      Lkt_Ellipsis_Pattern => Type_Index_For_Ellipsis_Pattern,
      Lkt_Integer_Pattern => Type_Index_For_Integer_Pattern,
      Lkt_List_Pattern => Type_Index_For_List_Pattern,
      Lkt_Not_Pattern => Type_Index_For_Not_Pattern,
      Lkt_Null_Pattern => Type_Index_For_Null_Pattern,
      Lkt_Or_Pattern => Type_Index_For_Or_Pattern,
      Lkt_Paren_Pattern => Type_Index_For_Paren_Pattern,
      Lkt_Regex_Pattern => Type_Index_For_Regex_Pattern,
      Lkt_Type_Pattern => Type_Index_For_Type_Pattern,
      Lkt_Field_Pattern_Detail => Type_Index_For_Field_Pattern_Detail,
      Lkt_Property_Pattern_Detail => Type_Index_For_Property_Pattern_Detail,
      Lkt_Default_List_Type_Ref => Type_Index_For_Default_List_Type_Ref,
      Lkt_Function_Type_Ref => Type_Index_For_Function_Type_Ref,
      Lkt_Generic_Type_Ref => Type_Index_For_Generic_Type_Ref,
      Lkt_Simple_Type_Ref => Type_Index_For_Simple_Type_Ref,
      Lkt_Var_Bind => Type_Index_For_Var_Bind);
   --  Associate a type index to each concrete node

   -----------------------------------------------------
   --  Getter/setter helpers for introspection values --
   -----------------------------------------------------

   --  These helpers factorize common code needed in array/struct generic
   --  access/construction operations.

   procedure Set_Unit
     (Intr_Value   : Internal_Acc_Analysis_Unit;
      Actual_Value : Analysis_Unit);

   function Get_Unit
     (Intr_Value : Internal_Rec_Analysis_Unit)
      return Analysis_Unit;

   procedure Set_Big_Int
     (Intr_Value   : Internal_Acc_Big_Int;
      Actual_Value : Big_Integer);

   procedure Get_Big_Int
     (Intr_Value   : Internal_Rec_Big_Int;
      Actual_Value : out Big_Integer);

   procedure Set_Node
     (Intr_Value   : Internal_Acc_Node;
      Actual_Value : Lkt_Node'Class);

   function Get_Node
     (Intr_Value : Internal_Rec_Node)
      return Lkt_Node;

end Liblktlang.Generic_Introspection;
