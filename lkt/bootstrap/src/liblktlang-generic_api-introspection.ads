
--  This package provides contants to refer to Liblktlang types and struct
--  members in the generic introspection API
--  (``Liblktlang_Support.Generic_API.Introspection``).

with Liblktlang_Support.Generic_API.Introspection;

package Liblktlang.Generic_API.Introspection is

   package G renames Liblktlang_Support.Generic_API.Introspection;

   ---------------------
   -- Type references --
   ---------------------

   package Type_Refs is
         Analysis_Unit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 1);
         Big_Integer : constant G.Type_Ref :=
           G.From_Index (Self_Id, 2);
         Boolean : constant G.Type_Ref :=
           G.From_Index (Self_Id, 3);
         Character_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 4);
         Integer : constant G.Type_Ref :=
           G.From_Index (Self_Id, 5);
         Source_Location : constant G.Type_Ref :=
           G.From_Index (Self_Id, 6);
         Source_Location_Range : constant G.Type_Ref :=
           G.From_Index (Self_Id, 7);
         Text_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 8);
         Token_Reference : constant G.Type_Ref :=
           G.From_Index (Self_Id, 9);
         Unbounded_Text_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 10);
         Analysis_Unit_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 11);
         Completion_Item_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 12);
         Designated_Env_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 13);
         Grammar_Rule : constant G.Type_Ref :=
           G.From_Index (Self_Id, 14);
         Lookup_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 15);
         Complete_Item_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 16);
         Lkt_Node_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 17);
         Def_Id_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 18);
         Fun_Decl_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 19);
         Logic_Context_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 20);
         Ref_Result_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 21);
         Solver_Diagnostic_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 22);
         Analysis_Unit_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 23);
         Complete_Item : constant G.Type_Ref :=
           G.From_Index (Self_Id, 24);
         Decoded_Char_Value : constant G.Type_Ref :=
           G.From_Index (Self_Id, 25);
         Decoded_String_Value : constant G.Type_Ref :=
           G.From_Index (Self_Id, 26);
         Logic_Context : constant G.Type_Ref :=
           G.From_Index (Self_Id, 27);
         Ref_Result : constant G.Type_Ref :=
           G.From_Index (Self_Id, 28);
         Solver_Diagnostic : constant G.Type_Ref :=
           G.From_Index (Self_Id, 29);
         Solver_Result : constant G.Type_Ref :=
           G.From_Index (Self_Id, 30);
         Lkt_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 31);
         Argument : constant G.Type_Ref :=
           G.From_Index (Self_Id, 32);
         Base_Import : constant G.Type_Ref :=
           G.From_Index (Self_Id, 33);
         Import : constant G.Type_Ref :=
           G.From_Index (Self_Id, 34);
         Import_All_From : constant G.Type_Ref :=
           G.From_Index (Self_Id, 35);
         Import_From : constant G.Type_Ref :=
           G.From_Index (Self_Id, 36);
         Base_Lexer_Case_Rule_Alt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 37);
         Error_Lexer_Case_Rule_Alt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 38);
         Lexer_Case_Rule_Cond_Alt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 39);
         Lexer_Case_Rule_Default_Alt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 40);
         Base_Match_Branch : constant G.Type_Ref :=
           G.From_Index (Self_Id, 41);
         Match_Branch : constant G.Type_Ref :=
           G.From_Index (Self_Id, 42);
         Pattern_Match_Branch : constant G.Type_Ref :=
           G.From_Index (Self_Id, 43);
         Block_Expr_Clause : constant G.Type_Ref :=
           G.From_Index (Self_Id, 44);
         Block_String_Line : constant G.Type_Ref :=
           G.From_Index (Self_Id, 45);
         Class_Qualifier : constant G.Type_Ref :=
           G.From_Index (Self_Id, 46);
         Class_Qualifier_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 47);
         Class_Qualifier_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 48);
         Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 49);
         Base_Grammar_Rule_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 50);
         Grammar_Rule_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 51);
         Synthetic_Lexer_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 52);
         Base_Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 53);
         Node_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 54);
         Self_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 55);
         User_Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 56);
         Binding_Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 57);
         Enum_Lit_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 58);
         Explicitly_Typed_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 59);
         Component_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 60);
         Field_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 61);
         Fun_Param_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 62);
         Lambda_Param_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 63);
         Dyn_Var_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 64);
         Match_Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 65);
         Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 66);
         Fun_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 67);
         Env_Spec_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 68);
         Error_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 69);
         Generic_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 70);
         Grammar_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 71);
         Lexer_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 72);
         Lexer_Family_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 73);
         Synth_Fun_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 74);
         Synth_Param_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 75);
         Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 76);
         Any_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 77);
         Enum_Class_Alt_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 78);
         Function_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 79);
         Generic_Param_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 80);
         Named_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 81);
         Basic_Class_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 82);
         Class_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 83);
         Enum_Class_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 84);
         Enum_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 85);
         Struct_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 86);
         Trait_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 87);
         Decl_Annotation : constant G.Type_Ref :=
           G.From_Index (Self_Id, 88);
         Decl_Annotation_Args : constant G.Type_Ref :=
           G.From_Index (Self_Id, 89);
         Dyn_Env_Wrapper : constant G.Type_Ref :=
           G.From_Index (Self_Id, 90);
         Elsif_Branch : constant G.Type_Ref :=
           G.From_Index (Self_Id, 91);
         Enum_Class_Case : constant G.Type_Ref :=
           G.From_Index (Self_Id, 92);
         Excludes_Null : constant G.Type_Ref :=
           G.From_Index (Self_Id, 93);
         Excludes_Null_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 94);
         Excludes_Null_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 95);
         Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 96);
         Any_Of : constant G.Type_Ref :=
           G.From_Index (Self_Id, 97);
         Array_Literal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 98);
         Base_Call_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 99);
         Call_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 100);
         Logic_Call_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 101);
         Logic_Predicate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 102);
         Logic_Propagate_Call : constant G.Type_Ref :=
           G.From_Index (Self_Id, 103);
         Bin_Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 104);
         Block_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 105);
         Cast_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 106);
         Dot_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 107);
         Error_On_Null : constant G.Type_Ref :=
           G.From_Index (Self_Id, 108);
         Generic_Instantiation : constant G.Type_Ref :=
           G.From_Index (Self_Id, 109);
         Grammar_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 110);
         Error_Grammar_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 111);
         Grammar_Cut : constant G.Type_Ref :=
           G.From_Index (Self_Id, 112);
         Grammar_Discard : constant G.Type_Ref :=
           G.From_Index (Self_Id, 113);
         Grammar_Dont_Skip : constant G.Type_Ref :=
           G.From_Index (Self_Id, 114);
         Grammar_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 115);
         Grammar_Null : constant G.Type_Ref :=
           G.From_Index (Self_Id, 116);
         Grammar_Opt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 117);
         Grammar_Opt_Error : constant G.Type_Ref :=
           G.From_Index (Self_Id, 118);
         Grammar_Opt_Error_Group : constant G.Type_Ref :=
           G.From_Index (Self_Id, 119);
         Grammar_Opt_Group : constant G.Type_Ref :=
           G.From_Index (Self_Id, 120);
         Grammar_Or_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 121);
         Grammar_Pick : constant G.Type_Ref :=
           G.From_Index (Self_Id, 122);
         Grammar_Implicit_Pick : constant G.Type_Ref :=
           G.From_Index (Self_Id, 123);
         Grammar_Predicate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 124);
         Grammar_Rule_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 125);
         Grammar_Skip : constant G.Type_Ref :=
           G.From_Index (Self_Id, 126);
         Grammar_Stop_Cut : constant G.Type_Ref :=
           G.From_Index (Self_Id, 127);
         Parse_Node_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 128);
         Token_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 129);
         Token_No_Case_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 130);
         Token_Pattern_Concat : constant G.Type_Ref :=
           G.From_Index (Self_Id, 131);
         Token_Pattern_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 132);
         Token_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 133);
         Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 134);
         Def_Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 135);
         Imported_Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 136);
         Module_Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 137);
         Ref_Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 138);
         If_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 139);
         Isa : constant G.Type_Ref :=
           G.From_Index (Self_Id, 140);
         Keep_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 141);
         Lambda_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 142);
         Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 143);
         Big_Num_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 144);
         Char_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 145);
         Null_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 146);
         Num_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 147);
         String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 148);
         Block_String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 149);
         Module_Doc_String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 150);
         Single_Line_String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 151);
         Pattern_Single_Line_String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 152);
         Logic_Assign : constant G.Type_Ref :=
           G.From_Index (Self_Id, 153);
         Logic_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 154);
         Logic_Propagate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 155);
         Logic_Unify : constant G.Type_Ref :=
           G.From_Index (Self_Id, 156);
         Match_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 157);
         Not_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 158);
         Paren_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 159);
         Query : constant G.Type_Ref :=
           G.From_Index (Self_Id, 160);
         Raise_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 161);
         Subscript_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 162);
         Try_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 163);
         Un_Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 164);
         Full_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 165);
         Grammar_List_Sep : constant G.Type_Ref :=
           G.From_Index (Self_Id, 166);
         Imported_Name : constant G.Type_Ref :=
           G.From_Index (Self_Id, 167);
         Langkit_Root : constant G.Type_Ref :=
           G.From_Index (Self_Id, 168);
         Lexer_Case_Rule : constant G.Type_Ref :=
           G.From_Index (Self_Id, 169);
         Lexer_Case_Rule_Send : constant G.Type_Ref :=
           G.From_Index (Self_Id, 170);
         List_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 171);
         List_Kind_One : constant G.Type_Ref :=
           G.From_Index (Self_Id, 172);
         List_Kind_Zero : constant G.Type_Ref :=
           G.From_Index (Self_Id, 173);
         Lkt_Node_Base_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 174);
         Argument_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 175);
         Base_Import_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 176);
         Base_Lexer_Case_Rule_Alt_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 177);
         Base_Match_Branch_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 178);
         Block_String_Line_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 179);
         Call_Expr_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 180);
         Decl_Annotation_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 181);
         Elsif_Branch_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 182);
         Enum_Class_Alt_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 183);
         Enum_Class_Case_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 184);
         Enum_Lit_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 185);
         Expr_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 186);
         Any_Of_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 187);
         Full_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 188);
         Decl_Block : constant G.Type_Ref :=
           G.From_Index (Self_Id, 189);
         Generic_Param_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 190);
         Fun_Param_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 191);
         Grammar_Expr_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 192);
         Grammar_Expr_List_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 193);
         Imported_Name_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 194);
         Lambda_Param_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 195);
         Lkt_Node_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 196);
         Module_Doc_String_Line_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 197);
         Pattern_Detail_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 198);
         Pattern_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 199);
         Ref_Id_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 200);
         Type_Ref_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 201);
         Synthetic_Type_Ref_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 202);
         Module_Doc_String_Line : constant G.Type_Ref :=
           G.From_Index (Self_Id, 203);
         Null_Cond_Qualifier : constant G.Type_Ref :=
           G.From_Index (Self_Id, 204);
         Null_Cond_Qualifier_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 205);
         Null_Cond_Qualifier_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 206);
         Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 207);
         Op_Amp : constant G.Type_Ref :=
           G.From_Index (Self_Id, 208);
         Op_And : constant G.Type_Ref :=
           G.From_Index (Self_Id, 209);
         Op_Div : constant G.Type_Ref :=
           G.From_Index (Self_Id, 210);
         Op_Eq : constant G.Type_Ref :=
           G.From_Index (Self_Id, 211);
         Op_Gt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 212);
         Op_Gte : constant G.Type_Ref :=
           G.From_Index (Self_Id, 213);
         Op_Logic_And : constant G.Type_Ref :=
           G.From_Index (Self_Id, 214);
         Op_Logic_Or : constant G.Type_Ref :=
           G.From_Index (Self_Id, 215);
         Op_Lt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 216);
         Op_Lte : constant G.Type_Ref :=
           G.From_Index (Self_Id, 217);
         Op_Minus : constant G.Type_Ref :=
           G.From_Index (Self_Id, 218);
         Op_Mult : constant G.Type_Ref :=
           G.From_Index (Self_Id, 219);
         Op_Ne : constant G.Type_Ref :=
           G.From_Index (Self_Id, 220);
         Op_Or : constant G.Type_Ref :=
           G.From_Index (Self_Id, 221);
         Op_Or_Int : constant G.Type_Ref :=
           G.From_Index (Self_Id, 222);
         Op_Plus : constant G.Type_Ref :=
           G.From_Index (Self_Id, 223);
         Op_Stream_Concat : constant G.Type_Ref :=
           G.From_Index (Self_Id, 224);
         Op_Stream_Cons : constant G.Type_Ref :=
           G.From_Index (Self_Id, 225);
         Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 226);
         Any_Type_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 227);
         Binding_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 228);
         Bool_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 229);
         Bool_Pattern_False : constant G.Type_Ref :=
           G.From_Index (Self_Id, 230);
         Bool_Pattern_True : constant G.Type_Ref :=
           G.From_Index (Self_Id, 231);
         Ellipsis_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 232);
         Extended_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 233);
         Filtered_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 234);
         Integer_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 235);
         List_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 236);
         Not_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 237);
         Null_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 238);
         Or_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 239);
         Paren_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 240);
         Regex_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 241);
         Tuple_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 242);
         Type_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 243);
         Pattern_Detail : constant G.Type_Ref :=
           G.From_Index (Self_Id, 244);
         Field_Pattern_Detail : constant G.Type_Ref :=
           G.From_Index (Self_Id, 245);
         Property_Pattern_Detail : constant G.Type_Ref :=
           G.From_Index (Self_Id, 246);
         Selector_Pattern_Detail : constant G.Type_Ref :=
           G.From_Index (Self_Id, 247);
         Selector_Call : constant G.Type_Ref :=
           G.From_Index (Self_Id, 248);
         Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 249);
         Default_List_Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 250);
         Function_Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 251);
         Generic_Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 252);
         Simple_Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 253);
         Var_Bind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 254);
   end Type_Refs;

   Kind_To_Type : constant array (Lkt_Node_Kind_Type) of G.Type_Ref :=
     (Lkt_Argument => Type_Refs.Argument,
      Lkt_Import => Type_Refs.Import,
      Lkt_Import_All_From => Type_Refs.Import_All_From,
      Lkt_Import_From => Type_Refs.Import_From,
      Lkt_Error_Lexer_Case_Rule_Alt => Type_Refs.Error_Lexer_Case_Rule_Alt,
      Lkt_Lexer_Case_Rule_Cond_Alt => Type_Refs.Lexer_Case_Rule_Cond_Alt,
      Lkt_Lexer_Case_Rule_Default_Alt => Type_Refs.Lexer_Case_Rule_Default_Alt,
      Lkt_Match_Branch => Type_Refs.Match_Branch,
      Lkt_Pattern_Match_Branch => Type_Refs.Pattern_Match_Branch,
      Lkt_Block_Expr_Clause => Type_Refs.Block_Expr_Clause,
      Lkt_Block_String_Line => Type_Refs.Block_String_Line,
      Lkt_Class_Qualifier_Absent => Type_Refs.Class_Qualifier_Absent,
      Lkt_Class_Qualifier_Present => Type_Refs.Class_Qualifier_Present,
      Lkt_Grammar_Rule_Decl => Type_Refs.Grammar_Rule_Decl,
      Lkt_Synthetic_Lexer_Decl => Type_Refs.Synthetic_Lexer_Decl,
      Lkt_Node_Decl => Type_Refs.Node_Decl,
      Lkt_Self_Decl => Type_Refs.Self_Decl,
      Lkt_Binding_Val_Decl => Type_Refs.Binding_Val_Decl,
      Lkt_Enum_Lit_Decl => Type_Refs.Enum_Lit_Decl,
      Lkt_Field_Decl => Type_Refs.Field_Decl,
      Lkt_Fun_Param_Decl => Type_Refs.Fun_Param_Decl,
      Lkt_Lambda_Param_Decl => Type_Refs.Lambda_Param_Decl,
      Lkt_Dyn_Var_Decl => Type_Refs.Dyn_Var_Decl,
      Lkt_Match_Val_Decl => Type_Refs.Match_Val_Decl,
      Lkt_Val_Decl => Type_Refs.Val_Decl,
      Lkt_Fun_Decl => Type_Refs.Fun_Decl,
      Lkt_Env_Spec_Decl => Type_Refs.Env_Spec_Decl,
      Lkt_Error_Decl => Type_Refs.Error_Decl,
      Lkt_Generic_Decl => Type_Refs.Generic_Decl,
      Lkt_Grammar_Decl => Type_Refs.Grammar_Decl,
      Lkt_Lexer_Decl => Type_Refs.Lexer_Decl,
      Lkt_Lexer_Family_Decl => Type_Refs.Lexer_Family_Decl,
      Lkt_Synth_Fun_Decl => Type_Refs.Synth_Fun_Decl,
      Lkt_Synth_Param_Decl => Type_Refs.Synth_Param_Decl,
      Lkt_Any_Type_Decl => Type_Refs.Any_Type_Decl,
      Lkt_Enum_Class_Alt_Decl => Type_Refs.Enum_Class_Alt_Decl,
      Lkt_Function_Type => Type_Refs.Function_Type,
      Lkt_Generic_Param_Type_Decl => Type_Refs.Generic_Param_Type_Decl,
      Lkt_Class_Decl => Type_Refs.Class_Decl,
      Lkt_Enum_Class_Decl => Type_Refs.Enum_Class_Decl,
      Lkt_Enum_Type_Decl => Type_Refs.Enum_Type_Decl,
      Lkt_Struct_Decl => Type_Refs.Struct_Decl,
      Lkt_Trait_Decl => Type_Refs.Trait_Decl,
      Lkt_Decl_Annotation => Type_Refs.Decl_Annotation,
      Lkt_Decl_Annotation_Args => Type_Refs.Decl_Annotation_Args,
      Lkt_Dyn_Env_Wrapper => Type_Refs.Dyn_Env_Wrapper,
      Lkt_Elsif_Branch => Type_Refs.Elsif_Branch,
      Lkt_Enum_Class_Case => Type_Refs.Enum_Class_Case,
      Lkt_Excludes_Null_Absent => Type_Refs.Excludes_Null_Absent,
      Lkt_Excludes_Null_Present => Type_Refs.Excludes_Null_Present,
      Lkt_Any_Of => Type_Refs.Any_Of,
      Lkt_Array_Literal => Type_Refs.Array_Literal,
      Lkt_Call_Expr => Type_Refs.Call_Expr,
      Lkt_Logic_Predicate => Type_Refs.Logic_Predicate,
      Lkt_Logic_Propagate_Call => Type_Refs.Logic_Propagate_Call,
      Lkt_Bin_Op => Type_Refs.Bin_Op,
      Lkt_Block_Expr => Type_Refs.Block_Expr,
      Lkt_Cast_Expr => Type_Refs.Cast_Expr,
      Lkt_Dot_Expr => Type_Refs.Dot_Expr,
      Lkt_Error_On_Null => Type_Refs.Error_On_Null,
      Lkt_Generic_Instantiation => Type_Refs.Generic_Instantiation,
      Lkt_Error_Grammar_Expr => Type_Refs.Error_Grammar_Expr,
      Lkt_Grammar_Cut => Type_Refs.Grammar_Cut,
      Lkt_Grammar_Discard => Type_Refs.Grammar_Discard,
      Lkt_Grammar_Dont_Skip => Type_Refs.Grammar_Dont_Skip,
      Lkt_Grammar_List => Type_Refs.Grammar_List,
      Lkt_Grammar_Null => Type_Refs.Grammar_Null,
      Lkt_Grammar_Opt => Type_Refs.Grammar_Opt,
      Lkt_Grammar_Opt_Error => Type_Refs.Grammar_Opt_Error,
      Lkt_Grammar_Opt_Error_Group => Type_Refs.Grammar_Opt_Error_Group,
      Lkt_Grammar_Opt_Group => Type_Refs.Grammar_Opt_Group,
      Lkt_Grammar_Or_Expr => Type_Refs.Grammar_Or_Expr,
      Lkt_Grammar_Pick => Type_Refs.Grammar_Pick,
      Lkt_Grammar_Implicit_Pick => Type_Refs.Grammar_Implicit_Pick,
      Lkt_Grammar_Predicate => Type_Refs.Grammar_Predicate,
      Lkt_Grammar_Rule_Ref => Type_Refs.Grammar_Rule_Ref,
      Lkt_Grammar_Skip => Type_Refs.Grammar_Skip,
      Lkt_Grammar_Stop_Cut => Type_Refs.Grammar_Stop_Cut,
      Lkt_Parse_Node_Expr => Type_Refs.Parse_Node_Expr,
      Lkt_Token_Lit => Type_Refs.Token_Lit,
      Lkt_Token_No_Case_Lit => Type_Refs.Token_No_Case_Lit,
      Lkt_Token_Pattern_Concat => Type_Refs.Token_Pattern_Concat,
      Lkt_Token_Pattern_Lit => Type_Refs.Token_Pattern_Lit,
      Lkt_Token_Ref => Type_Refs.Token_Ref,
      Lkt_Id => Type_Refs.Id,
      Lkt_Def_Id => Type_Refs.Def_Id,
      Lkt_Imported_Id => Type_Refs.Imported_Id,
      Lkt_Module_Id => Type_Refs.Module_Id,
      Lkt_Ref_Id => Type_Refs.Ref_Id,
      Lkt_If_Expr => Type_Refs.If_Expr,
      Lkt_Isa => Type_Refs.Isa,
      Lkt_Keep_Expr => Type_Refs.Keep_Expr,
      Lkt_Lambda_Expr => Type_Refs.Lambda_Expr,
      Lkt_Big_Num_Lit => Type_Refs.Big_Num_Lit,
      Lkt_Char_Lit => Type_Refs.Char_Lit,
      Lkt_Null_Lit => Type_Refs.Null_Lit,
      Lkt_Num_Lit => Type_Refs.Num_Lit,
      Lkt_Block_String_Lit => Type_Refs.Block_String_Lit,
      Lkt_Module_Doc_String_Lit => Type_Refs.Module_Doc_String_Lit,
      Lkt_Single_Line_String_Lit => Type_Refs.Single_Line_String_Lit,
      Lkt_Pattern_Single_Line_String_Lit => Type_Refs.Pattern_Single_Line_String_Lit,
      Lkt_Logic_Assign => Type_Refs.Logic_Assign,
      Lkt_Logic_Expr => Type_Refs.Logic_Expr,
      Lkt_Logic_Propagate => Type_Refs.Logic_Propagate,
      Lkt_Logic_Unify => Type_Refs.Logic_Unify,
      Lkt_Match_Expr => Type_Refs.Match_Expr,
      Lkt_Not_Expr => Type_Refs.Not_Expr,
      Lkt_Paren_Expr => Type_Refs.Paren_Expr,
      Lkt_Query => Type_Refs.Query,
      Lkt_Raise_Expr => Type_Refs.Raise_Expr,
      Lkt_Subscript_Expr => Type_Refs.Subscript_Expr,
      Lkt_Try_Expr => Type_Refs.Try_Expr,
      Lkt_Un_Op => Type_Refs.Un_Op,
      Lkt_Full_Decl => Type_Refs.Full_Decl,
      Lkt_Grammar_List_Sep => Type_Refs.Grammar_List_Sep,
      Lkt_Imported_Name => Type_Refs.Imported_Name,
      Lkt_Langkit_Root => Type_Refs.Langkit_Root,
      Lkt_Lexer_Case_Rule => Type_Refs.Lexer_Case_Rule,
      Lkt_Lexer_Case_Rule_Send => Type_Refs.Lexer_Case_Rule_Send,
      Lkt_List_Kind_One => Type_Refs.List_Kind_One,
      Lkt_List_Kind_Zero => Type_Refs.List_Kind_Zero,
      Lkt_Argument_List => Type_Refs.Argument_List,
      Lkt_Base_Import_List => Type_Refs.Base_Import_List,
      Lkt_Base_Lexer_Case_Rule_Alt_List => Type_Refs.Base_Lexer_Case_Rule_Alt_List,
      Lkt_Base_Match_Branch_List => Type_Refs.Base_Match_Branch_List,
      Lkt_Block_String_Line_List => Type_Refs.Block_String_Line_List,
      Lkt_Call_Expr_List => Type_Refs.Call_Expr_List,
      Lkt_Decl_Annotation_List => Type_Refs.Decl_Annotation_List,
      Lkt_Elsif_Branch_List => Type_Refs.Elsif_Branch_List,
      Lkt_Enum_Class_Alt_Decl_List => Type_Refs.Enum_Class_Alt_Decl_List,
      Lkt_Enum_Class_Case_List => Type_Refs.Enum_Class_Case_List,
      Lkt_Enum_Lit_Decl_List => Type_Refs.Enum_Lit_Decl_List,
      Lkt_Expr_List => Type_Refs.Expr_List,
      Lkt_Any_Of_List => Type_Refs.Any_Of_List,
      Lkt_Full_Decl_List => Type_Refs.Full_Decl_List,
      Lkt_Decl_Block => Type_Refs.Decl_Block,
      Lkt_Generic_Param_Decl_List => Type_Refs.Generic_Param_Decl_List,
      Lkt_Fun_Param_Decl_List => Type_Refs.Fun_Param_Decl_List,
      Lkt_Grammar_Expr_List => Type_Refs.Grammar_Expr_List,
      Lkt_Grammar_Expr_List_List => Type_Refs.Grammar_Expr_List_List,
      Lkt_Imported_Name_List => Type_Refs.Imported_Name_List,
      Lkt_Lambda_Param_Decl_List => Type_Refs.Lambda_Param_Decl_List,
      Lkt_Lkt_Node_List => Type_Refs.Lkt_Node_List,
      Lkt_Module_Doc_String_Line_List => Type_Refs.Module_Doc_String_Line_List,
      Lkt_Pattern_Detail_List => Type_Refs.Pattern_Detail_List,
      Lkt_Pattern_List => Type_Refs.Pattern_List,
      Lkt_Ref_Id_List => Type_Refs.Ref_Id_List,
      Lkt_Type_Ref_List => Type_Refs.Type_Ref_List,
      Lkt_Synthetic_Type_Ref_List => Type_Refs.Synthetic_Type_Ref_List,
      Lkt_Module_Doc_String_Line => Type_Refs.Module_Doc_String_Line,
      Lkt_Null_Cond_Qualifier_Absent => Type_Refs.Null_Cond_Qualifier_Absent,
      Lkt_Null_Cond_Qualifier_Present => Type_Refs.Null_Cond_Qualifier_Present,
      Lkt_Op_Amp => Type_Refs.Op_Amp,
      Lkt_Op_And => Type_Refs.Op_And,
      Lkt_Op_Div => Type_Refs.Op_Div,
      Lkt_Op_Eq => Type_Refs.Op_Eq,
      Lkt_Op_Gt => Type_Refs.Op_Gt,
      Lkt_Op_Gte => Type_Refs.Op_Gte,
      Lkt_Op_Logic_And => Type_Refs.Op_Logic_And,
      Lkt_Op_Logic_Or => Type_Refs.Op_Logic_Or,
      Lkt_Op_Lt => Type_Refs.Op_Lt,
      Lkt_Op_Lte => Type_Refs.Op_Lte,
      Lkt_Op_Minus => Type_Refs.Op_Minus,
      Lkt_Op_Mult => Type_Refs.Op_Mult,
      Lkt_Op_Ne => Type_Refs.Op_Ne,
      Lkt_Op_Or => Type_Refs.Op_Or,
      Lkt_Op_Or_Int => Type_Refs.Op_Or_Int,
      Lkt_Op_Plus => Type_Refs.Op_Plus,
      Lkt_Op_Stream_Concat => Type_Refs.Op_Stream_Concat,
      Lkt_Op_Stream_Cons => Type_Refs.Op_Stream_Cons,
      Lkt_Any_Type_Pattern => Type_Refs.Any_Type_Pattern,
      Lkt_Binding_Pattern => Type_Refs.Binding_Pattern,
      Lkt_Bool_Pattern_False => Type_Refs.Bool_Pattern_False,
      Lkt_Bool_Pattern_True => Type_Refs.Bool_Pattern_True,
      Lkt_Ellipsis_Pattern => Type_Refs.Ellipsis_Pattern,
      Lkt_Extended_Pattern => Type_Refs.Extended_Pattern,
      Lkt_Filtered_Pattern => Type_Refs.Filtered_Pattern,
      Lkt_Integer_Pattern => Type_Refs.Integer_Pattern,
      Lkt_List_Pattern => Type_Refs.List_Pattern,
      Lkt_Not_Pattern => Type_Refs.Not_Pattern,
      Lkt_Null_Pattern => Type_Refs.Null_Pattern,
      Lkt_Or_Pattern => Type_Refs.Or_Pattern,
      Lkt_Paren_Pattern => Type_Refs.Paren_Pattern,
      Lkt_Regex_Pattern => Type_Refs.Regex_Pattern,
      Lkt_Tuple_Pattern => Type_Refs.Tuple_Pattern,
      Lkt_Type_Pattern => Type_Refs.Type_Pattern,
      Lkt_Field_Pattern_Detail => Type_Refs.Field_Pattern_Detail,
      Lkt_Property_Pattern_Detail => Type_Refs.Property_Pattern_Detail,
      Lkt_Selector_Pattern_Detail => Type_Refs.Selector_Pattern_Detail,
      Lkt_Selector_Call => Type_Refs.Selector_Call,
      Lkt_Default_List_Type_Ref => Type_Refs.Default_List_Type_Ref,
      Lkt_Function_Type_Ref => Type_Refs.Function_Type_Ref,
      Lkt_Generic_Type_Ref => Type_Refs.Generic_Type_Ref,
      Lkt_Simple_Type_Ref => Type_Refs.Simple_Type_Ref,
      Lkt_Var_Bind => Type_Refs.Var_Bind);

   -----------------------
   -- Member references --
   -----------------------

   package Member_Refs is
         Complete_Item_Declaration : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 1);
         Decoded_Char_Value_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 2);
         Decoded_Char_Value_Has_Error : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 3);
         Decoded_Char_Value_Error_Sloc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 4);
         Decoded_Char_Value_Error_Message : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 5);
         Decoded_String_Value_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 6);
         Decoded_String_Value_Has_Error : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 7);
         Decoded_String_Value_Error_Sloc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 8);
         Decoded_String_Value_Error_Message : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 9);
         Logic_Context_Ref_Node : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 10);
         Logic_Context_Decl_Node : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 11);
         Ref_Result_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 12);
         Solver_Diagnostic_Message_Template : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 13);
         Solver_Diagnostic_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 14);
         Solver_Diagnostic_Location : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 15);
         Solver_Diagnostic_Contexts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 16);
         Solver_Diagnostic_Round : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 17);
         Solver_Result_Success : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 18);
         Solver_Result_Diagnostics : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 19);
         Argument_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 20);
         Argument_F_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 21);
         Base_Import_F_Module_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 22);
         Import_F_Renaming : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 23);
         Import_From_F_Imported_Names : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 24);
         Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 25);
         Lexer_Case_Rule_Cond_Alt_F_Send : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 26);
         Lexer_Case_Rule_Default_Alt_F_Send : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 27);
         Base_Match_Branch_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 28);
         Match_Branch_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 29);
         Pattern_Match_Branch_F_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 30);
         Block_Expr_Clause_F_Clause : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 31);
         Decl_F_Syn_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 32);
         Base_Grammar_Rule_Decl_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 33);
         Explicitly_Typed_Decl_F_Decl_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 34);
         Component_Decl_F_Default_Val : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 35);
         Field_Decl_F_Trait_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 36);
         Fun_Param_Decl_F_Decl_Annotations : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 37);
         Val_Decl_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 38);
         Fun_Decl_F_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 39);
         Fun_Decl_F_Return_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 40);
         Fun_Decl_F_Trait_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 41);
         Fun_Decl_F_Body : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 42);
         Env_Spec_Decl_F_Actions : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 43);
         Generic_Decl_F_Generic_Param_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 44);
         Generic_Decl_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 45);
         Grammar_Decl_F_Rules : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 46);
         Lexer_Decl_F_Rules : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 47);
         Lexer_Family_Decl_F_Rules : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 48);
         Type_Decl_F_Traits : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 49);
         Type_Decl_F_Syn_Base_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 50);
         Generic_Param_Type_Decl_F_Has_Class : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 51);
         Named_Type_Decl_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 52);
         Enum_Class_Decl_F_Branches : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 53);
         Enum_Type_Decl_F_Literals : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 54);
         Decl_Annotation_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 55);
         Decl_Annotation_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 56);
         Decl_Annotation_Args_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 57);
         Elsif_Branch_F_Cond_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 58);
         Elsif_Branch_F_Then_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 59);
         Enum_Class_Case_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 60);
         Any_Of_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 61);
         Any_Of_F_Values : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 62);
         Array_Literal_F_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 63);
         Array_Literal_F_Element_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 64);
         Base_Call_Expr_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 65);
         Base_Call_Expr_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 66);
         Bin_Op_F_Left : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 67);
         Bin_Op_F_Op : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 68);
         Bin_Op_F_Right : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 69);
         Block_Expr_F_Clauses : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 70);
         Cast_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 71);
         Cast_Expr_F_Null_Cond : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 72);
         Cast_Expr_F_Excludes_Null : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 73);
         Cast_Expr_F_Dest_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 74);
         Dot_Expr_F_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 75);
         Dot_Expr_F_Null_Cond : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 76);
         Dot_Expr_F_Suffix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 77);
         Error_On_Null_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 78);
         Generic_Instantiation_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 79);
         Generic_Instantiation_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 80);
         Grammar_Discard_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 81);
         Grammar_Dont_Skip_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 82);
         Grammar_Dont_Skip_F_Dont_Skip : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 83);
         Grammar_List_F_List_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 84);
         Grammar_List_F_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 85);
         Grammar_List_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 86);
         Grammar_List_F_Sep : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 87);
         Grammar_Null_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 88);
         Grammar_Opt_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 89);
         Grammar_Opt_Error_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 90);
         Grammar_Opt_Error_Group_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 91);
         Grammar_Opt_Group_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 92);
         Grammar_Or_Expr_F_Sub_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 93);
         Grammar_Pick_F_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 94);
         Grammar_Predicate_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 95);
         Grammar_Predicate_F_Prop_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 96);
         Grammar_Rule_Ref_F_Node_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 97);
         Grammar_Skip_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 98);
         Grammar_Stop_Cut_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 99);
         Parse_Node_Expr_F_Node_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 100);
         Parse_Node_Expr_F_Sub_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 101);
         Token_No_Case_Lit_F_Lit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 102);
         Token_Pattern_Concat_F_Left : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 103);
         Token_Pattern_Concat_F_Right : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 104);
         Token_Ref_F_Token_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 105);
         Token_Ref_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 106);
         If_Expr_F_Cond_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 107);
         If_Expr_F_Then_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 108);
         If_Expr_F_Alternatives : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 109);
         If_Expr_F_Else_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 110);
         Isa_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 111);
         Isa_F_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 112);
         Keep_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 113);
         Keep_Expr_F_Null_Cond : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 114);
         Keep_Expr_F_Keep_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 115);
         Lambda_Expr_F_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 116);
         Lambda_Expr_F_Return_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 117);
         Lambda_Expr_F_Body : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 118);
         Null_Lit_F_Dest_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 119);
         Block_String_Lit_F_Lines : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 120);
         Module_Doc_String_Lit_F_Lines : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 121);
         Logic_Assign_F_Dest_Var : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 122);
         Logic_Assign_F_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 123);
         Logic_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 124);
         Logic_Propagate_F_Dest_Var : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 125);
         Logic_Propagate_F_Call : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 126);
         Logic_Unify_F_Lhs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 127);
         Logic_Unify_F_Rhs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 128);
         Match_Expr_F_Match_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 129);
         Match_Expr_F_Branches : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 130);
         Not_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 131);
         Paren_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 132);
         Query_F_Source : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 133);
         Query_F_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 134);
         Query_F_Mapping : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 135);
         Query_F_Guard : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 136);
         Raise_Expr_F_Dest_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 137);
         Raise_Expr_F_Except_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 138);
         Subscript_Expr_F_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 139);
         Subscript_Expr_F_Null_Cond : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 140);
         Subscript_Expr_F_Index : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 141);
         Try_Expr_F_Try_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 142);
         Try_Expr_F_Or_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 143);
         Un_Op_F_Op : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 144);
         Un_Op_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 145);
         Full_Decl_F_Doc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 146);
         Full_Decl_F_Decl_Annotations : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 147);
         Full_Decl_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 148);
         Grammar_List_Sep_F_Token : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 149);
         Grammar_List_Sep_F_Extra : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 150);
         Imported_Name_F_Original_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 151);
         Imported_Name_F_Renaming : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 152);
         Langkit_Root_F_Doc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 153);
         Langkit_Root_F_Imports : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 154);
         Langkit_Root_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 155);
         Lexer_Case_Rule_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 156);
         Lexer_Case_Rule_F_Alts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 157);
         Lexer_Case_Rule_Send_F_Sent : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 158);
         Lexer_Case_Rule_Send_F_Match_Size : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 159);
         Binding_Pattern_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 160);
         Binding_Pattern_F_Sub_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 161);
         Ellipsis_Pattern_F_Binding : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 162);
         Extended_Pattern_F_Sub_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 163);
         Extended_Pattern_F_Details : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 164);
         Filtered_Pattern_F_Sub_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 165);
         Filtered_Pattern_F_Predicate : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 166);
         List_Pattern_F_Sub_Patterns : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 167);
         Not_Pattern_F_Sub_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 168);
         Or_Pattern_F_Left_Sub_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 169);
         Or_Pattern_F_Right_Sub_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 170);
         Paren_Pattern_F_Sub_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 171);
         Tuple_Pattern_F_Sub_Patterns : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 172);
         Type_Pattern_F_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 173);
         Field_Pattern_Detail_F_Id : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 174);
         Field_Pattern_Detail_F_Expected_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 175);
         Property_Pattern_Detail_F_Call : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 176);
         Property_Pattern_Detail_F_Expected_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 177);
         Selector_Pattern_Detail_F_Call : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 178);
         Selector_Pattern_Detail_F_Sub_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 179);
         Selector_Call_F_Quantifier : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 180);
         Selector_Call_F_Binding : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 181);
         Selector_Call_F_Selector_Call : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 182);
         Function_Type_Ref_F_Param_Types : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 183);
         Function_Type_Ref_F_Return_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 184);
         Generic_Type_Ref_F_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 185);
         Generic_Type_Ref_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 186);
         Simple_Type_Ref_F_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 187);
         Var_Bind_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 188);
         Var_Bind_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 189);
         Parent : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 190);
         Parents : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 191);
         Children : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 192);
         Token_Start : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 193);
         Token_End : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 194);
         Child_Index : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 195);
         Previous_Sibling : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 196);
         Next_Sibling : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 197);
         Unit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 198);
         Is_Ghost : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 199);
         Full_Sloc_Image : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 200);
         Completion_Item_Kind_To_Int : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 201);
         Lkt_Node_P_Set_Solver_Debug_Mode : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 202);
         Lkt_Node_P_Basic_Trait_Gen : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 203);
         Lkt_Node_P_Basic_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 204);
         Lkt_Node_P_Node_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 205);
         Lkt_Node_P_Node_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 206);
         Lkt_Node_P_Indexable_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 207);
         Lkt_Node_P_Indexable_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 208);
         Lkt_Node_P_Token_Node_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 209);
         Lkt_Node_P_Error_Node_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 210);
         Lkt_Node_P_Char_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 211);
         Lkt_Node_P_Int_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 212);
         Lkt_Node_P_Bool_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 213);
         Lkt_Node_P_Bigint_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 214);
         Lkt_Node_P_String_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 215);
         Lkt_Node_P_Symbol_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 216);
         Lkt_Node_P_Property_Error_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 217);
         Lkt_Node_P_Regexp_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 218);
         Lkt_Node_P_Entity_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 219);
         Lkt_Node_P_Entity_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 220);
         Lkt_Node_P_Logicvar_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 221);
         Lkt_Node_P_Equation_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 222);
         Lkt_Node_P_Array_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 223);
         Lkt_Node_P_Array_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 224);
         Lkt_Node_P_Astlist_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 225);
         Lkt_Node_P_Astlist_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 226);
         Lkt_Node_P_Node_Builder_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 227);
         Lkt_Node_P_Node_Builder_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 228);
         Lkt_Node_P_Iterator_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 229);
         Lkt_Node_P_Iterator_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 230);
         Lkt_Node_P_Analysis_Unit_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 231);
         Lkt_Node_P_Analysis_Unit_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 232);
         Lkt_Node_P_Topmost_Invalid_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 233);
         Lkt_Node_P_Nameres_Diagnostics : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 234);
         Lkt_Node_P_Solve_Enclosing_Context : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 235);
         Lkt_Node_P_Xref_Entry_Point : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 236);
         Lkt_Node_P_Complete : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 237);
         Base_Import_P_Referenced_Unit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 238);
         Base_Match_Branch_P_Match_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 239);
         Class_Qualifier_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 240);
         Decl_P_Custom_Image : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 241);
         Decl_P_Decl_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 242);
         Decl_P_Def_Ids : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 243);
         Decl_P_As_Bare_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 244);
         Decl_P_Get_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 245);
         Decl_P_Get_Cast_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 246);
         Decl_P_Get_Keep_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 247);
         Decl_P_Get_Suffix_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 248);
         Decl_P_Is_Generic : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 249);
         Decl_P_Return_Type_Is_Instantiated : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 250);
         Decl_P_Is_Instantiated : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 251);
         Decl_P_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 252);
         Decl_P_Full_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 253);
         Fun_Decl_P_Is_Dynamic_Combiner : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 254);
         Fun_Decl_P_Find_All_Overrides : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 255);
         Type_Decl_P_Def_Id : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 256);
         Type_Decl_P_Base_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 257);
         Type_Decl_P_Base_Type_If_Entity : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 258);
         Excludes_Null_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 259);
         Expr_P_Get_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 260);
         Expr_P_Get_Generic_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 261);
         Expr_P_Get_Expected_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 262);
         Expr_P_Referenced_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 263);
         Token_Lit_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 264);
         Token_Pattern_Lit_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 265);
         Id_P_Custom_Image : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 266);
         Def_Id_P_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 267);
         Def_Id_P_Get_Implementatinons : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 268);
         Def_Id_P_Decl_Detail : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 269);
         Def_Id_P_Completion_Item_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 270);
         Def_Id_P_Doc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 271);
         Def_Id_P_Find_All_References : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 272);
         Ref_Id_P_Referenced_Defining_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 273);
         Char_Lit_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 274);
         String_Lit_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 275);
         String_Lit_P_Is_Prefixed_String : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 276);
         String_Lit_P_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 277);
         String_Lit_P_Is_Regexp_Literal : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 278);
         Full_Decl_P_Has_Annotation : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 279);
         Langkit_Root_P_Fetch_Prelude : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 280);
         Null_Cond_Qualifier_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 281);
         Type_Ref_P_Referenced_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 282);
   end Member_Refs;

end Liblktlang.Generic_API.Introspection;
