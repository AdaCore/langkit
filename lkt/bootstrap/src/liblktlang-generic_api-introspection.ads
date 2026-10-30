
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
           --  @exclude-value
         Big_Integer : constant G.Type_Ref :=
           G.From_Index (Self_Id, 2);
           --  @exclude-value
         Boolean : constant G.Type_Ref :=
           G.From_Index (Self_Id, 3);
           --  @exclude-value
         Character_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 4);
           --  @exclude-value
         Integer : constant G.Type_Ref :=
           G.From_Index (Self_Id, 5);
           --  @exclude-value
         Source_Location : constant G.Type_Ref :=
           G.From_Index (Self_Id, 6);
           --  @exclude-value
         Source_Location_Range : constant G.Type_Ref :=
           G.From_Index (Self_Id, 7);
           --  @exclude-value
         Text_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 8);
           --  @exclude-value
         Token_Reference : constant G.Type_Ref :=
           G.From_Index (Self_Id, 9);
           --  @exclude-value
         Unbounded_Text_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 10);
           --  @exclude-value
         Analysis_Unit_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 11);
           --  @exclude-value
         Completion_Item_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 12);
           --  @exclude-value
         Designated_Env_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 13);
           --  @exclude-value
         Grammar_Rule : constant G.Type_Ref :=
           G.From_Index (Self_Id, 14);
           --  @exclude-value
         Language_Mode : constant G.Type_Ref :=
           G.From_Index (Self_Id, 15);
           --  @exclude-value
         Lookup_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 16);
           --  @exclude-value
         Complete_Item_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 17);
           --  @exclude-value
         Lkt_Node_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 18);
           --  @exclude-value
         Def_Id_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 19);
           --  @exclude-value
         Fun_Decl_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 20);
           --  @exclude-value
         Logic_Context_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 21);
           --  @exclude-value
         Ref_Result_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 22);
           --  @exclude-value
         Solver_Diagnostic_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 23);
           --  @exclude-value
         Analysis_Unit_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 24);
           --  @exclude-value
         Complete_Item : constant G.Type_Ref :=
           G.From_Index (Self_Id, 25);
           --  @exclude-value
         Decoded_Char_Value : constant G.Type_Ref :=
           G.From_Index (Self_Id, 26);
           --  @exclude-value
         Decoded_String_Value : constant G.Type_Ref :=
           G.From_Index (Self_Id, 27);
           --  @exclude-value
         Logic_Context : constant G.Type_Ref :=
           G.From_Index (Self_Id, 28);
           --  @exclude-value
         Ref_Result : constant G.Type_Ref :=
           G.From_Index (Self_Id, 29);
           --  @exclude-value
         Solver_Diagnostic : constant G.Type_Ref :=
           G.From_Index (Self_Id, 30);
           --  @exclude-value
         Solver_Result : constant G.Type_Ref :=
           G.From_Index (Self_Id, 31);
           --  @exclude-value
         Lkt_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 32);
           --  @exclude-value
         Argument : constant G.Type_Ref :=
           G.From_Index (Self_Id, 33);
           --  @exclude-value
         Base_Import : constant G.Type_Ref :=
           G.From_Index (Self_Id, 34);
           --  @exclude-value
         Import : constant G.Type_Ref :=
           G.From_Index (Self_Id, 35);
           --  @exclude-value
         Import_All_From : constant G.Type_Ref :=
           G.From_Index (Self_Id, 36);
           --  @exclude-value
         Import_From : constant G.Type_Ref :=
           G.From_Index (Self_Id, 37);
           --  @exclude-value
         Base_Lexer_Case_Rule_Alt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 38);
           --  @exclude-value
         Error_Lexer_Case_Rule_Alt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 39);
           --  @exclude-value
         Lexer_Case_Rule_Cond_Alt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 40);
           --  @exclude-value
         Lexer_Case_Rule_Default_Alt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 41);
           --  @exclude-value
         Base_Match_Branch : constant G.Type_Ref :=
           G.From_Index (Self_Id, 42);
           --  @exclude-value
         Match_Branch : constant G.Type_Ref :=
           G.From_Index (Self_Id, 43);
           --  @exclude-value
         Pattern_Match_Branch : constant G.Type_Ref :=
           G.From_Index (Self_Id, 44);
           --  @exclude-value
         Block_Expr_Clause : constant G.Type_Ref :=
           G.From_Index (Self_Id, 45);
           --  @exclude-value
         Block_String_Line : constant G.Type_Ref :=
           G.From_Index (Self_Id, 46);
           --  @exclude-value
         Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 47);
           --  @exclude-value
         Base_Grammar_Rule_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 48);
           --  @exclude-value
         Grammar_Rule_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 49);
           --  @exclude-value
         Synthetic_Lexer_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 50);
           --  @exclude-value
         Base_Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 51);
           --  @exclude-value
         Node_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 52);
           --  @exclude-value
         Self_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 53);
           --  @exclude-value
         User_Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 54);
           --  @exclude-value
         Binding_Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 55);
           --  @exclude-value
         Enum_Lit_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 56);
           --  @exclude-value
         Explicitly_Typed_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 57);
           --  @exclude-value
         Component_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 58);
           --  @exclude-value
         Field_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 59);
           --  @exclude-value
         Fun_Param_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 60);
           --  @exclude-value
         Lambda_Param_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 61);
           --  @exclude-value
         Dyn_Var_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 62);
           --  @exclude-value
         Match_Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 63);
           --  @exclude-value
         Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 64);
           --  @exclude-value
         Fun_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 65);
           --  @exclude-value
         Env_Spec_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 66);
           --  @exclude-value
         Error_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 67);
           --  @exclude-value
         Generic_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 68);
           --  @exclude-value
         Grammar_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 69);
           --  @exclude-value
         Langkit_Root : constant G.Type_Ref :=
           G.From_Index (Self_Id, 70);
           --  @exclude-value
         Lexer_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 71);
           --  @exclude-value
         Lexer_Family_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 72);
           --  @exclude-value
         Synth_Fun_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 73);
           --  @exclude-value
         Synth_Param_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 74);
           --  @exclude-value
         Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 75);
           --  @exclude-value
         Any_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 76);
           --  @exclude-value
         Base_Generic_Param_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 77);
           --  @exclude-value
         Generic_Class_Param_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 78);
           --  @exclude-value
         Generic_Param_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 79);
           --  @exclude-value
         Enum_Class_Alt_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 80);
           --  @exclude-value
         Function_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 81);
           --  @exclude-value
         Named_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 82);
           --  @exclude-value
         Basic_Class_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 83);
           --  @exclude-value
         Class_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 84);
           --  @exclude-value
         Enum_Class_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 85);
           --  @exclude-value
         Enum_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 86);
           --  @exclude-value
         Struct_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 87);
           --  @exclude-value
         Trait_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 88);
           --  @exclude-value
         Decl_Annotation : constant G.Type_Ref :=
           G.From_Index (Self_Id, 89);
           --  @exclude-value
         Decl_Annotation_Args : constant G.Type_Ref :=
           G.From_Index (Self_Id, 90);
           --  @exclude-value
         Dyn_Env_Wrapper : constant G.Type_Ref :=
           G.From_Index (Self_Id, 91);
           --  @exclude-value
         Elsif_Branch : constant G.Type_Ref :=
           G.From_Index (Self_Id, 92);
           --  @exclude-value
         Enum_Class_Case : constant G.Type_Ref :=
           G.From_Index (Self_Id, 93);
           --  @exclude-value
         Excludes_Null : constant G.Type_Ref :=
           G.From_Index (Self_Id, 94);
           --  @exclude-value
         Excludes_Null_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 95);
           --  @exclude-value
         Excludes_Null_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 96);
           --  @exclude-value
         Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 97);
           --  @exclude-value
         Any_Of : constant G.Type_Ref :=
           G.From_Index (Self_Id, 98);
           --  @exclude-value
         Array_Literal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 99);
           --  @exclude-value
         Base_Call_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 100);
           --  @exclude-value
         Call_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 101);
           --  @exclude-value
         Logic_Call_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 102);
           --  @exclude-value
         Logic_Predicate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 103);
           --  @exclude-value
         Logic_Propagate_Call : constant G.Type_Ref :=
           G.From_Index (Self_Id, 104);
           --  @exclude-value
         Bin_Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 105);
           --  @exclude-value
         Block_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 106);
           --  @exclude-value
         Cast_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 107);
           --  @exclude-value
         Dot_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 108);
           --  @exclude-value
         Error_On_Null : constant G.Type_Ref :=
           G.From_Index (Self_Id, 109);
           --  @exclude-value
         Generic_Instantiation : constant G.Type_Ref :=
           G.From_Index (Self_Id, 110);
           --  @exclude-value
         Grammar_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 111);
           --  @exclude-value
         Error_Grammar_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 112);
           --  @exclude-value
         Grammar_Cut : constant G.Type_Ref :=
           G.From_Index (Self_Id, 113);
           --  @exclude-value
         Grammar_Discard : constant G.Type_Ref :=
           G.From_Index (Self_Id, 114);
           --  @exclude-value
         Grammar_Dont_Skip : constant G.Type_Ref :=
           G.From_Index (Self_Id, 115);
           --  @exclude-value
         Grammar_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 116);
           --  @exclude-value
         Grammar_Null : constant G.Type_Ref :=
           G.From_Index (Self_Id, 117);
           --  @exclude-value
         Grammar_Opt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 118);
           --  @exclude-value
         Grammar_Opt_Error : constant G.Type_Ref :=
           G.From_Index (Self_Id, 119);
           --  @exclude-value
         Grammar_Opt_Error_Group : constant G.Type_Ref :=
           G.From_Index (Self_Id, 120);
           --  @exclude-value
         Grammar_Opt_Group : constant G.Type_Ref :=
           G.From_Index (Self_Id, 121);
           --  @exclude-value
         Grammar_Or_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 122);
           --  @exclude-value
         Grammar_Pick : constant G.Type_Ref :=
           G.From_Index (Self_Id, 123);
           --  @exclude-value
         Grammar_Implicit_Pick : constant G.Type_Ref :=
           G.From_Index (Self_Id, 124);
           --  @exclude-value
         Grammar_Predicate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 125);
           --  @exclude-value
         Grammar_Rule_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 126);
           --  @exclude-value
         Grammar_Skip : constant G.Type_Ref :=
           G.From_Index (Self_Id, 127);
           --  @exclude-value
         Grammar_Stop_Cut : constant G.Type_Ref :=
           G.From_Index (Self_Id, 128);
           --  @exclude-value
         Parse_Node_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 129);
           --  @exclude-value
         Token_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 130);
           --  @exclude-value
         Token_No_Case_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 131);
           --  @exclude-value
         Token_Pattern_Concat : constant G.Type_Ref :=
           G.From_Index (Self_Id, 132);
           --  @exclude-value
         Token_Pattern_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 133);
           --  @exclude-value
         Token_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 134);
           --  @exclude-value
         Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 135);
           --  @exclude-value
         Def_Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 136);
           --  @exclude-value
         Imported_Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 137);
           --  @exclude-value
         Module_Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 138);
           --  @exclude-value
         Ref_Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 139);
           --  @exclude-value
         If_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 140);
           --  @exclude-value
         Isa : constant G.Type_Ref :=
           G.From_Index (Self_Id, 141);
           --  @exclude-value
         Keep_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 142);
           --  @exclude-value
         Lambda_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 143);
           --  @exclude-value
         Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 144);
           --  @exclude-value
         Big_Num_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 145);
           --  @exclude-value
         Char_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 146);
           --  @exclude-value
         Null_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 147);
           --  @exclude-value
         Num_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 148);
           --  @exclude-value
         String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 149);
           --  @exclude-value
         Block_String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 150);
           --  @exclude-value
         Module_Doc_String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 151);
           --  @exclude-value
         Single_Line_String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 152);
           --  @exclude-value
         Pattern_Single_Line_String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 153);
           --  @exclude-value
         Logic_Assign : constant G.Type_Ref :=
           G.From_Index (Self_Id, 154);
           --  @exclude-value
         Logic_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 155);
           --  @exclude-value
         Logic_Propagate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 156);
           --  @exclude-value
         Logic_Unify : constant G.Type_Ref :=
           G.From_Index (Self_Id, 157);
           --  @exclude-value
         Match_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 158);
           --  @exclude-value
         Not_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 159);
           --  @exclude-value
         Paren_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 160);
           --  @exclude-value
         Query : constant G.Type_Ref :=
           G.From_Index (Self_Id, 161);
           --  @exclude-value
         Raise_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 162);
           --  @exclude-value
         Subscript_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 163);
           --  @exclude-value
         Try_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 164);
           --  @exclude-value
         Un_Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 165);
           --  @exclude-value
         Full_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 166);
           --  @exclude-value
         Grammar_List_Sep : constant G.Type_Ref :=
           G.From_Index (Self_Id, 167);
           --  @exclude-value
         Imported_Name : constant G.Type_Ref :=
           G.From_Index (Self_Id, 168);
           --  @exclude-value
         Lexer_Case_Rule : constant G.Type_Ref :=
           G.From_Index (Self_Id, 169);
           --  @exclude-value
         Lexer_Case_Rule_Send : constant G.Type_Ref :=
           G.From_Index (Self_Id, 170);
           --  @exclude-value
         List_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 171);
           --  @exclude-value
         List_Kind_One : constant G.Type_Ref :=
           G.From_Index (Self_Id, 172);
           --  @exclude-value
         List_Kind_Zero : constant G.Type_Ref :=
           G.From_Index (Self_Id, 173);
           --  @exclude-value
         Lkt_Node_Base_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 174);
           --  @exclude-value
         Argument_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 175);
           --  @exclude-value
         Base_Import_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 176);
           --  @exclude-value
         Base_Lexer_Case_Rule_Alt_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 177);
           --  @exclude-value
         Base_Match_Branch_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 178);
           --  @exclude-value
         Block_String_Line_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 179);
           --  @exclude-value
         Call_Expr_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 180);
           --  @exclude-value
         Decl_Annotation_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 181);
           --  @exclude-value
         Elsif_Branch_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 182);
           --  @exclude-value
         Enum_Class_Alt_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 183);
           --  @exclude-value
         Enum_Class_Case_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 184);
           --  @exclude-value
         Enum_Lit_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 185);
           --  @exclude-value
         Expr_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 186);
           --  @exclude-value
         Any_Of_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 187);
           --  @exclude-value
         Full_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 188);
           --  @exclude-value
         Decl_Block : constant G.Type_Ref :=
           G.From_Index (Self_Id, 189);
           --  @exclude-value
         Generic_Param_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 190);
           --  @exclude-value
         Fun_Param_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 191);
           --  @exclude-value
         Grammar_Expr_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 192);
           --  @exclude-value
         Grammar_Expr_List_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 193);
           --  @exclude-value
         Imported_Name_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 194);
           --  @exclude-value
         Lambda_Param_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 195);
           --  @exclude-value
         Lkt_Node_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 196);
           --  @exclude-value
         Module_Doc_String_Line_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 197);
           --  @exclude-value
         Pattern_Detail_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 198);
           --  @exclude-value
         Pattern_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 199);
           --  @exclude-value
         Ref_Id_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 200);
           --  @exclude-value
         Type_Ref_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 201);
           --  @exclude-value
         Synthetic_Type_Ref_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 202);
           --  @exclude-value
         Module_Doc_String_Line : constant G.Type_Ref :=
           G.From_Index (Self_Id, 203);
           --  @exclude-value
         Null_Cond_Qualifier : constant G.Type_Ref :=
           G.From_Index (Self_Id, 204);
           --  @exclude-value
         Null_Cond_Qualifier_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 205);
           --  @exclude-value
         Null_Cond_Qualifier_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 206);
           --  @exclude-value
         Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 207);
           --  @exclude-value
         Op_Amp : constant G.Type_Ref :=
           G.From_Index (Self_Id, 208);
           --  @exclude-value
         Op_And : constant G.Type_Ref :=
           G.From_Index (Self_Id, 209);
           --  @exclude-value
         Op_Div : constant G.Type_Ref :=
           G.From_Index (Self_Id, 210);
           --  @exclude-value
         Op_Eq : constant G.Type_Ref :=
           G.From_Index (Self_Id, 211);
           --  @exclude-value
         Op_Gt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 212);
           --  @exclude-value
         Op_Gte : constant G.Type_Ref :=
           G.From_Index (Self_Id, 213);
           --  @exclude-value
         Op_Logic_And : constant G.Type_Ref :=
           G.From_Index (Self_Id, 214);
           --  @exclude-value
         Op_Logic_Or : constant G.Type_Ref :=
           G.From_Index (Self_Id, 215);
           --  @exclude-value
         Op_Lt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 216);
           --  @exclude-value
         Op_Lte : constant G.Type_Ref :=
           G.From_Index (Self_Id, 217);
           --  @exclude-value
         Op_Minus : constant G.Type_Ref :=
           G.From_Index (Self_Id, 218);
           --  @exclude-value
         Op_Mult : constant G.Type_Ref :=
           G.From_Index (Self_Id, 219);
           --  @exclude-value
         Op_Ne : constant G.Type_Ref :=
           G.From_Index (Self_Id, 220);
           --  @exclude-value
         Op_Or : constant G.Type_Ref :=
           G.From_Index (Self_Id, 221);
           --  @exclude-value
         Op_Or_Int : constant G.Type_Ref :=
           G.From_Index (Self_Id, 222);
           --  @exclude-value
         Op_Plus : constant G.Type_Ref :=
           G.From_Index (Self_Id, 223);
           --  @exclude-value
         Op_Stream_Concat : constant G.Type_Ref :=
           G.From_Index (Self_Id, 224);
           --  @exclude-value
         Op_Stream_Cons : constant G.Type_Ref :=
           G.From_Index (Self_Id, 225);
           --  @exclude-value
         Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 226);
           --  @exclude-value
         Any_Type_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 227);
           --  @exclude-value
         Binding_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 228);
           --  @exclude-value
         Complex_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 229);
           --  @exclude-value
         Renaming_Complex_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 230);
           --  @exclude-value
         Ellipsis_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 231);
           --  @exclude-value
         Bool_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 232);
           --  @exclude-value
         Bool_Pattern_False : constant G.Type_Ref :=
           G.From_Index (Self_Id, 233);
           --  @exclude-value
         Bool_Pattern_True : constant G.Type_Ref :=
           G.From_Index (Self_Id, 234);
           --  @exclude-value
         Integer_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 235);
           --  @exclude-value
         List_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 236);
           --  @exclude-value
         Null_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 237);
           --  @exclude-value
         Or_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 238);
           --  @exclude-value
         Paren_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 239);
           --  @exclude-value
         Regex_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 240);
           --  @exclude-value
         Scoped_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 241);
           --  @exclude-value
         Not_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 242);
           --  @exclude-value
         Type_Pattern : constant G.Type_Ref :=
           G.From_Index (Self_Id, 243);
           --  @exclude-value
         Pattern_Detail : constant G.Type_Ref :=
           G.From_Index (Self_Id, 244);
           --  @exclude-value
         Destructuring_Pattern_Detail : constant G.Type_Ref :=
           G.From_Index (Self_Id, 245);
           --  @exclude-value
         Field_Pattern_Detail : constant G.Type_Ref :=
           G.From_Index (Self_Id, 246);
           --  @exclude-value
         Property_Pattern_Detail : constant G.Type_Ref :=
           G.From_Index (Self_Id, 247);
           --  @exclude-value
         Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 248);
           --  @exclude-value
         Default_List_Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 249);
           --  @exclude-value
         Function_Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 250);
           --  @exclude-value
         Generic_Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 251);
           --  @exclude-value
         Simple_Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 252);
           --  @exclude-value
         Var_Bind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 253);
           --  @exclude-value
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
      Lkt_Langkit_Root => Type_Refs.Langkit_Root,
      Lkt_Lexer_Decl => Type_Refs.Lexer_Decl,
      Lkt_Lexer_Family_Decl => Type_Refs.Lexer_Family_Decl,
      Lkt_Synth_Fun_Decl => Type_Refs.Synth_Fun_Decl,
      Lkt_Synth_Param_Decl => Type_Refs.Synth_Param_Decl,
      Lkt_Any_Type_Decl => Type_Refs.Any_Type_Decl,
      Lkt_Generic_Class_Param_Type_Decl => Type_Refs.Generic_Class_Param_Type_Decl,
      Lkt_Generic_Param_Type_Decl => Type_Refs.Generic_Param_Type_Decl,
      Lkt_Enum_Class_Alt_Decl => Type_Refs.Enum_Class_Alt_Decl,
      Lkt_Function_Type => Type_Refs.Function_Type,
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
      Lkt_Complex_Pattern => Type_Refs.Complex_Pattern,
      Lkt_Renaming_Complex_Pattern => Type_Refs.Renaming_Complex_Pattern,
      Lkt_Ellipsis_Pattern => Type_Refs.Ellipsis_Pattern,
      Lkt_Bool_Pattern_False => Type_Refs.Bool_Pattern_False,
      Lkt_Bool_Pattern_True => Type_Refs.Bool_Pattern_True,
      Lkt_Integer_Pattern => Type_Refs.Integer_Pattern,
      Lkt_List_Pattern => Type_Refs.List_Pattern,
      Lkt_Null_Pattern => Type_Refs.Null_Pattern,
      Lkt_Or_Pattern => Type_Refs.Or_Pattern,
      Lkt_Paren_Pattern => Type_Refs.Paren_Pattern,
      Lkt_Regex_Pattern => Type_Refs.Regex_Pattern,
      Lkt_Scoped_Pattern => Type_Refs.Scoped_Pattern,
      Lkt_Not_Pattern => Type_Refs.Not_Pattern,
      Lkt_Type_Pattern => Type_Refs.Type_Pattern,
      Lkt_Destructuring_Pattern_Detail => Type_Refs.Destructuring_Pattern_Detail,
      Lkt_Field_Pattern_Detail => Type_Refs.Field_Pattern_Detail,
      Lkt_Property_Pattern_Detail => Type_Refs.Property_Pattern_Detail,
      Lkt_Default_List_Type_Ref => Type_Refs.Default_List_Type_Ref,
      Lkt_Function_Type_Ref => Type_Refs.Function_Type_Ref,
      Lkt_Generic_Type_Ref => Type_Refs.Generic_Type_Ref,
      Lkt_Simple_Type_Ref => Type_Refs.Simple_Type_Ref,
      Lkt_Var_Bind => Type_Refs.Var_Bind);
   --  @exclude-value

   -----------------------
   -- Member references --
   -----------------------

   package Member_Refs is
         Complete_Item_Declaration : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 1);
         --  @exclude-value
         Decoded_Char_Value_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 2);
         --  @exclude-value
         Decoded_Char_Value_Has_Error : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 3);
         --  @exclude-value
         Decoded_Char_Value_Error_Sloc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 4);
         --  @exclude-value
         Decoded_Char_Value_Error_Message : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 5);
         --  @exclude-value
         Decoded_String_Value_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 6);
         --  @exclude-value
         Decoded_String_Value_Has_Error : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 7);
         --  @exclude-value
         Decoded_String_Value_Error_Sloc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 8);
         --  @exclude-value
         Decoded_String_Value_Error_Message : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 9);
         --  @exclude-value
         Logic_Context_Ref_Node : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 10);
         --  @exclude-value
         Logic_Context_Decl_Node : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 11);
         --  @exclude-value
         Ref_Result_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 12);
         --  @exclude-value
         Solver_Diagnostic_Message_Template : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 13);
         --  @exclude-value
         Solver_Diagnostic_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 14);
         --  @exclude-value
         Solver_Diagnostic_Location : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 15);
         --  @exclude-value
         Solver_Diagnostic_Contexts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 16);
         --  @exclude-value
         Solver_Diagnostic_Round : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 17);
         --  @exclude-value
         Solver_Result_Success : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 18);
         --  @exclude-value
         Solver_Result_Diagnostics : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 19);
         --  @exclude-value
         Argument_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 20);
         --  @exclude-value
         Argument_F_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 21);
         --  @exclude-value
         Import_F_Imported_Names : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 22);
         --  @exclude-value
         Import_All_From_F_Module_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 23);
         --  @exclude-value
         Import_From_F_Module_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 24);
         --  @exclude-value
         Import_From_F_Imported_Names : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 25);
         --  @exclude-value
         Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 26);
         --  @exclude-value
         Lexer_Case_Rule_Cond_Alt_F_Send : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 27);
         --  @exclude-value
         Lexer_Case_Rule_Default_Alt_F_Send : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 28);
         --  @exclude-value
         Base_Match_Branch_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 29);
         --  @exclude-value
         Match_Branch_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 30);
         --  @exclude-value
         Pattern_Match_Branch_F_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 31);
         --  @exclude-value
         Block_Expr_Clause_F_Clause : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 32);
         --  @exclude-value
         Decl_F_Syn_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 33);
         --  @exclude-value
         Base_Grammar_Rule_Decl_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 34);
         --  @exclude-value
         Explicitly_Typed_Decl_F_Decl_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 35);
         --  @exclude-value
         Component_Decl_F_Default_Val : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 36);
         --  @exclude-value
         Field_Decl_F_Trait_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 37);
         --  @exclude-value
         Fun_Param_Decl_F_Decl_Annotations : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 38);
         --  @exclude-value
         Val_Decl_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 39);
         --  @exclude-value
         Fun_Decl_F_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 40);
         --  @exclude-value
         Fun_Decl_F_Return_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 41);
         --  @exclude-value
         Fun_Decl_F_Trait_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 42);
         --  @exclude-value
         Fun_Decl_F_Body : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 43);
         --  @exclude-value
         Env_Spec_Decl_F_Actions : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 44);
         --  @exclude-value
         Generic_Decl_F_Generic_Param_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 45);
         --  @exclude-value
         Generic_Decl_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 46);
         --  @exclude-value
         Grammar_Decl_F_Rules : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 47);
         --  @exclude-value
         Langkit_Root_F_Doc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 48);
         --  @exclude-value
         Langkit_Root_F_Imports : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 49);
         --  @exclude-value
         Langkit_Root_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 50);
         --  @exclude-value
         Lexer_Decl_F_Rules : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 51);
         --  @exclude-value
         Lexer_Family_Decl_F_Rules : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 52);
         --  @exclude-value
         Type_Decl_F_Traits : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 53);
         --  @exclude-value
         Type_Decl_F_Syn_Base_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 54);
         --  @exclude-value
         Named_Type_Decl_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 55);
         --  @exclude-value
         Enum_Class_Decl_F_Branches : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 56);
         --  @exclude-value
         Enum_Type_Decl_F_Literals : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 57);
         --  @exclude-value
         Decl_Annotation_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 58);
         --  @exclude-value
         Decl_Annotation_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 59);
         --  @exclude-value
         Decl_Annotation_Args_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 60);
         --  @exclude-value
         Elsif_Branch_F_Cond_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 61);
         --  @exclude-value
         Elsif_Branch_F_Then_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 62);
         --  @exclude-value
         Enum_Class_Case_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 63);
         --  @exclude-value
         Any_Of_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 64);
         --  @exclude-value
         Any_Of_F_Values : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 65);
         --  @exclude-value
         Array_Literal_F_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 66);
         --  @exclude-value
         Array_Literal_F_Element_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 67);
         --  @exclude-value
         Base_Call_Expr_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 68);
         --  @exclude-value
         Base_Call_Expr_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 69);
         --  @exclude-value
         Bin_Op_F_Left : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 70);
         --  @exclude-value
         Bin_Op_F_Op : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 71);
         --  @exclude-value
         Bin_Op_F_Right : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 72);
         --  @exclude-value
         Block_Expr_F_Clauses : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 73);
         --  @exclude-value
         Cast_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 74);
         --  @exclude-value
         Cast_Expr_F_Null_Cond : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 75);
         --  @exclude-value
         Cast_Expr_F_Excludes_Null : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 76);
         --  @exclude-value
         Cast_Expr_F_Dest_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 77);
         --  @exclude-value
         Dot_Expr_F_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 78);
         --  @exclude-value
         Dot_Expr_F_Null_Cond : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 79);
         --  @exclude-value
         Dot_Expr_F_Suffix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 80);
         --  @exclude-value
         Error_On_Null_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 81);
         --  @exclude-value
         Generic_Instantiation_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 82);
         --  @exclude-value
         Generic_Instantiation_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 83);
         --  @exclude-value
         Grammar_Discard_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 84);
         --  @exclude-value
         Grammar_Dont_Skip_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 85);
         --  @exclude-value
         Grammar_Dont_Skip_F_Dont_Skip : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 86);
         --  @exclude-value
         Grammar_List_F_List_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 87);
         --  @exclude-value
         Grammar_List_F_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 88);
         --  @exclude-value
         Grammar_List_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 89);
         --  @exclude-value
         Grammar_List_F_Sep : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 90);
         --  @exclude-value
         Grammar_Null_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 91);
         --  @exclude-value
         Grammar_Opt_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 92);
         --  @exclude-value
         Grammar_Opt_Error_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 93);
         --  @exclude-value
         Grammar_Opt_Error_Group_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 94);
         --  @exclude-value
         Grammar_Opt_Group_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 95);
         --  @exclude-value
         Grammar_Or_Expr_F_Sub_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 96);
         --  @exclude-value
         Grammar_Pick_F_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 97);
         --  @exclude-value
         Grammar_Predicate_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 98);
         --  @exclude-value
         Grammar_Predicate_F_Prop_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 99);
         --  @exclude-value
         Grammar_Rule_Ref_F_Node_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 100);
         --  @exclude-value
         Grammar_Skip_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 101);
         --  @exclude-value
         Grammar_Stop_Cut_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 102);
         --  @exclude-value
         Parse_Node_Expr_F_Node_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 103);
         --  @exclude-value
         Parse_Node_Expr_F_Sub_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 104);
         --  @exclude-value
         Token_No_Case_Lit_F_Lit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 105);
         --  @exclude-value
         Token_Pattern_Concat_F_Left : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 106);
         --  @exclude-value
         Token_Pattern_Concat_F_Right : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 107);
         --  @exclude-value
         Token_Ref_F_Token_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 108);
         --  @exclude-value
         Token_Ref_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 109);
         --  @exclude-value
         If_Expr_F_Cond_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 110);
         --  @exclude-value
         If_Expr_F_Then_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 111);
         --  @exclude-value
         If_Expr_F_Alternatives : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 112);
         --  @exclude-value
         If_Expr_F_Else_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 113);
         --  @exclude-value
         Isa_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 114);
         --  @exclude-value
         Isa_F_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 115);
         --  @exclude-value
         Keep_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 116);
         --  @exclude-value
         Keep_Expr_F_Null_Cond : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 117);
         --  @exclude-value
         Keep_Expr_F_Keep_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 118);
         --  @exclude-value
         Lambda_Expr_F_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 119);
         --  @exclude-value
         Lambda_Expr_F_Return_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 120);
         --  @exclude-value
         Lambda_Expr_F_Body : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 121);
         --  @exclude-value
         Null_Lit_F_Dest_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 122);
         --  @exclude-value
         Block_String_Lit_F_Lines : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 123);
         --  @exclude-value
         Module_Doc_String_Lit_F_Lines : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 124);
         --  @exclude-value
         Logic_Assign_F_Dest_Var : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 125);
         --  @exclude-value
         Logic_Assign_F_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 126);
         --  @exclude-value
         Logic_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 127);
         --  @exclude-value
         Logic_Propagate_F_Dest_Var : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 128);
         --  @exclude-value
         Logic_Propagate_F_Call : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 129);
         --  @exclude-value
         Logic_Unify_F_Lhs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 130);
         --  @exclude-value
         Logic_Unify_F_Rhs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 131);
         --  @exclude-value
         Match_Expr_F_Match_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 132);
         --  @exclude-value
         Match_Expr_F_Branches : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 133);
         --  @exclude-value
         Not_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 134);
         --  @exclude-value
         Paren_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 135);
         --  @exclude-value
         Query_F_Source : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 136);
         --  @exclude-value
         Query_F_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 137);
         --  @exclude-value
         Query_F_Mapping : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 138);
         --  @exclude-value
         Query_F_Guard : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 139);
         --  @exclude-value
         Raise_Expr_F_Dest_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 140);
         --  @exclude-value
         Raise_Expr_F_Except_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 141);
         --  @exclude-value
         Subscript_Expr_F_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 142);
         --  @exclude-value
         Subscript_Expr_F_Null_Cond : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 143);
         --  @exclude-value
         Subscript_Expr_F_Index : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 144);
         --  @exclude-value
         Try_Expr_F_Try_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 145);
         --  @exclude-value
         Try_Expr_F_Or_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 146);
         --  @exclude-value
         Un_Op_F_Op : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 147);
         --  @exclude-value
         Un_Op_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 148);
         --  @exclude-value
         Full_Decl_F_Doc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 149);
         --  @exclude-value
         Full_Decl_F_Decl_Annotations : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 150);
         --  @exclude-value
         Full_Decl_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 151);
         --  @exclude-value
         Grammar_List_Sep_F_Token : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 152);
         --  @exclude-value
         Grammar_List_Sep_F_Extra : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 153);
         --  @exclude-value
         Imported_Name_F_Original_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 154);
         --  @exclude-value
         Imported_Name_F_Renaming : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 155);
         --  @exclude-value
         Lexer_Case_Rule_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 156);
         --  @exclude-value
         Lexer_Case_Rule_F_Alts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 157);
         --  @exclude-value
         Lexer_Case_Rule_Send_F_Sent : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 158);
         --  @exclude-value
         Lexer_Case_Rule_Send_F_Match_Size : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 159);
         --  @exclude-value
         Binding_Pattern_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 160);
         --  @exclude-value
         Complex_Pattern_F_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 161);
         --  @exclude-value
         Complex_Pattern_F_Details : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 162);
         --  @exclude-value
         Complex_Pattern_F_Predicate : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 163);
         --  @exclude-value
         List_Pattern_F_Sub_Patterns : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 164);
         --  @exclude-value
         Or_Pattern_F_Left_Sub_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 165);
         --  @exclude-value
         Or_Pattern_F_Right_Sub_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 166);
         --  @exclude-value
         Paren_Pattern_F_Sub_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 167);
         --  @exclude-value
         Scoped_Pattern_F_Sub_Pattern : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 168);
         --  @exclude-value
         Type_Pattern_F_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 169);
         --  @exclude-value
         Destructuring_Pattern_Detail_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 170);
         --  @exclude-value
         Field_Pattern_Detail_F_Id : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 171);
         --  @exclude-value
         Field_Pattern_Detail_F_Expected_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 172);
         --  @exclude-value
         Property_Pattern_Detail_F_Call : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 173);
         --  @exclude-value
         Property_Pattern_Detail_F_Expected_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 174);
         --  @exclude-value
         Function_Type_Ref_F_Param_Types : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 175);
         --  @exclude-value
         Function_Type_Ref_F_Return_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 176);
         --  @exclude-value
         Generic_Type_Ref_F_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 177);
         --  @exclude-value
         Generic_Type_Ref_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 178);
         --  @exclude-value
         Simple_Type_Ref_F_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 179);
         --  @exclude-value
         Var_Bind_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 180);
         --  @exclude-value
         Var_Bind_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 181);
         --  @exclude-value
         Parent : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 182);
         --  @exclude-value
         Parents : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 183);
         --  @exclude-value
         Children : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 184);
         --  @exclude-value
         Token_Start : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 185);
         --  @exclude-value
         Token_End : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 186);
         --  @exclude-value
         Child_Index : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 187);
         --  @exclude-value
         Previous_Sibling : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 188);
         --  @exclude-value
         Next_Sibling : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 189);
         --  @exclude-value
         Unit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 190);
         --  @exclude-value
         Is_Ghost : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 191);
         --  @exclude-value
         Full_Sloc_Image : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 192);
         --  @exclude-value
         Completion_Item_Kind_To_Int : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 193);
         --  @exclude-value
         Lkt_Node_P_Set_Solver_Debug_Mode : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 194);
         --  @exclude-value
         Lkt_Node_P_Prelude_Unit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 195);
         --  @exclude-value
         Lkt_Node_P_Basic_Trait_Gen : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 196);
         --  @exclude-value
         Lkt_Node_P_Basic_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 197);
         --  @exclude-value
         Lkt_Node_P_Node_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 198);
         --  @exclude-value
         Lkt_Node_P_Node_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 199);
         --  @exclude-value
         Lkt_Node_P_Indexable_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 200);
         --  @exclude-value
         Lkt_Node_P_Indexable_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 201);
         --  @exclude-value
         Lkt_Node_P_Iterable_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 202);
         --  @exclude-value
         Lkt_Node_P_Iterable_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 203);
         --  @exclude-value
         Lkt_Node_P_Token_Node_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 204);
         --  @exclude-value
         Lkt_Node_P_Error_Node_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 205);
         --  @exclude-value
         Lkt_Node_P_Char_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 206);
         --  @exclude-value
         Lkt_Node_P_Int_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 207);
         --  @exclude-value
         Lkt_Node_P_Bool_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 208);
         --  @exclude-value
         Lkt_Node_P_Bigint_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 209);
         --  @exclude-value
         Lkt_Node_P_String_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 210);
         --  @exclude-value
         Lkt_Node_P_Symbol_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 211);
         --  @exclude-value
         Lkt_Node_P_Property_Error_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 212);
         --  @exclude-value
         Lkt_Node_P_Regexp_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 213);
         --  @exclude-value
         Lkt_Node_P_Entity_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 214);
         --  @exclude-value
         Lkt_Node_P_Entity_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 215);
         --  @exclude-value
         Lkt_Node_P_Logicvar_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 216);
         --  @exclude-value
         Lkt_Node_P_Equation_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 217);
         --  @exclude-value
         Lkt_Node_P_Array_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 218);
         --  @exclude-value
         Lkt_Node_P_Array_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 219);
         --  @exclude-value
         Lkt_Node_P_Stream_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 220);
         --  @exclude-value
         Lkt_Node_P_Stream_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 221);
         --  @exclude-value
         Lkt_Node_P_Astlist_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 222);
         --  @exclude-value
         Lkt_Node_P_Astlist_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 223);
         --  @exclude-value
         Lkt_Node_P_Node_Builder_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 224);
         --  @exclude-value
         Lkt_Node_P_Node_Builder_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 225);
         --  @exclude-value
         Lkt_Node_P_Iterator_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 226);
         --  @exclude-value
         Lkt_Node_P_Iterator_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 227);
         --  @exclude-value
         Lkt_Node_P_Analysis_Unit_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 228);
         --  @exclude-value
         Lkt_Node_P_Analysis_Unit_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 229);
         --  @exclude-value
         Lkt_Node_P_Topmost_Invalid_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 230);
         --  @exclude-value
         Lkt_Node_P_Nameres_Diagnostics : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 231);
         --  @exclude-value
         Lkt_Node_P_Solve_Enclosing_Context : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 232);
         --  @exclude-value
         Lkt_Node_P_Xref_Entry_Point : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 233);
         --  @exclude-value
         Lkt_Node_P_Complete : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 234);
         --  @exclude-value
         Base_Import_P_Referenced_Units : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 235);
         --  @exclude-value
         Base_Match_Branch_P_Match_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 236);
         --  @exclude-value
         Decl_P_Custom_Image : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 237);
         --  @exclude-value
         Decl_P_Decl_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 238);
         --  @exclude-value
         Decl_P_Def_Ids : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 239);
         --  @exclude-value
         Decl_P_As_Bare_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 240);
         --  @exclude-value
         Decl_P_Get_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 241);
         --  @exclude-value
         Decl_P_Get_Cast_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 242);
         --  @exclude-value
         Decl_P_Get_Keep_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 243);
         --  @exclude-value
         Decl_P_Get_Suffix_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 244);
         --  @exclude-value
         Decl_P_Is_Generic : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 245);
         --  @exclude-value
         Decl_P_Return_Type_Is_Instantiated : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 246);
         --  @exclude-value
         Decl_P_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 247);
         --  @exclude-value
         Decl_P_Full_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 248);
         --  @exclude-value
         Fun_Decl_P_Is_Dynamic_Combiner : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 249);
         --  @exclude-value
         Fun_Decl_P_Find_All_Overrides : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 250);
         --  @exclude-value
         Type_Decl_P_Def_Id : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 251);
         --  @exclude-value
         Type_Decl_P_Base_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 252);
         --  @exclude-value
         Excludes_Null_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 253);
         --  @exclude-value
         Expr_P_Get_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 254);
         --  @exclude-value
         Expr_P_Get_Generic_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 255);
         --  @exclude-value
         Expr_P_Get_Expected_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 256);
         --  @exclude-value
         Expr_P_Referenced_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 257);
         --  @exclude-value
         Token_Lit_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 258);
         --  @exclude-value
         Token_Pattern_Lit_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 259);
         --  @exclude-value
         Id_P_Custom_Image : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 260);
         --  @exclude-value
         Def_Id_P_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 261);
         --  @exclude-value
         Def_Id_P_Get_Implementatinons : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 262);
         --  @exclude-value
         Def_Id_P_Decl_Detail : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 263);
         --  @exclude-value
         Def_Id_P_Completion_Item_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 264);
         --  @exclude-value
         Def_Id_P_Doc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 265);
         --  @exclude-value
         Def_Id_P_Find_All_References : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 266);
         --  @exclude-value
         Ref_Id_P_Referenced_Defining_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 267);
         --  @exclude-value
         Char_Lit_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 268);
         --  @exclude-value
         String_Lit_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 269);
         --  @exclude-value
         String_Lit_P_Is_Prefixed_String : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 270);
         --  @exclude-value
         String_Lit_P_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 271);
         --  @exclude-value
         String_Lit_P_Is_Regexp_Literal : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 272);
         --  @exclude-value
         Full_Decl_P_Has_Annotation : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 273);
         --  @exclude-value
         Is_Empty_List : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 274);
         --  @exclude-value
         Null_Cond_Qualifier_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 275);
         --  @exclude-value
         Regex_Pattern_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 276);
         --  @exclude-value
         Type_Ref_P_Referenced_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 277);
         --  @exclude-value
   end Member_Refs;

end Liblktlang.Generic_API.Introspection;
