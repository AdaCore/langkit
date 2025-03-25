
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
         Lkt_Node_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 16);
         Logic_Context_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 17);
         Solver_Diagnostic_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 18);
         Decoded_Char_Value : constant G.Type_Ref :=
           G.From_Index (Self_Id, 19);
         Decoded_String_Value : constant G.Type_Ref :=
           G.From_Index (Self_Id, 20);
         Logic_Context : constant G.Type_Ref :=
           G.From_Index (Self_Id, 21);
         Solver_Diagnostic : constant G.Type_Ref :=
           G.From_Index (Self_Id, 22);
         Solver_Result : constant G.Type_Ref :=
           G.From_Index (Self_Id, 23);
         Lkt_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 24);
         Argument : constant G.Type_Ref :=
           G.From_Index (Self_Id, 25);
         Base_Lexer_Case_Rule_Alt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 26);
         Lexer_Case_Rule_Cond_Alt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 27);
         Lexer_Case_Rule_Default_Alt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 28);
         Block_String_Line : constant G.Type_Ref :=
           G.From_Index (Self_Id, 29);
         Class_Qualifier : constant G.Type_Ref :=
           G.From_Index (Self_Id, 30);
         Class_Qualifier_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 31);
         Class_Qualifier_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 32);
         Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 33);
         Base_Grammar_Rule_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 34);
         Grammar_Rule_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 35);
         Synthetic_Lexer_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 36);
         Base_Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 37);
         Node_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 38);
         Self_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 39);
         User_Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 40);
         Enum_Lit_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 41);
         Explicitly_Typed_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 42);
         Component_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 43);
         Field_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 44);
         Fun_Param_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 45);
         Lambda_Param_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 46);
         Dyn_Var_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 47);
         Match_Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 48);
         Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 49);
         Fun_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 50);
         Env_Spec_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 51);
         Generic_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 52);
         Grammar_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 53);
         Lexer_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 54);
         Lexer_Family_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 55);
         Synth_Fun_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 56);
         Synth_Param_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 57);
         Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 58);
         Any_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 59);
         Enum_Class_Alt_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 60);
         Function_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 61);
         Generic_Param_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 62);
         Named_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 63);
         Basic_Class_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 64);
         Class_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 65);
         Enum_Class_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 66);
         Enum_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 67);
         Struct_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 68);
         Trait_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 69);
         Decl_Annotation : constant G.Type_Ref :=
           G.From_Index (Self_Id, 70);
         Decl_Annotation_Args : constant G.Type_Ref :=
           G.From_Index (Self_Id, 71);
         Dyn_Env_Wrapper : constant G.Type_Ref :=
           G.From_Index (Self_Id, 72);
         Elsif_Branch : constant G.Type_Ref :=
           G.From_Index (Self_Id, 73);
         Enum_Class_Case : constant G.Type_Ref :=
           G.From_Index (Self_Id, 74);
         Excludes_Null : constant G.Type_Ref :=
           G.From_Index (Self_Id, 75);
         Excludes_Null_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 76);
         Excludes_Null_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 77);
         Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 78);
         Any_Of : constant G.Type_Ref :=
           G.From_Index (Self_Id, 79);
         Array_Literal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 80);
         Base_Call_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 81);
         Call_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 82);
         Logic_Call_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 83);
         Logic_Predicate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 84);
         Logic_Propagate_Call : constant G.Type_Ref :=
           G.From_Index (Self_Id, 85);
         Base_Dot_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 86);
         Dot_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 87);
         Null_Cond_Dotted_Name : constant G.Type_Ref :=
           G.From_Index (Self_Id, 88);
         Bin_Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 89);
         Block_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 90);
         Cast_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 91);
         Error_On_Null : constant G.Type_Ref :=
           G.From_Index (Self_Id, 92);
         Generic_Instantiation : constant G.Type_Ref :=
           G.From_Index (Self_Id, 93);
         Grammar_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 94);
         Grammar_Cut : constant G.Type_Ref :=
           G.From_Index (Self_Id, 95);
         Grammar_Discard : constant G.Type_Ref :=
           G.From_Index (Self_Id, 96);
         Grammar_Dont_Skip : constant G.Type_Ref :=
           G.From_Index (Self_Id, 97);
         Grammar_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 98);
         Grammar_Null : constant G.Type_Ref :=
           G.From_Index (Self_Id, 99);
         Grammar_Opt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 100);
         Grammar_Opt_Error : constant G.Type_Ref :=
           G.From_Index (Self_Id, 101);
         Grammar_Opt_Error_Group : constant G.Type_Ref :=
           G.From_Index (Self_Id, 102);
         Grammar_Opt_Group : constant G.Type_Ref :=
           G.From_Index (Self_Id, 103);
         Grammar_Or_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 104);
         Grammar_Pick : constant G.Type_Ref :=
           G.From_Index (Self_Id, 105);
         Grammar_Implicit_Pick : constant G.Type_Ref :=
           G.From_Index (Self_Id, 106);
         Grammar_Predicate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 107);
         Grammar_Rule_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 108);
         Grammar_Skip : constant G.Type_Ref :=
           G.From_Index (Self_Id, 109);
         Grammar_Stop_Cut : constant G.Type_Ref :=
           G.From_Index (Self_Id, 110);
         Parse_Node_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 111);
         Token_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 112);
         Token_No_Case_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 113);
         Token_Pattern_Concat : constant G.Type_Ref :=
           G.From_Index (Self_Id, 114);
         Token_Pattern_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 115);
         Token_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 116);
         Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 117);
         Def_Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 118);
         Module_Ref_Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 119);
         Ref_Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 120);
         If_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 121);
         Isa : constant G.Type_Ref :=
           G.From_Index (Self_Id, 122);
         Keep_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 123);
         Lambda_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 124);
         Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 125);
         Big_Num_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 126);
         Char_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 127);
         Null_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 128);
         Num_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 129);
         String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 130);
         Block_String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 131);
         Single_Line_String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 132);
         Pattern_Single_Line_String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 133);
         Logic_Assign : constant G.Type_Ref :=
           G.From_Index (Self_Id, 134);
         Logic_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 135);
         Logic_Propagate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 136);
         Logic_Unify : constant G.Type_Ref :=
           G.From_Index (Self_Id, 137);
         Match_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 138);
         Not_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 139);
         Paren_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 140);
         Raise_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 141);
         Subscript_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 142);
         Null_Cond_Subscript_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 143);
         Try_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 144);
         Un_Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 145);
         Full_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 146);
         Grammar_List_Sep : constant G.Type_Ref :=
           G.From_Index (Self_Id, 147);
         Import : constant G.Type_Ref :=
           G.From_Index (Self_Id, 148);
         Langkit_Root : constant G.Type_Ref :=
           G.From_Index (Self_Id, 149);
         Lexer_Case_Rule : constant G.Type_Ref :=
           G.From_Index (Self_Id, 150);
         Lexer_Case_Rule_Send : constant G.Type_Ref :=
           G.From_Index (Self_Id, 151);
         List_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 152);
         List_Kind_One : constant G.Type_Ref :=
           G.From_Index (Self_Id, 153);
         List_Kind_Zero : constant G.Type_Ref :=
           G.From_Index (Self_Id, 154);
         Lkt_Node_Base_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 155);
         Argument_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 156);
         Base_Lexer_Case_Rule_Alt_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 157);
         Block_String_Line_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 158);
         Call_Expr_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 159);
         Decl_Annotation_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 160);
         Elsif_Branch_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 161);
         Enum_Class_Alt_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 162);
         Enum_Class_Case_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 163);
         Enum_Lit_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 164);
         Expr_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 165);
         Any_Of_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 166);
         Full_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 167);
         Decl_Block : constant G.Type_Ref :=
           G.From_Index (Self_Id, 168);
         Generic_Param_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 169);
         Fun_Param_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 170);
         Grammar_Expr_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 171);
         Grammar_Expr_List_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 172);
         Import_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 173);
         Lambda_Param_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 174);
         Lkt_Node_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 175);
         Block_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 176);
         Match_Branch_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 177);
         Ref_Id_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 178);
         Type_Ref_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 179);
         Isa_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 180);
         Match_Branch : constant G.Type_Ref :=
           G.From_Index (Self_Id, 181);
         Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 182);
         Op_Amp : constant G.Type_Ref :=
           G.From_Index (Self_Id, 183);
         Op_And : constant G.Type_Ref :=
           G.From_Index (Self_Id, 184);
         Op_Div : constant G.Type_Ref :=
           G.From_Index (Self_Id, 185);
         Op_Eq : constant G.Type_Ref :=
           G.From_Index (Self_Id, 186);
         Op_Gt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 187);
         Op_Gte : constant G.Type_Ref :=
           G.From_Index (Self_Id, 188);
         Op_Logic_And : constant G.Type_Ref :=
           G.From_Index (Self_Id, 189);
         Op_Logic_Or : constant G.Type_Ref :=
           G.From_Index (Self_Id, 190);
         Op_Lt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 191);
         Op_Lte : constant G.Type_Ref :=
           G.From_Index (Self_Id, 192);
         Op_Minus : constant G.Type_Ref :=
           G.From_Index (Self_Id, 193);
         Op_Mult : constant G.Type_Ref :=
           G.From_Index (Self_Id, 194);
         Op_Ne : constant G.Type_Ref :=
           G.From_Index (Self_Id, 195);
         Op_Or : constant G.Type_Ref :=
           G.From_Index (Self_Id, 196);
         Op_Or_Int : constant G.Type_Ref :=
           G.From_Index (Self_Id, 197);
         Op_Plus : constant G.Type_Ref :=
           G.From_Index (Self_Id, 198);
         Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 199);
         Default_List_Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 200);
         Function_Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 201);
         Generic_Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 202);
         Simple_Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 203);
         Var_Bind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 204);
   end Type_Refs;

   Kind_To_Type : constant array (Lkt_Node_Kind_Type) of G.Type_Ref :=
     (Lkt_Argument => Type_Refs.Argument,
      Lkt_Lexer_Case_Rule_Cond_Alt => Type_Refs.Lexer_Case_Rule_Cond_Alt,
      Lkt_Lexer_Case_Rule_Default_Alt => Type_Refs.Lexer_Case_Rule_Default_Alt,
      Lkt_Block_String_Line => Type_Refs.Block_String_Line,
      Lkt_Class_Qualifier_Absent => Type_Refs.Class_Qualifier_Absent,
      Lkt_Class_Qualifier_Present => Type_Refs.Class_Qualifier_Present,
      Lkt_Grammar_Rule_Decl => Type_Refs.Grammar_Rule_Decl,
      Lkt_Synthetic_Lexer_Decl => Type_Refs.Synthetic_Lexer_Decl,
      Lkt_Node_Decl => Type_Refs.Node_Decl,
      Lkt_Self_Decl => Type_Refs.Self_Decl,
      Lkt_Enum_Lit_Decl => Type_Refs.Enum_Lit_Decl,
      Lkt_Field_Decl => Type_Refs.Field_Decl,
      Lkt_Fun_Param_Decl => Type_Refs.Fun_Param_Decl,
      Lkt_Lambda_Param_Decl => Type_Refs.Lambda_Param_Decl,
      Lkt_Dyn_Var_Decl => Type_Refs.Dyn_Var_Decl,
      Lkt_Match_Val_Decl => Type_Refs.Match_Val_Decl,
      Lkt_Val_Decl => Type_Refs.Val_Decl,
      Lkt_Fun_Decl => Type_Refs.Fun_Decl,
      Lkt_Env_Spec_Decl => Type_Refs.Env_Spec_Decl,
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
      Lkt_Dot_Expr => Type_Refs.Dot_Expr,
      Lkt_Null_Cond_Dotted_Name => Type_Refs.Null_Cond_Dotted_Name,
      Lkt_Bin_Op => Type_Refs.Bin_Op,
      Lkt_Block_Expr => Type_Refs.Block_Expr,
      Lkt_Cast_Expr => Type_Refs.Cast_Expr,
      Lkt_Error_On_Null => Type_Refs.Error_On_Null,
      Lkt_Generic_Instantiation => Type_Refs.Generic_Instantiation,
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
      Lkt_Module_Ref_Id => Type_Refs.Module_Ref_Id,
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
      Lkt_Single_Line_String_Lit => Type_Refs.Single_Line_String_Lit,
      Lkt_Pattern_Single_Line_String_Lit => Type_Refs.Pattern_Single_Line_String_Lit,
      Lkt_Logic_Assign => Type_Refs.Logic_Assign,
      Lkt_Logic_Expr => Type_Refs.Logic_Expr,
      Lkt_Logic_Propagate => Type_Refs.Logic_Propagate,
      Lkt_Logic_Unify => Type_Refs.Logic_Unify,
      Lkt_Match_Expr => Type_Refs.Match_Expr,
      Lkt_Not_Expr => Type_Refs.Not_Expr,
      Lkt_Paren_Expr => Type_Refs.Paren_Expr,
      Lkt_Raise_Expr => Type_Refs.Raise_Expr,
      Lkt_Subscript_Expr => Type_Refs.Subscript_Expr,
      Lkt_Null_Cond_Subscript_Expr => Type_Refs.Null_Cond_Subscript_Expr,
      Lkt_Try_Expr => Type_Refs.Try_Expr,
      Lkt_Un_Op => Type_Refs.Un_Op,
      Lkt_Full_Decl => Type_Refs.Full_Decl,
      Lkt_Grammar_List_Sep => Type_Refs.Grammar_List_Sep,
      Lkt_Import => Type_Refs.Import,
      Lkt_Langkit_Root => Type_Refs.Langkit_Root,
      Lkt_Lexer_Case_Rule => Type_Refs.Lexer_Case_Rule,
      Lkt_Lexer_Case_Rule_Send => Type_Refs.Lexer_Case_Rule_Send,
      Lkt_List_Kind_One => Type_Refs.List_Kind_One,
      Lkt_List_Kind_Zero => Type_Refs.List_Kind_Zero,
      Lkt_Argument_List => Type_Refs.Argument_List,
      Lkt_Base_Lexer_Case_Rule_Alt_List => Type_Refs.Base_Lexer_Case_Rule_Alt_List,
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
      Lkt_Import_List => Type_Refs.Import_List,
      Lkt_Lambda_Param_Decl_List => Type_Refs.Lambda_Param_Decl_List,
      Lkt_Lkt_Node_List => Type_Refs.Lkt_Node_List,
      Lkt_Block_Decl_List => Type_Refs.Block_Decl_List,
      Lkt_Match_Branch_List => Type_Refs.Match_Branch_List,
      Lkt_Ref_Id_List => Type_Refs.Ref_Id_List,
      Lkt_Type_Ref_List => Type_Refs.Type_Ref_List,
      Lkt_Isa_List => Type_Refs.Isa_List,
      Lkt_Match_Branch => Type_Refs.Match_Branch,
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
      Lkt_Default_List_Type_Ref => Type_Refs.Default_List_Type_Ref,
      Lkt_Function_Type_Ref => Type_Refs.Function_Type_Ref,
      Lkt_Generic_Type_Ref => Type_Refs.Generic_Type_Ref,
      Lkt_Simple_Type_Ref => Type_Refs.Simple_Type_Ref,
      Lkt_Var_Bind => Type_Refs.Var_Bind);

   -----------------------
   -- Member references --
   -----------------------

   package Member_Refs is
         Decoded_Char_Value_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 1);
         Decoded_Char_Value_Has_Error : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 2);
         Decoded_Char_Value_Error_Sloc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 3);
         Decoded_Char_Value_Error_Message : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 4);
         Decoded_String_Value_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 5);
         Decoded_String_Value_Has_Error : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 6);
         Decoded_String_Value_Error_Sloc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 7);
         Decoded_String_Value_Error_Message : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 8);
         Logic_Context_Ref_Node : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 9);
         Logic_Context_Decl_Node : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 10);
         Solver_Diagnostic_Message_Template : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 11);
         Solver_Diagnostic_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 12);
         Solver_Diagnostic_Location : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 13);
         Solver_Diagnostic_Contexts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 14);
         Solver_Diagnostic_Round : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 15);
         Solver_Result_Success : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 16);
         Solver_Result_Diagnostics : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 17);
         Argument_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 18);
         Argument_F_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 19);
         Base_Lexer_Case_Rule_Alt_F_Send : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 20);
         Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 21);
         Decl_F_Syn_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 22);
         Base_Grammar_Rule_Decl_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 23);
         Explicitly_Typed_Decl_F_Decl_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 24);
         Component_Decl_F_Default_Val : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 25);
         Field_Decl_F_Trait_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 26);
         Fun_Param_Decl_F_Decl_Annotations : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 27);
         Val_Decl_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 28);
         Fun_Decl_F_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 29);
         Fun_Decl_F_Return_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 30);
         Fun_Decl_F_Trait_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 31);
         Fun_Decl_F_Body : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 32);
         Env_Spec_Decl_F_Actions : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 33);
         Generic_Decl_F_Generic_Param_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 34);
         Generic_Decl_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 35);
         Grammar_Decl_F_Rules : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 36);
         Lexer_Decl_F_Rules : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 37);
         Lexer_Family_Decl_F_Rules : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 38);
         Type_Decl_F_Traits : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 39);
         Type_Decl_F_Syn_Base_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 40);
         Generic_Param_Type_Decl_F_Has_Class : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 41);
         Named_Type_Decl_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 42);
         Enum_Class_Decl_F_Branches : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 43);
         Enum_Type_Decl_F_Literals : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 44);
         Decl_Annotation_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 45);
         Decl_Annotation_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 46);
         Decl_Annotation_Args_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 47);
         Elsif_Branch_F_Cond_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 48);
         Elsif_Branch_F_Then_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 49);
         Enum_Class_Case_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 50);
         Any_Of_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 51);
         Any_Of_F_Values : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 52);
         Array_Literal_F_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 53);
         Array_Literal_F_Element_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 54);
         Base_Call_Expr_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 55);
         Base_Call_Expr_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 56);
         Base_Dot_Expr_F_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 57);
         Base_Dot_Expr_F_Suffix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 58);
         Bin_Op_F_Left : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 59);
         Bin_Op_F_Op : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 60);
         Bin_Op_F_Right : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 61);
         Block_Expr_F_Val_Defs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 62);
         Block_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 63);
         Cast_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 64);
         Cast_Expr_F_Excludes_Null : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 65);
         Cast_Expr_F_Dest_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 66);
         Error_On_Null_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 67);
         Generic_Instantiation_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 68);
         Generic_Instantiation_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 69);
         Grammar_Discard_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 70);
         Grammar_Dont_Skip_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 71);
         Grammar_Dont_Skip_F_Dont_Skip : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 72);
         Grammar_List_F_List_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 73);
         Grammar_List_F_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 74);
         Grammar_List_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 75);
         Grammar_List_F_Sep : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 76);
         Grammar_Null_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 77);
         Grammar_Opt_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 78);
         Grammar_Opt_Error_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 79);
         Grammar_Opt_Error_Group_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 80);
         Grammar_Opt_Group_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 81);
         Grammar_Or_Expr_F_Sub_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 82);
         Grammar_Pick_F_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 83);
         Grammar_Predicate_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 84);
         Grammar_Predicate_F_Prop_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 85);
         Grammar_Rule_Ref_F_Node_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 86);
         Grammar_Skip_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 87);
         Grammar_Stop_Cut_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 88);
         Parse_Node_Expr_F_Node_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 89);
         Parse_Node_Expr_F_Sub_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 90);
         Token_No_Case_Lit_F_Lit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 91);
         Token_Pattern_Concat_F_Left : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 92);
         Token_Pattern_Concat_F_Right : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 93);
         Token_Ref_F_Token_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 94);
         Token_Ref_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 95);
         If_Expr_F_Cond_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 96);
         If_Expr_F_Then_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 97);
         If_Expr_F_Alternatives : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 98);
         If_Expr_F_Else_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 99);
         Isa_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 100);
         Isa_F_Dest_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 101);
         Keep_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 102);
         Keep_Expr_F_Keep_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 103);
         Lambda_Expr_F_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 104);
         Lambda_Expr_F_Return_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 105);
         Lambda_Expr_F_Body : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 106);
         Null_Lit_F_Dest_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 107);
         Block_String_Lit_F_Lines : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 108);
         Logic_Assign_F_Dest_Var : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 109);
         Logic_Assign_F_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 110);
         Logic_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 111);
         Logic_Propagate_F_Dest_Var : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 112);
         Logic_Propagate_F_Call : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 113);
         Logic_Unify_F_Lhs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 114);
         Logic_Unify_F_Rhs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 115);
         Match_Expr_F_Match_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 116);
         Match_Expr_F_Branches : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 117);
         Not_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 118);
         Paren_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 119);
         Raise_Expr_F_Dest_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 120);
         Raise_Expr_F_Except_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 121);
         Subscript_Expr_F_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 122);
         Subscript_Expr_F_Index : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 123);
         Try_Expr_F_Try_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 124);
         Try_Expr_F_Or_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 125);
         Un_Op_F_Op : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 126);
         Un_Op_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 127);
         Full_Decl_F_Doc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 128);
         Full_Decl_F_Decl_Annotations : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 129);
         Full_Decl_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 130);
         Grammar_List_Sep_F_Token : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 131);
         Grammar_List_Sep_F_Extra : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 132);
         Import_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 133);
         Langkit_Root_F_Imports : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 134);
         Langkit_Root_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 135);
         Lexer_Case_Rule_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 136);
         Lexer_Case_Rule_F_Alts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 137);
         Lexer_Case_Rule_Send_F_Sent : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 138);
         Lexer_Case_Rule_Send_F_Match_Size : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 139);
         Match_Branch_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 140);
         Match_Branch_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 141);
         Function_Type_Ref_F_Param_Types : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 142);
         Function_Type_Ref_F_Return_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 143);
         Generic_Type_Ref_F_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 144);
         Generic_Type_Ref_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 145);
         Simple_Type_Ref_F_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 146);
         Var_Bind_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 147);
         Var_Bind_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 148);
         Parent : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 149);
         Parents : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 150);
         Children : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 151);
         Token_Start : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 152);
         Token_End : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 153);
         Child_Index : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 154);
         Previous_Sibling : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 155);
         Next_Sibling : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 156);
         Unit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 157);
         Is_Ghost : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 158);
         Full_Sloc_Image : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 159);
         Completion_Item_Kind_To_Int : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 160);
         Lkt_Node_P_Set_Solver_Debug_Mode : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 161);
         Lkt_Node_P_Basic_Trait_Gen : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 162);
         Lkt_Node_P_Basic_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 163);
         Lkt_Node_P_Node_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 164);
         Lkt_Node_P_Node_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 165);
         Lkt_Node_P_Indexable_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 166);
         Lkt_Node_P_Indexable_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 167);
         Lkt_Node_P_Token_Node_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 168);
         Lkt_Node_P_Error_Node_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 169);
         Lkt_Node_P_Char_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 170);
         Lkt_Node_P_Int_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 171);
         Lkt_Node_P_Bool_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 172);
         Lkt_Node_P_Bigint_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 173);
         Lkt_Node_P_String_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 174);
         Lkt_Node_P_Symbol_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 175);
         Lkt_Node_P_Property_Error_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 176);
         Lkt_Node_P_Regexp_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 177);
         Lkt_Node_P_Entity_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 178);
         Lkt_Node_P_Entity_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 179);
         Lkt_Node_P_Logicvar_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 180);
         Lkt_Node_P_Equation_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 181);
         Lkt_Node_P_Array_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 182);
         Lkt_Node_P_Array_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 183);
         Lkt_Node_P_Astlist_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 184);
         Lkt_Node_P_Astlist_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 185);
         Lkt_Node_P_Node_Builder_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 186);
         Lkt_Node_P_Node_Builder_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 187);
         Lkt_Node_P_Iterator_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 188);
         Lkt_Node_P_Iterator_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 189);
         Lkt_Node_P_Analysis_Unit_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 190);
         Lkt_Node_P_Analysis_Unit_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 191);
         Lkt_Node_P_Topmost_Invalid_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 192);
         Lkt_Node_P_Nameres_Diagnostics : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 193);
         Lkt_Node_P_Solve_Enclosing_Context : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 194);
         Lkt_Node_P_Xref_Entry_Point : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 195);
         Class_Qualifier_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 196);
         Decl_P_Custom_Image : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 197);
         Decl_P_Decl_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 198);
         Decl_P_As_Bare_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 199);
         Decl_P_Get_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 200);
         Decl_P_Get_Cast_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 201);
         Decl_P_Get_Keep_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 202);
         Decl_P_Get_Suffix_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 203);
         Decl_P_Is_Generic : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 204);
         Decl_P_Return_Type_Is_Instantiated : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 205);
         Decl_P_Is_Instantiated : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 206);
         Decl_P_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 207);
         Decl_P_Full_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 208);
         Fun_Decl_P_Is_Dynamic_Combiner : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 209);
         Type_Decl_P_Base_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 210);
         Type_Decl_P_Base_Type_If_Entity : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 211);
         Excludes_Null_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 212);
         Expr_P_Get_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 213);
         Expr_P_Get_Generic_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 214);
         Expr_P_Get_Expected_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 215);
         Expr_P_Referenced_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 216);
         Token_Lit_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 217);
         Token_Pattern_Lit_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 218);
         Id_P_Custom_Image : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 219);
         Char_Lit_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 220);
         String_Lit_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 221);
         String_Lit_P_Is_Prefixed_String : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 222);
         String_Lit_P_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 223);
         String_Lit_P_Is_Regexp_Literal : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 224);
         Full_Decl_P_Has_Annotation : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 225);
         Import_P_Referenced_Unit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 226);
         Langkit_Root_P_Fetch_Prelude : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 227);
         Type_Ref_P_Referenced_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 228);
   end Member_Refs;

end Liblktlang.Generic_API.Introspection;
