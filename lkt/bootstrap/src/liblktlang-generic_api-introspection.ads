
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
         Lookup_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 12);
         Designated_Env_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 13);
         Grammar_Rule : constant G.Type_Ref :=
           G.From_Index (Self_Id, 14);
         Lkt_Node_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 15);
         Logic_Context_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 16);
         Solver_Diagnostic_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 17);
         Decoded_Char_Value : constant G.Type_Ref :=
           G.From_Index (Self_Id, 18);
         Decoded_String_Value : constant G.Type_Ref :=
           G.From_Index (Self_Id, 19);
         Logic_Context : constant G.Type_Ref :=
           G.From_Index (Self_Id, 20);
         Solver_Diagnostic : constant G.Type_Ref :=
           G.From_Index (Self_Id, 21);
         Solver_Result : constant G.Type_Ref :=
           G.From_Index (Self_Id, 22);
         Lkt_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 23);
         Base_Lexer_Case_Rule_Alt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 24);
         Lexer_Case_Rule_Cond_Alt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 25);
         Lexer_Case_Rule_Default_Alt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 26);
         Block_String_Line : constant G.Type_Ref :=
           G.From_Index (Self_Id, 27);
         Class_Qualifier : constant G.Type_Ref :=
           G.From_Index (Self_Id, 28);
         Class_Qualifier_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 29);
         Class_Qualifier_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 30);
         Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 31);
         Base_Grammar_Rule_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 32);
         Grammar_Rule_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 33);
         Synthetic_Lexer_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 34);
         Base_Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 35);
         Node_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 36);
         Self_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 37);
         User_Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 38);
         Enum_Lit_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 39);
         Explicitly_Typed_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 40);
         Component_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 41);
         Field_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 42);
         Fun_Arg_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 43);
         Lambda_Arg_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 44);
         Dyn_Var_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 45);
         Match_Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 46);
         Val_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 47);
         Fun_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 48);
         Env_Spec_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 49);
         Generic_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 50);
         Grammar_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 51);
         Lexer_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 52);
         Lexer_Family_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 53);
         Synth_Arg_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 54);
         Synth_Fun_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 55);
         Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 56);
         Any_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 57);
         Enum_Class_Alt_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 58);
         Function_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 59);
         Generic_Formal_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 60);
         Named_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 61);
         Basic_Class_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 62);
         Class_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 63);
         Enum_Class_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 64);
         Enum_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 65);
         Struct_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 66);
         Trait_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 67);
         Decl_Annotation : constant G.Type_Ref :=
           G.From_Index (Self_Id, 68);
         Decl_Annotation_Params : constant G.Type_Ref :=
           G.From_Index (Self_Id, 69);
         Dyn_Env_Wrapper : constant G.Type_Ref :=
           G.From_Index (Self_Id, 70);
         Elsif_Branch : constant G.Type_Ref :=
           G.From_Index (Self_Id, 71);
         Enum_Class_Case : constant G.Type_Ref :=
           G.From_Index (Self_Id, 72);
         Excludes_Null : constant G.Type_Ref :=
           G.From_Index (Self_Id, 73);
         Excludes_Null_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 74);
         Excludes_Null_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 75);
         Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 76);
         Any_Of : constant G.Type_Ref :=
           G.From_Index (Self_Id, 77);
         Array_Literal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 78);
         Base_Call_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 79);
         Call_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 80);
         Logic_Call_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 81);
         Logic_Predicate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 82);
         Logic_Propagate_Call : constant G.Type_Ref :=
           G.From_Index (Self_Id, 83);
         Base_Dot_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 84);
         Dot_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 85);
         Null_Cond_Dotted_Name : constant G.Type_Ref :=
           G.From_Index (Self_Id, 86);
         Bin_Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 87);
         Block_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 88);
         Cast_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 89);
         Error_On_Null : constant G.Type_Ref :=
           G.From_Index (Self_Id, 90);
         Generic_Instantiation : constant G.Type_Ref :=
           G.From_Index (Self_Id, 91);
         Grammar_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 92);
         Grammar_Cut : constant G.Type_Ref :=
           G.From_Index (Self_Id, 93);
         Grammar_Discard : constant G.Type_Ref :=
           G.From_Index (Self_Id, 94);
         Grammar_Dont_Skip : constant G.Type_Ref :=
           G.From_Index (Self_Id, 95);
         Grammar_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 96);
         Grammar_Null : constant G.Type_Ref :=
           G.From_Index (Self_Id, 97);
         Grammar_Opt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 98);
         Grammar_Opt_Error : constant G.Type_Ref :=
           G.From_Index (Self_Id, 99);
         Grammar_Opt_Error_Group : constant G.Type_Ref :=
           G.From_Index (Self_Id, 100);
         Grammar_Opt_Group : constant G.Type_Ref :=
           G.From_Index (Self_Id, 101);
         Grammar_Or_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 102);
         Grammar_Pick : constant G.Type_Ref :=
           G.From_Index (Self_Id, 103);
         Grammar_Implicit_Pick : constant G.Type_Ref :=
           G.From_Index (Self_Id, 104);
         Grammar_Predicate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 105);
         Grammar_Rule_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 106);
         Grammar_Skip : constant G.Type_Ref :=
           G.From_Index (Self_Id, 107);
         Grammar_Stop_Cut : constant G.Type_Ref :=
           G.From_Index (Self_Id, 108);
         Parse_Node_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 109);
         Token_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 110);
         Token_No_Case_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 111);
         Token_Pattern_Concat : constant G.Type_Ref :=
           G.From_Index (Self_Id, 112);
         Token_Pattern_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 113);
         Token_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 114);
         Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 115);
         Def_Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 116);
         Module_Ref_Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 117);
         Ref_Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 118);
         If_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 119);
         Isa : constant G.Type_Ref :=
           G.From_Index (Self_Id, 120);
         Keep_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 121);
         Lambda_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 122);
         Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 123);
         Big_Num_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 124);
         Char_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 125);
         Null_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 126);
         Num_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 127);
         String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 128);
         Block_String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 129);
         Single_Line_String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 130);
         Pattern_Single_Line_String_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 131);
         Logic_Assign : constant G.Type_Ref :=
           G.From_Index (Self_Id, 132);
         Logic_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 133);
         Logic_Propagate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 134);
         Logic_Unify : constant G.Type_Ref :=
           G.From_Index (Self_Id, 135);
         Match_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 136);
         Not_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 137);
         Paren_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 138);
         Raise_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 139);
         Subscript_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 140);
         Null_Cond_Subscript_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 141);
         Try_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 142);
         Un_Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 143);
         Full_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 144);
         Grammar_List_Sep : constant G.Type_Ref :=
           G.From_Index (Self_Id, 145);
         Import : constant G.Type_Ref :=
           G.From_Index (Self_Id, 146);
         Langkit_Root : constant G.Type_Ref :=
           G.From_Index (Self_Id, 147);
         Lexer_Case_Rule : constant G.Type_Ref :=
           G.From_Index (Self_Id, 148);
         Lexer_Case_Rule_Send : constant G.Type_Ref :=
           G.From_Index (Self_Id, 149);
         List_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 150);
         List_Kind_One : constant G.Type_Ref :=
           G.From_Index (Self_Id, 151);
         List_Kind_Zero : constant G.Type_Ref :=
           G.From_Index (Self_Id, 152);
         Lkt_Node_Base_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 153);
         Base_Lexer_Case_Rule_Alt_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 154);
         Block_String_Line_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 155);
         Call_Expr_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 156);
         Decl_Annotation_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 157);
         Elsif_Branch_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 158);
         Enum_Class_Alt_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 159);
         Enum_Class_Case_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 160);
         Enum_Lit_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 161);
         Expr_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 162);
         Any_Of_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 163);
         Full_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 164);
         Decl_Block : constant G.Type_Ref :=
           G.From_Index (Self_Id, 165);
         Generic_Formal_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 166);
         Fun_Arg_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 167);
         Grammar_Expr_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 168);
         Grammar_Expr_List_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 169);
         Import_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 170);
         Lambda_Arg_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 171);
         Lkt_Node_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 172);
         Block_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 173);
         Match_Branch_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 174);
         Param_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 175);
         Ref_Id_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 176);
         Type_Ref_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 177);
         Isa_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 178);
         Match_Branch : constant G.Type_Ref :=
           G.From_Index (Self_Id, 179);
         Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 180);
         Op_Amp : constant G.Type_Ref :=
           G.From_Index (Self_Id, 181);
         Op_And : constant G.Type_Ref :=
           G.From_Index (Self_Id, 182);
         Op_Div : constant G.Type_Ref :=
           G.From_Index (Self_Id, 183);
         Op_Eq : constant G.Type_Ref :=
           G.From_Index (Self_Id, 184);
         Op_Gt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 185);
         Op_Gte : constant G.Type_Ref :=
           G.From_Index (Self_Id, 186);
         Op_Logic_And : constant G.Type_Ref :=
           G.From_Index (Self_Id, 187);
         Op_Logic_Or : constant G.Type_Ref :=
           G.From_Index (Self_Id, 188);
         Op_Lt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 189);
         Op_Lte : constant G.Type_Ref :=
           G.From_Index (Self_Id, 190);
         Op_Minus : constant G.Type_Ref :=
           G.From_Index (Self_Id, 191);
         Op_Mult : constant G.Type_Ref :=
           G.From_Index (Self_Id, 192);
         Op_Ne : constant G.Type_Ref :=
           G.From_Index (Self_Id, 193);
         Op_Or : constant G.Type_Ref :=
           G.From_Index (Self_Id, 194);
         Op_Or_Int : constant G.Type_Ref :=
           G.From_Index (Self_Id, 195);
         Op_Plus : constant G.Type_Ref :=
           G.From_Index (Self_Id, 196);
         Param : constant G.Type_Ref :=
           G.From_Index (Self_Id, 197);
         Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 198);
         Default_List_Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 199);
         Function_Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 200);
         Generic_Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 201);
         Simple_Type_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 202);
         Var_Bind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 203);
   end Type_Refs;

   Kind_To_Type : constant array (Lkt_Node_Kind_Type) of G.Type_Ref :=
     (Lkt_Lexer_Case_Rule_Cond_Alt => Type_Refs.Lexer_Case_Rule_Cond_Alt,
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
      Lkt_Fun_Arg_Decl => Type_Refs.Fun_Arg_Decl,
      Lkt_Lambda_Arg_Decl => Type_Refs.Lambda_Arg_Decl,
      Lkt_Dyn_Var_Decl => Type_Refs.Dyn_Var_Decl,
      Lkt_Match_Val_Decl => Type_Refs.Match_Val_Decl,
      Lkt_Val_Decl => Type_Refs.Val_Decl,
      Lkt_Fun_Decl => Type_Refs.Fun_Decl,
      Lkt_Env_Spec_Decl => Type_Refs.Env_Spec_Decl,
      Lkt_Generic_Decl => Type_Refs.Generic_Decl,
      Lkt_Grammar_Decl => Type_Refs.Grammar_Decl,
      Lkt_Lexer_Decl => Type_Refs.Lexer_Decl,
      Lkt_Lexer_Family_Decl => Type_Refs.Lexer_Family_Decl,
      Lkt_Synth_Arg_Decl => Type_Refs.Synth_Arg_Decl,
      Lkt_Synth_Fun_Decl => Type_Refs.Synth_Fun_Decl,
      Lkt_Any_Type_Decl => Type_Refs.Any_Type_Decl,
      Lkt_Enum_Class_Alt_Decl => Type_Refs.Enum_Class_Alt_Decl,
      Lkt_Function_Type => Type_Refs.Function_Type,
      Lkt_Generic_Formal_Type_Decl => Type_Refs.Generic_Formal_Type_Decl,
      Lkt_Class_Decl => Type_Refs.Class_Decl,
      Lkt_Enum_Class_Decl => Type_Refs.Enum_Class_Decl,
      Lkt_Enum_Type_Decl => Type_Refs.Enum_Type_Decl,
      Lkt_Struct_Decl => Type_Refs.Struct_Decl,
      Lkt_Trait_Decl => Type_Refs.Trait_Decl,
      Lkt_Decl_Annotation => Type_Refs.Decl_Annotation,
      Lkt_Decl_Annotation_Params => Type_Refs.Decl_Annotation_Params,
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
      Lkt_Generic_Formal_Decl_List => Type_Refs.Generic_Formal_Decl_List,
      Lkt_Fun_Arg_Decl_List => Type_Refs.Fun_Arg_Decl_List,
      Lkt_Grammar_Expr_List => Type_Refs.Grammar_Expr_List,
      Lkt_Grammar_Expr_List_List => Type_Refs.Grammar_Expr_List_List,
      Lkt_Import_List => Type_Refs.Import_List,
      Lkt_Lambda_Arg_Decl_List => Type_Refs.Lambda_Arg_Decl_List,
      Lkt_Lkt_Node_List => Type_Refs.Lkt_Node_List,
      Lkt_Block_Decl_List => Type_Refs.Block_Decl_List,
      Lkt_Match_Branch_List => Type_Refs.Match_Branch_List,
      Lkt_Param_List => Type_Refs.Param_List,
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
      Lkt_Param => Type_Refs.Param,
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
         Base_Lexer_Case_Rule_Alt_F_Send : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 18);
         Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 19);
         Decl_F_Syn_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 20);
         Base_Grammar_Rule_Decl_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 21);
         Explicitly_Typed_Decl_F_Decl_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 22);
         Component_Decl_F_Default_Val : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 23);
         Fun_Arg_Decl_F_Decl_Annotations : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 24);
         Val_Decl_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 25);
         Fun_Decl_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 26);
         Fun_Decl_F_Return_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 27);
         Fun_Decl_F_Body : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 28);
         Env_Spec_Decl_F_Actions : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 29);
         Generic_Decl_F_Generic_Formal_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 30);
         Generic_Decl_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 31);
         Grammar_Decl_F_Rules : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 32);
         Lexer_Decl_F_Rules : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 33);
         Lexer_Family_Decl_F_Rules : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 34);
         Type_Decl_F_Traits : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 35);
         Type_Decl_F_Syn_Base_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 36);
         Generic_Formal_Type_Decl_F_Has_Class : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 37);
         Named_Type_Decl_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 38);
         Enum_Class_Decl_F_Branches : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 39);
         Enum_Type_Decl_F_Literals : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 40);
         Decl_Annotation_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 41);
         Decl_Annotation_F_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 42);
         Decl_Annotation_Params_F_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 43);
         Elsif_Branch_F_Cond_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 44);
         Elsif_Branch_F_Then_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 45);
         Enum_Class_Case_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 46);
         Any_Of_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 47);
         Any_Of_F_Values : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 48);
         Array_Literal_F_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 49);
         Array_Literal_F_Element_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 50);
         Base_Call_Expr_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 51);
         Base_Call_Expr_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 52);
         Base_Dot_Expr_F_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 53);
         Base_Dot_Expr_F_Suffix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 54);
         Bin_Op_F_Left : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 55);
         Bin_Op_F_Op : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 56);
         Bin_Op_F_Right : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 57);
         Block_Expr_F_Val_Defs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 58);
         Block_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 59);
         Cast_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 60);
         Cast_Expr_F_Excludes_Null : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 61);
         Cast_Expr_F_Dest_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 62);
         Error_On_Null_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 63);
         Generic_Instantiation_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 64);
         Generic_Instantiation_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 65);
         Grammar_Discard_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 66);
         Grammar_Dont_Skip_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 67);
         Grammar_Dont_Skip_F_Dont_Skip : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 68);
         Grammar_List_F_List_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 69);
         Grammar_List_F_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 70);
         Grammar_List_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 71);
         Grammar_List_F_Sep : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 72);
         Grammar_Null_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 73);
         Grammar_Opt_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 74);
         Grammar_Opt_Error_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 75);
         Grammar_Opt_Error_Group_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 76);
         Grammar_Opt_Group_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 77);
         Grammar_Or_Expr_F_Sub_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 78);
         Grammar_Pick_F_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 79);
         Grammar_Predicate_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 80);
         Grammar_Predicate_F_Prop_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 81);
         Grammar_Rule_Ref_F_Node_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 82);
         Grammar_Skip_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 83);
         Grammar_Stop_Cut_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 84);
         Parse_Node_Expr_F_Node_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 85);
         Parse_Node_Expr_F_Sub_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 86);
         Token_No_Case_Lit_F_Lit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 87);
         Token_Pattern_Concat_F_Left : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 88);
         Token_Pattern_Concat_F_Right : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 89);
         Token_Ref_F_Token_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 90);
         Token_Ref_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 91);
         If_Expr_F_Cond_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 92);
         If_Expr_F_Then_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 93);
         If_Expr_F_Alternatives : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 94);
         If_Expr_F_Else_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 95);
         Isa_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 96);
         Isa_F_Dest_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 97);
         Keep_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 98);
         Keep_Expr_F_Keep_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 99);
         Lambda_Expr_F_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 100);
         Lambda_Expr_F_Return_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 101);
         Lambda_Expr_F_Body : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 102);
         Null_Lit_F_Dest_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 103);
         Block_String_Lit_F_Lines : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 104);
         Logic_Assign_F_Dest_Var : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 105);
         Logic_Assign_F_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 106);
         Logic_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 107);
         Logic_Propagate_F_Dest_Var : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 108);
         Logic_Propagate_F_Call : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 109);
         Logic_Unify_F_Lhs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 110);
         Logic_Unify_F_Rhs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 111);
         Match_Expr_F_Match_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 112);
         Match_Expr_F_Branches : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 113);
         Not_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 114);
         Paren_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 115);
         Raise_Expr_F_Dest_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 116);
         Raise_Expr_F_Except_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 117);
         Subscript_Expr_F_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 118);
         Subscript_Expr_F_Index : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 119);
         Try_Expr_F_Try_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 120);
         Try_Expr_F_Or_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 121);
         Un_Op_F_Op : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 122);
         Un_Op_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 123);
         Full_Decl_F_Doc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 124);
         Full_Decl_F_Decl_Annotations : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 125);
         Full_Decl_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 126);
         Grammar_List_Sep_F_Token : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 127);
         Grammar_List_Sep_F_Extra : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 128);
         Import_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 129);
         Langkit_Root_F_Imports : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 130);
         Langkit_Root_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 131);
         Lexer_Case_Rule_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 132);
         Lexer_Case_Rule_F_Alts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 133);
         Lexer_Case_Rule_Send_F_Sent : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 134);
         Lexer_Case_Rule_Send_F_Match_Size : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 135);
         Match_Branch_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 136);
         Match_Branch_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 137);
         Param_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 138);
         Param_F_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 139);
         Function_Type_Ref_F_Args_Types : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 140);
         Function_Type_Ref_F_Return_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 141);
         Generic_Type_Ref_F_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 142);
         Generic_Type_Ref_F_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 143);
         Simple_Type_Ref_F_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 144);
         Var_Bind_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 145);
         Var_Bind_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 146);
         Lkt_Node_P_Set_Solver_Debug_Mode : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 147);
         Lkt_Node_P_Basic_Trait_Gen : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 148);
         Lkt_Node_P_Basic_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 149);
         Lkt_Node_P_Node_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 150);
         Lkt_Node_P_Node_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 151);
         Lkt_Node_P_Indexable_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 152);
         Lkt_Node_P_Indexable_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 153);
         Lkt_Node_P_Token_Node_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 154);
         Lkt_Node_P_Error_Node_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 155);
         Lkt_Node_P_Char_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 156);
         Lkt_Node_P_Int_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 157);
         Lkt_Node_P_Bool_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 158);
         Lkt_Node_P_Bigint_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 159);
         Lkt_Node_P_String_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 160);
         Lkt_Node_P_Symbol_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 161);
         Lkt_Node_P_Property_Error_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 162);
         Lkt_Node_P_Regexp_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 163);
         Lkt_Node_P_Entity_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 164);
         Lkt_Node_P_Entity_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 165);
         Lkt_Node_P_Logicvar_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 166);
         Lkt_Node_P_Equation_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 167);
         Lkt_Node_P_Array_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 168);
         Lkt_Node_P_Array_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 169);
         Lkt_Node_P_Astlist_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 170);
         Lkt_Node_P_Astlist_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 171);
         Lkt_Node_P_Node_Builder_Gen_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 172);
         Lkt_Node_P_Node_Builder_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 173);
         Lkt_Node_P_Iterator_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 174);
         Lkt_Node_P_Iterator_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 175);
         Lkt_Node_P_Analysis_Unit_Gen_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 176);
         Lkt_Node_P_Analysis_Unit_Trait : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 177);
         Lkt_Node_P_Topmost_Invalid_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 178);
         Lkt_Node_P_Nameres_Diagnostics : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 179);
         Lkt_Node_P_Solve_Enclosing_Context : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 180);
         Lkt_Node_P_Xref_Entry_Point : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 181);
         Parent : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 182);
         Parents : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 183);
         Children : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 184);
         Token_Start : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 185);
         Token_End : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 186);
         Child_Index : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 187);
         Previous_Sibling : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 188);
         Next_Sibling : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 189);
         Unit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 190);
         Is_Ghost : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 191);
         Full_Sloc_Image : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 192);
         Class_Qualifier_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 193);
         Decl_P_Custom_Image : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 194);
         Decl_P_Decl_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 195);
         Decl_P_As_Bare_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 196);
         Decl_P_Get_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 197);
         Decl_P_Get_Cast_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 198);
         Decl_P_Get_Keep_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 199);
         Decl_P_Get_Suffix_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 200);
         Decl_P_Is_Generic : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 201);
         Decl_P_Return_Type_Is_Instantiated : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 202);
         Decl_P_Is_Instantiated : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 203);
         Decl_P_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 204);
         Decl_P_Full_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 205);
         Fun_Decl_P_Is_Dynamic_Combiner : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 206);
         Type_Decl_P_Base_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 207);
         Type_Decl_P_Base_Type_If_Entity : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 208);
         Excludes_Null_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 209);
         Expr_P_Get_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 210);
         Expr_P_Get_Generic_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 211);
         Expr_P_Get_Expected_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 212);
         Expr_P_Referenced_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 213);
         Token_Lit_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 214);
         Token_Pattern_Lit_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 215);
         Id_P_Custom_Image : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 216);
         Char_Lit_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 217);
         String_Lit_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 218);
         String_Lit_P_Is_Prefixed_String : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 219);
         String_Lit_P_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 220);
         String_Lit_P_Is_Regexp_Literal : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 221);
         Full_Decl_P_Has_Annotation : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 222);
         Import_P_Referenced_Unit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 223);
         Type_Ref_P_Referenced_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 224);
   end Member_Refs;

end Liblktlang.Generic_API.Introspection;
