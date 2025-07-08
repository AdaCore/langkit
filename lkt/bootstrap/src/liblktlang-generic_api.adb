
with Ada.Unchecked_Conversion;

with System;

with Liblktlang_Support.Internal.Analysis;
use Liblktlang_Support.Internal.Analysis;
with Liblktlang_Support.Internal.Conversions;

with Liblktlang.Generic_Impl;      use Liblktlang.Generic_Impl;
with Liblktlang.Generic_Introspection;
use Liblktlang.Generic_Introspection;
with Liblktlang.Implementation;
with Liblktlang.Public_Converters; use Liblktlang.Public_Converters;

package body Liblktlang.Generic_API is

   Desc_Address : constant System.Address := Desc'Address
     with Export, External_Name => "Liblktlang__language_id";

   package Lk_Convs renames Liblktlang_Support.Internal.Conversions;

   subtype Generic_Internal_Context is
     Liblktlang_Support.Internal.Analysis.Internal_Context;
   subtype Specific_Internal_Context is
     Liblktlang.Implementation.Internal_Context;

   subtype Generic_Internal_Unit is
     Liblktlang_Support.Internal.Analysis.Internal_Unit;
   subtype Specific_Internal_Unit is
     Liblktlang.Implementation.Internal_Unit;

   function "+" is new Ada.Unchecked_Conversion
     (Generic_Internal_Context, Specific_Internal_Context);
   function "+" is new Ada.Unchecked_Conversion
     (Specific_Internal_Context, Generic_Internal_Context);

   function "+" is new Ada.Unchecked_Conversion
     (Generic_Internal_Unit, Specific_Internal_Unit);
   function "+" is new Ada.Unchecked_Conversion
     (Specific_Internal_Unit, Generic_Internal_Unit);

   ------------------------
   -- To_Generic_Context --
   ------------------------

   function To_Generic_Context (Context : Analysis_Context) return Lk_Context
   is
      Ctx : constant Specific_Internal_Context := Unwrap_Context.all (Context);
   begin
      return Lk_Convs.Wrap_Context (Self_Id, +Ctx);
   end To_Generic_Context;

   --------------------------
   -- From_Generic_Context --
   --------------------------

   function From_Generic_Context (Context : Lk_Context) return Analysis_Context
   is
   begin
      if Context = No_Lk_Context then
         return No_Analysis_Context;
      elsif Context.Language /= Self_Id then
         raise Precondition_Failure with "context belongs to another language";
      else
         declare
            Ctx : constant Generic_Internal_Context :=
              Lk_Convs.Unwrap_Context (Context);
         begin
            return Wrap_Context.all (+Ctx);
         end;
      end if;
   end From_Generic_Context;

   ---------------------
   -- To_Generic_Unit --
   ---------------------

   function To_Generic_Unit (Unit : Analysis_Unit) return Lk_Unit is
      U : constant Specific_Internal_Unit := Unwrap_Unit.all (Unit);
   begin
      return Lk_Convs.Wrap_Unit (Self_Id, +U);
   end To_Generic_Unit;

   -----------------------
   -- From_Generic_Unit --
   -----------------------

   function From_Generic_Unit (Unit : Lk_Unit) return Analysis_Unit is
   begin
      if Unit = No_Lk_Unit then
         return No_Analysis_Unit;
      elsif Unit.Language /= Self_Id then
         raise Precondition_Failure with "unit belongs to another language";
      else
         declare
            U : constant Generic_Internal_Unit := Lk_Convs.Unwrap_Unit (Unit);
         begin
            return Wrap_Unit.all (+U);
         end;
      end if;
   end From_Generic_Unit;

   -----------------------------
   -- To_Generic_Grammar_Rule --
   -----------------------------

   function To_Generic_Grammar_Rule
     (Rule : Grammar_Rule) return Liblktlang_Support.Generic_API.Grammar_Rule_Ref
   is
   begin
      --  'Pos is 0-based whereas Grammar_Rule_Index is 1-based

      return From_Index (Self_Id, Grammar_Rule'Pos (Rule) + 1);
   end To_Generic_Grammar_Rule;

   -------------------------------
   -- From_Generic_Grammar_Rule --
   -------------------------------

   function From_Generic_Grammar_Rule
     (Rule : Liblktlang_Support.Generic_API.Grammar_Rule_Ref) return Grammar_Rule
   is
   begin
      if Rule = Liblktlang_Support.Generic_API.No_Grammar_Rule_Ref then
         raise Precondition_Failure
           with "null grammar rule";
      elsif Language (Rule) /= Self_Id then
         raise Precondition_Failure
           with "grammar rule belongs to another language";
      end if;

      --  'Pos is 0-based whereas Grammar_Rule_Index is 1-based

      return Grammar_Rule'Val (To_Index (Rule) - 1);
   end From_Generic_Grammar_Rule;

   ---------------------
   -- To_Generic_Node --
   ---------------------

   function To_Generic_Node
     (Node : Lkt_Node'Class) return Lk_Node
   is
      E : constant Implementation.Internal_Entity :=
        Unwrap_Entity.all (Node);
   begin
      return Lk_Convs.Wrap_Node (Self_Id, +E);
   end To_Generic_Node;

   -----------------------
   -- From_Generic_Node --
   -----------------------

   function From_Generic_Node (Node : Lk_Node) return Lkt_Node
   is
   begin
      if Node.Is_Null then
         return No_Lkt_Node;
      elsif Node.Language /= Self_Id then
         raise Precondition_Failure with "node belongs to another language";
      else
         declare
            N : constant Liblktlang_Support.Internal.Analysis.Internal_Entity :=
              Lk_Convs.Unwrap_Node (Node);
            E : constant Implementation.Internal_Entity := +N;
         begin
            return Wrap_Node.all (E.Node, E.Info);
         end;
      end if;
   end From_Generic_Node;

   --------------------------
   -- To_Generic_Node_Type --
   --------------------------

   function To_Generic_Node_Type
     (Kind : Lkt_Node_Kind_Type)
      return Liblktlang_Support.Generic_API.Introspection.Type_Ref
   is
      use Liblktlang_Support.Generic_API.Introspection;
   begin
      return From_Index (Self_Id, Node_Kinds (Kind));
   end To_Generic_Node_Type;

   ----------------------------
   -- From_Generic_Node_Type --
   ----------------------------

   function From_Generic_Node_Type
     (Kind : Liblktlang_Support.Generic_API.Introspection.Type_Ref)
      return Lkt_Node_Kind_Type
   is
      use Liblktlang_Support.Generic_API.Introspection;
   begin
      if Kind /= Liblktlang_Support.Generic_API.Introspection.No_Type_Ref then
         if Kind.Language /= Self_Id then
            raise Precondition_Failure with "type belongs to another language";
         end if;

         case To_Index (Kind) is
                  when Type_Index_For_Argument =>
                     return Lkt_Argument;
                  when Type_Index_For_Lexer_Case_Rule_Cond_Alt =>
                     return Lkt_Lexer_Case_Rule_Cond_Alt;
                  when Type_Index_For_Lexer_Case_Rule_Default_Alt =>
                     return Lkt_Lexer_Case_Rule_Default_Alt;
                  when Type_Index_For_Match_Branch =>
                     return Lkt_Match_Branch;
                  when Type_Index_For_Pattern_Match_Branch =>
                     return Lkt_Pattern_Match_Branch;
                  when Type_Index_For_Block_Expr_Clause =>
                     return Lkt_Block_Expr_Clause;
                  when Type_Index_For_Block_String_Line =>
                     return Lkt_Block_String_Line;
                  when Type_Index_For_Class_Qualifier_Absent =>
                     return Lkt_Class_Qualifier_Absent;
                  when Type_Index_For_Class_Qualifier_Present =>
                     return Lkt_Class_Qualifier_Present;
                  when Type_Index_For_Grammar_Rule_Decl =>
                     return Lkt_Grammar_Rule_Decl;
                  when Type_Index_For_Synthetic_Lexer_Decl =>
                     return Lkt_Synthetic_Lexer_Decl;
                  when Type_Index_For_Node_Decl =>
                     return Lkt_Node_Decl;
                  when Type_Index_For_Self_Decl =>
                     return Lkt_Self_Decl;
                  when Type_Index_For_Binding_Val_Decl =>
                     return Lkt_Binding_Val_Decl;
                  when Type_Index_For_Enum_Lit_Decl =>
                     return Lkt_Enum_Lit_Decl;
                  when Type_Index_For_Field_Decl =>
                     return Lkt_Field_Decl;
                  when Type_Index_For_Fun_Param_Decl =>
                     return Lkt_Fun_Param_Decl;
                  when Type_Index_For_Lambda_Param_Decl =>
                     return Lkt_Lambda_Param_Decl;
                  when Type_Index_For_Dyn_Var_Decl =>
                     return Lkt_Dyn_Var_Decl;
                  when Type_Index_For_Match_Val_Decl =>
                     return Lkt_Match_Val_Decl;
                  when Type_Index_For_Val_Decl =>
                     return Lkt_Val_Decl;
                  when Type_Index_For_Fun_Decl =>
                     return Lkt_Fun_Decl;
                  when Type_Index_For_Env_Spec_Decl =>
                     return Lkt_Env_Spec_Decl;
                  when Type_Index_For_Error_Decl =>
                     return Lkt_Error_Decl;
                  when Type_Index_For_Generic_Decl =>
                     return Lkt_Generic_Decl;
                  when Type_Index_For_Grammar_Decl =>
                     return Lkt_Grammar_Decl;
                  when Type_Index_For_Lexer_Decl =>
                     return Lkt_Lexer_Decl;
                  when Type_Index_For_Lexer_Family_Decl =>
                     return Lkt_Lexer_Family_Decl;
                  when Type_Index_For_Synth_Fun_Decl =>
                     return Lkt_Synth_Fun_Decl;
                  when Type_Index_For_Synth_Param_Decl =>
                     return Lkt_Synth_Param_Decl;
                  when Type_Index_For_Any_Type_Decl =>
                     return Lkt_Any_Type_Decl;
                  when Type_Index_For_Enum_Class_Alt_Decl =>
                     return Lkt_Enum_Class_Alt_Decl;
                  when Type_Index_For_Function_Type =>
                     return Lkt_Function_Type;
                  when Type_Index_For_Generic_Param_Type_Decl =>
                     return Lkt_Generic_Param_Type_Decl;
                  when Type_Index_For_Class_Decl =>
                     return Lkt_Class_Decl;
                  when Type_Index_For_Enum_Class_Decl =>
                     return Lkt_Enum_Class_Decl;
                  when Type_Index_For_Enum_Type_Decl =>
                     return Lkt_Enum_Type_Decl;
                  when Type_Index_For_Struct_Decl =>
                     return Lkt_Struct_Decl;
                  when Type_Index_For_Trait_Decl =>
                     return Lkt_Trait_Decl;
                  when Type_Index_For_Decl_Annotation =>
                     return Lkt_Decl_Annotation;
                  when Type_Index_For_Decl_Annotation_Args =>
                     return Lkt_Decl_Annotation_Args;
                  when Type_Index_For_Dyn_Env_Wrapper =>
                     return Lkt_Dyn_Env_Wrapper;
                  when Type_Index_For_Elsif_Branch =>
                     return Lkt_Elsif_Branch;
                  when Type_Index_For_Enum_Class_Case =>
                     return Lkt_Enum_Class_Case;
                  when Type_Index_For_Excludes_Null_Absent =>
                     return Lkt_Excludes_Null_Absent;
                  when Type_Index_For_Excludes_Null_Present =>
                     return Lkt_Excludes_Null_Present;
                  when Type_Index_For_Any_Of =>
                     return Lkt_Any_Of;
                  when Type_Index_For_Array_Literal =>
                     return Lkt_Array_Literal;
                  when Type_Index_For_Call_Expr =>
                     return Lkt_Call_Expr;
                  when Type_Index_For_Logic_Predicate =>
                     return Lkt_Logic_Predicate;
                  when Type_Index_For_Logic_Propagate_Call =>
                     return Lkt_Logic_Propagate_Call;
                  when Type_Index_For_Bin_Op =>
                     return Lkt_Bin_Op;
                  when Type_Index_For_Block_Expr =>
                     return Lkt_Block_Expr;
                  when Type_Index_For_Cast_Expr =>
                     return Lkt_Cast_Expr;
                  when Type_Index_For_Dot_Expr =>
                     return Lkt_Dot_Expr;
                  when Type_Index_For_Error_On_Null =>
                     return Lkt_Error_On_Null;
                  when Type_Index_For_Generic_Instantiation =>
                     return Lkt_Generic_Instantiation;
                  when Type_Index_For_Grammar_Cut =>
                     return Lkt_Grammar_Cut;
                  when Type_Index_For_Grammar_Discard =>
                     return Lkt_Grammar_Discard;
                  when Type_Index_For_Grammar_Dont_Skip =>
                     return Lkt_Grammar_Dont_Skip;
                  when Type_Index_For_Grammar_List =>
                     return Lkt_Grammar_List;
                  when Type_Index_For_Grammar_Null =>
                     return Lkt_Grammar_Null;
                  when Type_Index_For_Grammar_Opt =>
                     return Lkt_Grammar_Opt;
                  when Type_Index_For_Grammar_Opt_Error =>
                     return Lkt_Grammar_Opt_Error;
                  when Type_Index_For_Grammar_Opt_Error_Group =>
                     return Lkt_Grammar_Opt_Error_Group;
                  when Type_Index_For_Grammar_Opt_Group =>
                     return Lkt_Grammar_Opt_Group;
                  when Type_Index_For_Grammar_Or_Expr =>
                     return Lkt_Grammar_Or_Expr;
                  when Type_Index_For_Grammar_Pick =>
                     return Lkt_Grammar_Pick;
                  when Type_Index_For_Grammar_Implicit_Pick =>
                     return Lkt_Grammar_Implicit_Pick;
                  when Type_Index_For_Grammar_Predicate =>
                     return Lkt_Grammar_Predicate;
                  when Type_Index_For_Grammar_Rule_Ref =>
                     return Lkt_Grammar_Rule_Ref;
                  when Type_Index_For_Grammar_Skip =>
                     return Lkt_Grammar_Skip;
                  when Type_Index_For_Grammar_Stop_Cut =>
                     return Lkt_Grammar_Stop_Cut;
                  when Type_Index_For_Parse_Node_Expr =>
                     return Lkt_Parse_Node_Expr;
                  when Type_Index_For_Token_Lit =>
                     return Lkt_Token_Lit;
                  when Type_Index_For_Token_No_Case_Lit =>
                     return Lkt_Token_No_Case_Lit;
                  when Type_Index_For_Token_Pattern_Concat =>
                     return Lkt_Token_Pattern_Concat;
                  when Type_Index_For_Token_Pattern_Lit =>
                     return Lkt_Token_Pattern_Lit;
                  when Type_Index_For_Token_Ref =>
                     return Lkt_Token_Ref;
                  when Type_Index_For_Id =>
                     return Lkt_Id;
                  when Type_Index_For_Def_Id =>
                     return Lkt_Def_Id;
                  when Type_Index_For_Module_Ref_Id =>
                     return Lkt_Module_Ref_Id;
                  when Type_Index_For_Ref_Id =>
                     return Lkt_Ref_Id;
                  when Type_Index_For_If_Expr =>
                     return Lkt_If_Expr;
                  when Type_Index_For_Isa =>
                     return Lkt_Isa;
                  when Type_Index_For_Keep_Expr =>
                     return Lkt_Keep_Expr;
                  when Type_Index_For_Lambda_Expr =>
                     return Lkt_Lambda_Expr;
                  when Type_Index_For_Big_Num_Lit =>
                     return Lkt_Big_Num_Lit;
                  when Type_Index_For_Char_Lit =>
                     return Lkt_Char_Lit;
                  when Type_Index_For_Null_Lit =>
                     return Lkt_Null_Lit;
                  when Type_Index_For_Num_Lit =>
                     return Lkt_Num_Lit;
                  when Type_Index_For_Block_String_Lit =>
                     return Lkt_Block_String_Lit;
                  when Type_Index_For_Single_Line_String_Lit =>
                     return Lkt_Single_Line_String_Lit;
                  when Type_Index_For_Pattern_Single_Line_String_Lit =>
                     return Lkt_Pattern_Single_Line_String_Lit;
                  when Type_Index_For_Logic_Assign =>
                     return Lkt_Logic_Assign;
                  when Type_Index_For_Logic_Expr =>
                     return Lkt_Logic_Expr;
                  when Type_Index_For_Logic_Propagate =>
                     return Lkt_Logic_Propagate;
                  when Type_Index_For_Logic_Unify =>
                     return Lkt_Logic_Unify;
                  when Type_Index_For_Match_Expr =>
                     return Lkt_Match_Expr;
                  when Type_Index_For_Not_Expr =>
                     return Lkt_Not_Expr;
                  when Type_Index_For_Paren_Expr =>
                     return Lkt_Paren_Expr;
                  when Type_Index_For_Raise_Expr =>
                     return Lkt_Raise_Expr;
                  when Type_Index_For_Subscript_Expr =>
                     return Lkt_Subscript_Expr;
                  when Type_Index_For_Try_Expr =>
                     return Lkt_Try_Expr;
                  when Type_Index_For_Un_Op =>
                     return Lkt_Un_Op;
                  when Type_Index_For_Full_Decl =>
                     return Lkt_Full_Decl;
                  when Type_Index_For_Grammar_List_Sep =>
                     return Lkt_Grammar_List_Sep;
                  when Type_Index_For_Import =>
                     return Lkt_Import;
                  when Type_Index_For_Langkit_Root =>
                     return Lkt_Langkit_Root;
                  when Type_Index_For_Lexer_Case_Rule =>
                     return Lkt_Lexer_Case_Rule;
                  when Type_Index_For_Lexer_Case_Rule_Send =>
                     return Lkt_Lexer_Case_Rule_Send;
                  when Type_Index_For_List_Kind_One =>
                     return Lkt_List_Kind_One;
                  when Type_Index_For_List_Kind_Zero =>
                     return Lkt_List_Kind_Zero;
                  when Type_Index_For_Argument_List =>
                     return Lkt_Argument_List;
                  when Type_Index_For_Base_Lexer_Case_Rule_Alt_List =>
                     return Lkt_Base_Lexer_Case_Rule_Alt_List;
                  when Type_Index_For_Base_Match_Branch_List =>
                     return Lkt_Base_Match_Branch_List;
                  when Type_Index_For_Block_String_Line_List =>
                     return Lkt_Block_String_Line_List;
                  when Type_Index_For_Call_Expr_List =>
                     return Lkt_Call_Expr_List;
                  when Type_Index_For_Decl_Annotation_List =>
                     return Lkt_Decl_Annotation_List;
                  when Type_Index_For_Elsif_Branch_List =>
                     return Lkt_Elsif_Branch_List;
                  when Type_Index_For_Enum_Class_Alt_Decl_List =>
                     return Lkt_Enum_Class_Alt_Decl_List;
                  when Type_Index_For_Enum_Class_Case_List =>
                     return Lkt_Enum_Class_Case_List;
                  when Type_Index_For_Enum_Lit_Decl_List =>
                     return Lkt_Enum_Lit_Decl_List;
                  when Type_Index_For_Expr_List =>
                     return Lkt_Expr_List;
                  when Type_Index_For_Any_Of_List =>
                     return Lkt_Any_Of_List;
                  when Type_Index_For_Full_Decl_List =>
                     return Lkt_Full_Decl_List;
                  when Type_Index_For_Decl_Block =>
                     return Lkt_Decl_Block;
                  when Type_Index_For_Generic_Param_Decl_List =>
                     return Lkt_Generic_Param_Decl_List;
                  when Type_Index_For_Fun_Param_Decl_List =>
                     return Lkt_Fun_Param_Decl_List;
                  when Type_Index_For_Grammar_Expr_List =>
                     return Lkt_Grammar_Expr_List;
                  when Type_Index_For_Grammar_Expr_List_List =>
                     return Lkt_Grammar_Expr_List_List;
                  when Type_Index_For_Import_List =>
                     return Lkt_Import_List;
                  when Type_Index_For_Lambda_Param_Decl_List =>
                     return Lkt_Lambda_Param_Decl_List;
                  when Type_Index_For_Lkt_Node_List =>
                     return Lkt_Lkt_Node_List;
                  when Type_Index_For_Pattern_Detail_List =>
                     return Lkt_Pattern_Detail_List;
                  when Type_Index_For_Pattern_List =>
                     return Lkt_Pattern_List;
                  when Type_Index_For_Ref_Id_List =>
                     return Lkt_Ref_Id_List;
                  when Type_Index_For_Type_Ref_List =>
                     return Lkt_Type_Ref_List;
                  when Type_Index_For_Synthetic_Type_Ref_List =>
                     return Lkt_Synthetic_Type_Ref_List;
                  when Type_Index_For_Null_Cond_Qualifier_Absent =>
                     return Lkt_Null_Cond_Qualifier_Absent;
                  when Type_Index_For_Null_Cond_Qualifier_Present =>
                     return Lkt_Null_Cond_Qualifier_Present;
                  when Type_Index_For_Op_Amp =>
                     return Lkt_Op_Amp;
                  when Type_Index_For_Op_And =>
                     return Lkt_Op_And;
                  when Type_Index_For_Op_Div =>
                     return Lkt_Op_Div;
                  when Type_Index_For_Op_Eq =>
                     return Lkt_Op_Eq;
                  when Type_Index_For_Op_Gt =>
                     return Lkt_Op_Gt;
                  when Type_Index_For_Op_Gte =>
                     return Lkt_Op_Gte;
                  when Type_Index_For_Op_Logic_And =>
                     return Lkt_Op_Logic_And;
                  when Type_Index_For_Op_Logic_Or =>
                     return Lkt_Op_Logic_Or;
                  when Type_Index_For_Op_Lt =>
                     return Lkt_Op_Lt;
                  when Type_Index_For_Op_Lte =>
                     return Lkt_Op_Lte;
                  when Type_Index_For_Op_Minus =>
                     return Lkt_Op_Minus;
                  when Type_Index_For_Op_Mult =>
                     return Lkt_Op_Mult;
                  when Type_Index_For_Op_Ne =>
                     return Lkt_Op_Ne;
                  when Type_Index_For_Op_Or =>
                     return Lkt_Op_Or;
                  when Type_Index_For_Op_Or_Int =>
                     return Lkt_Op_Or_Int;
                  when Type_Index_For_Op_Plus =>
                     return Lkt_Op_Plus;
                  when Type_Index_For_Any_Type_Pattern =>
                     return Lkt_Any_Type_Pattern;
                  when Type_Index_For_Binding_Pattern =>
                     return Lkt_Binding_Pattern;
                  when Type_Index_For_Bool_Pattern_False =>
                     return Lkt_Bool_Pattern_False;
                  when Type_Index_For_Bool_Pattern_True =>
                     return Lkt_Bool_Pattern_True;
                  when Type_Index_For_Ellipsis_Pattern =>
                     return Lkt_Ellipsis_Pattern;
                  when Type_Index_For_Extended_Pattern =>
                     return Lkt_Extended_Pattern;
                  when Type_Index_For_Filtered_Pattern =>
                     return Lkt_Filtered_Pattern;
                  when Type_Index_For_Integer_Pattern =>
                     return Lkt_Integer_Pattern;
                  when Type_Index_For_List_Pattern =>
                     return Lkt_List_Pattern;
                  when Type_Index_For_Not_Pattern =>
                     return Lkt_Not_Pattern;
                  when Type_Index_For_Null_Pattern =>
                     return Lkt_Null_Pattern;
                  when Type_Index_For_Or_Pattern =>
                     return Lkt_Or_Pattern;
                  when Type_Index_For_Paren_Pattern =>
                     return Lkt_Paren_Pattern;
                  when Type_Index_For_Regex_Pattern =>
                     return Lkt_Regex_Pattern;
                  when Type_Index_For_Tuple_Pattern =>
                     return Lkt_Tuple_Pattern;
                  when Type_Index_For_Type_Pattern =>
                     return Lkt_Type_Pattern;
                  when Type_Index_For_Field_Pattern_Detail =>
                     return Lkt_Field_Pattern_Detail;
                  when Type_Index_For_Property_Pattern_Detail =>
                     return Lkt_Property_Pattern_Detail;
                  when Type_Index_For_Selector_Pattern_Detail =>
                     return Lkt_Selector_Pattern_Detail;
                  when Type_Index_For_Selector_Call =>
                     return Lkt_Selector_Call;
                  when Type_Index_For_Default_List_Type_Ref =>
                     return Lkt_Default_List_Type_Ref;
                  when Type_Index_For_Function_Type_Ref =>
                     return Lkt_Function_Type_Ref;
                  when Type_Index_For_Generic_Type_Ref =>
                     return Lkt_Generic_Type_Ref;
                  when Type_Index_For_Simple_Type_Ref =>
                     return Lkt_Simple_Type_Ref;
                  when Type_Index_For_Var_Bind =>
                     return Lkt_Var_Bind;

            when others =>
               null;
         end case;
      end if;

      raise Precondition_Failure with "concrete node type expected";
   end From_Generic_Node_Type;

end Liblktlang.Generic_API;
