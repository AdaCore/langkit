
with Liblktlang_Support.Internal; use Liblktlang_Support.Internal;
with Liblktlang_Support.Internal.Unparsing;
use Liblktlang_Support.Internal.Unparsing;
with Liblktlang_Support.Text;     use Liblktlang_Support.Text;

with Liblktlang.Generic_Introspection;
use Liblktlang.Generic_Introspection;

--  This package provides description tables to enable the generic
--  unparsing API in Liblktlang_Support to work with this Langkit-generated
--  library.

private package Liblktlang.Unparsers is

   


   Token_Spacings : aliased constant Token_Spacing_Table_Impl := (
   
      Token_Index_For_Alphanumericals => (
         Token_Index_For_Alphanumericals => True
         , Token_Index_For_Default_Family => False
      )
      , Token_Index_For_Default_Family => (
         Token_Index_For_Alphanumericals => False
         , Token_Index_For_Default_Family => False
      )
   );

   Token_Newlines : aliased constant Token_Newline_Table_Impl :=
     (Token_Index_For_Lkt_Amp => False,
      Token_Index_For_Lkt_And_Kw => False,
      Token_Index_For_Lkt_At => False,
      Token_Index_For_Lkt_Big_Number => False,
      Token_Index_For_Lkt_Bind_Kw => False,
      Token_Index_For_Lkt_Block_String_Line => True,
      Token_Index_For_Lkt_Case_Kw => False,
      Token_Index_For_Lkt_Char => False,
      Token_Index_For_Lkt_Class_Kw => False,
      Token_Index_For_Lkt_Colon => False,
      Token_Index_For_Lkt_Comb => False,
      Token_Index_For_Lkt_Comma => False,
      Token_Index_For_Lkt_Comment => True,
      Token_Index_For_Lkt_Discard_Kw => False,
      Token_Index_For_Lkt_Div => False,
      Token_Index_For_Lkt_Doc_Comment => True,
      Token_Index_For_Lkt_Dot => False,
      Token_Index_For_Lkt_Dyn_Var_Kw => False,
      Token_Index_For_Lkt_E_Q => False,
      Token_Index_For_Lkt_Elif_Kw => False,
      Token_Index_For_Lkt_Else_Kw => False,
      Token_Index_For_Lkt_Enum_Kw => False,
      Token_Index_For_Lkt_Equal => False,
      Token_Index_For_Lkt_Excl_Mark => False,
      Token_Index_For_Lkt_Fat_Right_Arrow => False,
      Token_Index_For_Lkt_Fun_Kw => False,
      Token_Index_For_Lkt_G_T => False,
      Token_Index_For_Lkt_G_T_E => False,
      Token_Index_For_Lkt_Generic_Kw => False,
      Token_Index_For_Lkt_Grammar_Kw => False,
      Token_Index_For_Lkt_Identifier => False,
      Token_Index_For_Lkt_If_Kw => False,
      Token_Index_For_Lkt_Implements_Kw => False,
      Token_Index_For_Lkt_Import_Kw => False,
      Token_Index_For_Lkt_In_Kw => False,
      Token_Index_For_Lkt_Int_Mark => False,
      Token_Index_For_Lkt_Is_Kw => False,
      Token_Index_For_Lkt_L_Brace => False,
      Token_Index_For_Lkt_L_Brack => False,
      Token_Index_For_Lkt_L_Par => False,
      Token_Index_For_Lkt_L_T => False,
      Token_Index_For_Lkt_L_T_E => False,
      Token_Index_For_Lkt_Left_Arrow => False,
      Token_Index_For_Lkt_Lexer_Kw => False,
      Token_Index_For_Lkt_Lexing_Failure => False,
      Token_Index_For_Lkt_Match_Kw => False,
      Token_Index_For_Lkt_Minus => False,
      Token_Index_For_Lkt_N_E => False,
      Token_Index_For_Lkt_Not_Kw => False,
      Token_Index_For_Lkt_Null_Kw => False,
      Token_Index_For_Lkt_Number => False,
      Token_Index_For_Lkt_Or_Kw => False,
      Token_Index_For_Lkt_P_String => False,
      Token_Index_For_Lkt_Percent => False,
      Token_Index_For_Lkt_Pipe => False,
      Token_Index_For_Lkt_Plus => False,
      Token_Index_For_Lkt_Private_Kw => False,
      Token_Index_For_Lkt_Public_Kw => False,
      Token_Index_For_Lkt_R_Brace => False,
      Token_Index_For_Lkt_R_Brack => False,
      Token_Index_For_Lkt_R_Par => False,
      Token_Index_For_Lkt_Raise_Kw => False,
      Token_Index_For_Lkt_Right_Arrow => False,
      Token_Index_For_Lkt_Semicolon => False,
      Token_Index_For_Lkt_Splat => False,
      Token_Index_For_Lkt_String => False,
      Token_Index_For_Lkt_Struct_Kw => False,
      Token_Index_For_Lkt_Termination => False,
      Token_Index_For_Lkt_Then_Kw => False,
      Token_Index_For_Lkt_Times => False,
      Token_Index_For_Lkt_Trait_Kw => False,
      Token_Index_For_Lkt_Try_Kw => False,
      Token_Index_For_Lkt_Two_Sided_Arrow => False,
      Token_Index_For_Lkt_Val_Kw => False,
      Token_Index_For_Lkt_When_Kw => False,
      Token_Index_For_Lkt_Whitespace => False);


   
      
         Text_For_Token_Unparser_0 : aliased constant Text_Type := "!";
      Token_Unparser_0 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Excl_Mark, Text_For_Token_Unparser_0'Access);
      
         Text_For_Token_Unparser_1 : aliased constant Text_Type := "!=";
      Token_Unparser_1 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_N_E, Text_For_Token_Unparser_1'Access);
      
         Text_For_Token_Unparser_2 : aliased constant Text_Type := "%";
      Token_Unparser_2 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Percent, Text_For_Token_Unparser_2'Access);
      
         Text_For_Token_Unparser_3 : aliased constant Text_Type := "&";
      Token_Unparser_3 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Amp, Text_For_Token_Unparser_3'Access);
      
         Text_For_Token_Unparser_4 : aliased constant Text_Type := "(";
      Token_Unparser_4 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_L_Par, Text_For_Token_Unparser_4'Access);
      
         Text_For_Token_Unparser_5 : aliased constant Text_Type := ")";
      Token_Unparser_5 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_R_Par, Text_For_Token_Unparser_5'Access);
      
         Text_For_Token_Unparser_6 : aliased constant Text_Type := "*";
      Token_Unparser_6 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Times, Text_For_Token_Unparser_6'Access);
      
         Text_For_Token_Unparser_7 : aliased constant Text_Type := "+";
      Token_Unparser_7 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Plus, Text_For_Token_Unparser_7'Access);
      
         Text_For_Token_Unparser_8 : aliased constant Text_Type := ",";
      Token_Unparser_8 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Comma, Text_For_Token_Unparser_8'Access);
      
         Text_For_Token_Unparser_9 : aliased constant Text_Type := "-";
      Token_Unparser_9 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Minus, Text_For_Token_Unparser_9'Access);
      
         Text_For_Token_Unparser_10 : aliased constant Text_Type := "->";
      Token_Unparser_10 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Right_Arrow, Text_For_Token_Unparser_10'Access);
      
         Text_For_Token_Unparser_11 : aliased constant Text_Type := ".";
      Token_Unparser_11 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Dot, Text_For_Token_Unparser_11'Access);
      
         Text_For_Token_Unparser_12 : aliased constant Text_Type := "...";
      Token_Unparser_12 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Splat, Text_For_Token_Unparser_12'Access);
      
         Text_For_Token_Unparser_13 : aliased constant Text_Type := "/";
      Token_Unparser_13 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Div, Text_For_Token_Unparser_13'Access);
      
         Text_For_Token_Unparser_14 : aliased constant Text_Type := ":";
      Token_Unparser_14 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Colon, Text_For_Token_Unparser_14'Access);
      
         Text_For_Token_Unparser_15 : aliased constant Text_Type := ";";
      Token_Unparser_15 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Semicolon, Text_For_Token_Unparser_15'Access);
      
         Text_For_Token_Unparser_16 : aliased constant Text_Type := "<";
      Token_Unparser_16 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_L_T, Text_For_Token_Unparser_16'Access);
      
         Text_For_Token_Unparser_17 : aliased constant Text_Type := "<-";
      Token_Unparser_17 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Left_Arrow, Text_For_Token_Unparser_17'Access);
      
         Text_For_Token_Unparser_18 : aliased constant Text_Type := "<->";
      Token_Unparser_18 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Two_Sided_Arrow, Text_For_Token_Unparser_18'Access);
      
         Text_For_Token_Unparser_19 : aliased constant Text_Type := "<=";
      Token_Unparser_19 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_L_T_E, Text_For_Token_Unparser_19'Access);
      
         Text_For_Token_Unparser_20 : aliased constant Text_Type := "=";
      Token_Unparser_20 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Equal, Text_For_Token_Unparser_20'Access);
      
         Text_For_Token_Unparser_21 : aliased constant Text_Type := "==";
      Token_Unparser_21 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_E_Q, Text_For_Token_Unparser_21'Access);
      
         Text_For_Token_Unparser_22 : aliased constant Text_Type := "=>";
      Token_Unparser_22 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Fat_Right_Arrow, Text_For_Token_Unparser_22'Access);
      
         Text_For_Token_Unparser_23 : aliased constant Text_Type := ">";
      Token_Unparser_23 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_G_T, Text_For_Token_Unparser_23'Access);
      
         Text_For_Token_Unparser_24 : aliased constant Text_Type := ">=";
      Token_Unparser_24 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_G_T_E, Text_For_Token_Unparser_24'Access);
      
         Text_For_Token_Unparser_25 : aliased constant Text_Type := "?";
      Token_Unparser_25 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Int_Mark, Text_For_Token_Unparser_25'Access);
      
         Text_For_Token_Unparser_26 : aliased constant Text_Type := "@";
      Token_Unparser_26 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_At, Text_For_Token_Unparser_26'Access);
      
         Text_For_Token_Unparser_27 : aliased constant Text_Type := "[";
      Token_Unparser_27 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_L_Brack, Text_For_Token_Unparser_27'Access);
      
         Text_For_Token_Unparser_28 : aliased constant Text_Type := "]";
      Token_Unparser_28 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_R_Brack, Text_For_Token_Unparser_28'Access);
      
         Text_For_Token_Unparser_29 : aliased constant Text_Type := "and";
      Token_Unparser_29 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_And_Kw, Text_For_Token_Unparser_29'Access);
      
         Text_For_Token_Unparser_30 : aliased constant Text_Type := "as";
      Token_Unparser_30 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Identifier, Text_For_Token_Unparser_30'Access);
      
         Text_For_Token_Unparser_31 : aliased constant Text_Type := "bind";
      Token_Unparser_31 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Bind_Kw, Text_For_Token_Unparser_31'Access);
      
         Text_For_Token_Unparser_32 : aliased constant Text_Type := "case";
      Token_Unparser_32 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Case_Kw, Text_For_Token_Unparser_32'Access);
      
         Text_For_Token_Unparser_33 : aliased constant Text_Type := "class";
      Token_Unparser_33 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Class_Kw, Text_For_Token_Unparser_33'Access);
      
         Text_For_Token_Unparser_34 : aliased constant Text_Type := "discard";
      Token_Unparser_34 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Discard_Kw, Text_For_Token_Unparser_34'Access);
      
         Text_For_Token_Unparser_35 : aliased constant Text_Type := "dont_skip";
      Token_Unparser_35 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Identifier, Text_For_Token_Unparser_35'Access);
      
         Text_For_Token_Unparser_36 : aliased constant Text_Type := "dynvar";
      Token_Unparser_36 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Dyn_Var_Kw, Text_For_Token_Unparser_36'Access);
      
         Text_For_Token_Unparser_37 : aliased constant Text_Type := "elif";
      Token_Unparser_37 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Elif_Kw, Text_For_Token_Unparser_37'Access);
      
         Text_For_Token_Unparser_38 : aliased constant Text_Type := "else";
      Token_Unparser_38 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Else_Kw, Text_For_Token_Unparser_38'Access);
      
         Text_For_Token_Unparser_39 : aliased constant Text_Type := "enum";
      Token_Unparser_39 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Enum_Kw, Text_For_Token_Unparser_39'Access);
      
         Text_For_Token_Unparser_40 : aliased constant Text_Type := "false";
      Token_Unparser_40 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Identifier, Text_For_Token_Unparser_40'Access);
      
         Text_For_Token_Unparser_41 : aliased constant Text_Type := "family";
      Token_Unparser_41 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Identifier, Text_For_Token_Unparser_41'Access);
      
         Text_For_Token_Unparser_42 : aliased constant Text_Type := "fun";
      Token_Unparser_42 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Fun_Kw, Text_For_Token_Unparser_42'Access);
      
         Text_For_Token_Unparser_43 : aliased constant Text_Type := "generic";
      Token_Unparser_43 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Generic_Kw, Text_For_Token_Unparser_43'Access);
      
         Text_For_Token_Unparser_44 : aliased constant Text_Type := "grammar";
      Token_Unparser_44 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Grammar_Kw, Text_For_Token_Unparser_44'Access);
      
         Text_For_Token_Unparser_45 : aliased constant Text_Type := "if";
      Token_Unparser_45 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_If_Kw, Text_For_Token_Unparser_45'Access);
      
         Text_For_Token_Unparser_46 : aliased constant Text_Type := "implements";
      Token_Unparser_46 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Implements_Kw, Text_For_Token_Unparser_46'Access);
      
         Text_For_Token_Unparser_47 : aliased constant Text_Type := "import";
      Token_Unparser_47 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Import_Kw, Text_For_Token_Unparser_47'Access);
      
         Text_For_Token_Unparser_48 : aliased constant Text_Type := "in";
      Token_Unparser_48 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_In_Kw, Text_For_Token_Unparser_48'Access);
      
         Text_For_Token_Unparser_49 : aliased constant Text_Type := "is";
      Token_Unparser_49 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Is_Kw, Text_For_Token_Unparser_49'Access);
      
         Text_For_Token_Unparser_50 : aliased constant Text_Type := "keep";
      Token_Unparser_50 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Identifier, Text_For_Token_Unparser_50'Access);
      
         Text_For_Token_Unparser_51 : aliased constant Text_Type := "lexer";
      Token_Unparser_51 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Lexer_Kw, Text_For_Token_Unparser_51'Access);
      
         Text_For_Token_Unparser_52 : aliased constant Text_Type := "match";
      Token_Unparser_52 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Match_Kw, Text_For_Token_Unparser_52'Access);
      
         Text_For_Token_Unparser_53 : aliased constant Text_Type := "no_case";
      Token_Unparser_53 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Identifier, Text_For_Token_Unparser_53'Access);
      
         Text_For_Token_Unparser_54 : aliased constant Text_Type := "not";
      Token_Unparser_54 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Not_Kw, Text_For_Token_Unparser_54'Access);
      
         Text_For_Token_Unparser_55 : aliased constant Text_Type := "null";
      Token_Unparser_55 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Null_Kw, Text_For_Token_Unparser_55'Access);
      
         Text_For_Token_Unparser_56 : aliased constant Text_Type := "or";
      Token_Unparser_56 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Or_Kw, Text_For_Token_Unparser_56'Access);
      
         Text_For_Token_Unparser_57 : aliased constant Text_Type := "pick";
      Token_Unparser_57 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Identifier, Text_For_Token_Unparser_57'Access);
      
         Text_For_Token_Unparser_58 : aliased constant Text_Type := "previous_token";
      Token_Unparser_58 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Identifier, Text_For_Token_Unparser_58'Access);
      
         Text_For_Token_Unparser_59 : aliased constant Text_Type := "raise";
      Token_Unparser_59 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Raise_Kw, Text_For_Token_Unparser_59'Access);
      
         Text_For_Token_Unparser_60 : aliased constant Text_Type := "send";
      Token_Unparser_60 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Identifier, Text_For_Token_Unparser_60'Access);
      
         Text_For_Token_Unparser_61 : aliased constant Text_Type := "skip";
      Token_Unparser_61 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Identifier, Text_For_Token_Unparser_61'Access);
      
         Text_For_Token_Unparser_62 : aliased constant Text_Type := "stop_cut";
      Token_Unparser_62 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Identifier, Text_For_Token_Unparser_62'Access);
      
         Text_For_Token_Unparser_63 : aliased constant Text_Type := "struct";
      Token_Unparser_63 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Struct_Kw, Text_For_Token_Unparser_63'Access);
      
         Text_For_Token_Unparser_64 : aliased constant Text_Type := "then";
      Token_Unparser_64 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Then_Kw, Text_For_Token_Unparser_64'Access);
      
         Text_For_Token_Unparser_65 : aliased constant Text_Type := "trait";
      Token_Unparser_65 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Trait_Kw, Text_For_Token_Unparser_65'Access);
      
         Text_For_Token_Unparser_66 : aliased constant Text_Type := "true";
      Token_Unparser_66 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Identifier, Text_For_Token_Unparser_66'Access);
      
         Text_For_Token_Unparser_67 : aliased constant Text_Type := "try";
      Token_Unparser_67 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Try_Kw, Text_For_Token_Unparser_67'Access);
      
         Text_For_Token_Unparser_68 : aliased constant Text_Type := "val";
      Token_Unparser_68 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Val_Kw, Text_For_Token_Unparser_68'Access);
      
         Text_For_Token_Unparser_69 : aliased constant Text_Type := "when";
      Token_Unparser_69 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_When_Kw, Text_For_Token_Unparser_69'Access);
      
         Text_For_Token_Unparser_70 : aliased constant Text_Type := "{";
      Token_Unparser_70 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_L_Brace, Text_For_Token_Unparser_70'Access);
      
         Text_For_Token_Unparser_71 : aliased constant Text_Type := "|";
      Token_Unparser_71 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Pipe, Text_For_Token_Unparser_71'Access);
      
         Text_For_Token_Unparser_72 : aliased constant Text_Type := "|>";
      Token_Unparser_72 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_Comb, Text_For_Token_Unparser_72'Access);
      
         Text_For_Token_Unparser_73 : aliased constant Text_Type := "}";
      Token_Unparser_73 : aliased constant Token_Unparser_Impl :=
        (Token_Index_For_Lkt_R_Brace, Text_For_Token_Unparser_73'Access);

         Token_Sequence_1 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_0'Access);
         Token_Sequence_2 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_0'Access, 2 => Token_Unparser_4'Access);
         Token_Sequence_3 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_1'Access);
         Token_Sequence_4 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_2'Access);
         Token_Sequence_5 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_2'Access, 2 => Token_Unparser_4'Access);
         Token_Sequence_6 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_2'Access, 2 => Token_Unparser_29'Access);
         Token_Sequence_7 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_2'Access, 2 => Token_Unparser_56'Access);
         Token_Sequence_8 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_3'Access);
         Token_Sequence_9 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_4'Access);
         Token_Sequence_10 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_5'Access);
         Token_Sequence_11 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_5'Access, 2 => Token_Unparser_10'Access);
         Token_Sequence_12 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_5'Access, 2 => Token_Unparser_14'Access);
         Token_Sequence_13 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_6'Access);
         Token_Sequence_14 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_7'Access);
         Token_Sequence_15 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_8'Access);
         Token_Sequence_16 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_9'Access);
         Token_Sequence_17 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_11'Access);
         Token_Sequence_18 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_11'Access, 2 => Token_Unparser_30'Access);
         Token_Sequence_19 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_11'Access, 2 => Token_Unparser_35'Access, 3 => Token_Unparser_4'Access);
         Token_Sequence_20 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_11'Access, 2 => Token_Unparser_50'Access, 3 => Token_Unparser_27'Access);
         Token_Sequence_21 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_12'Access);
         Token_Sequence_22 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_13'Access);
         Token_Sequence_23 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_14'Access);
         Token_Sequence_24 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_15'Access);
         Token_Sequence_25 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_16'Access);
         Token_Sequence_26 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_17'Access);
         Token_Sequence_27 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_18'Access);
         Token_Sequence_28 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_19'Access);
         Token_Sequence_29 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_20'Access);
         Token_Sequence_30 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_21'Access);
         Token_Sequence_31 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_22'Access);
         Token_Sequence_32 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_23'Access);
         Token_Sequence_33 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_24'Access);
         Token_Sequence_34 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_25'Access);
         Token_Sequence_35 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_25'Access, 2 => Token_Unparser_4'Access);
         Token_Sequence_36 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_26'Access);
         Token_Sequence_37 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_27'Access);
         Token_Sequence_38 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_28'Access);
         Token_Sequence_39 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_29'Access);
         Token_Sequence_40 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_31'Access);
         Token_Sequence_41 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_32'Access);
         Token_Sequence_42 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_33'Access);
         Token_Sequence_43 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_34'Access, 2 => Token_Unparser_4'Access);
         Token_Sequence_44 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_36'Access);
         Token_Sequence_45 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_37'Access);
         Token_Sequence_46 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_38'Access);
         Token_Sequence_47 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_39'Access);
         Token_Sequence_48 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_39'Access, 2 => Token_Unparser_33'Access);
         Token_Sequence_49 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_40'Access);
         Token_Sequence_50 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_41'Access);
         Token_Sequence_51 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_42'Access);
         Token_Sequence_52 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_43'Access, 2 => Token_Unparser_27'Access);
         Token_Sequence_53 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_44'Access);
         Token_Sequence_54 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_45'Access);
         Token_Sequence_55 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_45'Access, 2 => Token_Unparser_58'Access, 3 => Token_Unparser_49'Access);
         Token_Sequence_56 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_46'Access);
         Token_Sequence_57 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_47'Access);
         Token_Sequence_58 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_48'Access);
         Token_Sequence_59 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_49'Access);
         Token_Sequence_60 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_51'Access);
         Token_Sequence_61 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_52'Access);
         Token_Sequence_62 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_53'Access, 2 => Token_Unparser_4'Access);
         Token_Sequence_63 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_54'Access);
         Token_Sequence_64 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_55'Access);
         Token_Sequence_65 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_55'Access, 2 => Token_Unparser_4'Access);
         Token_Sequence_66 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_56'Access);
         Token_Sequence_67 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_56'Access, 2 => Token_Unparser_4'Access);
         Token_Sequence_68 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_56'Access, 2 => Token_Unparser_25'Access);
         Token_Sequence_69 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_57'Access, 2 => Token_Unparser_4'Access);
         Token_Sequence_70 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_59'Access);
         Token_Sequence_71 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_60'Access, 2 => Token_Unparser_4'Access);
         Token_Sequence_72 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_61'Access, 2 => Token_Unparser_4'Access);
         Token_Sequence_73 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_62'Access, 2 => Token_Unparser_4'Access);
         Token_Sequence_74 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_63'Access);
         Token_Sequence_75 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_64'Access);
         Token_Sequence_76 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_65'Access);
         Token_Sequence_77 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_66'Access);
         Token_Sequence_78 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_67'Access);
         Token_Sequence_79 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_68'Access);
         Token_Sequence_80 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_69'Access);
         Token_Sequence_81 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_70'Access);
         Token_Sequence_82 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_70'Access, 2 => Token_Unparser_32'Access);
         Token_Sequence_83 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_71'Access);
         Token_Sequence_84 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_72'Access, 2 => Token_Unparser_69'Access, 3 => Token_Unparser_4'Access);
         Token_Sequence_85 : aliased constant Token_Sequence_Impl :=
           (1 => Token_Unparser_73'Access);



         

         Bare_Argument_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Argument_F_Name, Empty_Token_Sequence, Token_Sequence_29'Access, False),
                   2 => (Member_Index_For_Argument_F_Value, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence));

         

         Bare_Lexer_Case_Rule_Cond_Alt_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Base_Lexer_Case_Rule_Alt_F_Send, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_75'Access));

         

         Bare_Lexer_Case_Rule_Default_Alt_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Base_Lexer_Case_Rule_Alt_F_Send, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Binding_Pattern_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Binding_Pattern_F_Binding, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Binding_Pattern_F_Value_Pattern, Token_Sequence_36'Access, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence));

         

         Bare_Filtered_Pattern_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Filtered_Pattern_F_Pattern, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Filtered_Pattern_F_Predicate, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_80'Access));

         

         Bare_List_Pattern_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_List_Pattern_F_Patterns, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Extended_Node_Pattern_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Extended_Node_Pattern_F_Node_Pattern, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Extended_Node_Pattern_F_Details, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_9'Access));

         

         Bare_Type_Pattern_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Type_Pattern_F_Type_Name, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Not_Pattern_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Not_Pattern_F_Pattern, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Or_Pattern_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Or_Pattern_F_Left, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Or_Pattern_F_Right, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_83'Access));

         

         Bare_Paren_Pattern_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Paren_Pattern_F_Pattern, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Splat_Pattern_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Splat_Pattern_F_Binding, Empty_Token_Sequence, Token_Sequence_36'Access, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Tuple_Pattern_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Tuple_Pattern_F_Patterns, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Grammar_Rule_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Base_Grammar_Rule_Decl_F_Expr, Token_Sequence_26'Access, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence));

         

         Bare_Enum_Lit_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Field_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 4,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Explicitly_Typed_Decl_F_Decl_Type, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Field_Decl_F_Trait_Ref, Token_Sequence_56'Access, Empty_Token_Sequence, False),
                   4 => (Member_Index_For_Component_Decl_F_Default_Val, Token_Sequence_29'Access, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_23'Access,
                   3 => Empty_Token_Sequence,
                   4 => Empty_Token_Sequence));

         

         Bare_Fun_Param_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 4,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Fun_Param_Decl_F_Decl_Annotations, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Explicitly_Typed_Decl_F_Decl_Type, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   4 => (Member_Index_For_Component_Decl_F_Default_Val, Token_Sequence_29'Access, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Token_Sequence_23'Access,
                   4 => Empty_Token_Sequence));

         

         Bare_Lambda_Param_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 3,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Explicitly_Typed_Decl_F_Decl_Type, Token_Sequence_23'Access, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Component_Decl_F_Default_Val, Token_Sequence_29'Access, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Empty_Token_Sequence));

         

         Bare_Dyn_Var_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Explicitly_Typed_Decl_F_Decl_Type, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_23'Access));

         

         Bare_Match_Val_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Explicitly_Typed_Decl_F_Decl_Type, Token_Sequence_23'Access, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence));

         

         Bare_Val_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 3,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Explicitly_Typed_Decl_F_Decl_Type, Token_Sequence_23'Access, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Val_Decl_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Token_Sequence_29'Access));

         

         Bare_Fun_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 5,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Fun_Decl_F_Params, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Fun_Decl_F_Return_Type, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   4 => (Member_Index_For_Fun_Decl_F_Trait_Ref, Token_Sequence_56'Access, Empty_Token_Sequence, False),
                   5 => (Member_Index_For_Fun_Decl_F_Body, Token_Sequence_29'Access, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_9'Access,
                   3 => Token_Sequence_12'Access,
                   4 => Empty_Token_Sequence,
                   5 => Empty_Token_Sequence));

         

         Bare_Env_Spec_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Env_Spec_Decl_F_Actions, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_81'Access));

         

         Bare_Generic_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Generic_Decl_F_Generic_Param_Decls, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Generic_Decl_F_Decl, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_38'Access));

         

         Bare_Grammar_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Grammar_Decl_F_Rules, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_81'Access));

         

         Bare_Lexer_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Lexer_Decl_F_Rules, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_81'Access));

         

         Bare_Lexer_Family_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Lexer_Family_Decl_F_Rules, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_81'Access));

         

         Bare_Enum_Class_Alt_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Type_Decl_F_Traits, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence));

         

         Bare_Generic_Param_Type_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 3,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Generic_Param_Type_Decl_F_Has_Class, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Type_Decl_F_Traits, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Empty_Token_Sequence));

         

         Bare_Class_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 4,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Type_Decl_F_Syn_Base_Type, Token_Sequence_23'Access, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Type_Decl_F_Traits, Token_Sequence_56'Access, Empty_Token_Sequence, True),
                   4 => (Member_Index_For_Named_Type_Decl_F_Decls, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Empty_Token_Sequence,
                   4 => Token_Sequence_81'Access));

         

         Bare_Enum_Class_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 5,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Type_Decl_F_Syn_Base_Type, Token_Sequence_23'Access, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Type_Decl_F_Traits, Token_Sequence_56'Access, Empty_Token_Sequence, True),
                   4 => (Member_Index_For_Enum_Class_Decl_F_Branches, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   5 => (Member_Index_For_Named_Type_Decl_F_Decls, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Empty_Token_Sequence,
                   4 => Token_Sequence_81'Access,
                   5 => Empty_Token_Sequence));

         

         Bare_Enum_Type_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 4,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Type_Decl_F_Traits, Token_Sequence_56'Access, Empty_Token_Sequence, True),
                   3 => (Member_Index_For_Enum_Type_Decl_F_Literals, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   4 => (Member_Index_For_Named_Type_Decl_F_Decls, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Token_Sequence_82'Access,
                   4 => Empty_Token_Sequence));

         

         Bare_Struct_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 3,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Type_Decl_F_Traits, Token_Sequence_56'Access, Empty_Token_Sequence, True),
                   3 => (Member_Index_For_Named_Type_Decl_F_Decls, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Token_Sequence_81'Access));

         

         Bare_Trait_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 3,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_F_Syn_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Type_Decl_F_Traits, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Named_Type_Decl_F_Decls, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Token_Sequence_81'Access));

         

         Bare_Decl_Annotation_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_Annotation_F_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Decl_Annotation_F_Args, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence));

         

         Bare_Decl_Annotation_Args_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Decl_Annotation_Args_F_Args, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Elsif_Branch_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Elsif_Branch_F_Cond_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Elsif_Branch_F_Then_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_75'Access));

         

         Bare_Enum_Class_Case_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Enum_Class_Case_F_Decls, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Any_Of_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Any_Of_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Any_Of_F_Values, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_58'Access));

         

         Bare_Array_Literal_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Array_Literal_F_Exprs, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Array_Literal_F_Element_Type, Token_Sequence_23'Access, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_38'Access));

         

         Bare_Call_Expr_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Base_Call_Expr_F_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Base_Call_Expr_F_Args, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_9'Access));

         

         Bare_Logic_Predicate_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Base_Call_Expr_F_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Base_Call_Expr_F_Args, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_5'Access));

         

         Bare_Logic_Propagate_Call_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Base_Call_Expr_F_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Base_Call_Expr_F_Args, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_5'Access));

         

         Bare_Bin_Op_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 3,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Bin_Op_F_Left, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Bin_Op_F_Op, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Bin_Op_F_Right, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Empty_Token_Sequence));

         

         Bare_Block_Expr_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Block_Expr_F_Val_Defs, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Block_Expr_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_24'Access));

         

         Bare_Cast_Expr_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 4,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Cast_Expr_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Cast_Expr_F_Null_Cond, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Cast_Expr_F_Excludes_Null, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   4 => (Member_Index_For_Cast_Expr_F_Dest_Type, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Token_Sequence_18'Access,
                   4 => Token_Sequence_37'Access));

         

         Bare_Dot_Expr_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 3,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Dot_Expr_F_Prefix, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Dot_Expr_F_Null_Cond, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Dot_Expr_F_Suffix, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Token_Sequence_17'Access));

         

         Bare_Error_On_Null_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Error_On_Null_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Generic_Instantiation_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Generic_Instantiation_F_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Generic_Instantiation_F_Args, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_37'Access));

         

         Bare_Grammar_Discard_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Grammar_Discard_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Grammar_Dont_Skip_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Grammar_Dont_Skip_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Grammar_Dont_Skip_F_Dont_Skip, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_19'Access));

         

         Bare_Grammar_List_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 4,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Grammar_List_F_List_Type, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Grammar_List_F_Kind, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Grammar_List_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   4 => (Member_Index_For_Grammar_List_F_Sep, Token_Sequence_15'Access, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Token_Sequence_9'Access,
                   4 => Empty_Token_Sequence));

         

         Bare_Grammar_Null_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Grammar_Null_F_Name, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Grammar_Opt_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Grammar_Opt_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Grammar_Opt_Error_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Grammar_Opt_Error_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Grammar_Opt_Error_Group_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Grammar_Opt_Error_Group_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Grammar_Opt_Group_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Grammar_Opt_Group_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Grammar_Or_Expr_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Grammar_Or_Expr_F_Sub_Exprs, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Grammar_Pick_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Grammar_Pick_F_Exprs, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Grammar_Implicit_Pick_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Grammar_Pick_F_Exprs, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Grammar_Predicate_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Grammar_Predicate_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Grammar_Predicate_F_Prop_Ref, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_84'Access));

         

         Bare_Grammar_Rule_Ref_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Grammar_Rule_Ref_F_Node_Name, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Grammar_Skip_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Grammar_Skip_F_Name, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Grammar_Stop_Cut_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Grammar_Stop_Cut_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Parse_Node_Expr_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Parse_Node_Expr_F_Node_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Parse_Node_Expr_F_Sub_Exprs, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_9'Access));

         

         Bare_Token_No_Case_Lit_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Token_No_Case_Lit_F_Lit, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Token_Pattern_Concat_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Token_Pattern_Concat_F_Left, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Token_Pattern_Concat_F_Right, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_8'Access));

         

         Bare_Token_Ref_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Token_Ref_F_Token_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Token_Ref_F_Expr, Token_Sequence_9'Access, Token_Sequence_10'Access, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence));

         

         Bare_If_Expr_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 4,
                Field_Unparsers =>
                  (1 => (Member_Index_For_If_Expr_F_Cond_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_If_Expr_F_Then_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_If_Expr_F_Alternatives, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   4 => (Member_Index_For_If_Expr_F_Else_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_75'Access,
                   3 => Empty_Token_Sequence,
                   4 => Token_Sequence_46'Access));

         

         Bare_Isa_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Isa_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Isa_F_Pattern, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_59'Access));

         

         Bare_Keep_Expr_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 3,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Keep_Expr_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Keep_Expr_F_Null_Cond, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Keep_Expr_F_Keep_Type, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Token_Sequence_20'Access));

         

         Bare_Lambda_Expr_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 3,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Lambda_Expr_F_Params, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Lambda_Expr_F_Return_Type, Token_Sequence_23'Access, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Lambda_Expr_F_Body, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_10'Access,
                   3 => Token_Sequence_31'Access));

         

         Bare_Null_Lit_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Null_Lit_F_Dest_Type, Token_Sequence_37'Access, Token_Sequence_38'Access, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Block_String_Lit_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Block_String_Lit_F_Lines, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Logic_Assign_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Logic_Assign_F_Dest_Var, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Logic_Assign_F_Value, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_26'Access));

         

         Bare_Logic_Expr_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Logic_Expr_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Logic_Propagate_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Logic_Propagate_F_Dest_Var, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Logic_Propagate_F_Call, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_26'Access));

         

         Bare_Logic_Unify_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Logic_Unify_F_Lhs, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Logic_Unify_F_Rhs, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_27'Access));

         

         Bare_Match_Expr_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Match_Expr_F_Match_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Match_Expr_F_Branches, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_81'Access));

         

         Bare_Not_Expr_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Not_Expr_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Paren_Expr_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Paren_Expr_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Raise_Expr_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Raise_Expr_F_Dest_Type, Token_Sequence_37'Access, Token_Sequence_38'Access, False),
                   2 => (Member_Index_For_Raise_Expr_F_Except_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence));

         

         Bare_Subscript_Expr_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 3,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Subscript_Expr_F_Prefix, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Subscript_Expr_F_Null_Cond, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Subscript_Expr_F_Index, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Token_Sequence_37'Access));

         

         Bare_Try_Expr_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Try_Expr_F_Try_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Try_Expr_F_Or_Expr, Token_Sequence_46'Access, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence));

         

         Bare_Un_Op_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Un_Op_F_Op, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Un_Op_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence));

         

         Bare_Full_Decl_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 3,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Full_Decl_F_Doc, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Full_Decl_F_Decl_Annotations, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   3 => (Member_Index_For_Full_Decl_F_Decl, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Empty_Token_Sequence));

         

         Bare_Grammar_List_Sep_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Grammar_List_Sep_F_Token, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Grammar_List_Sep_F_Extra, Token_Sequence_15'Access, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence));

         

         Bare_Import_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Import_F_Name, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Langkit_Root_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Langkit_Root_F_Imports, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Langkit_Root_F_Decls, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence));

         

         Bare_Lexer_Case_Rule_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Lexer_Case_Rule_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Lexer_Case_Rule_F_Alts, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_81'Access));

         

         Bare_Lexer_Case_Rule_Send_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Lexer_Case_Rule_Send_F_Sent, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Lexer_Case_Rule_Send_F_Match_Size, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_15'Access));

         

         Bare_Match_Branch_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Match_Branch_F_Decl, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Match_Branch_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_31'Access));

         

         Bare_Node_Pattern_Field_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Node_Pattern_Field_F_Id, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Node_Pattern_Field_F_Expected_Value, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_23'Access));

         

         Bare_Node_Pattern_Property_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Node_Pattern_Property_F_Call, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Node_Pattern_Property_F_Expected_Value, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_23'Access));

         

         Bare_Node_Pattern_Selector_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Node_Pattern_Selector_F_Call, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Node_Pattern_Selector_F_Pattern, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_23'Access));

         

         Bare_Selector_Call_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 3,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Selector_Call_F_Quantifier, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Selector_Call_F_Binding, Empty_Token_Sequence, Token_Sequence_36'Access, False),
                   3 => (Member_Index_For_Selector_Call_F_Selector_Call, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Empty_Token_Sequence,
                   3 => Empty_Token_Sequence));

         

         Bare_Function_Type_Ref_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Function_Type_Ref_F_Param_Types, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Function_Type_Ref_F_Return_Type, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_11'Access));

         

         Bare_Generic_Type_Ref_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Generic_Type_Ref_F_Type_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Generic_Type_Ref_F_Args, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_37'Access));

         

         Bare_Simple_Type_Ref_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 1,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Simple_Type_Ref_F_Type_Name, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence));

         

         Bare_Var_Bind_Fields_Unparser_List
            : aliased constant Field_Unparser_List_Impl
            := (N               => 2,
                Field_Unparsers =>
                  (1 => (Member_Index_For_Var_Bind_F_Name, Empty_Token_Sequence, Empty_Token_Sequence, False),
                   2 => (Member_Index_For_Var_Bind_F_Expr, Empty_Token_Sequence, Empty_Token_Sequence, False)),
                Inter_Tokens    =>
                  (1 => Empty_Token_Sequence,
                   2 => Token_Sequence_29'Access));


      

      

         Unparser_For_Argument : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Argument_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

         Unparser_For_Lexer_Case_Rule_Cond_Alt : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_55'Access,
            Field_Unparsers => Bare_Lexer_Case_Rule_Cond_Alt_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Lexer_Case_Rule_Default_Alt : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_46'Access,
            Field_Unparsers => Bare_Lexer_Case_Rule_Default_Alt_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

         Unparser_For_Binding_Pattern : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Binding_Pattern_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Filtered_Pattern : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Filtered_Pattern_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

      

         Unparser_For_Bool_Pattern_False : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_49'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Bool_Pattern_True : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_77'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Integer_Pattern : aliased constant Node_Unparser_Impl :=
           (Kind => Token);
      

         Unparser_For_List_Pattern : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_37'Access,
            Field_Unparsers => Bare_List_Pattern_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_38'Access);
      

      

         Unparser_For_Extended_Node_Pattern : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Extended_Node_Pattern_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Type_Pattern : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Type_Pattern_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Not_Pattern : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_63'Access,
            Field_Unparsers => Bare_Not_Pattern_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Null_Pattern : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_64'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Or_Pattern : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Or_Pattern_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Paren_Pattern : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_9'Access,
            Field_Unparsers => Bare_Paren_Pattern_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Regex_Pattern : aliased constant Node_Unparser_Impl :=
           (Kind => Token);
      

         Unparser_For_Splat_Pattern : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Splat_Pattern_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_21'Access);
      

         Unparser_For_Tuple_Pattern : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_9'Access,
            Field_Unparsers => Bare_Tuple_Pattern_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Universal_Pattern : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_13'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Block_String_Line : aliased constant Node_Unparser_Impl :=
           (Kind => Token);
      

      

         Unparser_For_Class_Qualifier_Absent : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Class_Qualifier_Present : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_42'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

      

         Unparser_For_Grammar_Rule_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Grammar_Rule_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

      

      

      

      

         Unparser_For_Enum_Lit_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Enum_Lit_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

      

         Unparser_For_Field_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Field_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Fun_Param_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Fun_Param_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Lambda_Param_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Lambda_Param_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Dyn_Var_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_44'Access,
            Field_Unparsers => Bare_Dyn_Var_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Match_Val_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Match_Val_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Val_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_79'Access,
            Field_Unparsers => Bare_Val_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Fun_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_51'Access,
            Field_Unparsers => Bare_Fun_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Env_Spec_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Env_Spec_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_85'Access);
      

         Unparser_For_Generic_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_52'Access,
            Field_Unparsers => Bare_Generic_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Grammar_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_53'Access,
            Field_Unparsers => Bare_Grammar_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_85'Access);
      

         Unparser_For_Lexer_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_60'Access,
            Field_Unparsers => Bare_Lexer_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_85'Access);
      

         Unparser_For_Lexer_Family_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_50'Access,
            Field_Unparsers => Bare_Lexer_Family_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_85'Access);
      

      

      

      

      

         Unparser_For_Enum_Class_Alt_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Enum_Class_Alt_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

         Unparser_For_Generic_Param_Type_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Generic_Param_Type_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

      

         Unparser_For_Class_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_42'Access,
            Field_Unparsers => Bare_Class_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_85'Access);
      

         Unparser_For_Enum_Class_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_48'Access,
            Field_Unparsers => Bare_Enum_Class_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_85'Access);
      

         Unparser_For_Enum_Type_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_47'Access,
            Field_Unparsers => Bare_Enum_Type_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_85'Access);
      

         Unparser_For_Struct_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_74'Access,
            Field_Unparsers => Bare_Struct_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_85'Access);
      

         Unparser_For_Trait_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_76'Access,
            Field_Unparsers => Bare_Trait_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_85'Access);
      

         Unparser_For_Decl_Annotation : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_36'Access,
            Field_Unparsers => Bare_Decl_Annotation_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Decl_Annotation_Args : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_9'Access,
            Field_Unparsers => Bare_Decl_Annotation_Args_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

      

         Unparser_For_Elsif_Branch : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_45'Access,
            Field_Unparsers => Bare_Elsif_Branch_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Enum_Class_Case : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_41'Access,
            Field_Unparsers => Bare_Enum_Class_Case_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

         Unparser_For_Excludes_Null_Absent : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Excludes_Null_Present : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_1'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

         Unparser_For_Any_Of : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Any_Of_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Array_Literal : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_37'Access,
            Field_Unparsers => Bare_Array_Literal_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

         Unparser_For_Call_Expr : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Call_Expr_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

      

         Unparser_For_Logic_Predicate : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Logic_Predicate_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Logic_Propagate_Call : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Logic_Propagate_Call_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Bin_Op : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Bin_Op_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Block_Expr : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_81'Access,
            Field_Unparsers => Bare_Block_Expr_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_85'Access);
      

         Unparser_For_Cast_Expr : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Cast_Expr_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_38'Access);
      

         Unparser_For_Dot_Expr : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Dot_Expr_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Error_On_Null : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Error_On_Null_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_1'Access);
      

         Unparser_For_Generic_Instantiation : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Generic_Instantiation_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_38'Access);
      

      

         Unparser_For_Grammar_Cut : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_22'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Grammar_Discard : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_43'Access,
            Field_Unparsers => Bare_Grammar_Discard_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Grammar_Dont_Skip : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Grammar_Dont_Skip_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Grammar_List : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Grammar_List_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Grammar_Null : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_65'Access,
            Field_Unparsers => Bare_Grammar_Null_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Grammar_Opt : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_34'Access,
            Field_Unparsers => Bare_Grammar_Opt_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Grammar_Opt_Error : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_1'Access,
            Field_Unparsers => Bare_Grammar_Opt_Error_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Grammar_Opt_Error_Group : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_2'Access,
            Field_Unparsers => Bare_Grammar_Opt_Error_Group_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Grammar_Opt_Group : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_35'Access,
            Field_Unparsers => Bare_Grammar_Opt_Group_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Grammar_Or_Expr : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_67'Access,
            Field_Unparsers => Bare_Grammar_Or_Expr_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Grammar_Pick : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_69'Access,
            Field_Unparsers => Bare_Grammar_Pick_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Grammar_Implicit_Pick : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Grammar_Implicit_Pick_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Grammar_Predicate : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Grammar_Predicate_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Grammar_Rule_Ref : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Grammar_Rule_Ref_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Grammar_Skip : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_72'Access,
            Field_Unparsers => Bare_Grammar_Skip_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Grammar_Stop_Cut : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_73'Access,
            Field_Unparsers => Bare_Grammar_Stop_Cut_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Parse_Node_Expr : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Parse_Node_Expr_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Token_Lit : aliased constant Node_Unparser_Impl :=
           (Kind => Token);
      

         Unparser_For_Token_No_Case_Lit : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_62'Access,
            Field_Unparsers => Bare_Token_No_Case_Lit_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Token_Pattern_Concat : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Token_Pattern_Concat_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Token_Pattern_Lit : aliased constant Node_Unparser_Impl :=
           (Kind => Token);
      

         Unparser_For_Token_Ref : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_36'Access,
            Field_Unparsers => Bare_Token_Ref_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Id : aliased constant Node_Unparser_Impl :=
           (Kind => Token);
      

         Unparser_For_Def_Id : aliased constant Node_Unparser_Impl :=
           (Kind => Token);
      

         Unparser_For_Module_Ref_Id : aliased constant Node_Unparser_Impl :=
           (Kind => Token);
      

         Unparser_For_Ref_Id : aliased constant Node_Unparser_Impl :=
           (Kind => Token);
      

         Unparser_For_If_Expr : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_54'Access,
            Field_Unparsers => Bare_If_Expr_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Isa : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Isa_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Keep_Expr : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Keep_Expr_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_38'Access);
      

         Unparser_For_Lambda_Expr : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_9'Access,
            Field_Unparsers => Bare_Lambda_Expr_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

         Unparser_For_Big_Num_Lit : aliased constant Node_Unparser_Impl :=
           (Kind => Token);
      

         Unparser_For_Char_Lit : aliased constant Node_Unparser_Impl :=
           (Kind => Token);
      

         Unparser_For_Null_Lit : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_64'Access,
            Field_Unparsers => Bare_Null_Lit_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Num_Lit : aliased constant Node_Unparser_Impl :=
           (Kind => Token);
      

      

         Unparser_For_Block_String_Lit : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Block_String_Lit_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Single_Line_String_Lit : aliased constant Node_Unparser_Impl :=
           (Kind => Token);
      

         Unparser_For_Pattern_Single_Line_String_Lit : aliased constant Node_Unparser_Impl :=
           (Kind => Token);
      

         Unparser_For_Logic_Assign : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Logic_Assign_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Logic_Expr : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_4'Access,
            Field_Unparsers => Bare_Logic_Expr_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Logic_Propagate : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Logic_Propagate_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Logic_Unify : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Logic_Unify_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Match_Expr : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_61'Access,
            Field_Unparsers => Bare_Match_Expr_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_85'Access);
      

         Unparser_For_Not_Expr : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_63'Access,
            Field_Unparsers => Bare_Not_Expr_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Paren_Expr : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_9'Access,
            Field_Unparsers => Bare_Paren_Expr_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

         Unparser_For_Raise_Expr : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_70'Access,
            Field_Unparsers => Bare_Raise_Expr_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Subscript_Expr : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Subscript_Expr_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_38'Access);
      

         Unparser_For_Try_Expr : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_78'Access,
            Field_Unparsers => Bare_Try_Expr_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Un_Op : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Un_Op_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Full_Decl : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Full_Decl_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Grammar_List_Sep : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Grammar_List_Sep_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Import : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_57'Access,
            Field_Unparsers => Bare_Import_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Langkit_Root : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Langkit_Root_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Lexer_Case_Rule : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_61'Access,
            Field_Unparsers => Bare_Lexer_Case_Rule_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_85'Access);
      

         Unparser_For_Lexer_Case_Rule_Send : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_71'Access,
            Field_Unparsers => Bare_Lexer_Case_Rule_Send_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_10'Access);
      

      

         Unparser_For_List_Kind_One : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_14'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_List_Kind_Zero : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_13'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

         Unparser_For_Argument_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => Token_Unparser_8'Access,
            Sep_Extra => Allow_None);
      

         Unparser_For_Base_Lexer_Case_Rule_Alt_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => null,
            Sep_Extra => Allow_None);
      

         Unparser_For_Base_Pattern_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => Token_Unparser_8'Access,
            Sep_Extra => Allow_None);
      

         Unparser_For_Block_String_Line_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => null,
            Sep_Extra => Allow_None);
      

         Unparser_For_Call_Expr_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => null,
            Sep_Extra => Allow_None);
      

         Unparser_For_Decl_Annotation_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => null,
            Sep_Extra => Allow_None);
      

         Unparser_For_Elsif_Branch_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => null,
            Sep_Extra => Allow_None);
      

         Unparser_For_Enum_Class_Alt_Decl_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => Token_Unparser_8'Access,
            Sep_Extra => Allow_None);
      

         Unparser_For_Enum_Class_Case_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => null,
            Sep_Extra => Allow_None);
      

         Unparser_For_Enum_Lit_Decl_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => Token_Unparser_8'Access,
            Sep_Extra => Allow_None);
      

         Unparser_For_Expr_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => Token_Unparser_8'Access,
            Sep_Extra => Allow_None);
      

         Unparser_For_Any_Of_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => Token_Unparser_71'Access,
            Sep_Extra => Allow_None);
      

         Unparser_For_Full_Decl_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => null,
            Sep_Extra => Allow_None);
      

         Unparser_For_Decl_Block : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => null,
            Sep_Extra => Allow_None);
      

         Unparser_For_Generic_Param_Decl_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => Token_Unparser_8'Access,
            Sep_Extra => Allow_None);
      

         Unparser_For_Fun_Param_Decl_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => Token_Unparser_8'Access,
            Sep_Extra => Allow_None);
      

         Unparser_For_Grammar_Expr_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => null,
            Sep_Extra => Allow_None);
      

         Unparser_For_Grammar_Expr_List_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => Token_Unparser_71'Access,
            Sep_Extra => Allow_Leading);
      

         Unparser_For_Import_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => null,
            Sep_Extra => Allow_None);
      

         Unparser_For_Lambda_Param_Decl_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => Token_Unparser_8'Access,
            Sep_Extra => Allow_None);
      

         Unparser_For_Lkt_Node_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => null,
            Sep_Extra => Allow_None);
      

         Unparser_For_Block_Decl_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => Token_Unparser_15'Access,
            Sep_Extra => Allow_None);
      

         Unparser_For_Match_Branch_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => null,
            Sep_Extra => Allow_None);
      

         Unparser_For_Node_Pattern_Detail_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => Token_Unparser_8'Access,
            Sep_Extra => Allow_None);
      

         Unparser_For_Ref_Id_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => Token_Unparser_71'Access,
            Sep_Extra => Allow_None);
      

         Unparser_For_Type_Ref_List : aliased constant Node_Unparser_Impl :=
           (Kind => List,
            Separator => Token_Unparser_8'Access,
            Sep_Extra => Allow_None);
      

      

         Unparser_For_Match_Branch : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_41'Access,
            Field_Unparsers => Bare_Match_Branch_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

         Unparser_For_Node_Pattern_Field : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Node_Pattern_Field_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Node_Pattern_Property : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Node_Pattern_Property_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Node_Pattern_Selector : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Node_Pattern_Selector_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

         Unparser_For_Null_Cond_Qualifier_Absent : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Null_Cond_Qualifier_Present : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_34'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

         Unparser_For_Op_Amp : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_8'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Op_And : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_39'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Op_Div : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_22'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Op_Eq : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_30'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Op_Gt : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_32'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Op_Gte : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_33'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Op_Logic_And : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_6'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Op_Logic_Or : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_7'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Op_Lt : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_25'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Op_Lte : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_28'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Op_Minus : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_16'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Op_Mult : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_13'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Op_Ne : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_3'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Op_Or : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_66'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Op_Or_Int : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_68'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Op_Plus : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_14'Access,
            Field_Unparsers => Empty_Field_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Selector_Call : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Selector_Call_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

      

         Unparser_For_Default_List_Type_Ref : aliased constant Node_Unparser_Impl :=
           (Kind => Token);
      

         Unparser_For_Function_Type_Ref : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_9'Access,
            Field_Unparsers => Bare_Function_Type_Ref_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Generic_Type_Ref : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Generic_Type_Ref_Fields_Unparser_List'Access,
            Post_Tokens => Token_Sequence_38'Access);
      

         Unparser_For_Simple_Type_Ref : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Empty_Token_Sequence,
            Field_Unparsers => Bare_Simple_Type_Ref_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);
      

         Unparser_For_Var_Bind : aliased constant Node_Unparser_Impl :=
           (Kind => Regular,
            Pre_Tokens => Token_Sequence_40'Access,
            Field_Unparsers => Bare_Var_Bind_Fields_Unparser_List'Access,
            Post_Tokens => Empty_Token_Sequence);


   Node_Unparsers : aliased constant Node_Unparser_Map_Impl := (
         
         Type_Index_For_Lkt_Node => null
         ,
         Type_Index_For_Argument => Unparser_For_Argument'Access
         ,
         Type_Index_For_Base_Lexer_Case_Rule_Alt => null
         ,
         Type_Index_For_Lexer_Case_Rule_Cond_Alt => Unparser_For_Lexer_Case_Rule_Cond_Alt'Access
         ,
         Type_Index_For_Lexer_Case_Rule_Default_Alt => Unparser_For_Lexer_Case_Rule_Default_Alt'Access
         ,
         Type_Index_For_Base_Pattern => null
         ,
         Type_Index_For_Binding_Pattern => Unparser_For_Binding_Pattern'Access
         ,
         Type_Index_For_Filtered_Pattern => Unparser_For_Filtered_Pattern'Access
         ,
         Type_Index_For_Value_Pattern => null
         ,
         Type_Index_For_Bool_Pattern => null
         ,
         Type_Index_For_Bool_Pattern_False => Unparser_For_Bool_Pattern_False'Access
         ,
         Type_Index_For_Bool_Pattern_True => Unparser_For_Bool_Pattern_True'Access
         ,
         Type_Index_For_Integer_Pattern => Unparser_For_Integer_Pattern'Access
         ,
         Type_Index_For_List_Pattern => Unparser_For_List_Pattern'Access
         ,
         Type_Index_For_Node_Pattern => null
         ,
         Type_Index_For_Extended_Node_Pattern => Unparser_For_Extended_Node_Pattern'Access
         ,
         Type_Index_For_Type_Pattern => Unparser_For_Type_Pattern'Access
         ,
         Type_Index_For_Not_Pattern => Unparser_For_Not_Pattern'Access
         ,
         Type_Index_For_Null_Pattern => Unparser_For_Null_Pattern'Access
         ,
         Type_Index_For_Or_Pattern => Unparser_For_Or_Pattern'Access
         ,
         Type_Index_For_Paren_Pattern => Unparser_For_Paren_Pattern'Access
         ,
         Type_Index_For_Regex_Pattern => Unparser_For_Regex_Pattern'Access
         ,
         Type_Index_For_Splat_Pattern => Unparser_For_Splat_Pattern'Access
         ,
         Type_Index_For_Tuple_Pattern => Unparser_For_Tuple_Pattern'Access
         ,
         Type_Index_For_Universal_Pattern => Unparser_For_Universal_Pattern'Access
         ,
         Type_Index_For_Block_String_Line => Unparser_For_Block_String_Line'Access
         ,
         Type_Index_For_Class_Qualifier => null
         ,
         Type_Index_For_Class_Qualifier_Absent => Unparser_For_Class_Qualifier_Absent'Access
         ,
         Type_Index_For_Class_Qualifier_Present => Unparser_For_Class_Qualifier_Present'Access
         ,
         Type_Index_For_Decl => null
         ,
         Type_Index_For_Base_Grammar_Rule_Decl => null
         ,
         Type_Index_For_Grammar_Rule_Decl => Unparser_For_Grammar_Rule_Decl'Access
         ,
         Type_Index_For_Synthetic_Lexer_Decl => null
         ,
         Type_Index_For_Base_Val_Decl => null
         ,
         Type_Index_For_Node_Decl => null
         ,
         Type_Index_For_Self_Decl => null
         ,
         Type_Index_For_User_Val_Decl => null
         ,
         Type_Index_For_Enum_Lit_Decl => Unparser_For_Enum_Lit_Decl'Access
         ,
         Type_Index_For_Explicitly_Typed_Decl => null
         ,
         Type_Index_For_Component_Decl => null
         ,
         Type_Index_For_Field_Decl => Unparser_For_Field_Decl'Access
         ,
         Type_Index_For_Fun_Param_Decl => Unparser_For_Fun_Param_Decl'Access
         ,
         Type_Index_For_Lambda_Param_Decl => Unparser_For_Lambda_Param_Decl'Access
         ,
         Type_Index_For_Dyn_Var_Decl => Unparser_For_Dyn_Var_Decl'Access
         ,
         Type_Index_For_Match_Val_Decl => Unparser_For_Match_Val_Decl'Access
         ,
         Type_Index_For_Val_Decl => Unparser_For_Val_Decl'Access
         ,
         Type_Index_For_Fun_Decl => Unparser_For_Fun_Decl'Access
         ,
         Type_Index_For_Env_Spec_Decl => Unparser_For_Env_Spec_Decl'Access
         ,
         Type_Index_For_Generic_Decl => Unparser_For_Generic_Decl'Access
         ,
         Type_Index_For_Grammar_Decl => Unparser_For_Grammar_Decl'Access
         ,
         Type_Index_For_Lexer_Decl => Unparser_For_Lexer_Decl'Access
         ,
         Type_Index_For_Lexer_Family_Decl => Unparser_For_Lexer_Family_Decl'Access
         ,
         Type_Index_For_Synth_Fun_Decl => null
         ,
         Type_Index_For_Synth_Param_Decl => null
         ,
         Type_Index_For_Type_Decl => null
         ,
         Type_Index_For_Any_Type_Decl => null
         ,
         Type_Index_For_Enum_Class_Alt_Decl => Unparser_For_Enum_Class_Alt_Decl'Access
         ,
         Type_Index_For_Function_Type => null
         ,
         Type_Index_For_Generic_Param_Type_Decl => Unparser_For_Generic_Param_Type_Decl'Access
         ,
         Type_Index_For_Named_Type_Decl => null
         ,
         Type_Index_For_Basic_Class_Decl => null
         ,
         Type_Index_For_Class_Decl => Unparser_For_Class_Decl'Access
         ,
         Type_Index_For_Enum_Class_Decl => Unparser_For_Enum_Class_Decl'Access
         ,
         Type_Index_For_Enum_Type_Decl => Unparser_For_Enum_Type_Decl'Access
         ,
         Type_Index_For_Struct_Decl => Unparser_For_Struct_Decl'Access
         ,
         Type_Index_For_Trait_Decl => Unparser_For_Trait_Decl'Access
         ,
         Type_Index_For_Decl_Annotation => Unparser_For_Decl_Annotation'Access
         ,
         Type_Index_For_Decl_Annotation_Args => Unparser_For_Decl_Annotation_Args'Access
         ,
         Type_Index_For_Dyn_Env_Wrapper => null
         ,
         Type_Index_For_Elsif_Branch => Unparser_For_Elsif_Branch'Access
         ,
         Type_Index_For_Enum_Class_Case => Unparser_For_Enum_Class_Case'Access
         ,
         Type_Index_For_Excludes_Null => null
         ,
         Type_Index_For_Excludes_Null_Absent => Unparser_For_Excludes_Null_Absent'Access
         ,
         Type_Index_For_Excludes_Null_Present => Unparser_For_Excludes_Null_Present'Access
         ,
         Type_Index_For_Expr => null
         ,
         Type_Index_For_Any_Of => Unparser_For_Any_Of'Access
         ,
         Type_Index_For_Array_Literal => Unparser_For_Array_Literal'Access
         ,
         Type_Index_For_Base_Call_Expr => null
         ,
         Type_Index_For_Call_Expr => Unparser_For_Call_Expr'Access
         ,
         Type_Index_For_Logic_Call_Expr => null
         ,
         Type_Index_For_Logic_Predicate => Unparser_For_Logic_Predicate'Access
         ,
         Type_Index_For_Logic_Propagate_Call => Unparser_For_Logic_Propagate_Call'Access
         ,
         Type_Index_For_Bin_Op => Unparser_For_Bin_Op'Access
         ,
         Type_Index_For_Block_Expr => Unparser_For_Block_Expr'Access
         ,
         Type_Index_For_Cast_Expr => Unparser_For_Cast_Expr'Access
         ,
         Type_Index_For_Dot_Expr => Unparser_For_Dot_Expr'Access
         ,
         Type_Index_For_Error_On_Null => Unparser_For_Error_On_Null'Access
         ,
         Type_Index_For_Generic_Instantiation => Unparser_For_Generic_Instantiation'Access
         ,
         Type_Index_For_Grammar_Expr => null
         ,
         Type_Index_For_Grammar_Cut => Unparser_For_Grammar_Cut'Access
         ,
         Type_Index_For_Grammar_Discard => Unparser_For_Grammar_Discard'Access
         ,
         Type_Index_For_Grammar_Dont_Skip => Unparser_For_Grammar_Dont_Skip'Access
         ,
         Type_Index_For_Grammar_List => Unparser_For_Grammar_List'Access
         ,
         Type_Index_For_Grammar_Null => Unparser_For_Grammar_Null'Access
         ,
         Type_Index_For_Grammar_Opt => Unparser_For_Grammar_Opt'Access
         ,
         Type_Index_For_Grammar_Opt_Error => Unparser_For_Grammar_Opt_Error'Access
         ,
         Type_Index_For_Grammar_Opt_Error_Group => Unparser_For_Grammar_Opt_Error_Group'Access
         ,
         Type_Index_For_Grammar_Opt_Group => Unparser_For_Grammar_Opt_Group'Access
         ,
         Type_Index_For_Grammar_Or_Expr => Unparser_For_Grammar_Or_Expr'Access
         ,
         Type_Index_For_Grammar_Pick => Unparser_For_Grammar_Pick'Access
         ,
         Type_Index_For_Grammar_Implicit_Pick => Unparser_For_Grammar_Implicit_Pick'Access
         ,
         Type_Index_For_Grammar_Predicate => Unparser_For_Grammar_Predicate'Access
         ,
         Type_Index_For_Grammar_Rule_Ref => Unparser_For_Grammar_Rule_Ref'Access
         ,
         Type_Index_For_Grammar_Skip => Unparser_For_Grammar_Skip'Access
         ,
         Type_Index_For_Grammar_Stop_Cut => Unparser_For_Grammar_Stop_Cut'Access
         ,
         Type_Index_For_Parse_Node_Expr => Unparser_For_Parse_Node_Expr'Access
         ,
         Type_Index_For_Token_Lit => Unparser_For_Token_Lit'Access
         ,
         Type_Index_For_Token_No_Case_Lit => Unparser_For_Token_No_Case_Lit'Access
         ,
         Type_Index_For_Token_Pattern_Concat => Unparser_For_Token_Pattern_Concat'Access
         ,
         Type_Index_For_Token_Pattern_Lit => Unparser_For_Token_Pattern_Lit'Access
         ,
         Type_Index_For_Token_Ref => Unparser_For_Token_Ref'Access
         ,
         Type_Index_For_Id => Unparser_For_Id'Access
         ,
         Type_Index_For_Def_Id => Unparser_For_Def_Id'Access
         ,
         Type_Index_For_Module_Ref_Id => Unparser_For_Module_Ref_Id'Access
         ,
         Type_Index_For_Ref_Id => Unparser_For_Ref_Id'Access
         ,
         Type_Index_For_If_Expr => Unparser_For_If_Expr'Access
         ,
         Type_Index_For_Isa => Unparser_For_Isa'Access
         ,
         Type_Index_For_Keep_Expr => Unparser_For_Keep_Expr'Access
         ,
         Type_Index_For_Lambda_Expr => Unparser_For_Lambda_Expr'Access
         ,
         Type_Index_For_Lit => null
         ,
         Type_Index_For_Big_Num_Lit => Unparser_For_Big_Num_Lit'Access
         ,
         Type_Index_For_Char_Lit => Unparser_For_Char_Lit'Access
         ,
         Type_Index_For_Null_Lit => Unparser_For_Null_Lit'Access
         ,
         Type_Index_For_Num_Lit => Unparser_For_Num_Lit'Access
         ,
         Type_Index_For_String_Lit => null
         ,
         Type_Index_For_Block_String_Lit => Unparser_For_Block_String_Lit'Access
         ,
         Type_Index_For_Single_Line_String_Lit => Unparser_For_Single_Line_String_Lit'Access
         ,
         Type_Index_For_Pattern_Single_Line_String_Lit => Unparser_For_Pattern_Single_Line_String_Lit'Access
         ,
         Type_Index_For_Logic_Assign => Unparser_For_Logic_Assign'Access
         ,
         Type_Index_For_Logic_Expr => Unparser_For_Logic_Expr'Access
         ,
         Type_Index_For_Logic_Propagate => Unparser_For_Logic_Propagate'Access
         ,
         Type_Index_For_Logic_Unify => Unparser_For_Logic_Unify'Access
         ,
         Type_Index_For_Match_Expr => Unparser_For_Match_Expr'Access
         ,
         Type_Index_For_Not_Expr => Unparser_For_Not_Expr'Access
         ,
         Type_Index_For_Paren_Expr => Unparser_For_Paren_Expr'Access
         ,
         Type_Index_For_Raise_Expr => Unparser_For_Raise_Expr'Access
         ,
         Type_Index_For_Subscript_Expr => Unparser_For_Subscript_Expr'Access
         ,
         Type_Index_For_Try_Expr => Unparser_For_Try_Expr'Access
         ,
         Type_Index_For_Un_Op => Unparser_For_Un_Op'Access
         ,
         Type_Index_For_Full_Decl => Unparser_For_Full_Decl'Access
         ,
         Type_Index_For_Grammar_List_Sep => Unparser_For_Grammar_List_Sep'Access
         ,
         Type_Index_For_Import => Unparser_For_Import'Access
         ,
         Type_Index_For_Langkit_Root => Unparser_For_Langkit_Root'Access
         ,
         Type_Index_For_Lexer_Case_Rule => Unparser_For_Lexer_Case_Rule'Access
         ,
         Type_Index_For_Lexer_Case_Rule_Send => Unparser_For_Lexer_Case_Rule_Send'Access
         ,
         Type_Index_For_List_Kind => null
         ,
         Type_Index_For_List_Kind_One => Unparser_For_List_Kind_One'Access
         ,
         Type_Index_For_List_Kind_Zero => Unparser_For_List_Kind_Zero'Access
         ,
         Type_Index_For_Lkt_Node_Base_List => null
         ,
         Type_Index_For_Argument_List => Unparser_For_Argument_List'Access
         ,
         Type_Index_For_Base_Lexer_Case_Rule_Alt_List => Unparser_For_Base_Lexer_Case_Rule_Alt_List'Access
         ,
         Type_Index_For_Base_Pattern_List => Unparser_For_Base_Pattern_List'Access
         ,
         Type_Index_For_Block_String_Line_List => Unparser_For_Block_String_Line_List'Access
         ,
         Type_Index_For_Call_Expr_List => Unparser_For_Call_Expr_List'Access
         ,
         Type_Index_For_Decl_Annotation_List => Unparser_For_Decl_Annotation_List'Access
         ,
         Type_Index_For_Elsif_Branch_List => Unparser_For_Elsif_Branch_List'Access
         ,
         Type_Index_For_Enum_Class_Alt_Decl_List => Unparser_For_Enum_Class_Alt_Decl_List'Access
         ,
         Type_Index_For_Enum_Class_Case_List => Unparser_For_Enum_Class_Case_List'Access
         ,
         Type_Index_For_Enum_Lit_Decl_List => Unparser_For_Enum_Lit_Decl_List'Access
         ,
         Type_Index_For_Expr_List => Unparser_For_Expr_List'Access
         ,
         Type_Index_For_Any_Of_List => Unparser_For_Any_Of_List'Access
         ,
         Type_Index_For_Full_Decl_List => Unparser_For_Full_Decl_List'Access
         ,
         Type_Index_For_Decl_Block => Unparser_For_Decl_Block'Access
         ,
         Type_Index_For_Generic_Param_Decl_List => Unparser_For_Generic_Param_Decl_List'Access
         ,
         Type_Index_For_Fun_Param_Decl_List => Unparser_For_Fun_Param_Decl_List'Access
         ,
         Type_Index_For_Grammar_Expr_List => Unparser_For_Grammar_Expr_List'Access
         ,
         Type_Index_For_Grammar_Expr_List_List => Unparser_For_Grammar_Expr_List_List'Access
         ,
         Type_Index_For_Import_List => Unparser_For_Import_List'Access
         ,
         Type_Index_For_Lambda_Param_Decl_List => Unparser_For_Lambda_Param_Decl_List'Access
         ,
         Type_Index_For_Lkt_Node_List => Unparser_For_Lkt_Node_List'Access
         ,
         Type_Index_For_Block_Decl_List => Unparser_For_Block_Decl_List'Access
         ,
         Type_Index_For_Match_Branch_List => Unparser_For_Match_Branch_List'Access
         ,
         Type_Index_For_Node_Pattern_Detail_List => Unparser_For_Node_Pattern_Detail_List'Access
         ,
         Type_Index_For_Ref_Id_List => Unparser_For_Ref_Id_List'Access
         ,
         Type_Index_For_Type_Ref_List => Unparser_For_Type_Ref_List'Access
         ,
         Type_Index_For_Synthetic_Type_Ref_List => null
         ,
         Type_Index_For_Match_Branch => Unparser_For_Match_Branch'Access
         ,
         Type_Index_For_Node_Pattern_Detail => null
         ,
         Type_Index_For_Node_Pattern_Field => Unparser_For_Node_Pattern_Field'Access
         ,
         Type_Index_For_Node_Pattern_Property => Unparser_For_Node_Pattern_Property'Access
         ,
         Type_Index_For_Node_Pattern_Selector => Unparser_For_Node_Pattern_Selector'Access
         ,
         Type_Index_For_Null_Cond_Qualifier => null
         ,
         Type_Index_For_Null_Cond_Qualifier_Absent => Unparser_For_Null_Cond_Qualifier_Absent'Access
         ,
         Type_Index_For_Null_Cond_Qualifier_Present => Unparser_For_Null_Cond_Qualifier_Present'Access
         ,
         Type_Index_For_Op => null
         ,
         Type_Index_For_Op_Amp => Unparser_For_Op_Amp'Access
         ,
         Type_Index_For_Op_And => Unparser_For_Op_And'Access
         ,
         Type_Index_For_Op_Div => Unparser_For_Op_Div'Access
         ,
         Type_Index_For_Op_Eq => Unparser_For_Op_Eq'Access
         ,
         Type_Index_For_Op_Gt => Unparser_For_Op_Gt'Access
         ,
         Type_Index_For_Op_Gte => Unparser_For_Op_Gte'Access
         ,
         Type_Index_For_Op_Logic_And => Unparser_For_Op_Logic_And'Access
         ,
         Type_Index_For_Op_Logic_Or => Unparser_For_Op_Logic_Or'Access
         ,
         Type_Index_For_Op_Lt => Unparser_For_Op_Lt'Access
         ,
         Type_Index_For_Op_Lte => Unparser_For_Op_Lte'Access
         ,
         Type_Index_For_Op_Minus => Unparser_For_Op_Minus'Access
         ,
         Type_Index_For_Op_Mult => Unparser_For_Op_Mult'Access
         ,
         Type_Index_For_Op_Ne => Unparser_For_Op_Ne'Access
         ,
         Type_Index_For_Op_Or => Unparser_For_Op_Or'Access
         ,
         Type_Index_For_Op_Or_Int => Unparser_For_Op_Or_Int'Access
         ,
         Type_Index_For_Op_Plus => Unparser_For_Op_Plus'Access
         ,
         Type_Index_For_Selector_Call => Unparser_For_Selector_Call'Access
         ,
         Type_Index_For_Type_Ref => null
         ,
         Type_Index_For_Default_List_Type_Ref => Unparser_For_Default_List_Type_Ref'Access
         ,
         Type_Index_For_Function_Type_Ref => Unparser_For_Function_Type_Ref'Access
         ,
         Type_Index_For_Generic_Type_Ref => Unparser_For_Generic_Type_Ref'Access
         ,
         Type_Index_For_Simple_Type_Ref => Unparser_For_Simple_Type_Ref'Access
         ,
         Type_Index_For_Var_Bind => Unparser_For_Var_Bind'Access
   );


   Default_Config : aliased constant String :=
          "{" & Character'Val (10)
      & "  ""node_configs"": {" & Character'Val (10)
      & "    ""AnyOf"": {" & Character'Val (10)
      & "      ""node"": {" & Character'Val (10)
      & "        ""kind"": ""group""," & Character'Val (10)
      & "        ""document"": [" & Character'Val (10)
      & "          {""kind"": ""recurse_field"", ""field"": ""f_expr""}," & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": ""in""}," & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""indent""," & Character'Val (10)
      & "            ""contents"": {" & Character'Val (10)
      & "              ""kind"": ""group""," & Character'Val (10)
      & "              ""document"": {""kind"": ""recurse_field"", ""field"": ""f_values""}" & Character'Val (10)
      & "            }" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""AnyOfList"": {" & Character'Val (10)
      & "      ""sep"": [""line"", ""recurse"", ""whitespace""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""ArrayLiteral"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_exprs"": {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            {" & Character'Val (10)
      & "              ""kind"": ""indent"", ""contents"": [" & Character'Val (10)
      & "                ""softline""," & Character'Val (10)
      & "                {""kind"": ""group"", ""document"": ""recurse""}" & Character'Val (10)
      & "              ]" & Character'Val (10)
      & "            }," & Character'Val (10)
      & "            ""softline""" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }," & Character'Val (10)
      & "        ""f_element_type"": [" & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": "":""}," & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          ""recurse""" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""BasePattern"": {}," & Character'Val (10)
      & "    ""ExtendedNodePattern"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_node_pattern"": {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": {" & Character'Val (10)
      & "            ""kind"": ""recurse_flatten""," & Character'Val (10)
      & "            ""if"": [""OrPattern""]" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        }," & Character'Val (10)
      & "        ""f_details"": {" & Character'Val (10)
      & "          ""kind"": ""ifEmpty""," & Character'Val (10)
      & "          ""then"": ""recurse""," & Character'Val (10)
      & "          ""else"": {" & Character'Val (10)
      & "            ""kind"": ""group""," & Character'Val (10)
      & "            ""document"": [" & Character'Val (10)
      & "              {" & Character'Val (10)
      & "                ""kind"": ""indent""," & Character'Val (10)
      & "                ""contents"": [""softline"", ""recurse""]" & Character'Val (10)
      & "              }," & Character'Val (10)
      & "              ""softline""" & Character'Val (10)
      & "            ]" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""TypePattern"": {}," & Character'Val (10)
      & "    ""NotPattern"": {}," & Character'Val (10)
      & "    ""NullPattern"": {}," & Character'Val (10)
      & "    ""OrPattern"": {" & Character'Val (10)
      & "      ""node"": {" & Character'Val (10)
      & "        ""kind"": ""group"", ""document"": ""recurse""" & Character'Val (10)
      & "      }," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_left"": [" & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""recurse_flatten""," & Character'Val (10)
      & "            ""if"": [""OrPattern""]" & Character'Val (10)
      & "          }," & Character'Val (10)
      & "          ""line""" & Character'Val (10)
      & "        ]," & Character'Val (10)
      & "        ""f_right"": [" & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""recurse_flatten""," & Character'Val (10)
      & "            ""if"": [""OrPattern""]" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""ParenPattern"": {}," & Character'Val (10)
      & "    ""RegexPattern"": {}," & Character'Val (10)
      & "    ""SplatPattern"": {}," & Character'Val (10)
      & "    ""TuplePattern"": {}," & Character'Val (10)
      & "    ""UniversalPattern"": {}," & Character'Val (10)
      & "    ""BasePatternList"": {}," & Character'Val (10)
      & "    ""NodePatternDetailList"": {" & Character'Val (10)
      & "      ""sep"": [""recurse"", ""line""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & Character'Val (10)
      & "    ""NodePatternField"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_id""}," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": "":""}," & Character'Val (10)
      & "        ""whitespace""," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_expected_value""}" & Character'Val (10)
      & "      ]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""NodePatternProperty"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_call""}," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": "":""}," & Character'Val (10)
      & "        ""whitespace""," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_expected_value""}" & Character'Val (10)
      & "      ]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""NodePatternSelector"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_call""}," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": "":""}," & Character'Val (10)
      & "        ""whitespace""," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_pattern""}" & Character'Val (10)
      & "      ]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & Character'Val (10)
      & "    ""SelectorCall"": {}," & Character'Val (10)
      & "    ""BaseCallExpr"": {" & Character'Val (10)
      & "      ""node"": {" & Character'Val (10)
      & "        ""kind"": ""group"", ""document"": ""recurse""" & Character'Val (10)
      & "      }," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_name"": {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": {" & Character'Val (10)
      & "            ""kind"": ""recurse_flatten""," & Character'Val (10)
      & "            ""if"": [""DotExpr"", ""KeepExpr"", ""CastExpr"", ""CallExpr""]" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        }," & Character'Val (10)
      & "        ""f_args"": {" & Character'Val (10)
      & "          ""kind"": ""ifEmpty""," & Character'Val (10)
      & "          ""then"": ""recurse""," & Character'Val (10)
      & "          ""else"": {" & Character'Val (10)
      & "            ""kind"": ""group""," & Character'Val (10)
      & "            ""document"": [" & Character'Val (10)
      & "              {" & Character'Val (10)
      & "                ""kind"": ""indent""," & Character'Val (10)
      & "                ""contents"": [""softline"", ""recurse""]" & Character'Val (10)
      & "              }," & Character'Val (10)
      & "              ""softline""" & Character'Val (10)
      & "            ]" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""BaseLexerCaseRuleAlt"": {}," & Character'Val (10)
      & "    ""BaseLexerCaseRuleAltList"": {" & Character'Val (10)
      & "      ""sep"": [""recurse"", ""hardline""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""BigNumLit"": {}," & Character'Val (10)
      & "    ""BinOp"": {" & Character'Val (10)
      & "      ""node"": {" & Character'Val (10)
      & "        ""kind"": ""group""," & Character'Val (10)
      & "        ""document"": [" & Character'Val (10)
      & "           {""kind"": ""recurse_field"", ""field"": ""f_left""}," & Character'Val (10)
      & "           ""line""," & Character'Val (10)
      & "           {""kind"": ""recurse_field"", ""field"": ""f_op""}," & Character'Val (10)
      & "           ""whitespace""," & Character'Val (10)
      & "           {""kind"": ""recurse_field"", ""field"": ""f_right""}" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""BlockDeclList"": {" & Character'Val (10)
      & "      ""sep"": [{""kind"": ""group"", ""document"": ""recurse""}, ""hardline""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""BlockExpr"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""{""}," & Character'Val (10)
      & "         {" & Character'Val (10)
      & "           ""kind"": ""indent""," & Character'Val (10)
      & "           ""contents"": [" & Character'Val (10)
      & "             ""hardline""," & Character'Val (10)
      & "             {""kind"": ""recurse_field"", ""field"": ""f_val_defs""}," & Character'Val (10)
      & "             {""kind"": ""text"", ""text"": "";""}," & Character'Val (10)
      & "             ""hardline""," & Character'Val (10)
      & "             ""flushLineBreaks""," & Character'Val (10)
      & "             {" & Character'Val (10)
      & "               ""kind"": ""group""," & Character'Val (10)
      & "               ""document"": {""kind"": ""recurse_field"", ""field"": ""f_expr""}" & Character'Val (10)
      & "             }" & Character'Val (10)
      & "           ]" & Character'Val (10)
      & "         }," & Character'Val (10)
      & "         ""hardline""," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""}""}" & Character'Val (10)
      & "      ]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""BlockStringLine"": {}," & Character'Val (10)
      & "    ""BlockStringLineList"": {}," & Character'Val (10)
      & "    ""BlockStringLit"": {}," & Character'Val (10)
      & "    ""CallExprList"": {" & Character'Val (10)
      & "      ""sep"": [""recurse"", ""hardline""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""CastExpr"": {" & Character'Val (10)
      & "      ""node"": {" & Character'Val (10)
      & "        ""kind"": ""group"", ""document"": ""recurse""" & Character'Val (10)
      & "      }," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_expr"": [" & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""recurse_flatten""," & Character'Val (10)
      & "            ""if"": [""DotExpr"", ""KeepExpr"", ""CastExpr"", ""CallExpr""]" & Character'Val (10)
      & "          }," & Character'Val (10)
      & "          ""softline""" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""CharLit"": {}," & Character'Val (10)
      & "    ""ClassDecl"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""class""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_syn_name""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_syn_base_type""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_traits""}," & Character'Val (10)
      & "         ""whitespace""," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""{""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_decls""}," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""}""}" & Character'Val (10)
      & "      ]," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_syn_base_type"": [" & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": "":""}," & Character'Val (10)
      & "         ""whitespace""," & Character'Val (10)
      & "         ""recurse""" & Character'Val (10)
      & "        ]," & Character'Val (10)
      & "        ""f_traits"": [" & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": ""implements""}," & Character'Val (10)
      & "          ""recurse""" & Character'Val (10)
      & "        ]," & Character'Val (10)
      & "        ""f_decls"": [" & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""ifEmpty""," & Character'Val (10)
      & "            ""then"": [""hardline"", ""recurse""]," & Character'Val (10)
      & "            ""else"": [" & Character'Val (10)
      & "              {""kind"": ""indent"", ""contents"": [""hardline"", ""recurse""]}," & Character'Val (10)
      & "              ""hardline""" & Character'Val (10)
      & "            ]" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""ClassQualifier"": {}," & Character'Val (10)
      & "    ""DeclAnnotation"": {" & Character'Val (10)
      & "      ""node"": [""recurse"", ""hardline""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""DeclAnnotationList"": {}," & Character'Val (10)
      & "    ""DeclAnnotationArgs"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_args"": {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            {" & Character'Val (10)
      & "              ""kind"": ""indent""," & Character'Val (10)
      & "              ""contents"": [" & Character'Val (10)
      & "                ""softline""," & Character'Val (10)
      & "                {""kind"": ""group"", ""document"": ""recurse""}" & Character'Val (10)
      & "              ]" & Character'Val (10)
      & "            }," & Character'Val (10)
      & "            ""softline""" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""DefId"": {}," & Character'Val (10)
      & "    ""DefaultListTypeRef"": {}," & Character'Val (10)
      & "    ""DotExpr"": {" & Character'Val (10)
      & "      ""node"": {" & Character'Val (10)
      & "        ""kind"": ""group"", ""document"": ""recurse""" & Character'Val (10)
      & "      }," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_prefix"": [" & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""recurse_flatten""," & Character'Val (10)
      & "            ""if"": [""DotExpr"", ""KeepExpr"", ""CastExpr"", ""CallExpr""]" & Character'Val (10)
      & "          }," & Character'Val (10)
      & "          ""softline""" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""DynVarDecl"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_decl_type"": [""whitespace"", ""recurse""]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""ElsifBranch"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "        ""hardline""," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""elif""}," & Character'Val (10)
      & "        ""whitespace""," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_cond_expr""}," & Character'Val (10)
      & "        {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            ""line""," & Character'Val (10)
      & "            {""kind"": ""text"", ""text"": ""then""}," & Character'Val (10)
      & "            ""whitespace""," & Character'Val (10)
      & "            {""kind"": ""recurse_field"", ""field"": ""f_then_expr""}" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      ]," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_cond_expr"": {" & Character'Val (10)
      & "          ""kind"": ""ifKind""," & Character'Val (10)
      & "          ""matchers"": [" & Character'Val (10)
      & "            {""kind"": [""BlockExpr"", ""ParenExpr""], ""document"": ""recurse""}" & Character'Val (10)
      & "          ]," & Character'Val (10)
      & "          ""default"": {" & Character'Val (10)
      & "            ""kind"": ""group""," & Character'Val (10)
      & "            ""document"": {""kind"": ""indent"", ""contents"": [""softline"", ""recurse""]}" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        }," & Character'Val (10)
      & "        ""f_then_expr"": {" & Character'Val (10)
      & "          ""kind"": ""ifKind""," & Character'Val (10)
      & "          ""matchers"": [" & Character'Val (10)
      & "            {""kind"": [""BlockExpr"", ""ParenExpr""], ""document"": ""recurse""}" & Character'Val (10)
      & "          ]," & Character'Val (10)
      & "          ""default"": {" & Character'Val (10)
      & "            ""kind"": ""group""," & Character'Val (10)
      & "            ""document"": {""kind"": ""indent"", ""contents"": [""softline"", ""recurse""]}" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""ElsifBranchList"": {" & Character'Val (10)
      & "      ""flush_before_children"": false" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""EnumClassAltDecl"": {}," & Character'Val (10)
      & "    ""EnumClassAltDeclList"": {" & Character'Val (10)
      & "      ""sep"": [""recurse"", ""line""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""EnumClassCase"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""case""}," & Character'Val (10)
      & "        {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            {" & Character'Val (10)
      & "              ""kind"": ""indent""," & Character'Val (10)
      & "              ""contents"": [" & Character'Val (10)
      & "                ""line""," & Character'Val (10)
      & "                {""kind"": ""recurse_field"", ""field"": ""f_decls""}" & Character'Val (10)
      & "              ]" & Character'Val (10)
      & "            }" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      ]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""EnumClassCaseList"": {" & Character'Val (10)
      & "      ""sep"": [""recurse"", ""hardline""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""EnumClassDecl"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""enum""}," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""class""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_syn_name""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_syn_base_type""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_traits""}," & Character'Val (10)
      & "         ""whitespace""," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""{""}," & Character'Val (10)
      & "         {" & Character'Val (10)
      & "           ""kind"": ""indent""," & Character'Val (10)
      & "           ""contents"": [" & Character'Val (10)
      & "             {""kind"": ""recurse_field"", ""field"": ""f_branches""}," & Character'Val (10)
      & "             {""kind"": ""recurse_field"", ""field"": ""f_decls""}" & Character'Val (10)
      & "           ]" & Character'Val (10)
      & "         }," & Character'Val (10)
      & "         ""hardline""," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""}""}" & Character'Val (10)
      & "      ]," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_syn_base_type"": [" & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": "":""}," & Character'Val (10)
      & "         ""whitespace""," & Character'Val (10)
      & "         ""recurse""" & Character'Val (10)
      & "        ]," & Character'Val (10)
      & "        ""f_branches"": {" & Character'Val (10)
      & "          ""kind"": ""ifEmpty""," & Character'Val (10)
      & "          ""then"": [""recurse""]," & Character'Val (10)
      & "          ""else"": [""hardline"", ""recurse""]" & Character'Val (10)
      & "        }," & Character'Val (10)
      & "        ""f_decls"": {" & Character'Val (10)
      & "          ""kind"": ""ifEmpty""," & Character'Val (10)
      & "          ""then"": [""recurse""]," & Character'Val (10)
      & "          ""else"": [""hardline"", ""recurse""]" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""EnumLitDecl"": {}," & Character'Val (10)
      & "    ""EnumLitDeclList"": {" & Character'Val (10)
      & "      ""sep"": [""recurse"", ""line""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""EnumTypeDecl"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""enum""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_syn_name""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_traits""}," & Character'Val (10)
      & "         ""whitespace""," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""{""}," & Character'Val (10)
      & "         {" & Character'Val (10)
      & "           ""kind"": ""indent""," & Character'Val (10)
      & "           ""contents"": [" & Character'Val (10)
      & "             ""hardline""," & Character'Val (10)
      & "             {""kind"": ""text"", ""text"": ""case""}," & Character'Val (10)
      & "             {" & Character'Val (10)
      & "               ""kind"": ""group""," & Character'Val (10)
      & "               ""document"": {" & Character'Val (10)
      & "                 ""kind"": ""indent""," & Character'Val (10)
      & "                 ""contents"": [" & Character'Val (10)
      & "                   ""line""," & Character'Val (10)
      & "                   {""kind"": ""recurse_field"", ""field"": ""f_literals""}" & Character'Val (10)
      & "                 ]" & Character'Val (10)
      & "               }" & Character'Val (10)
      & "             }," & Character'Val (10)
      & "             {""kind"": ""recurse_field"", ""field"": ""f_decls""}" & Character'Val (10)
      & "           ]" & Character'Val (10)
      & "         }," & Character'Val (10)
      & "         ""hardline""," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""}""}" & Character'Val (10)
      & "      ]," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_decls"": {" & Character'Val (10)
      & "          ""kind"": ""ifEmpty""," & Character'Val (10)
      & "          ""then"": [""recurse""]," & Character'Val (10)
      & "          ""else"": [""hardline"", ""recurse""]" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""EnvSpecDecl"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_syn_name"": [""recurse"", ""whitespace""]," & Character'Val (10)
      & "        ""f_actions"": [" & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""indent""," & Character'Val (10)
      & "            ""contents"": [" & Character'Val (10)
      & "              {""kind"": ""ifEmpty"", ""then"": [], ""else"": ""hardline""}," & Character'Val (10)
      & "              ""recurse""" & Character'Val (10)
      & "            ]" & Character'Val (10)
      & "          }," & Character'Val (10)
      & "          ""hardline""" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""ErrorOnNull"": {}," & Character'Val (10)
      & "    ""ExcludesNull"": {}," & Character'Val (10)
      & "    ""ExprList"": {" & Character'Val (10)
      & "      ""sep"": [""recurse"", ""line""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""FieldDecl"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_syn_name""}," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": "":""}," & Character'Val (10)
      & "        ""whitespace""," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_decl_type""}," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_trait_ref""}," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_default_val""}" & Character'Val (10)
      & "      ]," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_trait_ref"": [""whitespace"", ""recurse""]," & Character'Val (10)
      & "        ""f_default_val"": [" & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": ""=""}," & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""ifKind""," & Character'Val (10)
      & "            ""matchers"": [" & Character'Val (10)
      & "              {""kind"": ""BlockExpr"", ""document"": [""whitespace"", ""recurse""]}," & Character'Val (10)
      & "              {""kind"": ""ParenExpr"", ""document"": [""whitespace"", ""recurse""]}" & Character'Val (10)
      & "            ]," & Character'Val (10)
      & "            ""default"": {" & Character'Val (10)
      & "              ""kind"": ""group""," & Character'Val (10)
      & "              ""document"": {""kind"": ""indent"", ""contents"": [""line"", ""recurse""]}" & Character'Val (10)
      & "            }" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""FullDecl"": {}," & Character'Val (10)
      & "    ""FullDeclList"": {" & Character'Val (10)
      & "      ""sep"": [""recurse"", ""hardline""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""FunParamDecl"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_decl_annotations""}," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_syn_name""}," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": "":""}," & Character'Val (10)
      & "        ""whitespace""," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_decl_type""}," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_default_val""}" & Character'Val (10)
      & "      ]," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_default_val"": [" & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": ""=""}," & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""group""," & Character'Val (10)
      & "            ""document"": {""kind"": ""indent"", ""contents"": [""line"", ""recurse""]}" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""FunParamDeclList"": {" & Character'Val (10)
      & "      ""sep"": [""recurse"", ""line""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""FunDecl"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""fun""}," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_syn_name""}," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""(""}," & Character'Val (10)
      & "        {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            {" & Character'Val (10)
      & "              ""kind"": ""indent""," & Character'Val (10)
      & "              ""contents"": [" & Character'Val (10)
      & "                ""softline""," & Character'Val (10)
      & "                {""kind"": ""recurse_field"", ""field"": ""f_params""}" & Character'Val (10)
      & "              ]" & Character'Val (10)
      & "            }," & Character'Val (10)
      & "            ""softline""" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": "")""}," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": "":""}," & Character'Val (10)
      & "        ""whitespace""," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_return_type""}," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_trait_ref""}," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_body""}" & Character'Val (10)
      & "      ]," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_trait_ref"": [" & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": ""implements""}," & Character'Val (10)
      & "          ""recurse""" & Character'Val (10)
      & "        ]," & Character'Val (10)
      & "        ""f_body"": [" & Character'Val (10)
      & "         ""whitespace""," & Character'Val (10)
      & "         {" & Character'Val (10)
      & "           ""kind"": ""ifKind""," & Character'Val (10)
      & "           ""matchers"": [" & Character'Val (10)
      & "             {" & Character'Val (10)
      & "               ""kind"": [""BlockExpr"", ""MatchExpr""]," & Character'Val (10)
      & "               ""document"": [" & Character'Val (10)
      & "                 {""kind"": ""text"", ""text"": ""=""}," & Character'Val (10)
      & "                 ""whitespace""," & Character'Val (10)
      & "                 ""recurse""" & Character'Val (10)
      & "               ]" & Character'Val (10)
      & "             }" & Character'Val (10)
      & "           ]," & Character'Val (10)
      & "           ""default"": {" & Character'Val (10)
      & "             ""kind"": ""group""," & Character'Val (10)
      & "             ""document"": [" & Character'Val (10)
      & "               {" & Character'Val (10)
      & "                 ""kind"": ""indent""," & Character'Val (10)
      & "                 ""contents"": [" & Character'Val (10)
      & "                   {""kind"": ""text"", ""text"": ""=""}," & Character'Val (10)
      & "                   ""line""," & Character'Val (10)
      & "                   ""recurse""" & Character'Val (10)
      & "                 ]" & Character'Val (10)
      & "               }" & Character'Val (10)
      & "             ]" & Character'Val (10)
      & "           }" & Character'Val (10)
      & "         }" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""FunctionTypeRef"": {}," & Character'Val (10)
      & "    ""GenericDecl"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""generic""}," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""[""}," & Character'Val (10)
      & "        {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            {" & Character'Val (10)
      & "              ""kind"": ""indent""," & Character'Val (10)
      & "              ""contents"": [" & Character'Val (10)
      & "                ""softline""," & Character'Val (10)
      & "                {""kind"": ""recurse_field"", ""field"": ""f_generic_param_decls""}" & Character'Val (10)
      & "              ]" & Character'Val (10)
      & "            }," & Character'Val (10)
      & "            ""softline""" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""]""}," & Character'Val (10)
      & "        ""hardline""," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_decl""}" & Character'Val (10)
      & "      ]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""GenericParamDeclList"": {" & Character'Val (10)
      & "      ""sep"": [""recurse"", ""line""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""GenericParamTypeDecl"": {}," & Character'Val (10)
      & "    ""GenericInstantiation"": {}," & Character'Val (10)
      & "    ""GenericTypeRef"": {}," & Character'Val (10)
      & "    ""GrammarCut"": {}," & Character'Val (10)
      & "    ""GrammarDecl"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""grammar""}," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_syn_name""}," & Character'Val (10)
      & "        ""whitespace""," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""{""}," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_rules""}," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""}""}" & Character'Val (10)
      & "      ]," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_rules"": [" & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""ifEmpty""," & Character'Val (10)
      & "            ""then"": [""hardline"", ""recurse""]," & Character'Val (10)
      & "            ""else"": [" & Character'Val (10)
      & "              {""kind"": ""indent"", ""contents"": [""hardline"", ""recurse""]}," & Character'Val (10)
      & "              ""hardline""" & Character'Val (10)
      & "            ]" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""GrammarDiscard"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_expr"": {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            {" & Character'Val (10)
      & "              ""kind"": ""indent""," & Character'Val (10)
      & "              ""contents"": [""softline"", ""recurse""]" & Character'Val (10)
      & "            }," & Character'Val (10)
      & "            ""softline""" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""GrammarDontSkip"": {}," & Character'Val (10)
      & "    ""GrammarExprList"": {" & Character'Val (10)
      & "      ""sep"": [""recurse"", ""line""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""GrammarExprListList"": {" & Character'Val (10)
      & "      ""sep"": [""line"", ""recurse"", ""whitespace""]," & Character'Val (10)
      & "      ""leading_sep"": [""recurse"", ""whitespace""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""GrammarImplicitPick"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_exprs"": ""recurse""" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""GrammarList"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_list_type""}," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_kind""}," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""(""}," & Character'Val (10)
      & "        {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            {" & Character'Val (10)
      & "              ""kind"": ""indent""," & Character'Val (10)
      & "              ""contents"": [" & Character'Val (10)
      & "                ""softline""," & Character'Val (10)
      & "                {""kind"": ""recurse_field"", ""field"": ""f_expr""}," & Character'Val (10)
      & "                {""kind"": ""recurse_field"", ""field"": ""f_sep""}" & Character'Val (10)
      & "              ]" & Character'Val (10)
      & "            }," & Character'Val (10)
      & "            ""softline""" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": "")""}" & Character'Val (10)
      & "      ]," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_sep"": [" & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": "",""}," & Character'Val (10)
      & "          ""line""," & Character'Val (10)
      & "          ""recurse""" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""GrammarListSep"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_extra"": [" & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": "",""}," & Character'Val (10)
      & "          ""line""," & Character'Val (10)
      & "          ""recurse""" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""GrammarNull"": {}," & Character'Val (10)
      & "    ""GrammarOpt"": {}," & Character'Val (10)
      & "    ""GrammarOptError"": {}," & Character'Val (10)
      & "    ""GrammarOptErrorGroup"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_expr"": {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            {" & Character'Val (10)
      & "              ""kind"": ""indent""," & Character'Val (10)
      & "              ""contents"": [""softline"", ""recurse""]" & Character'Val (10)
      & "            }," & Character'Val (10)
      & "            ""softline""" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""GrammarOptGroup"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_expr"": {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            {" & Character'Val (10)
      & "              ""kind"": ""indent""," & Character'Val (10)
      & "              ""contents"": [""softline"", ""recurse""]" & Character'Val (10)
      & "            }," & Character'Val (10)
      & "            ""softline""" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""GrammarOrExpr"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_sub_exprs"": {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            {" & Character'Val (10)
      & "              ""kind"": ""indent""," & Character'Val (10)
      & "              ""contents"": [""softline"", ""recurse""]" & Character'Val (10)
      & "            }," & Character'Val (10)
      & "            ""softline""" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""GrammarPick"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_exprs"": {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            {" & Character'Val (10)
      & "              ""kind"": ""indent""," & Character'Val (10)
      & "              ""contents"": [""softline"", ""recurse""]" & Character'Val (10)
      & "            }," & Character'Val (10)
      & "            ""softline""" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""GrammarPredicate"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_expr""}," & Character'Val (10)
      & "         ""whitespace""," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""|>""}," & Character'Val (10)
      & "         ""whitespace""," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""when""}," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""(""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_prop_ref""}," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": "")""}" & Character'Val (10)
      & "      ]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""GrammarRuleDecl"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_expr"": [" & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": ""<-""}," & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""ifKind""," & Character'Val (10)
      & "            ""matchers"": [" & Character'Val (10)
      & "              {" & Character'Val (10)
      & "                ""kind"": [""TokenLit"", ""TokenPatternLit""]," & Character'Val (10)
      & "                ""document"": {" & Character'Val (10)
      & "                  ""kind"": ""indent""," & Character'Val (10)
      & "                  ""contents"": {" & Character'Val (10)
      & "                    ""kind"": ""group""," & Character'Val (10)
      & "                    ""document"": [""line"", ""recurse""]" & Character'Val (10)
      & "                  }" & Character'Val (10)
      & "                }" & Character'Val (10)
      & "              }" & Character'Val (10)
      & "            ]," & Character'Val (10)
      & "            ""default"": [""whitespace"", ""recurse""]" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""GrammarRuleRef"": {}," & Character'Val (10)
      & "    ""GrammarSkip"": {}," & Character'Val (10)
      & "    ""GrammarStopCut"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_expr"": {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            {" & Character'Val (10)
      & "              ""kind"": ""indent""," & Character'Val (10)
      & "              ""contents"": [""softline"", ""recurse""]" & Character'Val (10)
      & "            }," & Character'Val (10)
      & "            ""softline""" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""Id"": {}," & Character'Val (10)
      & "    ""IfExpr"": {" & Character'Val (10)
      & "      ""node"": {" & Character'Val (10)
      & "        ""kind"": ""group""," & Character'Val (10)
      & "        ""document"": [" & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""group""," & Character'Val (10)
      & "            ""document"": [" & Character'Val (10)
      & "              {""kind"": ""text"", ""text"": ""if""}," & Character'Val (10)
      & "              ""whitespace""," & Character'Val (10)
      & "              {""kind"": ""recurse_field"", ""field"": ""f_cond_expr""}," & Character'Val (10)
      & "              ""line""," & Character'Val (10)
      & "              {""kind"": ""text"", ""text"": ""then""}," & Character'Val (10)
      & "              ""whitespace""," & Character'Val (10)
      & "              {""kind"": ""recurse_field"", ""field"": ""f_then_expr""}" & Character'Val (10)
      & "            ]" & Character'Val (10)
      & "          }," & Character'Val (10)
      & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""group""," & Character'Val (10)
      & "            ""document"": [" & Character'Val (10)
      & "              {""kind"": ""recurse_field"", ""field"": ""f_alternatives""}" & Character'Val (10)
      & "            ]" & Character'Val (10)
      & "          }," & Character'Val (10)
      & Character'Val (10)
      & "          ""line""," & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""group""," & Character'Val (10)
      & "            ""document"": [" & Character'Val (10)
      & "              {""kind"": ""text"", ""text"": ""else""}," & Character'Val (10)
      & "              ""whitespace""," & Character'Val (10)
      & "              {""kind"": ""recurse_field"", ""field"": ""f_else_expr""}" & Character'Val (10)
      & "            ]" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_cond_expr"": {" & Character'Val (10)
      & "          ""kind"": ""ifKind""," & Character'Val (10)
      & "          ""matchers"": [" & Character'Val (10)
      & "            {""kind"": [""BlockExpr"", ""ParenExpr""], ""document"": ""recurse""}" & Character'Val (10)
      & "          ]," & Character'Val (10)
      & "          ""default"": {" & Character'Val (10)
      & "            ""kind"": ""group""," & Character'Val (10)
      & "            ""document"": {""kind"": ""indent"", ""contents"": [""softline"", ""recurse""]}" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        }," & Character'Val (10)
      & "        ""f_else_expr"": {" & Character'Val (10)
      & "          ""kind"": ""ifKind""," & Character'Val (10)
      & "          ""matchers"": [" & Character'Val (10)
      & "            {""kind"": [""BlockExpr"", ""ParenExpr""], ""document"": ""recurse""}" & Character'Val (10)
      & "          ]," & Character'Val (10)
      & "          ""default"": {" & Character'Val (10)
      & "            ""kind"": ""group""," & Character'Val (10)
      & "            ""document"": {""kind"": ""indent"", ""contents"": [""softline"", ""recurse""]}" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        }," & Character'Val (10)
      & "        ""f_then_expr"": {" & Character'Val (10)
      & "          ""kind"": ""ifKind""," & Character'Val (10)
      & "          ""matchers"": [" & Character'Val (10)
      & "            {""kind"": [""BlockExpr"", ""ParenExpr""], ""document"": ""recurse""}" & Character'Val (10)
      & "          ]," & Character'Val (10)
      & "          ""default"": {" & Character'Val (10)
      & "            ""kind"": ""group""," & Character'Val (10)
      & "            ""document"": {""kind"": ""indent"", ""contents"": [""softline"", ""recurse""]}" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""Import"": {" & Character'Val (10)
      & "      ""node"": [""recurse"", ""hardline""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""ImportList"": {}," & Character'Val (10)
      & "    ""Isa"": {" & Character'Val (10)
      & "      ""node"": {" & Character'Val (10)
      & "        ""kind"": ""group""," & Character'Val (10)
      & "        ""document"": [" & Character'Val (10)
      & "          {""kind"": ""recurse_field"", ""field"": ""f_expr""}," & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": ""is""}," & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          {""kind"": ""recurse_field"", ""field"": ""f_pattern""}" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""KeepExpr"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_expr"": [" & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""recurse_flatten""," & Character'Val (10)
      & "            ""if"": [""DotExpr"", ""KeepExpr"", ""CastExpr"", ""CallExpr""]" & Character'Val (10)
      & "          }," & Character'Val (10)
      & "          ""softline""" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""LambdaParamDecl"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_decl_type"": [" & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": "":""}," & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          ""recurse""" & Character'Val (10)
      & "        ]," & Character'Val (10)
      & "        ""f_default_val"": [" & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": ""=""}," & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          ""recurse""" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""LambdaParamDeclList"": {" & Character'Val (10)
      & "      ""sep"": [""recurse"", ""line""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""LambdaExpr"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""(""}," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_params""}," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": "")""}," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_return_type""}," & Character'Val (10)
      & "        ""whitespace""," & Character'Val (10)
      & "        {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""id"": ""comments""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            {""kind"": ""text"", ""text"": ""=>""}," & Character'Val (10)
      & "            ""flushLineBreaks""" & Character'Val (10)
      & "          ]," & Character'Val (10)
      & "          ""bubbleUpTrailingTrivias"": false" & Character'Val (10)
      & "        }," & Character'Val (10)
      & "        {" & Character'Val (10)
      & "          ""kind"": ""ifKind""," & Character'Val (10)
      & "          ""field"": ""f_body""," & Character'Val (10)
      & "          ""matchers"": [" & Character'Val (10)
      & "            {" & Character'Val (10)
      & "              ""kind"": ""BlockExpr""," & Character'Val (10)
      & "              ""document"": [" & Character'Val (10)
      & "                {" & Character'Val (10)
      & "                  ""kind"": ""ifBreak""," & Character'Val (10)
      & "                  ""groupId"": ""comments""," & Character'Val (10)
      & "                  ""breakContents"": []," & Character'Val (10)
      & "                  ""flatContents"": ""whitespace""" & Character'Val (10)
      & "                }," & Character'Val (10)
      & "                {""kind"": ""recurse_field"", ""field"": ""f_body""}" & Character'Val (10)
      & "              ]" & Character'Val (10)
      & "            }" & Character'Val (10)
      & "          ]," & Character'Val (10)
      & "          ""default"": {" & Character'Val (10)
      & "            ""kind"": ""group""," & Character'Val (10)
      & "            ""document"": [" & Character'Val (10)
      & "              {" & Character'Val (10)
      & "                ""kind"": ""ifBreak""," & Character'Val (10)
      & "                ""groupId"": ""comments""," & Character'Val (10)
      & "                ""breakContents"": []," & Character'Val (10)
      & "                ""flatContents"": ""line""" & Character'Val (10)
      & "              }," & Character'Val (10)
      & "              {""kind"": ""recurse_field"", ""field"": ""f_body""}" & Character'Val (10)
      & "            ]" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      ]," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_params"": {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            {" & Character'Val (10)
      & "              ""kind"": ""indent""," & Character'Val (10)
      & "              ""contents"": [""softline"", ""recurse""]" & Character'Val (10)
      & "            }," & Character'Val (10)
      & "            ""softline""" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }," & Character'Val (10)
      & "        ""f_return_type"": [" & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": "":""}," & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          ""recurse""" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""LangkitRoot"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_imports"": [" & Character'Val (10)
      & "          ""recurse""," & Character'Val (10)
      & "          {""kind"": ""ifEmpty"", ""then"": [], ""else"": ""hardline""}" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""LexerCaseRule"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""match""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_expr""}," & Character'Val (10)
      & "         ""whitespace""," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""{""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_alts""}," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""}""}" & Character'Val (10)
      & "      ]," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_alts"": [" & Character'Val (10)
      & "          {""kind"": ""indent"", ""contents"": [""hardline"", ""recurse""]}," & Character'Val (10)
      & "          ""hardline""" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""LexerCaseRuleSend"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_match_size"": [""whitespace"", ""recurse""]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""LexerDecl"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""lexer""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_syn_name""}," & Character'Val (10)
      & "         ""whitespace""," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""{""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_rules""}," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""}""}" & Character'Val (10)
      & "      ]," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_rules"": [" & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""ifEmpty""," & Character'Val (10)
      & "            ""then"": [""hardline"", ""recurse""]," & Character'Val (10)
      & "            ""else"": [" & Character'Val (10)
      & "              {""kind"": ""indent"", ""contents"": [""hardline"", ""recurse""]}," & Character'Val (10)
      & "              ""hardline""" & Character'Val (10)
      & "            ]" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""LexerFamilyDecl"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""family""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_syn_name""}," & Character'Val (10)
      & "         ""whitespace""," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""{""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_rules""}," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""}""}" & Character'Val (10)
      & "      ]," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_rules"": [" & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""ifEmpty""," & Character'Val (10)
      & "            ""then"": [""hardline"", ""recurse""]," & Character'Val (10)
      & "            ""else"": [" & Character'Val (10)
      & "              {""kind"": ""indent"", ""contents"": [""hardline"", ""recurse""]}," & Character'Val (10)
      & "              ""hardline""" & Character'Val (10)
      & "            ]" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""ListKind"": {}," & Character'Val (10)
      & "    ""LktNodeList"": {" & Character'Val (10)
      & "      ""sep"": [""recurse"", ""hardline""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""LogicAssign"": {" & Character'Val (10)
      & "      ""node"": {" & Character'Val (10)
      & "        ""kind"": ""group""," & Character'Val (10)
      & "        ""document"": [" & Character'Val (10)
      & "          {""kind"": ""recurse_field"", ""field"": ""f_dest_var""}," & Character'Val (10)
      & "          ""line""," & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": ""<-""}," & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          {""kind"": ""recurse_field"", ""field"": ""f_value""}" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""LogicExpr"": {}," & Character'Val (10)
      & "    ""LogicPredicate"": {}," & Character'Val (10)
      & "    ""LogicPropagate"": {" & Character'Val (10)
      & "      ""node"": {" & Character'Val (10)
      & "        ""kind"": ""group""," & Character'Val (10)
      & "        ""document"": [" & Character'Val (10)
      & "          {""kind"": ""recurse_field"", ""field"": ""f_dest_var""}," & Character'Val (10)
      & "          ""line""," & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": ""<-""}," & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          {""kind"": ""recurse_field"", ""field"": ""f_call""}" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""LogicUnify"": {" & Character'Val (10)
      & "      ""node"": {" & Character'Val (10)
      & "        ""kind"": ""group""," & Character'Val (10)
      & "        ""document"": [" & Character'Val (10)
      & "          {""kind"": ""recurse_field"", ""field"": ""f_lhs""}," & Character'Val (10)
      & "          ""line""," & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": ""<->""}," & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          {""kind"": ""recurse_field"", ""field"": ""f_rhs""}" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""MatchBranch"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""case""}," & Character'Val (10)
      & "        ""whitespace""," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_decl""}," & Character'Val (10)
      & "        ""whitespace""," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""=>""}," & Character'Val (10)
      & "        {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": {" & Character'Val (10)
      & "            ""kind"": ""indent""," & Character'Val (10)
      & "            ""contents"": [" & Character'Val (10)
      & "              ""line""," & Character'Val (10)
      & "              {""kind"": ""recurse_field"", ""field"": ""f_expr""}" & Character'Val (10)
      & "            ]" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      ]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""MatchBranchList"": {" & Character'Val (10)
      & "      ""sep"": [""recurse"", ""hardline""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""MatchExpr"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""match""}," & Character'Val (10)
      & "        {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            {" & Character'Val (10)
      & "              ""kind"": ""indent""," & Character'Val (10)
      & "              ""contents"": [" & Character'Val (10)
      & "                ""line""," & Character'Val (10)
      & "                {""kind"": ""recurse_field"", ""field"": ""f_match_expr""}" & Character'Val (10)
      & "              ]" & Character'Val (10)
      & "            }," & Character'Val (10)
      & "            ""line""" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""{""}," & Character'Val (10)
      & "        {" & Character'Val (10)
      & "          ""kind"": ""indent""," & Character'Val (10)
      & "          ""contents"": [" & Character'Val (10)
      & "            ""hardline""," & Character'Val (10)
      & "            {""kind"": ""recurse_field"", ""field"": ""f_branches""}" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }," & Character'Val (10)
      & "        ""hardline""," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""}""}" & Character'Val (10)
      & "      ]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""MatchValDecl"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_decl_type"": [""whitespace"", ""recurse""]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""ModuleRefId"": {}," & Character'Val (10)
      & "    ""NotExpr"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_expr"": {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [""whitespace"", ""recurse""]" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""NullCondQualifier"": {}," & Character'Val (10)
      & "    ""NullLit"": {}," & Character'Val (10)
      & "    ""NumLit"": {}," & Character'Val (10)
      & "    ""Op"": {}," & Character'Val (10)
      & "    ""Argument"": {}," & Character'Val (10)
      & "    ""ArgumentList"": {" & Character'Val (10)
      & "      ""sep"": [""recurse"", ""line""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""ParenExpr"": {" & Character'Val (10)
      & "      ""node"": {" & Character'Val (10)
      & "        ""kind"": ""group""," & Character'Val (10)
      & "        ""document"": [" & Character'Val (10)
      & "          {" & Character'Val (10)
      & "              ""kind"": ""indent""," & Character'Val (10)
      & "              ""contents"": [" & Character'Val (10)
      & "                  {""kind"": ""text"", ""text"": ""(""}," & Character'Val (10)
      & "                  ""softline""," & Character'Val (10)
      & "                  {" & Character'Val (10)
      & "                  ""kind"": ""group""," & Character'Val (10)
      & "                  ""document"": {""kind"": ""recurse_field"", ""field"": ""f_expr""}" & Character'Val (10)
      & "                  }" & Character'Val (10)
      & "              ]" & Character'Val (10)
      & "          }," & Character'Val (10)
      & "          ""softline""," & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": "")""}" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""ParseNodeExpr"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_sub_exprs"": {" & Character'Val (10)
      & "          ""kind"": ""group""," & Character'Val (10)
      & "          ""document"": [" & Character'Val (10)
      & "            {" & Character'Val (10)
      & "              ""kind"": ""indent""," & Character'Val (10)
      & "              ""contents"": [" & Character'Val (10)
      & "                ""softline""," & Character'Val (10)
      & "                {""kind"": ""group"", ""document"": ""recurse""}" & Character'Val (10)
      & "              ]" & Character'Val (10)
      & "            }," & Character'Val (10)
      & "            ""softline""" & Character'Val (10)
      & "          ]" & Character'Val (10)
      & "        }" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""RaiseExpr"": {" & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_except_expr"": [""whitespace"", ""recurse""]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""RefId"": {}," & Character'Val (10)
      & "    ""RefIdList"": {}," & Character'Val (10)
      & "    ""SimpleTypeRef"": {}," & Character'Val (10)
      & "    ""SingleLineStringLit"": {}," & Character'Val (10)
      & "    ""StructDecl"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""struct""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_syn_name""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_traits""}," & Character'Val (10)
      & "         ""whitespace""," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""{""}," & Character'Val (10)
      & "         {""kind"": ""recurse_field"", ""field"": ""f_decls""}," & Character'Val (10)
      & "         {""kind"": ""text"", ""text"": ""}""}" & Character'Val (10)
      & "      ]," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_decls"": [" & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""ifEmpty""," & Character'Val (10)
      & "            ""then"": [""hardline"", ""recurse""]," & Character'Val (10)
      & "            ""else"": [" & Character'Val (10)
      & "              {""kind"": ""indent"", ""contents"": [""hardline"", ""recurse""]}," & Character'Val (10)
      & "              ""hardline""" & Character'Val (10)
      & "            ]" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""SubscriptExpr"": {" & Character'Val (10)
      & "      ""node"": {""kind"": ""group"", ""document"": ""recurse""}," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_index"": [" & Character'Val (10)
      & "          {""kind"": ""indent"", ""contents"": [""softline"", ""recurse""]}," & Character'Val (10)
      & "          ""softline""" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""TokenLit"": {}," & Character'Val (10)
      & "    ""TokenNoCaseLit"": {}," & Character'Val (10)
      & "    ""TokenPatternConcat"": {" & Character'Val (10)
      & "      ""node"": {" & Character'Val (10)
      & "        ""kind"": ""group""," & Character'Val (10)
      & "        ""document"": [" & Character'Val (10)
      & "           {""kind"": ""recurse_field"", ""field"": ""f_left""}," & Character'Val (10)
      & "           ""line""," & Character'Val (10)
      & "           {""kind"": ""text"", ""text"": ""&""}," & Character'Val (10)
      & "           ""whitespace""," & Character'Val (10)
      & "           {""kind"": ""recurse_field"", ""field"": ""f_right""}" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""TokenPatternLit"": {}," & Character'Val (10)
      & "    ""TokenRef"": {}," & Character'Val (10)
      & "    ""TraitDecl"": {" & Character'Val (10)
      & "      ""node"": [" & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""trait""}," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_syn_name""}," & Character'Val (10)
      & "        ""whitespace""," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_traits""}," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""{""}," & Character'Val (10)
      & "        {""kind"": ""recurse_field"", ""field"": ""f_decls""}," & Character'Val (10)
      & "        {""kind"": ""text"", ""text"": ""}""}" & Character'Val (10)
      & "      ]," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "        ""f_decls"": [" & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""ifEmpty""," & Character'Val (10)
      & "            ""then"": [""hardline"", ""recurse""]," & Character'Val (10)
      & "            ""else"": [" & Character'Val (10)
      & "              {""kind"": ""indent"", ""contents"": [""hardline"", ""recurse""]}," & Character'Val (10)
      & "              ""hardline""" & Character'Val (10)
      & "            ]" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""TryExpr"": {" & Character'Val (10)
      & "      ""node"": {" & Character'Val (10)
      & "        ""kind"": ""group""," & Character'Val (10)
      & "        ""document"": [" & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""group""," & Character'Val (10)
      & "            ""document"": [" & Character'Val (10)
      & "               {""kind"": ""text"", ""text"": ""try""}," & Character'Val (10)
      & "               ""whitespace""," & Character'Val (10)
      & "               {""kind"": ""recurse_field"", ""field"": ""f_try_expr""}" & Character'Val (10)
      & "            ]" & Character'Val (10)
      & "          }," & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""ifKind""," & Character'Val (10)
      & "            ""field"": ""f_or_expr""," & Character'Val (10)
      & "            ""matchers"": []," & Character'Val (10)
      & "            ""default"": [" & Character'Val (10)
      & "              ""line""," & Character'Val (10)
      & "              {" & Character'Val (10)
      & "                ""kind"": ""group""," & Character'Val (10)
      & "                ""document"": {""kind"": ""recurse_field"", ""field"": ""f_or_expr""}" & Character'Val (10)
      & "              }" & Character'Val (10)
      & "            ]," & Character'Val (10)
      & "            ""absent"": {""kind"": ""recurse_field"", ""field"": ""f_or_expr""}" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""TypeRefList"": {" & Character'Val (10)
      & "      ""sep"": [""recurse"", ""whitespace""]" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""UnOp"": {}," & Character'Val (10)
      & "    ""ValDecl"": {" & Character'Val (10)
      & "      ""node"": {" & Character'Val (10)
      & "        ""kind"": ""group""," & Character'Val (10)
      & "        ""document"": [" & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": ""val""}," & Character'Val (10)
      & "          {""kind"": ""recurse_field"", ""field"": ""f_syn_name""}," & Character'Val (10)
      & "          {""kind"": ""recurse_field"", ""field"": ""f_decl_type""}," & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": ""=""}," & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""ifKind""," & Character'Val (10)
      & "            ""field"": ""f_expr""," & Character'Val (10)
      & "            ""__TODO__"": [" & Character'Val (10)
      & "              ""All matchers below are wrapped in a group in order to""," & Character'Val (10)
      & "              ""workaround #838""" & Character'Val (10)
      & "            ]," & Character'Val (10)
      & "            ""matchers"": [" & Character'Val (10)
      & "              {" & Character'Val (10)
      & "                ""kind"": [""BlockExpr"", ""ParenExpr"", ""MatchExpr""]," & Character'Val (10)
      & "                ""document"": [" & Character'Val (10)
      & "                  ""whitespace""," & Character'Val (10)
      & "                  {""kind"": ""recurse_field"", ""field"": ""f_expr""}" & Character'Val (10)
      & "                 ]" & Character'Val (10)
      & "              }" & Character'Val (10)
      & "            ]," & Character'Val (10)
      & "            ""default"": {" & Character'Val (10)
      & "              ""kind"": ""indent""," & Character'Val (10)
      & "              ""contents"": [" & Character'Val (10)
      & "                ""line""," & Character'Val (10)
      & "                {""kind"": ""recurse_field"", ""field"": ""f_expr""}" & Character'Val (10)
      & "              ]" & Character'Val (10)
      & "            }" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }," & Character'Val (10)
      & "      ""fields"": {" & Character'Val (10)
      & "         ""f_decl_type"": [" & Character'Val (10)
      & "           {""kind"": ""text"", ""text"": "":""}," & Character'Val (10)
      & "           ""whitespace""," & Character'Val (10)
      & "           ""recurse""" & Character'Val (10)
      & "         ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }," & Character'Val (10)
      & "    ""VarBind"": {" & Character'Val (10)
      & "      ""node"": {" & Character'Val (10)
      & "        ""kind"": ""group""," & Character'Val (10)
      & "        ""document"": [" & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": ""bind""}," & Character'Val (10)
      & "          {""kind"": ""recurse_field"", ""field"": ""f_name""}," & Character'Val (10)
      & "          ""whitespace""," & Character'Val (10)
      & "          {""kind"": ""text"", ""text"": ""=""}," & Character'Val (10)
      & "          {" & Character'Val (10)
      & "            ""kind"": ""indent""," & Character'Val (10)
      & "            ""contents"": [" & Character'Val (10)
      & "              ""line""," & Character'Val (10)
      & "              {""kind"": ""recurse_field"", ""field"": ""f_expr""}" & Character'Val (10)
      & "            ]" & Character'Val (10)
      & "          }" & Character'Val (10)
      & "        ]" & Character'Val (10)
      & "      }" & Character'Val (10)
      & "    }" & Character'Val (10)
      & "  }" & Character'Val (10)
      & "}" & Character'Val (10)
;

   Unparsers : aliased constant Unparsers_Impl :=
     (Token_Spacings'Access,
      Token_Newlines'Access,
      Node_Unparsers'Access,
      Default_Config'Access);

end Liblktlang.Unparsers;
