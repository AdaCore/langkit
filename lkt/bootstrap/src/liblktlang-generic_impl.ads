
with Ada.Unchecked_Conversion;

with Liblktlang_Support.Diagnostics;       use Liblktlang_Support.Diagnostics;
with Liblktlang_Support.File_Readers;      use Liblktlang_Support.File_Readers;
with Liblktlang_Support.Generic_API;       use Liblktlang_Support.Generic_API;
with Liblktlang_Support.Generic_API.Introspection;
use Liblktlang_Support.Generic_API.Introspection;
with Liblktlang_Support.Internal;          use Liblktlang_Support.Internal;
with Liblktlang_Support.Internal.Analysis; use Liblktlang_Support.Internal.Analysis;
with Liblktlang_Support.Internal.Descriptor;
use Liblktlang_Support.Internal.Descriptor;
with Liblktlang_Support.Slocs;             use Liblktlang_Support.Slocs;
with Liblktlang_Support.Text;              use Liblktlang_Support.Text;
with Liblktlang_Support.Types;             use Liblktlang_Support.Types;

with Liblktlang.Implementation;
with Liblktlang.Generic_Introspection;
use Liblktlang.Generic_Introspection;
with Liblktlang.Unparsers;
with Liblktlang.Private_Converters; use Liblktlang.Private_Converters;

with Liblktlang.Common;

--  This package provide Liblktlang-specific implementations for the
--  generic operations defined in Liblktlang_Support.Internal.Descriptor.

private package Liblktlang.Generic_Impl is

   

   function "+" is new Ada.Unchecked_Conversion
     (Internal_Context, Implementation.Internal_Context);
   function "+" is new Ada.Unchecked_Conversion
     (Implementation.Internal_Context, Internal_Context);

   function "+" is new Ada.Unchecked_Conversion
     (Internal_Unit, Implementation.Internal_Unit);
   function "+" is new Ada.Unchecked_Conversion
     (Implementation.Internal_Unit, Internal_Unit);

   function "+" is new Ada.Unchecked_Conversion
     (Internal_Node, Implementation.Bare_Lkt_Node);
   function "+" is new Ada.Unchecked_Conversion
     (Implementation.Bare_Lkt_Node, Internal_Node);

   function "+"
     (Entity : Internal_Entity) return Implementation.Internal_Entity
     with Export,
          External_Name => "Liblktlang__from_generic_internal_entity";
   function "+"
     (Entity : Implementation.Internal_Entity) return Internal_Entity
     with Export,
          External_Name => "Liblktlang__to_generic_internal_entity";

   function "+" (Rule : Grammar_Rule_Index) return Common.Grammar_Rule
   is (Common.Grammar_Rule'Val (Rule - 1));
   --  Grammar rules start at 1 in the generic API: rebase the value before
   --  converting it to the native type.

   function "+" (Token : Common.Token_Reference) return Internal_Token
   is ((Get_Token_TDH (Token), Get_Token_Index (Token)));

   function Wrap_Token
     (Context : Internal_Context;
      Token   : Internal_Token) return Common.Token_Reference
   is (Wrap_Token_Reference (+Context, Token.TDH, Token.Index));

   --  Descriptors for token kinds

   
      
      Token_Kind_Name_1 : aliased constant Text_Type :=
        "Amp";
      
      Token_Kind_Name_2 : aliased constant Text_Type :=
        "And_Kw";
      
      Token_Kind_Name_3 : aliased constant Text_Type :=
        "At";
      
      Token_Kind_Name_4 : aliased constant Text_Type :=
        "Big_Number";
      
      Token_Kind_Name_5 : aliased constant Text_Type :=
        "Bind_Kw";
      
      Token_Kind_Name_6 : aliased constant Text_Type :=
        "Block_String_Line";
      
      Token_Kind_Name_7 : aliased constant Text_Type :=
        "Case_Kw";
      
      Token_Kind_Name_8 : aliased constant Text_Type :=
        "Char";
      
      Token_Kind_Name_9 : aliased constant Text_Type :=
        "Class_Kw";
      
      Token_Kind_Name_10 : aliased constant Text_Type :=
        "Colon";
      
      Token_Kind_Name_11 : aliased constant Text_Type :=
        "Comb";
      
      Token_Kind_Name_12 : aliased constant Text_Type :=
        "Comma";
      
      Token_Kind_Name_13 : aliased constant Text_Type :=
        "Comment";
      
      Token_Kind_Name_14 : aliased constant Text_Type :=
        "Discard_Kw";
      
      Token_Kind_Name_15 : aliased constant Text_Type :=
        "Div";
      
      Token_Kind_Name_16 : aliased constant Text_Type :=
        "Doc_Comment";
      
      Token_Kind_Name_17 : aliased constant Text_Type :=
        "Dot";
      
      Token_Kind_Name_18 : aliased constant Text_Type :=
        "Dyn_Var_Kw";
      
      Token_Kind_Name_19 : aliased constant Text_Type :=
        "E_Q";
      
      Token_Kind_Name_20 : aliased constant Text_Type :=
        "Elif_Kw";
      
      Token_Kind_Name_21 : aliased constant Text_Type :=
        "Else_Kw";
      
      Token_Kind_Name_22 : aliased constant Text_Type :=
        "Enum_Kw";
      
      Token_Kind_Name_23 : aliased constant Text_Type :=
        "Equal";
      
      Token_Kind_Name_24 : aliased constant Text_Type :=
        "Excl_Mark";
      
      Token_Kind_Name_25 : aliased constant Text_Type :=
        "Fat_Right_Arrow";
      
      Token_Kind_Name_26 : aliased constant Text_Type :=
        "Fun_Kw";
      
      Token_Kind_Name_27 : aliased constant Text_Type :=
        "G_T";
      
      Token_Kind_Name_28 : aliased constant Text_Type :=
        "G_T_E";
      
      Token_Kind_Name_29 : aliased constant Text_Type :=
        "Generic_Kw";
      
      Token_Kind_Name_30 : aliased constant Text_Type :=
        "Grammar_Kw";
      
      Token_Kind_Name_31 : aliased constant Text_Type :=
        "Identifier";
      
      Token_Kind_Name_32 : aliased constant Text_Type :=
        "If_Kw";
      
      Token_Kind_Name_33 : aliased constant Text_Type :=
        "Implements_Kw";
      
      Token_Kind_Name_34 : aliased constant Text_Type :=
        "Import_Kw";
      
      Token_Kind_Name_35 : aliased constant Text_Type :=
        "In_Kw";
      
      Token_Kind_Name_36 : aliased constant Text_Type :=
        "Int_Mark";
      
      Token_Kind_Name_37 : aliased constant Text_Type :=
        "Is_Kw";
      
      Token_Kind_Name_38 : aliased constant Text_Type :=
        "L_Brace";
      
      Token_Kind_Name_39 : aliased constant Text_Type :=
        "L_Brack";
      
      Token_Kind_Name_40 : aliased constant Text_Type :=
        "L_Par";
      
      Token_Kind_Name_41 : aliased constant Text_Type :=
        "L_T";
      
      Token_Kind_Name_42 : aliased constant Text_Type :=
        "L_T_E";
      
      Token_Kind_Name_43 : aliased constant Text_Type :=
        "Left_Arrow";
      
      Token_Kind_Name_44 : aliased constant Text_Type :=
        "Lexer_Kw";
      
      Token_Kind_Name_45 : aliased constant Text_Type :=
        "Lexing_Failure";
      
      Token_Kind_Name_46 : aliased constant Text_Type :=
        "Match_Kw";
      
      Token_Kind_Name_47 : aliased constant Text_Type :=
        "Minus";
      
      Token_Kind_Name_48 : aliased constant Text_Type :=
        "N_E";
      
      Token_Kind_Name_49 : aliased constant Text_Type :=
        "Not_Kw";
      
      Token_Kind_Name_50 : aliased constant Text_Type :=
        "Null_Kw";
      
      Token_Kind_Name_51 : aliased constant Text_Type :=
        "Number";
      
      Token_Kind_Name_52 : aliased constant Text_Type :=
        "Or_Kw";
      
      Token_Kind_Name_53 : aliased constant Text_Type :=
        "P_String";
      
      Token_Kind_Name_54 : aliased constant Text_Type :=
        "Percent";
      
      Token_Kind_Name_55 : aliased constant Text_Type :=
        "Pipe";
      
      Token_Kind_Name_56 : aliased constant Text_Type :=
        "Plus";
      
      Token_Kind_Name_57 : aliased constant Text_Type :=
        "Private_Kw";
      
      Token_Kind_Name_58 : aliased constant Text_Type :=
        "Public_Kw";
      
      Token_Kind_Name_59 : aliased constant Text_Type :=
        "R_Brace";
      
      Token_Kind_Name_60 : aliased constant Text_Type :=
        "R_Brack";
      
      Token_Kind_Name_61 : aliased constant Text_Type :=
        "R_Par";
      
      Token_Kind_Name_62 : aliased constant Text_Type :=
        "Raise_Kw";
      
      Token_Kind_Name_63 : aliased constant Text_Type :=
        "Right_Arrow";
      
      Token_Kind_Name_64 : aliased constant Text_Type :=
        "Semicolon";
      
      Token_Kind_Name_65 : aliased constant Text_Type :=
        "String";
      
      Token_Kind_Name_66 : aliased constant Text_Type :=
        "Struct_Kw";
      
      Token_Kind_Name_67 : aliased constant Text_Type :=
        "Termination";
      
      Token_Kind_Name_68 : aliased constant Text_Type :=
        "Then_Kw";
      
      Token_Kind_Name_69 : aliased constant Text_Type :=
        "Times";
      
      Token_Kind_Name_70 : aliased constant Text_Type :=
        "Trait_Kw";
      
      Token_Kind_Name_71 : aliased constant Text_Type :=
        "Try_Kw";
      
      Token_Kind_Name_72 : aliased constant Text_Type :=
        "Two_Sided_Arrow";
      
      Token_Kind_Name_73 : aliased constant Text_Type :=
        "Val_Kw";
      
      Token_Kind_Name_74 : aliased constant Text_Type :=
        "Whitespace";
   Token_Kind_Descriptors : aliased constant Token_Kind_Descriptor_Array := (
      Token_Index_For_Lkt_Amp =>
       (Name       => Token_Kind_Name_1'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_And_Kw =>
       (Name       => Token_Kind_Name_2'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_At =>
       (Name       => Token_Kind_Name_3'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Big_Number =>
       (Name       => Token_Kind_Name_4'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Bind_Kw =>
       (Name       => Token_Kind_Name_5'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Block_String_Line =>
       (Name       => Token_Kind_Name_6'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Case_Kw =>
       (Name       => Token_Kind_Name_7'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Char =>
       (Name       => Token_Kind_Name_8'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Class_Kw =>
       (Name       => Token_Kind_Name_9'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Colon =>
       (Name       => Token_Kind_Name_10'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Comb =>
       (Name       => Token_Kind_Name_11'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Comma =>
       (Name       => Token_Kind_Name_12'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Comment =>
       (Name       => Token_Kind_Name_13'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => True),
      Token_Index_For_Lkt_Discard_Kw =>
       (Name       => Token_Kind_Name_14'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Div =>
       (Name       => Token_Kind_Name_15'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Doc_Comment =>
       (Name       => Token_Kind_Name_16'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Dot =>
       (Name       => Token_Kind_Name_17'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Dyn_Var_Kw =>
       (Name       => Token_Kind_Name_18'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_E_Q =>
       (Name       => Token_Kind_Name_19'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Elif_Kw =>
       (Name       => Token_Kind_Name_20'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Else_Kw =>
       (Name       => Token_Kind_Name_21'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Enum_Kw =>
       (Name       => Token_Kind_Name_22'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Equal =>
       (Name       => Token_Kind_Name_23'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Excl_Mark =>
       (Name       => Token_Kind_Name_24'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Fat_Right_Arrow =>
       (Name       => Token_Kind_Name_25'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Fun_Kw =>
       (Name       => Token_Kind_Name_26'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_G_T =>
       (Name       => Token_Kind_Name_27'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_G_T_E =>
       (Name       => Token_Kind_Name_28'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Generic_Kw =>
       (Name       => Token_Kind_Name_29'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Grammar_Kw =>
       (Name       => Token_Kind_Name_30'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Identifier =>
       (Name       => Token_Kind_Name_31'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_If_Kw =>
       (Name       => Token_Kind_Name_32'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Implements_Kw =>
       (Name       => Token_Kind_Name_33'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Import_Kw =>
       (Name       => Token_Kind_Name_34'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_In_Kw =>
       (Name       => Token_Kind_Name_35'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Int_Mark =>
       (Name       => Token_Kind_Name_36'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Is_Kw =>
       (Name       => Token_Kind_Name_37'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_L_Brace =>
       (Name       => Token_Kind_Name_38'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_L_Brack =>
       (Name       => Token_Kind_Name_39'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_L_Par =>
       (Name       => Token_Kind_Name_40'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_L_T =>
       (Name       => Token_Kind_Name_41'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_L_T_E =>
       (Name       => Token_Kind_Name_42'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Left_Arrow =>
       (Name       => Token_Kind_Name_43'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Lexer_Kw =>
       (Name       => Token_Kind_Name_44'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Lexing_Failure =>
       (Name       => Token_Kind_Name_45'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Match_Kw =>
       (Name       => Token_Kind_Name_46'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Minus =>
       (Name       => Token_Kind_Name_47'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_N_E =>
       (Name       => Token_Kind_Name_48'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Not_Kw =>
       (Name       => Token_Kind_Name_49'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Null_Kw =>
       (Name       => Token_Kind_Name_50'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Number =>
       (Name       => Token_Kind_Name_51'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Or_Kw =>
       (Name       => Token_Kind_Name_52'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_P_String =>
       (Name       => Token_Kind_Name_53'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Percent =>
       (Name       => Token_Kind_Name_54'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Pipe =>
       (Name       => Token_Kind_Name_55'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Plus =>
       (Name       => Token_Kind_Name_56'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Private_Kw =>
       (Name       => Token_Kind_Name_57'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Public_Kw =>
       (Name       => Token_Kind_Name_58'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_R_Brace =>
       (Name       => Token_Kind_Name_59'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_R_Brack =>
       (Name       => Token_Kind_Name_60'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_R_Par =>
       (Name       => Token_Kind_Name_61'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Raise_Kw =>
       (Name       => Token_Kind_Name_62'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Right_Arrow =>
       (Name       => Token_Kind_Name_63'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Semicolon =>
       (Name       => Token_Kind_Name_64'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_String =>
       (Name       => Token_Kind_Name_65'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Struct_Kw =>
       (Name       => Token_Kind_Name_66'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Termination =>
       (Name       => Token_Kind_Name_67'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Then_Kw =>
       (Name       => Token_Kind_Name_68'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Times =>
       (Name       => Token_Kind_Name_69'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Trait_Kw =>
       (Name       => Token_Kind_Name_70'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Try_Kw =>
       (Name       => Token_Kind_Name_71'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Two_Sided_Arrow =>
       (Name       => Token_Kind_Name_72'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False),
      Token_Index_For_Lkt_Val_Kw =>
       (Name       => Token_Kind_Name_73'Access,
        Family     => Token_Index_For_Alphanumericals,
        Is_Comment => False),
      Token_Index_For_Lkt_Whitespace =>
       (Name       => Token_Kind_Name_74'Access,
        Family     => Token_Index_For_Default_Family,
        Is_Comment => False)
   );

   --  Descriptors for token families

   
      
      Token_Family_Name_1 : aliased constant Text_Type := "Alphanumericals";
      
      Token_Family_Name_2 : aliased constant Text_Type := "Default_Family";
   Token_Family_Names : aliased constant Token_Family_Name_Array :=
     (1 => Token_Family_Name_1'Access, 2 => Token_Family_Name_2'Access);

   --  Implementations for generic operations on analysis types

   function Create_Context
     (Charset     : String;
      File_Reader : File_Reader_Reference;
      With_Trivia : Boolean;
      Tab_Stop    : Natural) return Internal_Context;

   procedure Context_Inc_Ref (Context : Internal_Context);
   procedure Context_Dec_Ref (Context : in out Internal_Context);
   function Context_Has_With_Trivia
     (Context : Internal_Context) return Boolean;
   function Context_Has_Unit
     (Context : Internal_Context; Unit_Filename : String) return Boolean;
   function Context_Get_From_File
     (Context           : Internal_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Rule              : Grammar_Rule_Index) return Internal_Unit;
   function Context_Get_From_Buffer
     (Context                   : Internal_Context;
      Filename, Buffer, Charset : String;
      Rule                      : Grammar_Rule_Index) return Internal_Unit;

   function Unit_Context (Unit : Internal_Unit) return Internal_Context;
   function Unit_Version (Unit : Internal_Unit) return Version_Number;
   procedure Unit_Reparse_From_File (Unit : Internal_Unit; Charset : String);
   procedure Unit_Reparse_From_Buffer
     (Unit : Internal_Unit; Buffer : String; Charset : String);
   function Unit_Filename (Unit : Internal_Unit) return String;
   function Unit_Charset (Unit : Internal_Unit) return String;
   function Unit_Diagnostics (Unit : Internal_Unit) return Diagnostics_Access;
   function Unit_Format_GNU_Diagnostic
     (Unit : Internal_Unit; D : Diagnostic) return String;
   function Unit_Root (Unit : Internal_Unit) return Internal_Node;
   function Unit_First_Token (Unit : Internal_Unit) return Internal_Token;
   function Unit_Last_Token (Unit : Internal_Unit) return Internal_Token;
   function Unit_Lookup_Token
     (Unit : Internal_Unit; Sloc : Source_Location) return Internal_Token;
   function Unit_Token_Count (Unit : Internal_Unit) return Natural;
   function Unit_Trivia_Count (Unit : Internal_Unit) return Natural;
   function Unit_Get_Line
     (Unit : Internal_Unit; Line_Number : Positive) return Text_Type;

   type Internal_Node_Metadata_Type is record
      Ref_Count : Natural;
      Internal  : Implementation.Internal_Metadata;
   end record;
   type Internal_Node_Metadata_Access is
      access all Internal_Node_Metadata_Type;

   function "+" is new Ada.Unchecked_Conversion
     (Internal_Node_Metadata, Internal_Node_Metadata_Access);
   function "+" is new Ada.Unchecked_Conversion
     (Internal_Node_Metadata_Access, Internal_Node_Metadata);

   procedure Node_Metadata_Inc_Ref (Metadata : Internal_Node_Metadata);
   procedure Node_Metadata_Dec_Ref (Metadata : in out Internal_Node_Metadata);
   function Node_Metadata_Compare
     (L, R : Internal_Node_Metadata) return Boolean;

   function Node_Unit (Node : Internal_Node) return Internal_Unit;
   function Node_Kind (Node : Internal_Node) return Type_Index;
   function Node_Parent (Node : Internal_Entity) return Internal_Entity;
   function Node_Parents
     (Node : Internal_Entity; With_Self : Boolean) return Internal_Entity_Array;
   function Node_Children_Count (Node : Internal_Node) return Natural;
   procedure Node_Get_Child
     (Node            : Internal_Node;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Internal_Node);
   function Node_Fetch_Sibling
     (Node : Internal_Node; Offset : Integer) return Internal_Node;
   function Node_Is_Ghost (Node : Analysis.Internal_Node) return Boolean;
   function Node_Token_Start (Node : Internal_Node) return Internal_Token;
   function Node_Token_End (Node : Internal_Node) return Internal_Token;
   function Node_Text (Node : Internal_Node) return Text_Type;
   function Node_Sloc_Range
     (Node : Internal_Node) return Source_Location_Range;
   function Node_Lookup
     (Node : Analysis.Internal_Node;
      Sloc : Source_Location) return Analysis.Internal_Node;
   function Node_Last_Attempted_Child (Node : Internal_Node) return Integer;
   function Node_Children_And_Trivia
     (Node : Internal_Node) return Node_Or_Token_Array_Access;

   function Entity_Image (Entity : Internal_Entity) return String;

   function Token_Is_Equivalent
     (Left, Right       : Internal_Token;
      Left_SN, Right_SN : Token_Safety_Net) return Boolean;

   --  Language descriptor table for Liblktlang.
   --
   --  We define it here and export its address to avoid making the
   --  $.Generic_API spec (which is public) depend on other implementation
   --  units, which allows not exporting the many symbols from the private
   --  units when building a shared library (Windows has a small limit for the
   --  number of exported symbols).

   Language_Name : aliased constant Text_Type :=
     "Lkt";

   No_Metadata_Value : aliased Internal_Node_Metadata_Type :=
     (0, Implementation.No_Metadata);
   No_Metadata       : Internal_Node_Metadata_Access :=
     No_Metadata_Value'Access;

   Desc : aliased constant Language_Descriptor :=
     (Language_Name => Language_Name'Access,

      Default_Grammar_Rule => 1,
      Grammar_Rules        => Grammar_Rules'Access,

      Token_Kinds        => Token_Kind_Descriptors'Access,
      Token_Family_Names => Token_Family_Names'Access,

      Types          => Generic_Introspection.Types'Access,
      Enum_Types     => Generic_Introspection.Enum_Types'Access,
      Array_Types    => Generic_Introspection.Array_Types'Access,
      Iterator_Types => Generic_Introspection.Iterator_Types'Access,
      Struct_Types   => Generic_Introspection.Struct_Types'Access,
      Builtin_Types  => Generic_Introspection.Builtin_Types'Access,
      First_Node     => Generic_Introspection.First_Node,
      Struct_Members => Generic_Introspection.Struct_Members'Access,
      First_Property => Generic_Introspection.First_Property,
      Unparsers      => Liblktlang.Unparsers.Unparsers'Access,

      Create_Context          => Create_Context'Access,
      Context_Inc_Ref         => Context_Inc_Ref'Access,
      Context_Dec_Ref         => Context_Dec_Ref'Access,
      Context_Has_With_Trivia => Context_Has_With_Trivia'Access,
      Context_Has_Unit        => Context_Has_Unit'Access,
      Context_Get_From_File   => Context_Get_From_File'Access,
      Context_Get_From_Buffer => Context_Get_From_Buffer'Access,

      Unit_Context               => Unit_Context'Access,
      Unit_Version               => Unit_Version'Access,
      Unit_Reparse_From_File     => Unit_Reparse_From_File'Access,
      Unit_Reparse_From_Buffer   => Unit_Reparse_From_Buffer'Access,
      Unit_Filename              => Unit_Filename'Access,
      Unit_Charset               => Unit_Charset'Access,
      Unit_Diagnostics           => Unit_Diagnostics'Access,
      Unit_Format_GNU_Diagnostic => Unit_Format_GNU_Diagnostic'Access,
      Unit_Root                  => Unit_Root'Access,
      Unit_First_Token           => Unit_First_Token'Access,
      Unit_Last_Token            => Unit_Last_Token'Access,
      Unit_Lookup_Token          => Unit_Lookup_Token'Access,
      Unit_Token_Count           => Unit_Token_Count'Access,
      Unit_Trivia_Count          => Unit_Trivia_Count'Access,
      Unit_Get_Line              => Unit_Get_Line'Access,

      Node_Metadata_Inc_Ref => Node_Metadata_Inc_Ref'Access,
      Node_Metadata_Dec_Ref => Node_Metadata_Dec_Ref'Access,
      Node_Metadata_Compare => Node_Metadata_Compare'Access,
      Null_Metadata         => +No_Metadata,

      Node_Unit                 => Node_Unit'Access,
      Node_Kind                 => Node_Kind'Access,
      Node_Parent               => Node_Parent'Access,
      Node_Parents              => Node_Parents'Access,
      Node_Children_Count       => Node_Children_Count'Access,
      Node_Get_Child            => Node_Get_Child'Access,
      Node_Fetch_Sibling        => Node_Fetch_Sibling'Access,
      Node_Is_Ghost             => Node_Is_Ghost'Access,
      Node_Token_Start          => Node_Token_Start'Access,
      Node_Token_End            => Node_Token_End'Access,
      Node_Text                 => Node_Text'Access,
      Node_Sloc_Range           => Node_Sloc_Range'Access,
      Node_Lookup               => Node_Lookup'Access,
      Node_Last_Attempted_Child => Node_Last_Attempted_Child'Access,
      Node_Children_And_Trivia  => Node_Children_And_Trivia'Access,

      Entity_Image => Entity_Image'Access,

      Token_Is_Equivalent => Token_Is_Equivalent'Access,

      Create_Enum      => Create_Enum'Access,
      Create_Array     => Create_Array'Access,
      Create_Struct    => Create_Struct'Access,
      Eval_Node_Member => Eval_Node_Member'Access,
      Is_Managed_Error => Implementation.Properties_May_Raise'Access);

end Liblktlang.Generic_Impl;
