--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Conversion;

with Langkit_Support.File_Readers;      use Langkit_Support.File_Readers;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Internal.Analysis; use Langkit_Support.Internal.Analysis;
with Langkit_Support.Internal.Introspection;
use Langkit_Support.Internal.Introspection;
with Langkit_Support.Slocs;             use Langkit_Support.Slocs;
with Langkit_Support.Types;             use Langkit_Support.Types;

--  This package provides common implementation details for Langkit-generated
--  libraries. Even though it is not private (to allow Langkit-generated
--  libraries to use it), it is not meant to be used beyond this. As such, this
--  API is considered unsafe and unstable.

package Langkit_Support.Internal.Descriptor is

   type Language_Descriptor;
   type Language_Descriptor_Access is access constant Language_Descriptor;
   --  Unique identifier for Langkit-generated libraries and dispatch table for
   --  all generic operations.

   --  Access types for the implementation of generic operations. Most
   --  operations are direct implementation for the API defined in
   --  Langkit_Support.Generic_API.* packages. Ref-counting operations are
   --  trivial to use, but all expect non-null arguments. All operations expect
   --  safe arguments (no stale reference) and non-null ones.

   type Create_Context_Type is access function
     (Charset     : String := "";
      File_Reader : File_Reader_Reference := No_File_Reader_Reference;
      With_Trivia : Boolean := True;
      Tab_Stop    : Natural := 0)
      return Internal_Context;
   type Context_Inc_Ref_Type is access procedure (Context : Internal_Context);
   type Context_Dec_Ref_Type is access procedure
     (Context : in out Internal_Context);
   type Context_Version_Type is access function
     (Context : Internal_Context) return Version_Number;
   type Context_Has_Unit_Type is access function
     (Context : Internal_Context; Unit_Filename : String) return Boolean;
   type Context_Get_From_File_Type is access function
     (Context           : Internal_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Rule              : Grammar_Rule_Index) return Internal_Unit;

   type Unit_Context_Type is access function
     (Unit : Internal_Unit) return Internal_Context;
   type Unit_Version_Type is access function
     (Unit : Internal_Unit) return Version_Number;
   type Unit_Filename_Type is access function
     (Unit : Internal_Unit) return String;
   type Unit_Root_Type is access function
     (Unit : Internal_Unit) return Analysis.Internal_Node;
   type Unit_Token_Getter_Type is access function
     (Unit : Internal_Unit) return Analysis.Internal_Token;
   type Unit_Get_Line_Type is access function
     (Unit : Internal_Unit; Line_Number : Positive) return Text_Type;

   type Node_Metadata_Inc_Ref_Type is access procedure
     (Metadata : Internal_Node_Metadata);
   type Node_Metadata_Dec_Ref_Type is access procedure
     (Metadata : in out Internal_Node_Metadata);
   type Node_Metadata_Compare_Type is access function
     (L, R : Analysis.Internal_Node_Metadata) return Boolean;

   type Node_Unit_Type is access function
     (Node : Analysis.Internal_Node) return Internal_Unit;
   type Node_Kind_Type is access function
     (Node : Analysis.Internal_Node) return Type_Index;
   type Node_Parent_Type is access function
     (Node : Analysis.Internal_Entity) return Analysis.Internal_Entity;
   type Node_Parents_Type is access function
     (Node      : Analysis.Internal_Entity;
      With_Self : Boolean) return Analysis.Internal_Entity_Array;
   type Node_Children_Count_Type is access function
     (Node : Analysis.Internal_Node) return Natural;
   type Node_Get_Child_Type is access procedure
     (Node            : Analysis.Internal_Node;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Analysis.Internal_Node);
   type Node_Fetch_Sibling_Type is access function
     (Node   : Analysis.Internal_Node;
      Offset : Integer) return Analysis.Internal_Node;
   type Node_Is_Ghost_Type is access function
     (Node : Analysis.Internal_Node) return Boolean;
   type Node_Token_Getter_Type is access function
     (Node : Analysis.Internal_Node) return Analysis.Internal_Token;
   type Node_Text_Type is access function
     (Node : Analysis.Internal_Node) return Text_Type;
   type Node_Sloc_Range_Type is access function
     (Node : Analysis.Internal_Node) return Source_Location_Range;
   type Node_Last_Attempted_Child_Type is access function
     (Node : Analysis.Internal_Node) return Integer;

   type Entity_Image_Type is access function
     (Entity : Internal_Entity) return String;

   type Token_Is_Equivalent_Type is access function
     (Left, Right       : Internal_Token;
      Left_SN, Right_SN : Token_Safety_Net) return Boolean;

   type Create_Enum_Type is access function
     (Enum_Type   : Type_Index;
      Value_Index : Enum_Value_Index) return Internal_Value_Access;
   type Create_Array_Type is access function
     (Array_Type : Type_Index;
      Values     : Internal_Value_Array) return Internal_Value_Access;
   type Create_Struct_Type is access function
     (Struct_Type : Type_Index;
      Values      : Internal_Value_Array) return Internal_Value_Access;
   type Eval_Node_Member_Type is access function
     (Node      : Internal_Acc_Node;
      Member    : Struct_Member_Index;
      Arguments : Internal_Value_Array) return Internal_Value_Access;

   type Language_Descriptor is limited record
      Language_Name : Text_Access;
      --  Name of the language that is analyzed (in camel-with-underscores
      --  casing).

      --  Descriptors for grammar rules. The table also defines the range of
      --  supported rules for this language.

      Default_Grammar_Rule : Grammar_Rule_Index;
      Grammar_Rules        : Grammar_Rule_Descriptor_Array_Access;

      --  Descriptors for token kinds. The table for names also defines the
      --  range of supported kinds for this language.

      Token_Kind_Names : Token_Kind_Name_Array_Access;

      --  Descriptors for introspection capabilities

      Types          : Type_Descriptor_Array_Access;
      Enum_Types     : Enum_Type_Descriptor_Array_Access;
      Array_Types    : Array_Type_Descriptor_Array_Access;
      Iterator_Types : Iterator_Type_Descriptor_Array_Access;
      Struct_Types   : Struct_Type_Descriptor_Array_Access;
      Builtin_Types  : Builtin_Types_Access;

      First_Node : Type_Index;
      --  Index of the first node descriptor in ``Struct_Types``. In
      --  ``Struct_Types``, descriptors from 1 to ``First_Node - 1`` are struct
      --  types (not nodes), and descriptors from ``First_Node`` to
      --  ``Struct_Types'Last`` are nodes.

      Struct_Members : Struct_Member_Descriptor_Array_Access;
      --  Descriptors for struct members: fields and properties. In
      --  ``Struct_Members``, descriptors from 1 to ``First_Property - 1`` are
      --  fields, and descriptors from ``First_Property`` to
      --  ``Struct_Members'Last`` are properties.

      First_Property : Struct_Member_Index;
      --  Index of the first property descriptor in ``Struct_Members``

      --  Implementation for generic operations

      Create_Context        : Create_Context_Type;
      Context_Inc_Ref       : Context_Inc_Ref_Type;
      Context_Dec_Ref       : Context_Dec_Ref_Type;
      Context_Version       : Context_Version_Type;
      Context_Get_From_File : Context_Get_From_File_Type;
      Context_Has_Unit      : Context_Has_Unit_Type;

      Unit_Context     : Unit_Context_Type;
      Unit_Version     : Unit_Version_Type;
      Unit_Filename    : Unit_Filename_Type;
      Unit_Root        : Unit_Root_Type;
      Unit_First_Token : Unit_Token_Getter_Type;
      Unit_Last_Token  : Unit_Token_Getter_Type;
      Unit_Get_Line    : Unit_Get_Line_Type;

      Node_Metadata_Inc_Ref : Node_Metadata_Inc_Ref_Type;
      Node_Metadata_Dec_Ref : Node_Metadata_Dec_Ref_Type;
      Node_Metadata_Compare : Node_Metadata_Compare_Type;
      Null_Metadata         : Internal_Node_Metadata;

      Node_Unit                 : Node_Unit_Type;
      Node_Kind                 : Node_Kind_Type;
      Node_Parent               : Node_Parent_Type;
      Node_Parents              : Node_Parents_Type;
      Node_Children_Count       : Node_Children_Count_Type;
      Node_Get_Child            : Node_Get_Child_Type;
      Node_Fetch_Sibling        : Node_Fetch_Sibling_Type;
      Node_Is_Ghost             : Node_Is_Ghost_Type;
      Node_Token_Start          : Node_Token_Getter_Type;
      Node_Token_End            : Node_Token_Getter_Type;
      Node_Text                 : Node_Text_Type;
      Node_Sloc_Range           : Node_Sloc_Range_Type;
      Node_Last_Attempted_Child : Node_Last_Attempted_Child_Type;

      Entity_Image : Entity_Image_Type;

      Token_Is_Equivalent : Token_Is_Equivalent_Type;

      --  Operations to build/inspect generic data types

      Create_Enum      : Create_Enum_Type;
      Create_Array     : Create_Array_Type;
      Create_Struct    : Create_Struct_Type;
      Eval_Node_Member : Eval_Node_Member_Type;
   end record;

   function "+" is new Ada.Unchecked_Conversion
     (Any_Language_Id, Language_Descriptor_Access);

end Langkit_Support.Internal.Descriptor;
