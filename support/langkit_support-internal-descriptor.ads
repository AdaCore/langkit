------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                         --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with Langkit_Support.File_Readers;      use Langkit_Support.File_Readers;
with Langkit_Support.Internal.Analysis; use Langkit_Support.Internal.Analysis;
with Langkit_Support.Internal.Introspection;
use Langkit_Support.Internal.Introspection;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
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

   type Unit_Version_Type is access function
     (Unit : Internal_Unit) return Version_Number;
   type Unit_Filename_Type is access function
     (Unit : Internal_Unit) return String;
   type Unit_Root_Type is access function
     (Unit : Internal_Unit) return Internal_Node;
   type Unit_Token_Getter_Type is access function
     (Unit : Internal_Unit) return Internal_Token;
   type Unit_Get_Line_Type is access function
     (Unit : Internal_Unit; Line_Number : Positive) return Text_Type;

   type Node_Metadata_Inc_Ref_Type is access procedure
     (Metadata : Internal_Node_Metadata);
   type Node_Metadata_Dec_Ref_Type is access procedure
     (Metadata : in out Internal_Node_Metadata);

   type Node_Parent_Type is access function
     (Node : Internal_Node) return Internal_Node;
   type Node_Children_Count_Type is access function
     (Node : Internal_Node) return Natural;
   type Node_Get_Child_Type is access procedure
     (Node            : Internal_Node;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Internal_Node);
   type Node_Fetch_Sibling_Type is access function
     (Node : Internal_Node; Offset : Integer) return Internal_Node;

   type Node_Token_Getter_Type is access function
     (Node : Internal_Node) return Internal_Token;

   type Entity_Image_Type is access function
     (Entity : Internal_Entity) return String;

   type Language_Descriptor is limited record
      Language_Name : Text_Access;
      --  Name of the language that is analyzed (in camel-with-underscores
      --  casing).

      --  Descriptors for grammar rules. The table for names also defines the
      --  range of supported rules for this language.

      Default_Grammar_Rule : Grammar_Rule_Index;
      Grammar_Rule_Names   : Grammar_Rule_Name_Array_Access;

      --  Descriptors for token kinds. The table for names also defines the
      --  range of supported kinds for this language.

      Token_Kind_Names : Token_Kind_Name_Array_Access;

      --  Descriptors for introspection capabilities

      Types        : Type_Descriptor_Array_Access;
      Enum_Types   : Enum_Type_Descriptor_Array_Access;
      Array_Types  : Array_Type_Descriptor_Array_Access;
      Struct_Types : Struct_Type_Descriptor_Array_Access;

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

      Unit_Version     : Unit_Version_Type;
      Unit_Filename    : Unit_Filename_Type;
      Unit_Root        : Unit_Root_Type;
      Unit_First_Token : Unit_Token_Getter_Type;
      Unit_Last_Token  : Unit_Token_Getter_Type;
      Unit_Get_Line    : Unit_Get_Line_Type;

      Node_Metadata_Inc_Ref : Node_Metadata_Inc_Ref_Type;
      Node_Metadata_Dec_Ref : Node_Metadata_Dec_Ref_Type;

      Node_Parent         : Node_Parent_Type;
      Node_Children_Count : Node_Children_Count_Type;
      Node_Get_Child      : Node_Get_Child_Type;
      Node_Fetch_Sibling  : Node_Fetch_Sibling_Type;
      Node_Token_Start    : Node_Token_Getter_Type;
      Node_Token_End      : Node_Token_Getter_Type;

      Entity_Image : Entity_Image_Type;
   end record;

end Langkit_Support.Internal.Descriptor;
