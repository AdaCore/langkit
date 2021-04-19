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

with System;

with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;
with Langkit_Support.Generic_API;  use Langkit_Support.Generic_API;
with Langkit_Support.Lexical_Envs; use Langkit_Support.Lexical_Envs;
with Langkit_Support.Text;         use Langkit_Support.Text;
with Langkit_Support.Types;        use Langkit_Support.Types;

--  This package and its children provide common implementation details for
--  Langkit-generated libraries. Even though it is not private (to allow
--  Langkit-generated libraries to use it), it is not meant to be used beyond
--  this. As such, this API is considered unsafe and unstable.

package Langkit_Support.Internal is

   --  Unique identifier for Langkit-generated libraries and dispatch table for
   --  all generic operations.

   type Language_Descriptor;
   type Language_Descriptor_Access is access constant Language_Descriptor;
   type String_Access is not null access constant String;

   --  Descriptors for grammar rules

   type Grammar_Rule_Name_Array is
     array (Grammar_Rule_Index range <>) of String_Access;
   type Grammar_Rule_Name_Array_Access is
     not null access constant Grammar_Rule_Name_Array;

   --  Bare pointers to library-specific resources. For contexts, units and
   --  nodes, these correspond to the access types defined in $.Implementation.

   type Internal_Context is new System.Address;
   type Internal_Unit is new System.Address;
   type Internal_Node is new System.Address;

   No_Internal_Context : constant Internal_Context :=
     Internal_Context (System.Null_Address);
   No_Internal_Unit    : constant Internal_Unit :=
     Internal_Unit (System.Null_Address);
   No_Internal_Node    : constant Internal_Node :=
     Internal_Node (System.Null_Address);

   type Internal_Node_Metadata is new System.Address;
   --  The contents and size of the node metadata record is different from one
   --  Langkit-generated library to another, so this generic API needs to refer
   --  to it by reference, with ref-counting for lifetime handling. Null
   --  addresses mean "default metadata", and the language descriptor table
   --  provides ref-counting primitives.

   No_Internal_Node_Metadata : constant Internal_Node_Metadata :=
     Internal_Node_Metadata (System.Null_Address);

   --  As everywhere else, entities are made up of bare nodes and entity
   --  information, with regular types from Langkit_Support.Lexical_Envs. The
   --  metadata has a special representation: see above (Internal_Node_Metadata
   --  type).

   type Internal_Entity is record
      Node         : Internal_Node;
      Rebindings   : Env_Rebindings;
      From_Rebound : Boolean;
      Metadata     : Internal_Node_Metadata;
   end record;

   No_Internal_Entity : constant Internal_Entity :=
     (No_Internal_Node, null, False, No_Internal_Node_Metadata);

   --  Safety nets keep track of information at "public reference value"
   --  creation so that later use can check whether the reference is still
   --  valid (used to ensure memory safety).

   type Node_Safety_Net is record
      Context         : Internal_Context;
      Context_Version : Version_Number;
      --  Analysis context and version number at the time this safety net was
      --  produced.

      Unit         : Internal_Unit;
      Unit_Version : Version_Number;
      --  Analysis unit and unit version at the time this safety net was
      --  produced.

      Rebindings_Version : Version_Number;
      --  Version of the associated rebinding at the time this safety net was
      --  procuded.
   end record;
   No_Node_Safety_Net : constant Node_Safety_Net :=
     (No_Internal_Context, 0, No_Internal_Unit, 0, 0);

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
   type Unit_Root_Type is access function
     (Unit : Internal_Unit) return Internal_Node;
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

   type Entity_Image_Type is access function
     (Entity : Internal_Entity) return String;

   type Language_Descriptor is limited record
      Language_Name : String_Access;
      --  Name of the language that is analyzed (in camel-with-underscores
      --  casing).

      --  Descriptors for grammar rules. The table for names also defines the
      --  range of supported rules for this language.

      Default_Grammar_Rule : Grammar_Rule_Index;
      Grammar_Rule_Names   : Grammar_Rule_Name_Array_Access;

      --  Implementation for generic operations

      Create_Context        : Create_Context_Type;
      Context_Inc_Ref       : Context_Inc_Ref_Type;
      Context_Dec_Ref       : Context_Dec_Ref_Type;
      Context_Version       : Context_Version_Type;
      Context_Get_From_File : Context_Get_From_File_Type;
      Context_Has_Unit      : Context_Has_Unit_Type;

      Unit_Version  : Unit_Version_Type;
      Unit_Root     : Unit_Root_Type;
      Unit_Get_Line : Unit_Get_Line_Type;

      Node_Metadata_Inc_Ref : Node_Metadata_Inc_Ref_Type;
      Node_Metadata_Dec_Ref : Node_Metadata_Dec_Ref_Type;

      Node_Parent         : Node_Parent_Type;
      Node_Children_Count : Node_Children_Count_Type;
      Node_Get_Child      : Node_Get_Child_Type;
      Node_Fetch_Sibling  : Node_Fetch_Sibling_Type;

      Entity_Image : Entity_Image_Type;
   end record;

end Langkit_Support.Internal;
