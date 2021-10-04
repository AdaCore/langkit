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

with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;

--  This package provides common implementation details for Langkit-generated
--  libraries. Even though it is not private (to allow Langkit-generated
--  libraries to use it), it is not meant to be used beyond this. As such, this
--  API is considered unsafe and unstable.

package Langkit_Support.Internal.Introspection is

   ----------------------------
   -- Value type descriptors --
   ----------------------------

   type Value_Type_Descriptor is record
      Debug_Name : Debug_String_Access;
      --  Free-form name of this type for debug purposes
   end record;

   type Value_Type_Descriptor_Access is
      not null access constant Value_Type_Descriptor;
   type Value_Type_Descriptor_Array is
      array (Value_Type range <>) of Value_Type_Descriptor_Access;
   type Value_Type_Descriptor_Array_Access is
      not null access constant Value_Type_Descriptor_Array;

   ---------------------------
   -- Enum type descriptors --
   ---------------------------

   type Enum_Value_Names is array (Enum_Value_Index range <>) of Text_Access;
   --  Mapping from indexes of enum values to the names (camel-with-undercores)
   --  of each enum value.

   type Enum_Type_Descriptor (Last_Value : Enum_Value_Index) is record
      Name : Text_Access;
      --  Name for this enumeration type in camel-with-underscores convention

      Default_Value : Any_Enum_Value_Index;
      --  If this enum type has a default value, this contains its index. Zero
      --  otherwise.

      Value_Names : Enum_Value_Names (1 .. Last_Value);
      --  Names for each available value for this enum type
   end record;

   type Enum_Type_Descriptor_Access is
     not null access constant Enum_Type_Descriptor;
   type Enum_Type_Descriptor_Array is
     array (Value_Type range <>) of Enum_Type_Descriptor_Access;
   type Enum_Type_Descriptor_Array_Access is
     not null access constant Enum_Type_Descriptor_Array;

   ----------------------------
   -- Array type descriptors --
   ----------------------------

   type Array_Type_Descriptor is record
      Element_Type : Value_Type;
      --  Type of elements in this array type
   end record;

   type Array_Type_Descriptor_Array is
     array (Value_Type range <>) of Array_Type_Descriptor;
   type Array_Type_Descriptor_Array_Access is
     not null access constant Array_Type_Descriptor_Array;

   ---------------------------
   -- Node type descriptors --
   ---------------------------

   type Node_Type_Descriptor (Derivations_Count : Natural) is record
      Base_Type : Any_Value_Type;
      --  Reference to the node type from which this derives

      Is_Abstract : Boolean;
      --  Whether this node type is abstract

      Name : Text_Access;
      --  Name for this type in camel-with-undercores convention

      Derivations : Value_Type_Array (1 .. Derivations_Count);
      --  Sorted list (by index) of node types for all node types that directly
      --  derives from this node.
   end record;

   type Node_Type_Descriptor_Access is
     not null access constant Node_Type_Descriptor;
   type Node_Type_Descriptor_Array is
     array (Value_Type range <>) of Node_Type_Descriptor_Access;
   type Node_Type_Descriptor_Array_Access is
     not null access constant Node_Type_Descriptor_Array;

end Langkit_Support.Internal.Introspection;
