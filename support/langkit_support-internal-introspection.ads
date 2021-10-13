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

   type Type_Index_Array is array (Positive range <>) of Type_Index;

   ----------------------
   -- Type descriptors --
   ----------------------

   type Type_Descriptor is record
      Debug_Name : Debug_String_Access;
      --  Free-form name of this type for debug purposes
   end record;

   type Type_Descriptor_Access is not null access constant Type_Descriptor;
   type Type_Descriptor_Array is
      array (Type_Index range <>) of Type_Descriptor_Access;
   type Type_Descriptor_Array_Access is
      not null access constant Type_Descriptor_Array;

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
     array (Type_Index range <>) of Enum_Type_Descriptor_Access;
   type Enum_Type_Descriptor_Array_Access is
     not null access constant Enum_Type_Descriptor_Array;

   ----------------------------
   -- Array type descriptors --
   ----------------------------

   type Array_Type_Descriptor is record
      Element_Type : Type_Index;
      --  Type of elements in this array type
   end record;

   type Array_Type_Descriptor_Array is
     array (Type_Index range <>) of Array_Type_Descriptor;
   type Array_Type_Descriptor_Array_Access is
     not null access constant Array_Type_Descriptor_Array;

   -------------------------------
   -- Struct member descriptors --
   -------------------------------

   type Argument_Descriptor is record
      Name : Text_Access;
      --  Name for this property argument in camel-with-underscores convention

      Argument_Type : Type_Index;
      --  Expected type for this property argument
   end record;

   type Argument_Descriptor_Array is
     array (Argument_Index range <>) of Argument_Descriptor;

   type Struct_Member_Descriptor (Last_Argument : Any_Argument_Index) is record
      Name : Text_Access;
      --  Name for this struct member in camel-with-underscores convention

      Member_Type : Type_Index;
      --  Field type (for struct fields or node syntax fields) or return type
      --  (for properties).

      Arguments : Argument_Descriptor_Array (1 .. Last_Argument);
      --  Descriptors for each argument of this property. Empty array for
      --  fields.
   end record;

   type Struct_Member_Descriptor_Access is
     not null access constant Struct_Member_Descriptor;
   type Struct_Member_Descriptor_Array is
     array (Struct_Member range <>) of Struct_Member_Descriptor_Access;
   type Struct_Member_Descriptor_Array_Access is
     not null access constant Struct_Member_Descriptor_Array;

   -----------------------------
   -- Struct type descriptors --
   -----------------------------

   --  Note that this descriptor is common to both actual structs and nodes
   --  (i.e. it is for all base structs). In the case of structs, we consider
   --  that there is no base type, only concrete structs and zero derivation.

   type Struct_Type_Descriptor
     (Derivations_Count : Natural; Member_Count : Natural)
   is record
      Base_Type : Any_Type_Index;
      --  Reference to the struct type from which this derives

      Is_Abstract : Boolean;
      --  Whether this struct type is abstract

      Name : Text_Access;
      --  Name for this type in camel-with-undercores convention

      Inherited_Members : Natural;
      --  Number of inherited members for this struct (``Members`` included)

      Derivations : Type_Index_Array (1 .. Derivations_Count);
      --  Sorted list (by index) of all struct types that directly derives from
      --  this struct.

      Members : Struct_Member_Array (1 .. Member_Count);
      --  List of members for this struct ,excluding inherited members
   end record;

   type Struct_Type_Descriptor_Access is
     not null access constant Struct_Type_Descriptor;
   type Struct_Type_Descriptor_Array is
     array (Type_Index range <>) of Struct_Type_Descriptor_Access;
   type Struct_Type_Descriptor_Array_Access is
     not null access constant Struct_Type_Descriptor_Array;

end Langkit_Support.Internal.Introspection;
