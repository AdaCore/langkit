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

with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;

with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Internal.Analysis; use Langkit_Support.Internal.Analysis;
with Langkit_Support.Slocs;             use Langkit_Support.Slocs;

--  This package provides common implementation details for Langkit-generated
--  libraries. Even though it is not private (to allow Langkit-generated
--  libraries to use it), it is not meant to be used beyond this. As such, this
--  API is considered unsafe and unstable.

package Langkit_Support.Internal.Introspection is

   type Type_Index_Array is array (Positive range <>) of Type_Index;
   type Struct_Member_Index_Array is
     array (Positive range <>) of Struct_Member_Index;

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
     array (Struct_Member_Index range <>) of Struct_Member_Descriptor_Access;
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

      Members : Struct_Member_Index_Array (1 .. Member_Count);
      --  List of members for this struct ,excluding inherited members
   end record;

   type Struct_Type_Descriptor_Access is
     not null access constant Struct_Type_Descriptor;
   type Struct_Type_Descriptor_Array is
     array (Type_Index range <>) of Struct_Type_Descriptor_Access;
   type Struct_Type_Descriptor_Array_Access is
     not null access constant Struct_Type_Descriptor_Array;

   -----------------------------------------------
   -- Interface to represent polymorphic values --
   -----------------------------------------------

   type Internal_Value is abstract tagged limited record
      Ref_Count : Natural;
      --  Reference count for this record. When it drops to zero, the
      --  ``Destroy`` primitive must be called.

      Id : Language_Id;
      --  Language for this value
   end record;

   procedure Destroy (Value : in out Internal_Value) is null;
   --  Free resources allocated for this value. Derivations can omit the
   --  overriding if there is nothing to free manually.

   function Type_Of (Value : Internal_Value) return Type_Index is abstract;
   --  Return the type of the ``Value`` polymorphic value

   function Type_Matches
     (Value : Internal_Value; T : Type_Index) return Boolean;
   --  Return whether ``Value`` is a valid value to be passed as a ``T``
   --  argument.
   --
   --  By default, we check that ``Type_Of (Value) = T`, but derivations can
   --  override this behavior, which use useful for instance for nodes
   --  subtyping.

   function Image (Value : Internal_Value) return String is abstract;
   --  Return a string that represents ``Value``, for logging/debugging
   --  purposes.

   type Internal_Value_Access is access all Internal_Value'Class;

   --------------------------------------------
   -- Implementations for some builtin types --
   --------------------------------------------

   --  Note: we provide here implementations for some builtin types only
   --  (boolean, integer, string, ...) so that we can provide converters
   --  between native Ada types and ``Value_Ref`` directly in Langkit_Support.
   --  We intentionally do not map all builtin types, for instance some enums
   --  (AnalysisUnitKind) as they exist at the Ada level only in generated
   --  libraries.

   type Builtin_Types_Record is record
      Analysis_Unit         : Type_Index;
      Big_Int               : Type_Index;
      Bool                  : Type_Index;
      Char                  : Type_Index;
      Int                   : Type_Index;
      Source_Location_Range : Type_Index;
      String                : Type_Index;
      Token                 : Type_Index;
      Symbol                : Type_Index;
   end record;
   --  Type indexes of builtin types, for which ``Langkit_Support`` provides
   --  common polymorphic values implementation code (see
   --  ``Langkit_Support.Internal.Introspection.Internal_Value``).

   type Builtin_Types_Access is not null access constant Builtin_Types_Record;

   -------------------
   -- Analysis unit --
   -------------------

   type Internal_Analysis_Unit is new Internal_Value with record
      Value : Lk_Unit;
   end record;
   type Internal_Analysis_Unit_Access is access all Internal_Analysis_Unit;

   overriding function Type_Of
     (Value : Internal_Analysis_Unit) return Type_Index;
   overriding function Image (Value : Internal_Analysis_Unit) return String;

   -----------------
   -- Big integer --
   -----------------

   type Internal_Big_Int is new Internal_Value with record
      Value : Big_Integer;
   end record;
   type Internal_Big_Int_Access is access all Internal_Big_Int;

   overriding function Type_Of (Value : Internal_Big_Int) return Type_Index;
   overriding function Image (Value : Internal_Big_Int) return String;

   -------------
   -- Boolean --
   -------------

   type Internal_Bool is new Internal_Value with record
      Value : Boolean;
   end record;
   type Internal_Bool_Access is access all Internal_Bool;

   overriding function Type_Of (Value : Internal_Bool) return Type_Index;
   overriding function Image (Value : Internal_Bool) return String;

   ----------
   -- Char --
   ----------

   type Internal_Char is new Internal_Value with record
      Value : Character_Type;
   end record;
   type Internal_Char_Access is access all Internal_Char;

   overriding function Type_Of (Value : Internal_Char) return Type_Index;
   overriding function Image (Value : Internal_Char) return String;

   ---------
   -- Int --
   ---------

   type Internal_Int is new Internal_Value with record
      Value : Integer;
   end record;
   type Internal_Int_Access is access all Internal_Int;

   overriding function Type_Of (Value : Internal_Int) return Type_Index;
   overriding function Image (Value : Internal_Int) return String;

   ---------------------------
   -- Source_Location_Range --
   ---------------------------

   type Internal_Source_Location_Range is new Internal_Value with record
      Value : Source_Location_Range;
   end record;
   type Internal_Source_Location_Range_Access is
     access all Internal_Source_Location_Range;

   overriding function Type_Of
     (Value : Internal_Source_Location_Range) return Type_Index;
   overriding function Image
     (Value : Internal_Source_Location_Range) return String;

   ------------
   -- String --
   ------------

   type Internal_String is new Internal_Value with record
      Value : Unbounded_Text_Type;
   end record;
   type Internal_String_Access is access all Internal_String;

   overriding function Type_Of (Value : Internal_String) return Type_Index;
   overriding function Image (Value : Internal_String) return String;

   -----------
   -- Token --
   -----------

   type Internal_Token is new Internal_Value with record
      Value : Lk_Token;
   end record;
   type Internal_Token_Access is access all Internal_Token;

   overriding function Type_Of (Value : Internal_Token) return Type_Index;
   overriding function Image (Value : Internal_Token) return String;

   ------------
   -- Symbol --
   ------------

   type Internal_Symbol is new Internal_Value with record
      Value : Unbounded_Text_Type;
   end record;
   type Internal_Symbol_Access is access all Internal_Symbol;

   overriding function Type_Of (Value : Internal_Symbol) return Type_Index;
   overriding function Image (Value : Internal_Symbol) return String;

   -----------
   -- Nodes --
   -----------

   type Internal_Node is new Internal_Value with record
      Value : Lk_Node;
   end record;
   type Internal_Node_Access is access all Internal_Node;

   overriding function Type_Of (Value : Internal_Node) return Type_Index;
   overriding function Type_Matches
     (Value : Internal_Node; T : Type_Index) return Boolean;
   overriding function Image (Value : Internal_Node) return String;

   function Unwrap_Node (Node : Lk_Node) return Internal_Entity
     with Import, External_Name => "lksp__unwrap_node";
   --  See the corresponding export declaration in
   --  Langkit_Support.Generic_API.Analysis.

   ------------------------------------------------------
   -- Abstract derivations for language-specific types --
   ------------------------------------------------------

   type Base_Internal_Enum_Value is abstract new Internal_Value
     with null record;

   type Base_Internal_Enum_Value_Access is
     access all Base_Internal_Enum_Value'Class;

   function Value_Index
     (Value : Base_Internal_Enum_Value) return Enum_Value_Index is abstract;
   --  Return the index of the given enum value

end Langkit_Support.Internal.Introspection;
