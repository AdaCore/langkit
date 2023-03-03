--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Deallocation;

with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;

with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

--  This package provides common implementation details for Langkit-generated
--  libraries. Even though it is not private (to allow Langkit-generated
--  libraries to use it), it is not meant to be used beyond this. As such, this
--  API is considered unsafe and unstable.

package Langkit_Support.Internal.Introspection is

   type Type_Index_Array is array (Positive range <>) of Type_Index;
   type Struct_Member_Index_Array is
     array (Positive range <>) of Struct_Member_Index;

   ------------------------------
   -- Grammar rule descriptors --
   ------------------------------

   type Grammar_Rule_Descriptor is record
      Name : Text_Access;
      --  Name of this grammar rule

      Is_Public : Boolean;
      --  Whether this grammar rule is public

      Doc : Text_Access;
      --  Documentation for this grammar rule

      Return_Type : Type_Index;
      --  Type for the nodes that this grammar rule creates
   end record;

   type Grammar_Rule_Descriptor_Access is
     not null access constant Grammar_Rule_Descriptor;
   type Grammar_Rule_Descriptor_Array is
     array (Grammar_Rule_Index range <>) of Grammar_Rule_Descriptor_Access;
   type Grammar_Rule_Descriptor_Array_Access is
     not null access constant Grammar_Rule_Descriptor_Array;

   ----------------------
   -- Type descriptors --
   ----------------------

   type Type_Descriptor is record
      Category : Type_Category;
      --  Category for this type

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
   --  Mapping from indexes of enum values to the names
   --  (camel-with-underscores) of each enum value.

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
   -- Iterator type descriptors --
   -------------------------------

   --  For now, iterator types have the same characteristics as array types

   subtype Iterator_Type_Descriptor is Array_Type_Descriptor;
   subtype Iterator_Type_Descriptor_Array is Array_Type_Descriptor_Array;
   subtype Iterator_Type_Descriptor_Array_Access is
     Array_Type_Descriptor_Array_Access;

   -------------------------------
   -- Struct member descriptors --
   -------------------------------

   type Type_Flags is array (Type_Index range <>) of Boolean;
   type Type_Flags_Access is access constant Type_Flags;

   type Syntax_Field_Indexes is array (Type_Index range <>) of Natural;
   type Syntax_Field_Indexes_Access is access constant Syntax_Field_Indexes;

   type Default_Value_Kind is
     (None,
      Boolean_Value,
      Integer_Value,
      Character_Value,
      Enum_Value,
      Null_Node_Value);
   type Default_Value_Descriptor (Kind : Default_Value_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Boolean_Value =>
            Boolean_Value : Boolean;

         when Integer_Value =>
            Integer_Value : Integer;

         when Character_Value =>
            Character_Value : Character_Type;

         when Enum_Value =>
            Enum_Type  : Type_Index;
            Enum_Value : Enum_Value_Index;

         when Null_Node_Value =>
            null;
      end case;
   end record;

   type Argument_Descriptor is record
      Name : Text_Access;
      --  Name for this property argument in camel-with-underscores convention

      Argument_Type : Type_Index;
      --  Expected type for this property argument

      Default_Value : Default_Value_Descriptor;
      --  Default value for this argument
   end record;

   type Argument_Descriptor_Array is
     array (Argument_Index range <>) of Argument_Descriptor;

   type Struct_Member_Descriptor (Last_Argument : Any_Argument_Index) is record
      Name : Text_Access;
      --  Name for this struct member in camel-with-underscores convention

      Owner : Type_Index;
      --  Type that owns this member

      Member_Type : Type_Index;
      --  Field type (for struct fields or node syntax fields) or return type
      --  (for properties).

      Null_For : Type_Flags_Access;
      --  This component is null for all members that are not syntax fields or
      --  for syntax fields that are never defined as "null" for a node.
      --
      --  For others, this component points to an array that maps all node
      --  types that have this member to whether this member is defined as
      --  "null" for that node type.

      Indexes : Syntax_Field_Indexes_Access;
      --  This compoment is null for all members that are not syntax fields.
      --
      --  For others, this component points to an array that maps all node
      --  types that have this member to the 1-based index of this member in
      --  that node, or 0 if the syntax field is null or abstract for this
      --  node.

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

      Is_Token_Node : Boolean;
      --  Whether this is a token node

      Is_List_Node : Boolean;
      --  Whether this is a list node

      Name : Text_Access;
      --  Name for this type in camel-with-underscores convention

      Repr_Name : Text_Access_Or_Null;
      --  "Representation" name (i.e. name used in text dumps) for this type.
      --  Null for all but node types.

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

   function "=" (Left, Right : Internal_Value) return Boolean is abstract;

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
   --  By default, we check that ``Type_Of (Value) = T``, but derivations can
   --  override this behavior, which use useful for instance for nodes
   --  subtyping.

   function Image (Value : Internal_Value) return String is abstract;
   --  Return a string that represents ``Value``, for logging/debugging
   --  purposes.

   type Internal_Value_Access is access all Internal_Value'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Value'Class, Internal_Value_Access);

   type Internal_Value_Array is
     array (Positive range <>) of Internal_Value_Access;

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

   type Internal_Rec_Analysis_Unit is new Internal_Value with record
      Value : Lk_Unit;
   end record;
   type Internal_Acc_Analysis_Unit is access all Internal_Rec_Analysis_Unit;

   overriding function "="
     (Left, Right : Internal_Rec_Analysis_Unit) return Boolean;
   overriding function Type_Of
     (Value : Internal_Rec_Analysis_Unit) return Type_Index;
   overriding function Image
     (Value : Internal_Rec_Analysis_Unit) return String;

   -----------------
   -- Big integer --
   -----------------

   type Internal_Rec_Big_Int is new Internal_Value with record
      Value : Big_Integer;
   end record;
   type Internal_Acc_Big_Int is access all Internal_Rec_Big_Int;

   overriding function "=" (Left, Right : Internal_Rec_Big_Int) return Boolean;
   overriding function Type_Of
     (Value : Internal_Rec_Big_Int) return Type_Index;
   overriding function Image (Value : Internal_Rec_Big_Int) return String;

   -------------
   -- Boolean --
   -------------

   type Internal_Rec_Bool is new Internal_Value with record
      Value : Boolean;
   end record;
   type Internal_Acc_Bool is access all Internal_Rec_Bool;

   overriding function "=" (Left, Right : Internal_Rec_Bool) return Boolean;
   overriding function Type_Of (Value : Internal_Rec_Bool) return Type_Index;
   overriding function Image (Value : Internal_Rec_Bool) return String;

   ----------
   -- Char --
   ----------

   type Internal_Rec_Character is new Internal_Value with record
      Value : Character_Type;
   end record;
   type Internal_Acc_Character is access all Internal_Rec_Character;

   overriding function "="
     (Left, Right : Internal_Rec_Character) return Boolean;
   overriding function Type_Of
     (Value : Internal_Rec_Character) return Type_Index;
   overriding function Image (Value : Internal_Rec_Character) return String;

   ---------
   -- Int --
   ---------

   type Internal_Rec_Int is new Internal_Value with record
      Value : Integer;
   end record;
   type Internal_Acc_Int is access all Internal_Rec_Int;

   overriding function "=" (Left, Right : Internal_Rec_Int) return Boolean;
   overriding function Type_Of (Value : Internal_Rec_Int) return Type_Index;
   overriding function Image (Value : Internal_Rec_Int) return String;

   ---------------------------
   -- Source_Location_Range --
   ---------------------------

   type Internal_Rec_Source_Location_Range is new Internal_Value with record
      Value : Source_Location_Range;
   end record;
   type Internal_Acc_Source_Location_Range is
     access all Internal_Rec_Source_Location_Range;

   overriding function "="
     (Left, Right : Internal_Rec_Source_Location_Range) return Boolean;
   overriding function Type_Of
     (Value : Internal_Rec_Source_Location_Range) return Type_Index;
   overriding function Image
     (Value : Internal_Rec_Source_Location_Range) return String;

   ------------
   -- String --
   ------------

   type Internal_Rec_String is new Internal_Value with record
      Value : Unbounded_Text_Type;
   end record;
   type Internal_Acc_String is access all Internal_Rec_String;

   overriding function "=" (Left, Right : Internal_Rec_String) return Boolean;
   overriding function Type_Of (Value : Internal_Rec_String) return Type_Index;
   overriding function Image (Value : Internal_Rec_String) return String;

   -----------
   -- Token --
   -----------

   type Internal_Rec_Token is new Internal_Value with record
      Value : Lk_Token;
   end record;
   type Internal_Acc_Token is access all Internal_Rec_Token;

   overriding function "=" (Left, Right : Internal_Rec_Token) return Boolean;
   overriding function Type_Of (Value : Internal_Rec_Token) return Type_Index;
   overriding function Image (Value : Internal_Rec_Token) return String;

   ------------
   -- Symbol --
   ------------

   type Internal_Rec_Symbol is new Internal_Value with record
      Value : Unbounded_Text_Type;
   end record;
   type Internal_Acc_Symbol is access all Internal_Rec_Symbol;

   overriding function "=" (Left, Right : Internal_Rec_Symbol) return Boolean;
   overriding function Type_Of (Value : Internal_Rec_Symbol) return Type_Index;
   overriding function Image (Value : Internal_Rec_Symbol) return String;

   -----------
   -- Nodes --
   -----------

   type Internal_Rec_Node is new Internal_Value with record
      Value : Lk_Node;
   end record;
   type Internal_Acc_Node is access all Internal_Rec_Node;

   overriding function "=" (Left, Right : Internal_Rec_Node) return Boolean;
   overriding function Type_Of (Value : Internal_Rec_Node) return Type_Index;
   overriding function Type_Matches
     (Value : Internal_Rec_Node; T : Type_Index) return Boolean;
   overriding function Image (Value : Internal_Rec_Node) return String;

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

   type Base_Internal_Array_Value is abstract new Internal_Value
     with null record;

   type Base_Internal_Array_Value_Access is
     access all Base_Internal_Array_Value'Class;

   overriding function Image (Value : Base_Internal_Array_Value) return String;

   function Array_Length (Value : Base_Internal_Array_Value) return Natural
   is abstract;
   --  Return the number of items in the ``Value`` array

   function Array_Item
     (Value : Base_Internal_Array_Value;
      Index : Positive) return Internal_Value_Access is abstract;
   --  Return the array item in ``Value`` at the given ``Index``. The index is
   --  assumed to be in-bounds.

   type Base_Internal_Iterator_Value is abstract new Internal_Value
     with null record;

   type Base_Internal_Iterator_Value_Access is
     access all Base_Internal_Iterator_Value'Class;

   overriding function Image
     (Value : Base_Internal_Iterator_Value) return String;

   function Next
     (Value : Base_Internal_Iterator_Value) return Internal_Value_Access
   is abstract;
   --  Consume and return the next item in the ``Value`` iterator, if there is
   --  one, otherwise return null.

   type Base_Internal_Struct_Value is abstract new Internal_Value
     with null record;

   type Base_Internal_Struct_Value_Access is
     access all Base_Internal_Struct_Value'Class;

   overriding function Image
     (Value : Base_Internal_Struct_Value) return String;

   function Eval_Member
     (Value  : Base_Internal_Struct_Value;
      Member : Struct_Member_Index) return Internal_Value_Access is abstract;
   --  Return the struct member in ``Value`` corresponding to the given
   --  ``Member`` index. The index is assumed to be valid for this struct.

end Langkit_Support.Internal.Introspection;
