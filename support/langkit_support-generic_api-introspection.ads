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

--  This package provides a generic API so that programs can work with the
--  $.Introspection packages of all Langkit-generated libraries.
--
--  Note that it is experimental at this stage, and thus not officially
--  supported.

with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;

package Langkit_Support.Generic_API.Introspection is

   ------------------------
   -- Polymorphic values --
   ------------------------

   type Type_Ref is private;
   --  Reference to the type of a polymorphic value: boolean, integer,
   --  character, ...

   No_Type_Ref : constant Type_Ref;
   --  Special value to express no type reference

   type Type_Ref_Array is array (Positive range <>) of Type_Ref;

   function Language_For (T : Type_Ref) return Language_Id;
   --  Return the language ID corresponding to the given type. Raise a
   --  ``Precondition_Failure`` exception if ``T`` is ``No_Type_Ref``.

   function Debug_Name (T : Type_Ref) return String;
   --  Return the free-form name of this type for debug purposes. Raise a
   --  ``Precondition_Failure`` exception if ``T`` is ``No_Type_Ref``.

   type Any_Type_Index is new Natural;
   --  Language-specific index to designate a type.
   --
   --  A given language defines types for the ``1 .. Last_Type (Language)``
   --  range: see the ``Last_Type`` function below.

   No_Type_Index : constant Any_Type_Index := 0;
   --  Special ``Any_Type_Index`` to mean: no reference to a type

   subtype Type_Index is Any_Type_Index range 1 ..  Any_Type_Index'Last;

   function To_Index (T : Type_Ref) return Type_Index;
   --  Return the index of the given type. Raise a ``Precondition_Failure``
   --  exception if ``T`` is ``No_Type_Ref``.

   function From_Index (Id : Language_Id; T : Type_Index) return Type_Ref;
   --  Return the type for the given language corresponding to the ``T`` index.
   --  Raise a ``Precondition_Failure`` exception if ``T`` is not a valid type
   --  index for the given language.

   function Last_Type (Id : Language_Id) return Type_Index;
   --  Return the last type index that is valid for the given language

   function Type_Of (Node : Lk_Node) return Type_Ref;
   --  Return the type of ``Node``. Raise a ``Precondition_Failure`` if
   --  ``Node`` is ``No_Lk_Node``.

   ----------------
   -- Enum types --
   ----------------

   function Is_Enum_Type (T : Type_Ref) return Boolean;
   --  Return  whether ``T`` references an enum type.
   --
   --  All functions below will raise a ``Precondition_Failure`` if passed a
   --  type which does not satisfy this predicate as ``Enum`` formals.

   function Enum_Type_Name (Enum : Type_Ref) return Name_Type;
   --  Return the name of the given enum type

   type Enum_Value_Ref is private;
   --  Reference to an enum type value

   No_Enum_Value_Ref : constant Enum_Value_Ref;
   --  Special value to express no enum value reference

   function Enum_For (Value : Enum_Value_Ref) return Type_Ref;
   --  Return the enum type that owns the given value

   function Enum_Default_Value (Enum : Type_Ref) return Enum_Value_Ref;
   --  Return the index of the default enum value for the given ``Enum`` enum
   --  type, or ``No_Enum_Value_Ref`` if this type does not have a default
   --  value.

   function Enum_Value_Name (Value : Enum_Value_Ref) return Name_Type;
   --  Return the name corresponding to the ``Index``th value for the ``Enum``
   --  enum type. This raises a ``Out_Of_Bounds_Error`` if ``Index`` is too big
   --  for this enum type.

   type Any_Enum_Value_Index is new Natural;
   subtype Enum_Value_Index is Any_Enum_Value_Index
      range 1 .. Any_Enum_Value_Index'Last;
   --  Index of an enum value for a given enum type

   No_Enum_Value_Index : constant Any_Enum_Value_Index := 0;
   --  Special ``Any_Enum_Value_Index`` to mean: no reference to a type

   function To_Index (Value : Enum_Value_Ref) return Enum_Value_Index;
   --  Return the index of the given type. Raise a ``Precondition_Failure``
   --  exception if ``Value`` is ``No_Enum_Value_Ref``.

   function From_Index
     (Enum : Type_Ref; Value : Enum_Value_Index) return Enum_Value_Ref;
   --  Return the value for the given enum type corresponding to the ``Value``
   --  index.  Raise a ``Precondition_Failure`` exception if ``Value`` is not a
   --  valid value index for that enum type.

   function Enum_Last_Value (Enum : Type_Ref) return Enum_Value_Index;
   --  Return the index of the last enum value for the given ``Enum`` enum type

   -----------------
   -- Array types --
   -----------------

   function Is_Array_Type (T : Type_Ref) return Boolean;
   --  Return whether ``T`` references an array type.
   --
   --  All functions below will raise a ``Precondition_Failure`` if passed a
   --  type which does not satisfy this predicate as ``T`` formals.

   function Array_Element_Type (T : Type_Ref) return Type_Ref;
   --  Return the type of elements in ``T`` arrays

   -----------------------
   -- Struct/node types --
   -----------------------

   function Is_Base_Struct_Type (T : Type_Ref) return Boolean;
   --  Return whether ``T`` references a struct or node type.
   --
   --  All functions below will raise a ``Precondition_Failure`` if passed a
   --  type which does not satisfy this predicate as ``T`` formals.

   function Base_Struct_Type_Name (T : Type_Ref) return Name_Type;
   --  Return the name for the given struct/node type

   ------------------
   -- Struct types --
   ------------------

   function Is_Struct_Type (T : Type_Ref) return Boolean;
   --  Return whether ``T`` references a struct type.
   --
   --  All functions below will raise a ``Precondition_Failure`` if passed a
   --  type which does not satisfy this predicate as ``Struct`` formals.

   function Struct_Type_Name (Struct : Type_Ref) return Name_Type;
   --  Return the name for the given struct type

   ----------------
   -- Node types --
   ----------------

   function Is_Node_Type (T : Type_Ref) return Boolean;
   --  Return whether ``T`` references a node type.
   --
   --  All functions below will raise a ``Precondition_Failure`` if passed
   --  a type which does not satisfy this predicate as ``Node``/``Parent``
   --  formals.

   function Root_Node_Type (Id : Language_Id) return Type_Ref;
   --  Return the type for the root node in the given language

   function Node_Type_Name (Node : Type_Ref) return Name_Type;
   --  Return the name for the given node type

   function Is_Abstract (Node : Type_Ref) return Boolean;
   --  Return whether ``Node`` designates an abstract node

   function Is_Concrete (Node : Type_Ref) return Boolean
   is (not Is_Abstract (Node));

   function Base_Type (Node : Type_Ref) return Type_Ref;
   --  If ``Node`` is the root node type, raise a ``Bad_Type_Error`` exception.
   --  Otherwise, return ``Node``'s base type.

   function Derived_Types (Node : Type_Ref) return Type_Ref_Array;
   --  Return type references for all direct derivations for ``Node``

   function Last_Derived_Type (Node : Type_Ref) return Type_Index;
   --  Return the index of the ``Result`` type so that the ``Node .. Result``
   --  range contains exactly all node types that derive (directly or
   --  indirectly) from ``Node``.

   function Is_Derived_From (Node, Parent : Type_Ref) return Boolean;
   --  Return whether the ``Node`` node type derives (directly or indirectly)
   --  from ``Parent``.

   -------------------------
   -- Struct/node members --
   -------------------------

   type Struct_Member_Ref is private;
   --  Reference to a struct member (field or property)

   No_Struct_Member_Ref : constant Struct_Member_Ref;
   --  Special value to express no struct member reference

   type Struct_Member_Ref_Array is
     array (Positive range <>) of Struct_Member_Ref;

   function Is_Property (Member : Struct_Member_Ref) return Boolean;
   --  Whether ``Member`` is a property

   function Members (Struct : Type_Ref) return Struct_Member_Ref_Array;
   --  Return the list of members that ``Struct`` has

   function Member_Name (Member : Struct_Member_Ref) return Name_Type;
   --  Return the name of ``Member``

   function Member_Type (Member : Struct_Member_Ref) return Type_Ref;
   --  Return the type of ``Member``

   type Any_Struct_Member_Index is new Natural;
   subtype Struct_Member_Index is
     Any_Struct_Member_Index range 1 .. Any_Struct_Member_Index'Last;
   --  Language-specific index to designate a struct member
   --
   --  A given language defines members for the ``1 .. Last_Struct_Member
   --  (Language)`` range: see the ``Last_Struct_Member`À`À function below.

   No_Struct_Member : constant Any_Struct_Member_Index := 0;
   --  Special ``Any_Struct_Member_Index`` to mean: no reference to an argument

   function To_Index (Member : Struct_Member_Ref) return Struct_Member_Index;
   --  Return the index of the given struct member. Raise a
   --  ``Precondition_Failure`` exception if ``Member`` is
   --  ``No_Struct_Member``.

   function From_Index
     (Id : Language_Id; Member : Struct_Member_Index) return Struct_Member_Ref;
   --  Return the struct member for the given language corresponding to the
   --  ``Member`` index.  Raise a ``Precondition_Failure`` exception if
   --  ``Member`` is not a valid member index for the given language.

   function Last_Struct_Member (Id : Language_Id) return Struct_Member_Index;
   --  Return the last struct member index that is valid for the given language

   type Any_Argument_Index is new Natural;
   subtype Argument_Index is
     Any_Argument_Index range 1 ..  Any_Argument_Index'Last;
   --  Index of a property argument

   No_Argument_Index : constant Any_Argument_Index := 0;

   function Member_Argument_Type
     (Member : Struct_Member_Ref; Argument : Argument_Index) return Type_Ref;
   --  Return the type of the given property argument

   function Member_Argument_Name
     (Member : Struct_Member_Ref; Argument : Argument_Index) return Name_Type;
   --  Return the name of the given property argument

   function Member_Last_Argument
     (Member : Struct_Member_Ref) return Any_Argument_Index;
   --  Return the index of ``Member``'s last argument according to the given
   --  language. If it has no argument, return ``No_Argument_Index``.

private

   type Type_Ref is record
      Id    : Any_Language_Id;
      Index : Any_Type_Index;
      --  Either this is ``No_Type_Ref``, and in that case both members should
      --  be null/zero, either ``Index`` designates a valid type for the
      --  language ``Id`` represents.
   end record;

   No_Type_Ref : constant Type_Ref := (null, 0);

   type Enum_Value_Ref is record
      Enum : Type_Ref;
      Index : Any_Enum_Value_Index;
      --  Either this is ``No_Enum_Value_Ref``, and in that case both members
      --  should be null/zero, either ``Index`` designates a valid value for
      --  the enum type ``Enum`` represents.
   end record;

   No_Enum_Value_Ref : constant Enum_Value_Ref := (No_Type_Ref, 0);

   type Struct_Member_Ref is record
      Id    : Any_Language_Id;
      Index : Any_Struct_Member_Index;
      --  Either this is ``No_Struct_Member_Ref``, and in that case both
      --  members should be null/zero, either ``Index`` designates a valid
      --  member for the language ``Id`` represents.
   end record;

   No_Struct_Member_Ref : constant Struct_Member_Ref := (null, 0);

end Langkit_Support.Generic_API.Introspection;
