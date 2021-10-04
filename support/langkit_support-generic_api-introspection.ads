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

package Langkit_Support.Generic_API.Introspection is

   ------------------------
   -- Polymorphic values --
   ------------------------

   type Any_Value_Type is new Natural;
   --  Generic type to designate the type of a polymorphic value: boolean,
   --  integer, character, ...
   --
   --  A given language defines types for the ``1 .. Last_Value_Type
   --  (Language)`` range: see the ``Last_Value_Type`` function below. All the
   --  subprograms below raise a ``Precondition_Failure`` exception if passed a
   --  value type index that is not in range for the given language.

   No_Value_Type : constant Any_Value_Type;
   --  Special ``Any_Value_Type`` to mean: no reference to a type

   subtype Value_Type is Any_Value_Type range 1 .. Any_Value_Type'Last;

   type Value_Type_Array is array (Positive range <>) of Value_Type;

   function Last_Value_Type (Id : Language_Id) return Value_Type;
   --  Return the last type index that is valid for the given language

   function Debug_Name (Id : Language_Id; T : Value_Type) return String;
   --  Return the free-form name of this type for debug purposes, according to
   --  the given language.

   ----------------
   -- Enum types --
   ----------------

   function Is_Enum_Type (Id : Language_Id; T : Value_Type) return Boolean;
   --  Return  whether ``T`` references an enum type for the given language.
   --
   --  All functions below will raise a ``Precondition_Failure`` if passed a
   --  type which does not satisfy this predicate as ``Enum`` formals.

   type Any_Enum_Value_Index is new Natural;
   subtype Enum_Value_Index is Any_Enum_Value_Index
      range 1 .. Any_Enum_Value_Index'Last;
   --  Index of an enum value for a given enum type

   No_Enum_Value_Index : constant Any_Enum_Value_Index := 0;

   function Enum_Type_Name
     (Id : Language_Id; Enum : Value_Type) return Name_Type;
   --  Return the name for the given enum type according to the given language

   function Enum_Last_Value
     (Id : Language_Id; Enum : Value_Type) return Enum_Value_Index;
   --  Return the index of the last enum value for the given ``Enum`` enum type

   function Enum_Default_Value
     (Id : Language_Id; Enum : Value_Type) return Any_Enum_Value_Index;
   --  Return the index of the default enum value for the given ``Enum`` enum
   --  type, or ``No_Enum_Value_Index`` if this type does not have a default
   --  value.

   function Enum_Value_Name
     (Id    : Language_Id;
      Enum  : Value_Type;
      Index : Enum_Value_Index) return Name_Type;
   --  Return the name corresponding to the ``Index``th value for the ``Enum``
   --  enum type. This raises a ``Out_Of_Bounds_Error`` if ``Index`` is too big
   --  for this enum type.

   -----------------
   -- Array types --
   -----------------

   function Is_Array_Type (Id : Language_Id; T : Value_Type) return Boolean;
   --  Return whether ``T`` references an array type for the given language.
   --
   --  All functions below will raise a ``Precondition_Failure`` if passed a
   --  type which does not satisfy this predicate as ``T`` formals.

   function Array_Element_Type
     (Id : Language_Id; T : Value_Type) return Value_Type;
   --  Return the type of elements in ``T`` arrays

   ----------------
   -- Node types --
   ----------------

   function Is_Node_Type (Id : Language_Id; T : Value_Type) return Boolean;
   --  Return whether ``T`` references a node type for the given language.
   --
   --  All functions below will raise a ``Precondition_Failure`` if passed
   --  a type which does not satisfy this predicate as ``Node``/``Parent``
   --  formals.

   function Root_Node_Type (Id : Language_Id) return Value_Type;
   --  Return the type for the root node in the given language

   function Node_Type_Name
     (Id : Language_Id; Node : Value_Type) return Name_Type;
   --  Return the name for the given node type according to the given language

   function Is_Abstract
     (Id : Language_Id; Node : Value_Type) return Boolean;
   --  Return whether ``Node`` designates an abstract node

   function Is_Concrete
     (Id : Language_Id; Node : Value_Type) return Boolean
   is (not Is_Abstract (Id, Node));

   function Base_Type
     (Id : Language_Id; Node : Value_Type) return Value_Type;
   --  If ``Node`` is the root node type, raise a ``Bad_Type_Error`` exception.
   --  Otherwise, return ``Node``'s base type.

   function Derived_Types
     (Id : Language_Id; Node : Value_Type) return Value_Type_Array;
   --  Return type references for all direct derivations for ``Node``

   function Last_Derived_Type
     (Id : Language_Id; Node : Value_Type) return Value_Type;
   --  Return the ``Result`` type so that the ``Node .. Result`` range contains
   --  exactly all node types that derive (directly or indirectly) from
   --  ``Node``.

   function Is_Derived_From
     (Id : Language_Id; Node, Parent : Value_Type) return Boolean;
   --  Return whether the ``Node`` node type derives (directly or indirectly)
   --  from ``Parent``.

private

   No_Value_Type : constant Any_Value_Type := 0;

end Langkit_Support.Generic_API.Introspection;
