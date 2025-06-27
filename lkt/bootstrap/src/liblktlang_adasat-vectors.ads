--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Unchecked_Conversion;

with System;

--  This Vector implementation is taken from Liblktlang_Support-Vectors and
--  shares the same desired benefits. However it was also modified so as to
--  be able to extract the underlying array (see function `Internal_Array`),
--  which required changes in the allocation and deallocation routines to
--  account for the potential array bounds we may need to insert.
--
--  TODO: Put this implementation in a separate library? (GNATCOLL?)

generic
   type Element_Type is private;
   type Elements_Array is array (Positive range <>) of Element_Type;
package Liblktlang_AdaSAT.Vectors is

   subtype Index_Type is Positive;

   subtype Iteration_Index_Type is Natural;
   --  Like Index_Type, but also covers zero, which is used to represent a
   --  dummy last index value for empty vectors.

   type Element_Access is not null access all Element_Type;

   type Vector is tagged private
     with Iterable =>
       (First       => First_Index,
        Next        => Next,
        Has_Element => Has_Element,
        Element     => Get);

   Empty_Vector : constant Vector;

   function Is_Empty (Self : Vector) return Boolean
     with Inline;
   --  Return whether Self is an empty vector

   procedure Append (Self : in out Vector; Element : Element_Type)
     with Inline;
   --  Appends Element to Self

   procedure Reserve (Self : in out Vector; Capacity : Natural)
     with Inline;
   --  Make sure that Self has enough room to contain Capacity elements in
   --  total.

   function Get
     (Self : Vector; Index : Iteration_Index_Type) return Element_Type
     with Inline;
   --  Get the element at Index

   function Get_Access
     (Self : Vector; Index : Iteration_Index_Type) return Element_Access
     with Inline;
   --  Get an access on element at Index

   procedure Set (Self : in out Vector; Index : Index_Type; E : Element_Type)
     with Inline;
   --  Set the element at Index to E

   procedure Destroy (Self : in out Vector)
     with Inline;
   --  Destroy this vector

   procedure Clear (Self : in out Vector)
     with Inline;
   --  Remove every element in this vector.
   --  NOTICE: this function does not actually free the memory of the vector!

   function First_Element (Self : Vector) return Element_Type
     with Inline;
   --  Return the first element in this vector

   function Last_Element (Self : Vector) return Element_Type
     with Inline;
   --  Return the last element in this vector

   function Length (Self : Vector) return Natural
     with Inline;
   --  Return the Length of the vector, ie. the number of elements it contains

   procedure Set_Length (Self : in out Vector; N : Natural)
     with Inline;
   --  Resize the given vector so that it holes N elements

   procedure Swap_And_Remove (Self : in out Vector; I : Positive)
     with Inline;
   --  Remove the element at the given index. That index will now hold
   --  the last element of the vector.

   function Pop (Self : in out Vector) return Element_Type
     with Inline;
   --  Remove the last element and return it

   function First_Index (Self : Vector) return Iteration_Index_Type
   is (Index_Type'First)
     with Inline;
   --  Return the first index, only used for the Iterable aspect

   function Last_Index (Self : Vector) return Iteration_Index_Type
   is (First_Index (Self) + Length (Self) - 1)
     with Inline;
   --  Return the index of the last element in this vector or
   --  First_Index (Self) - 1 if this vector is empty.

   function Next
     (Self : Vector; N : Iteration_Index_Type) return Iteration_Index_Type
   is (N + 1)
     with Inline;
   --  Given a vector and an index, return the next index. Only used for the
   --  iterable aspect.

   function Previous
     (Self : Vector; N : Iteration_Index_Type) return Iteration_Index_Type
   is (N - 1)
     with Inline;
   --  Given a vector and an index, return the next index. Only used for the
   --  iterable aspect.

   function Has_Element
     (Self : Vector; N : Iteration_Index_Type) return Boolean
   is (N in First_Index (Self) .. Last_Index (Self))
     with Inline;
   --  Given a vector and an index, return True if the index is in the vector
   --  range. Only used for the iterable aspect.

   procedure Move (Target : in out Vector; Source : in out Vector)
     with Inline;
   --  Move the internal elements array of vector Source to vector Target.
   --  Clear the source vector.

   function Copy (Self : Vector) return Vector;
   --  Return newly allocated copy of Self

   generic
      type User_Array_Access is access Elements_Array;
   function Internal_Array (Self : Vector) return User_Array_Access
     with Inline;
   --  Return the internal array that the vector is working on.

private

   subtype Internal_Elements_Array is Elements_Array (Index_Type);

   type Elements_Array_Access is access all Internal_Elements_Array;

   function To_Pointer is
     new Ada.Unchecked_Conversion (System.Address, Elements_Array_Access);

   type Vector is tagged record
      E        : Elements_Array_Access := null;
      Size     : Natural := 0;
      Capacity : Natural := 0;
   end record;

   Empty_Vector : constant Vector := (E => null, Size => 0, Capacity => 0);

end Liblktlang_AdaSAT.Vectors;
