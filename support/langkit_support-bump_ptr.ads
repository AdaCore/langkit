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

with System;
with System.Storage_Elements;       use System.Storage_Elements;
with System.Storage_Pools.Subpools; use System.Storage_Pools.Subpools;

with Langkit_Support.Vectors;

--  This package provides a pool allocator that is based on the bump pointer
--  allocation strategy. The principle is that each allocation just triggers an
--  increment on the current allocated page. When the page is full, a new page
--  is allocated.
--
--  The caveat is that you cannot deallocate an object once you allocated
--  it. That's what makes this allocator so simple and fast. The only way of
--  deallocating the objects is to deallocate the whole pool at once in the
--  end.

--  This package provides two interfaces: One uses Ada 2012 subpools to control
--  the lifetime of your objects. It is the recommended way of doing these kind
--  of things. Unfortunately GNAT's implementation at this time is unacceptably
--  slow, so we also provide a second mechanism, which gives you an ad-hoc
--  generic alloc function.

--  WARNING: This second mechanism is unsafe, and only works correctly for
--  a subset of types, namely simple non controlled POD types, tagged or non
--  tagged, with no alignment constraints.

package Langkit_Support.Bump_Ptr is

   -------------------------------------
   --  Generic (and fast) ad-hoc pool --
   -------------------------------------

   type Bump_Ptr_Pool is private;
   --  This type is a handle to a subpool. You need to initialize it via a call
   --  to Create.

   No_Pool : constant Bump_Ptr_Pool;

   function Create return Bump_Ptr_Pool;
   --  Create a new pool

   function Allocate
     (Pool : Bump_Ptr_Pool; S : Storage_Offset) return System.Address
     with Inline;
   --  Return the address of a free memory block of size S.
   --  This function is exposed in case you need to alloc raw memory blocks. It
   --  is used underneath by other allocation procedures.

   procedure Free (Pool : in out Bump_Ptr_Pool);
   --  Free all memory allocated by this pool.
   --
   --  BEWARE: This will make dangling pointers of every pointers allocated via
   --  this pool.

   generic
      type Element_T is private;
      type Element_Access is access all Element_T;
   package Alloc is
      function Alloc (Pool : Bump_Ptr_Pool) return System.Address
         with Inline;

      function Alloc (Pool : Bump_Ptr_Pool) return Element_Access
        with Inline;

      function To_Pointer is new Ada.Unchecked_Conversion
        (System.Address, Element_Access);

   end Alloc;
   --  This generic allocation package can be used to allocate an object of
   --  type Element_T.
   --
   --  BEWARE: This function will only work with *constrained* basic types or
   --  simple scalar types (no tagged, controlled, etc). If you need an all
   --  around allocation mechanism for bump pointer pools, use the subpools
   --  mechanism!

   generic
      type Element_T is tagged private;
      type Element_Access is access all Element_T;
   package Tagged_Alloc is
      function Alloc (Pool : Bump_Ptr_Pool) return Element_Access
         with Inline;
   end Tagged_Alloc;
   --  This generic allocation package can be used to allocate an object of
   --  tagged type Element_T.
   --
   --  BEWARE: This function will only work with *constrained* simple tagged
   --  types. It has not been tested with controlled objects, arrays, or
   --  discriminated types.

   generic
      type Element_T is private;
      type Index_Type is (<>);
   package Array_Alloc is
      type Element_Array is array (Index_Type) of Element_T;
      --  This package handles unsized array types to avoid having to deal with
      --  fat pointers.

      type Element_Array_Access is access all Element_Array;

      Empty_Array_Access : constant Element_Array_Access;
      --  Access to an empty array. As accessing its elements is forbidden, but
      --  dereferencing it to take an empty slice is allowed, it is designed to
      --  point to a non-null junk address.

      function Alloc
        (Pool : Bump_Ptr_Pool; Length : Natural) return System.Address
         with Inline;

      function Alloc
        (Pool : Bump_Ptr_Pool; Length : Natural) return Element_Array_Access
         with Inline;
      --  If Length is zero, return Empty_Array_Access. Otherwise, allocate an
      --  array that is large enough to contain Length elements and return
      --  an access to it.

      function To_Pointer is new Ada.Unchecked_Conversion
        (System.Address, Element_Array_Access);

   private

      Empty_Array_Access : constant Element_Array_Access := To_Pointer
        (System.Null_Address + 1);

   end Array_Alloc;
   --  This generic allocation package can be used to allocate an array of
   --  Element_T objects.
   --
   --  BEWARE: This function will only work with *constrained* basic types or
   --  simple scalar types (no tagged, controlled, etc).

   -------------------------------------------------------
   -- Ada 2012 Pool with subpools safe & slow mechanism --
   -------------------------------------------------------

   type Ada_Bump_Ptr_Pool is new Root_Storage_Pool_With_Subpools
   with null record;
   --  This type is meant to be used as a storage pool. See section 6.4 of the
   --  Ada 2012 rationale for details. Here is a basic example of the use::
   --
   --     Root_Pool : Ada_Bump_Ptr_Pool;
   --     type Int_Access is access all Integer;
   --     for Int_Access'Storage_Pool use Root_Pool;
   --
   --     Subpool   : Pool_Handle := Create_Subpool (Root_Pool);
   --
   --     A : Int_Access := new (Subpool) Integer'(42);
   --
   --  While the performance is still improved when compared to regular
   --  malloc/free, it is much slower than the ad-hoc mechanism, so
   --  coherent use cases might be:
   --
   --  - When you cannot use the ad-hoc mechanism because of the type of
   --    objects you want to allocate.
   --
   --  - When you don't care about the performance but still want the
   --    simplified memory management that this kind of pools provide.

   overriding function Create_Subpool
     (Pool : in out Ada_Bump_Ptr_Pool)
      return not null Subpool_Handle;

   overriding procedure Deallocate_Subpool
     (Pool    : in out Ada_Bump_Ptr_Pool;
      Subpool : in out Subpool_Handle);

private
   subtype Page_Ptr is System.Address;

   Page_Size : constant := 2 ** 14;
   --  16kb. This constant has been chosen heuristically to be the lowest
   --  value that gives the best performance. Bigger values did not make
   --  any difference, and that way we ensure that pools can stay small.

   package Pages_Vector is new Langkit_Support.Vectors (Page_Ptr);

   type Bump_Ptr_Pool_Type is new Root_Subpool with record
      Current_Page   : Page_Ptr;
      Current_Offset : Storage_Offset := Page_Size;
      Pages          : Pages_Vector.Vector;
   end record;

   type Bump_Ptr_Pool is access all Bump_Ptr_Pool_Type;

   No_Pool : constant Bump_Ptr_Pool := null;

   overriding procedure Allocate_From_Subpool
     (Pool                     : in out Ada_Bump_Ptr_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count;
      Subpool                  : not null Subpool_Handle);

end Langkit_Support.Bump_Ptr;
