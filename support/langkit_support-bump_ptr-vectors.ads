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

--  This package provides the user of bump pointer pools with a Vector like
--  container. Traditional vectors reallocate their whole storage when we
--  outgrow their current capacity, but this is not possible without a very
--  large memory cost with bump pointer pools, since you cannot free individual
--  chunks of memory.
--
--  So in this package, we have a vector that is constituted of exponentially
--  growing chunks of memory. Since the maximum number of chunks is a constant
--  fixed by the memory of the system (32 being the limit on a modern machine
--  with 16gb of ram), random element access is still amortized O(1), with a
--  large constant.

--  Beware though, random access is still on the average of 100x slower than in
--  order iteration, so *never* use Get_At_Index to iterate over the vector!

generic
   type Element_Type is private;
package Langkit_Support.Bump_Ptr.Vectors is

   subtype Index_Type is Positive;

   type Vector is private
      with Iterable => (First       => First,
                        Next        => Next,
                        Has_Element => Has_Element,
                        Element     => Get);

   type Cursor is private;

   Empty_Cursor : constant Cursor;

   type Element_Access is not null access all Element_Type;

   function Create (P : Bump_Ptr_Pool) return Vector;
   --  Returns a newly created vector using P as it's pool storage

   function Length (Self : Vector) return Natural
      with Inline;
   --  Return the Length of the vector, ie. the number of elements it contains

   function First_Index (Self : Vector) return Index_Type
     with Inline;
   --  Return the index of the first element in Self

   function Last_Index (Self : Vector) return Integer
      with Inline;
   --  Return the index of the last element in Self, or First_Index (Self) - 1
   --  if Self is empty.

   procedure Append (Self : in out Vector; Element : Element_Type)
      with Inline;
   --  Appends Element to Self

   function Get (Self : Vector; C : Cursor) return Element_Type
      with Inline;
   --  Get the element at Index

   function Get_At_Index (Self : Vector; I : Index_Type) return Element_Type
      with Inline,
           Pre => I <= Last_Index (Self);
   --  Get the element at Index

   function Get_Access (Self : Vector; C : Cursor) return Element_Access
      with Inline;
   --  Get an access to the element at Index. The lifetime of the access is the
   --  one of the vector.

   function First (Self : Vector) return Cursor
      with Inline;
   --  Return the first index, only used for the Iterable aspect

   function Next (Self : Vector; C : Cursor) return Cursor
      with Inline;
   --  Given a vector and an index, return the next index. Only used for the
   --  iterable aspect.

   function Has_Element (Self : Vector; C : Cursor) return Boolean
      with Inline;
   --  Given a vector and an index, return True if the index is in the vector
   --  range. Only used for the iterable aspect.

private

   type Elements_Array is array (Positive range <>) of Element_Type;

   type Chunk;
   type Chunk_Access is access all Chunk;
   pragma No_Strict_Aliasing (Chunk_Access);

   type Chunk (Capacity : Natural) is record
      Elements   : Elements_Array (1 .. Capacity);
      Next_Chunk : Chunk_Access;
      Length     : Natural := 0;
   end record;

   type Vector is record
      Pool                       : Bump_Ptr_Pool;
      First_Chunk, Current_Chunk : Chunk_Access := null;
      Length                     : Natural := 0;
   end record;

   type Cursor is record
      Chunk          : Chunk_Access;
      Index_In_Chunk : Natural;
   end record;

   Empty_Cursor : constant Cursor := (null, 0);

end Langkit_Support.Bump_Ptr.Vectors;
