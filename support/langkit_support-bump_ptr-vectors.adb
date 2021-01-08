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

package body Langkit_Support.Bump_Ptr.Vectors is

   function Alloc_Chunk (P : Bump_Ptr_Pool; S : Natural) return Chunk_Access;

   -----------------
   -- Alloc_Chunk --
   -----------------

   function Alloc_Chunk (P : Bump_Ptr_Pool; S : Natural) return Chunk_Access
   is
      subtype C is Chunk (S);

      function To_Pointer is
        new Ada.Unchecked_Conversion (System.Address, Chunk_Access);

      Ret_Memory : constant System.Address :=
        Allocate (P, C'Max_Size_In_Storage_Elements);
      --  Allocate a chunk of memory for the result
      --  discriminated record...

      Ret_Disc   : Natural;
      for Ret_Disc'Address use Ret_Memory;
      --  And initialize its discriminant properly as the
      --  runtime would do with regular allocation.
   begin
      Ret_Disc := S;
      return To_Pointer (Ret_Memory);
   end Alloc_Chunk;

   function Create (P : Bump_Ptr_Pool) return Vector
   is
     (Vector'(Pool => P, others => <>));

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out Vector; Element : Element_Type)
   is

      procedure Init_Chunk (C : in out Chunk) with Inline;

      ----------------
      -- Init_Chunk --
      ----------------

      procedure Init_Chunk (C : in out Chunk) is
      begin
         C.Next_Chunk := null;
         C.Length := 0;
      end Init_Chunk;

      Old_Chunk : Chunk_Access;
   begin
      --  First append, create a chunk and initialize it
      if Self.Length = 0 then
         Self.First_Chunk := Alloc_Chunk (Self.Pool, 2);
         Init_Chunk (Self.First_Chunk.all);
         Self.Current_Chunk := Self.First_Chunk;
      end if;

      --  We filled the current chunk completely, create a new chunk and
      --  initialize it, chain it with the previous chunk.
      if Self.Current_Chunk.Length = Self.Current_Chunk.Capacity then
         Old_Chunk := Self.Current_Chunk;
         Self.Current_Chunk := Alloc_Chunk (Self.Pool, Old_Chunk.Capacity * 2);
         Init_Chunk (Self.Current_Chunk.all);
         Old_Chunk.Next_Chunk := Self.Current_Chunk;
      end if;

      --  At this stage we know the current chunk can contain element, insert
      --  it.
      Self.Current_Chunk.Length := Self.Current_Chunk.Length + 1;
      Self.Length := Self.Length + 1;
      Self.Current_Chunk.Elements (Self.Current_Chunk.Length) := Element;
   end Append;

   ---------
   -- Get --
   ---------

   function Get (Self : Vector; C : Cursor) return Element_Type is
      pragma Unreferenced (Self);
   begin
      return C.Chunk.Elements (C.Index_In_Chunk);
   end Get;

   ------------------
   -- Get_At_Index --
   ------------------

   function Get_At_Index (Self : Vector; I : Index_Type) return Element_Type
   is
      function Get_In_Chunk
        (Chunk             : Chunk_Access;
         Chunk_Start_Index : Index_Type)
         return Element_Type
      is (Chunk.Elements (I - Chunk_Start_Index + 1));
      --  Assuming that 1) Chunk's first element has index Chunk_Start_Index
      --  and that 2) the I index is inside this chunk, return the element
      --  corresponding to I.
   begin
      --  As the size of chunks double for each appened chunk, the element we
      --  are looking for should be in the current chunk more than half of the
      --  times (assuming equiprobable accesses). So let's just check if it's
      --  the case.
      declare
         Current_Chunk_Start_Index : constant Index_Type :=
            Index_Type'First + Self.Length - Self.Current_Chunk.Length;
      begin
         if I >= Current_Chunk_Start_Index then
            return Get_In_Chunk
              (Self.Current_Chunk, Current_Chunk_Start_Index);
         end if;
      end;

      --  We had no luck: go through all chunks to find the one that contains
      --  the element at index I.
      declare
         Chunk_Start_Index : Index_Type := Index_Type'First;
         Current_Chunk     : Chunk_Access := Self.First_Chunk;
      begin
         while Current_Chunk /= null
           and then I >= Chunk_Start_Index + Current_Chunk.Capacity
         loop
            Chunk_Start_Index := Chunk_Start_Index + Current_Chunk.Capacity;
            Current_Chunk := Current_Chunk.Next_Chunk;
         end loop;

         return Get_In_Chunk (Current_Chunk, Chunk_Start_Index);
      end;
   end Get_At_Index;

   ----------------
   -- Get_Access --
   ----------------

   function Get_Access (Self : Vector; C : Cursor) return Element_Access is
      pragma Unreferenced (Self);
   begin
      return C.Chunk.Elements (C.Index_In_Chunk)'Unrestricted_Access;
   end Get_Access;

   ------------
   -- Length --
   ------------

   function Length (Self : Vector) return Natural is
   begin
      return Self.Length;
   end Length;

   -----------
   -- First --
   -----------

   function First (Self : Vector) return Cursor is
   begin
      return Cursor'(Chunk => Self.First_Chunk, Index_In_Chunk => 1);
   end First;

   ----------
   -- Next --
   ----------

   function Next (Self : Vector; C : Cursor) return Cursor is
      pragma Unreferenced (Self);
   begin
      if C.Index_In_Chunk = C.Chunk.Capacity then
         return Cursor'(C.Chunk.Next_Chunk, 1);
      else
         return Cursor'(C.Chunk, C.Index_In_Chunk + 1);
      end if;
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Self : Vector; C : Cursor) return Boolean is
      pragma Unreferenced (Self);
   begin
      return C.Chunk /= null and then C.Index_In_Chunk <= C.Chunk.Length;
   end Has_Element;

   -----------------
   -- First_Index --
   -----------------

   function First_Index (Self : Vector) return Index_Type is
      pragma Unreferenced (Self);
   begin
      return Index_Type'First;
   end First_Index;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (Self : Vector) return Integer is
   begin
      return Index_Type'First + Length (Self) - 1;
   end Last_Index;

end Langkit_Support.Bump_Ptr.Vectors;
