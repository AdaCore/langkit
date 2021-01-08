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

with Ada.Containers.Generic_Array_Sort;

with System;        use System;
with System.Memory; use System.Memory;

-----------------------------
-- Langkit_Support.Vectors --
-----------------------------

package body Langkit_Support.Vectors is

   El_Size : constant size_t := Elements_Array'Component_Size / Storage_Unit;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Vector) return Boolean is
   begin
      return Self.Size = 0;
   end Is_Empty;

   -------------
   -- Reserve --
   -------------

   procedure Reserve (Self : in out Vector; Capacity : Natural) is
      Siz : constant size_t := size_t (Capacity) * El_Size;
   begin
      if Small_Vector_Capacity > 0
        and then Self.Capacity = Small_Vector_Capacity
      then
         --  We have an inline small vector, and we're still using it

         --  If the small vector has capacity, then Reserve is a no-op
         if Capacity <= Self.Capacity then
            return;
         end if;

         --  The small vector is smaller than the required capacity. So we'll
         --  allocate and transfer the items from the small vector to the
         --  dynamically allocated one.
         Self.E := To_Pointer (Alloc (Siz));
         for I in Self.SV'Range loop
            Self.E.all (I) := Self.SV (I);
         end loop;
      else
         if Self.E = null then
            --  E is null: First alloc
            Self.E := To_Pointer (Alloc (Siz));
         else
            --  E is not null: realloc
            Self.E := To_Pointer (Realloc (Self.E.all'Address, Siz));
         end if;

      end if;

      Self.Capacity := Capacity;
   end Reserve;

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out Vector; Element : Element_Type) is
   begin
      if Self.Capacity = Self.Size then
         Reserve (Self, (Self.Capacity * 2) + 1);
      end if;
      Self.Size := Self.Size + 1;

      declare
         Index : constant Index_Type := Last_Index (Self);
      begin
         if Small_Vector_Capacity = 0 then
            Self.E.all (Index) := Element;
         else
            if Self.Capacity = Small_Vector_Capacity then
               Self.SV (Index) := Element;
            else
               Self.E.all (Index) := Element;
            end if;
         end if;
      end;
   end Append;

   ------------
   -- Concat --
   ------------

   procedure Concat (Self : in out Vector; Elements : Elements_Array) is
   begin
      for El of Elements loop
         Self.Append (El);
      end loop;
   end Concat;

   ------------
   -- Concat --
   ------------

   procedure Concat (Self : in out Vector; Elements : Vector) is
   begin
      Self.Reserve (Self.Length + Elements.Length);
      for I in Elements.First_Index .. Elements.Last_Index loop
         Self.Append (Elements.Get (I));
      end loop;
   end Concat;

   ---------------
   -- Remove_At --
   ---------------

   procedure Remove_At (Self : in out Vector; Index : Index_Type) is
   begin
      for I in Index .. Self.Length - 1 loop
         Set (Self, I, Get (Self, I + 1));
      end loop;
      Pop (Self);
   end Remove_At;

   ---------
   -- Get --
   ---------

   function Get
     (Self : Vector; Index : Iteration_Index_Type) return Element_Type
   is
   begin
      if Index > Self.Size then
         raise Constraint_Error with "Out of bound access";
      end if;

      if Small_Vector_Capacity = 0 then
         return Self.E (Index);
      else
         if Self.Capacity = Small_Vector_Capacity then
            return Self.SV (Index);
         else
            return Self.E (Index);
         end if;
      end if;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (Self : in out Vector; Index : Index_Type; E : Element_Type)
   is
   begin
      if Small_Vector_Capacity = 0 then
         Self.E (Index) := E;
      else
         if Self.Capacity = Small_Vector_Capacity then
            Self.SV (Index) := E;
         else
            Self.E (Index) := E;
         end if;
      end if;
   end Set;

   ----------------
   -- Get_Access --
   ----------------

   function Get_Access
     (Self : Vector; Index : Index_Type) return Element_Access
   is
   begin
      if Small_Vector_Capacity = 0 then
         return Self.E (Index)'Unrestricted_Access;
      else
         if Self.Capacity = Small_Vector_Capacity then
            return Self.SV (Index)'Unrestricted_Access;
         else
            return Self.E (Index)'Unrestricted_Access;
         end if;
      end if;
   end Get_Access;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Vector) is
   begin
      Free (Self.E);
   end Destroy;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Vector) is
   begin
      Self.Size := 0;
   end Clear;

   ---------
   -- Pop --
   ---------

   function Pop (Self : in out Vector) return Element_Type is
      Index : constant Index_Type := Last_Index (Self);
      Res : constant Element_Type := Get (Self, Index);
   begin
      Self.Size := Self.Size - 1;
      return Res;
   end Pop;

   ---------
   -- Pop --
   ---------

   procedure Pop (Self : in out Vector) is
      Discard : constant Element_Type := Pop (Self);
   begin
      null;
   end Pop;

   ---------
   -- Pop --
   ---------

   function Pop (Self : in out Vector; N : Index_Type) return Element_Type is
      Result : constant Element_Type := Self.Get (N);
   begin
      Self.Set (N, Self.Last_Element);
      Self.Size := Self.Size - 1;
      return Result;
   end Pop;

   ---------
   -- Pop --
   ---------

   procedure Pop (Self : in out Vector; N : Index_Type) is
      Discard : constant Element_Type := Self.Pop (N);
   begin
      null;
   end Pop;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Self : Vector) return Element_Type
   is (Get (Self, First_Index (Self)));

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Self : Vector) return Element_Type
   is
   begin
      return Get (Self, Last_Index (Self));
   end Last_Element;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Self : Vector) return Element_Access
   is
   begin
      return Get_Access (Self, Last_Index (Self));
   end Last_Element;

   ------------
   -- Length --
   ------------

   function Length (Self : Vector) return Natural is (Self.Size);

   -----------
   -- Slice --
   -----------

   function Slice
     (Self : Vector; First, Last : Natural) return Elements_Array
   is
   begin
      if Small_Vector_Capacity = 0 then
         return Self.E (First .. Last);
      else
         if Self.Capacity = Small_Vector_Capacity then
            return Self.SV (First .. Last);
         else
            return Self.E (First .. Last);
         end if;
      end if;
   end Slice;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Self : Vector) return Elements_Array
   is
   begin
      if Self.Size = 0 then
         return Empty_Array;
      else
         return Slice (Self, First_Index (Self), Last_Index (Self));
      end if;
   end To_Array;

   -----------
   -- Image --
   -----------

   function Image (Self : Vector) return String is
      function Image (Self : Vector; I : Index_Type) return String
      is
        (if I < Last_Index (Self)
         then Image (Get (Self, I)) & ", " & Image (Self, I + 1)
         else Image (Get (Self, I)));
   begin
      return "[" & (if Self.Size > 0
                    then Image (Self, First_Index (Self))
                    else "") & "]";
   end Image;

   ----------
   -- Copy --
   ----------

   function Copy (Self : Vector) return Vector is
      N : Vector;
   begin
      if Self.Length > 0 then
         N.Reserve (Self.Length);
      end if;
      for El of Self loop
         N.Append (El);
      end loop;
      return N;
   end Copy;

   ------------------
   -- Generic_Sort --
   ------------------

   procedure Generic_Sort (Self : in out Vector) is
      procedure Sort is new Ada.Containers.Generic_Array_Sort
        (Positive, Element_Type, Elements_Array, "<");
   begin
      if Self.Capacity = Small_Vector_Capacity then
         Sort (Self.SV (1 .. Self.Size));
      else
         Sort (Self.E.all (1 .. Self.Size));
      end if;
   end Generic_Sort;

end Langkit_Support.Vectors;
