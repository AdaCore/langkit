--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System;                  use System;
with System.Memory;           use System.Memory;
with System.Storage_Elements; use System.Storage_Elements;

package body Liblktlang_AdaSAT.Vectors is
   type Array_Bounds is record
      First : Natural;
      Last  : Natural;
   end record;

   pragma Assert
     (Array_Bounds'Size = Elements_Array'Descriptor_Size,
      "Unexpected size for array bounds");

   type Thin_Unconstrained_Array_Access is access all Elements_Array;
   for Thin_Unconstrained_Array_Access'Size use Standard'Address_Size;

   pragma Warnings (Off);
   function To_Thin_Access is new Ada.Unchecked_Conversion
     (System.Address, Thin_Unconstrained_Array_Access);
   pragma Warnings (On);

   type Fat_Unconstrained_Array_Access is access all Elements_Array;

   El_Size : constant size_t := Elements_Array'Component_Size / Storage_Unit;
   Bd_Size : constant size_t := Array_Bounds'Size / Storage_Unit;
   Offset  : constant Storage_Count := Storage_Count (Bd_Size);

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Vector) return Boolean
   is (Self.Size = 0);

   -------------
   -- Reserve --
   -------------

   procedure Reserve (Self : in out Vector; Capacity : Natural) is
      Siz  : constant size_t := Bd_Size + size_t (Capacity) * El_Size;
      --  Compute the size so as to include room for the bounds, in case
      --  they are needed (see subprogram Internal_Array).
   begin
      if Self.E = null then
         --  E is null: First alloc
         Self.E := To_Pointer (Alloc (Siz) + Offset);
      else
         --  E is not null: realloc
         Self.E := To_Pointer
           (Realloc (Self.E.all'Address - Offset, Siz) + Offset);
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
      Self.E.all (Self.Size) := Element;
   end Append;

   ---------
   -- Get --
   ---------

   function Get
     (Self : Vector; Index : Iteration_Index_Type) return Element_Type
   is (Self.E (Index));

   ----------------
   -- Get_Access --
   ----------------

   function Get_Access
     (Self : Vector; Index : Iteration_Index_Type) return Element_Access
   is (Self.E (Index)'Unrestricted_Access);

   ---------
   -- Set --
   ---------

   procedure Set (Self : in out Vector; Index : Index_Type; E : Element_Type)
   is
   begin
      Self.E (Index) := E;
   end Set;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Vector) is
   begin
      if Self.E /= null then
         Free (Self.E.all'Address - Offset);
      end if;
   end Destroy;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Vector) is
   begin
      Self.Size := 0;
   end Clear;

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

   ------------
   -- Length --
   ------------

   function Length (Self : Vector) return Natural is (Self.Size);

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length (Self : in out Vector; N : Natural) is
   begin
      Self.Size := N;
   end Set_Length;

   ---------------------
   -- Swap_And_Remove --
   ---------------------

   procedure Swap_And_Remove (Self : in out Vector; I : Positive) is
   begin
      Self.E.all (I) := Self.E.all (Self.Size);
      Self.Size := Self.Size - 1;
   end Swap_And_Remove;

   ---------
   -- Pop --
   ---------

   function Pop (Self : in out Vector) return Element_Type is
      R : constant Element_Type := Self.E (Self.Size);
   begin
      Self.Size := Self.Size - 1;
      return R;
   end Pop;

   ----------
   -- Move --
   ----------

   procedure Move (Target : in out Vector; Source : in out Vector) is
      Target_E : constant Elements_Array_Access := Target.E;
      Target_C : constant Natural := Target.Capacity;
   begin
      Target.E := Source.E;
      Source.E := Target_E;

      Target.Capacity := Source.Capacity;
      Target.Size := Source.Size;

      Source.Capacity := Target_C;
      Source.Size := 0;
   end Move;

   ----------
   -- Copy --
   ----------

   function Copy (Self : Vector) return Vector is
      N : Vector;
   begin
      if Self.Length > 0 then
         N.Reserve (Self.Length);
         for El of Self loop
            N.Append (El);
         end loop;
      end if;
      return N;
   end Copy;

   --------------------
   -- Internal_Array --
   --------------------

   function Internal_Array (Self : Vector) return User_Array_Access is
   begin
      if Self.E = null then
         return new Elements_Array'(1 .. 0 => <>);
      end if;
      declare
         function To_User_Access is new Ada.Unchecked_Conversion
           (Fat_Unconstrained_Array_Access, User_Array_Access);

         Thin_Access : constant Thin_Unconstrained_Array_Access :=
            To_Thin_Access (Self.E.all'Address);
         --  GNAT thinks Thin_Access is a legit thin access on an unconstrained
         --  array.

         Bounds : Array_Bounds := (1, Self.Size);
         for Bounds'Address use Self.E.all'Address - Offset;
         --  We now manually set the values for the bounds

         Fat_Access : constant Fat_Unconstrained_Array_Access :=
            Fat_Unconstrained_Array_Access (Thin_Access);
         --  Use an Ada conversion to create a fat access from the thin access.
         --  We need a fat access because that's how the user access type is
         --  layed out.
      begin
         return To_User_Access (Fat_Access);
      end;
   end Internal_Array;

end Liblktlang_AdaSAT.Vectors;
