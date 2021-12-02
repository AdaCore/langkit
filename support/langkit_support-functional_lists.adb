------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
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

package body Langkit_Support.Functional_Lists is

   package Alloc_List is new Bump.Alloc (List_Node, Node_Ptr);

   function Create return List is (List'(Pool => Bump.Create, others => <>));

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out List) is
   begin
      Bump.Free (Self.Pool);
      Self := No_List;
   end Destroy;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out List) is
   begin
      Self.Length := 0;
      Self.First := null;
   end Clear;

   ---------
   -- "&" --
   ---------

   function "&" (Head : T; Tail : List) return List is
      Actual_Tail : constant List := (if Tail = No_List then Create else Tail);
   begin
      --  Create a list that uses the same pool as the tail, point it to the
      --  head and link it to the tail's elements.

      return Ret : constant List :=
        (Pool   => Actual_Tail.Pool,
         First  => Alloc_List.Alloc (Actual_Tail.Pool),
         Length => Actual_Tail.Length + 1)
      do
         Ret.First.all := (Head, Actual_Tail.First);
      end return;
   end "&";

   ----------
   -- Head --
   ----------

   function Head (Self : List) return T is
   begin
      return Self.First.El;
   end Head;

   ----------
   -- Tail --
   ----------

   function Tail (Self : List) return List is
   begin
      if Self.First.Next /= null then
         return List'(Self.Pool, Self.First.Next, Self.Length - 1);
      else
         return No_List;
      end if;
   end Tail;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Self : List) return Boolean is
   begin
      return Self.First /= null;
   end Has_Element;

   ------------
   -- Length --
   ------------

   function Length (Self : List) return Natural is (Self.Length);

   --------------
   -- To_Array --
   --------------

   function To_Array (Self : List) return T_Array is
      Current : List := Self;
   begin
      return Result : T_Array (1 .. Self.Length) do
         for I in Result'Range loop
            Result (I) := Head (Current);
            Current := Tail (Current);
         end loop;
      end return;
   end To_Array;

end Langkit_Support.Functional_Lists;
