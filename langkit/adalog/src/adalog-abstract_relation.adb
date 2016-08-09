------------------------------------------------------------------------------
--                               A D A L O G                                --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Tags;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

package body Adalog.Abstract_Relation is

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : Relation) is
   begin
      if Self /= null then
         Self.Ref_Count := Self.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Relation) is
      procedure Unchecked_Free
      is new Ada.Unchecked_Deallocation (I_Relation'Class, Relation);
   begin
      if Self = null then
         return;
      end if;

      Self.Ref_Count := Self.Ref_Count - 1;
      if Self.Ref_Count = 0 then
         Self.Cleanup;
         Unchecked_Free (Self);
      end if;

   end Dec_Ref;

   --------------------
   -- Print_Relation --
   --------------------

   procedure Print_Relation (Self : Relation) is
      procedure Internal (R : Relation; Level : Integer);
      procedure Internal (R : Relation; Level : Integer) is
      begin
         if R = null then
            return;
         end if;

         for Dummy in 0 .. Level * 4 loop
            Put (" ");
         end loop;

         declare
            Names : constant Unbounded_String_Array
              := Split (Ada.Tags.Expanded_Name (R.all'Tag), On => '.');
            Name  : constant String :=
              (if Names (Names'Last) = "Rel"
               then To_String (Names (Names'Last - 1))
               else To_String (Names (Names'Last)));
         begin
            Put_Line (Name & " Refcount:" & R.Ref_Count'Image);
         end;

         for C of R.Children loop
            Internal (C, Level + 1);
         end loop;
      end Internal;
   begin
      Internal (Self, 0);
   end Print_Relation;

end Adalog.Abstract_Relation;
