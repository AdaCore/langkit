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

with Adalog.Debug;               use Adalog.Debug;

package body Adalog.Abstract_Relation is

   Current_Solving_Relation : Relation := null;
   --  NOTE??? We don't want to require a solving context or whatever for the
   --  moment, so we have the Current_Solving_Relation as a global. This is
   --  only used for tracing/debugging purposes, but should ultimately be
   --  removed anyway.

   -----------
   -- Solve --
   -----------

   function Solve (Inst : in out I_Relation) return Boolean is
      procedure Wait;

      --  Wait for user input
      procedure Wait is
      begin
         pragma Warnings (Off, "always");
         if Debug_State = Step then
            pragma Warnings (On, "always");
            Put_Line ("Press enter to continue ..");
            declare
               Dummy : String := Ada.Text_IO.Get_Line;
            begin null; end;
         end if;
      end Wait;
   begin
      if Current_Solving_Relation /= null then
         if Debug.Debug then
            Print_Relation (Current_Solving_Relation,
                            Inst'Unrestricted_Access);
         end if;
         Wait;
      end if;

      return Res : constant Boolean := I_Relation'Class (Inst).Solve_Impl do
         Trace (Res'Image);
         Wait;
      end return;
   end Solve;

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

   procedure Print_Relation
     (Self : Relation; Current_Relation : Relation := null)
   is
      procedure Internal (R : Relation; Level : Integer);

      procedure Internal (R : Relation; Level : Integer) is

         procedure Start_Line;
         procedure Start_Line is
         begin
            for Dummy in 0 .. Level * 4 loop
               Put (" ");
            end loop;
         end Start_Line;

         Is_Current : constant Boolean :=
           Current_Relation /= null
           and then Current_Relation = R;
      begin
         if Is_Current then
            Put (ASCII.ESC & "[92m");
         end if;

         if R = null then
            return;
         end if;

         declare
            Names : constant Unbounded_String_Array
              := Split (Ada.Tags.Expanded_Name (R.all'Tag), On => '.');
            Name  : constant String :=
              (if Names (Names'Last) = "REL"
               then To_String (Names (Names'Last - 1))
               else To_String (Names (Names'Last)));

            Custom_Image : constant String := R.Custom_Image;
         begin
            Start_Line;
            Put (if Custom_Image /= "" then Custom_Image else Name);
         end;

         declare
            Children : constant Relation_Array := R.Children;
         begin
            if Children /= Empty_Array then
               Put_Line (":");
               for C of Children loop
                  Internal (C, Level + 1);
               end loop;
            else
               Put_Line ("");
            end if;
         end;

         if Is_Current then
            Put (ASCII.ESC & "[0m");

         end if;
      end Internal;
   begin
      Internal (Self, 0);
   end Print_Relation;

   -----------
   -- Solve --
   -----------

   function Solve (Self : Relation) return Boolean is
   begin
      Current_Solving_Relation := Self;
      declare
         Ret : constant Boolean := Self.all.Solve;
      begin
         Trace ("The relation solving resulted in " & Ret'Image);
         Current_Solving_Relation := null;
         return Ret;
      end;
   end Solve;

end Adalog.Abstract_Relation;
