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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Adalog.Debug; use Langkit_Support.Adalog.Debug;

package body Langkit_Support.Adalog.Abstract_Relation is

   -----------
   -- Solve --
   -----------

   function Solve
     (Self    : in out Base_Relation'Class;
      Context : in out Solving_Context) return Solving_State
   is

      procedure Wait;
      --  Wait for user input

      procedure Wait is
      begin
         if Debug_State = Step then
            Put_Line ("Press enter to continue...");
            declare
               Dummy : String := Ada.Text_IO.Get_Line;
            begin
               null;
            end;
         end if;
      end Wait;

   begin
      if Debug.Debug then
         Print_Relation (Context.Root_Relation,
                         Self'Unrestricted_Access,
                         With_Colors => True);
      end if;
      Wait;

      return Res : constant Solving_State := Self.Solve_Impl (Context) do
         if Debug_State = Step_At_First_Unsat and then Res = Unsatisfied then
            Set_Debug_State (Step);
         end if;

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
      is new Ada.Unchecked_Deallocation (Base_Relation'Class, Relation);
   begin
      if Self = null then
         return;
      end if;

      Self.Ref_Count := Self.Ref_Count - 1;
      if Self.Ref_Count = 0 then
         Self.Cleanup;
         Unchecked_Free (Self);
      end if;
      Self := null;
   end Dec_Ref;

   --------------------
   -- Print_Relation --
   --------------------

   procedure Print_Relation
     (Self             : Relation;
      Current_Relation : Relation := null;
      With_Colors      : Boolean := False)
   is
      procedure Start_Line (Level : Integer);
      procedure Internal (R : Relation; Level : Integer);

      ----------------
      -- Start_Line --
      ----------------

      procedure Start_Line (Level : Integer) is
      begin
         for Dummy in 1 .. (Level * 2) loop
            Put ("| ");
         end loop;
      end Start_Line;

      --------------
      -- Internal --
      --------------

      procedure Internal (R : Relation; Level : Integer) is
         Is_Current : constant Boolean :=
            Current_Relation /= null and then Current_Relation = R;
      begin
         if With_Colors and then Is_Current then
            Put (ASCII.ESC & "[92m");
         end if;

         if R = null then
            return;
         end if;

         Start_Line (Level);
         Put (R.Custom_Image);
         if R.Sloc_Info /= null then
            Put (" " & R.Sloc_Info.all);
         end if;

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

         if With_Colors and then Is_Current then
            Put (ASCII.ESC & "[0m");
         end if;
      end Internal;
   begin
      Internal (Self, 0);
   end Print_Relation;

   -----------
   -- Solve --
   -----------

   function Solve (Self : Relation; Timeout : Natural := 0) return Boolean is
      Context : Solving_Context :=
        (Root_Relation => Self,
         Timeout       => Timeout);
   begin
      declare
         Ret : constant Solving_State := Self.all.Solve (Context);
      begin
         Trace ("The relation solving resulted in " & Ret'Image);
         case Ret is
            when Progress | No_Progress =>
               raise Early_Binding_Error;

            when Satisfied =>
               return True;

            when Unsatisfied =>
               return False;
         end case;
      end;
   end Solve;

   ----------
   -- Tick --
   ----------

   procedure Tick (Context : in out Solving_Context) is
   begin
      if Context.Timeout = 0 then
         return;
      end if;

      Context.Timeout := Context.Timeout - 1;
      if Context.Timeout = 0 then
         raise Timeout_Error;
      end if;
   end Tick;

end Langkit_Support.Adalog.Abstract_Relation;
