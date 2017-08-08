with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Adalog.Debug; use Langkit_Support.Adalog.Debug;

package body Langkit_Support.Adalog.Abstract_Relation is

   Current_Solving_Relation : Relation := null;
   --  NOTE??? We don't want to require a solving context or whatever for the
   --  moment, so we have the Current_Solving_Relation as a global. This is
   --  only used for tracing/debugging purposes, but should ultimately be
   --  removed anyway.

   -----------
   -- Solve --
   -----------

   function Solve (Self : in out Base_Relation'Class) return Solving_State is

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
            begin
               null;
            end;
         end if;
      end Wait;

   begin
      if Current_Solving_Relation /= null then
         if Debug.Debug then
            Print_Relation (Current_Solving_Relation,
                            Self'Unrestricted_Access,
                            With_Colors => True);
         end if;
         Wait;
      end if;

      return Res : constant Solving_State := Self.Solve_Impl do
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

   function Solve (Self : Relation) return Boolean is
   begin
      Current_Solving_Relation := Self;
      declare
         Ret : constant Solving_State := Self.all.Solve;
      begin
         Trace ("The relation solving resulted in " & Ret'Image);
         Current_Solving_Relation := null;
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

end Langkit_Support.Adalog.Abstract_Relation;
