--  Check that vars are either defined (in which case they have correct values)
--  or undefined when expected after Solve_First.

with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with Langkit_Support.Images; use Langkit_Support.Images;

procedure Main is
   use T_Solver, Refs;

   procedure Solve (R : Relation);

   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");
   Z : constant Refs.Logic_Var := Create ("Z");

   Vars : constant Logic_Var_Array := (X, Y, Z);

   R  : constant Relation := (X = 1 and Y = 2) or (Y = 3 and Z = 4);
   R2 : constant Relation := Y = 3;
   R3 : constant Relation := Y = Z and Y = 2;

   -----------
   -- Solve --
   -----------

   procedure Solve (R : Relation) is
   begin
      for V of Vars loop
         Reset (V);
      end loop;

      if Solve_First (R) then
         for V of Vars loop
            Put_Line
              (Image (V) & " = "
               & (if Is_Defined (V)
                 then Stripped_Image (Get_Value (V))
                 else "<undefined>"));
         end loop;
         New_Line;
      end if;
   end Solve;
begin
   Solve (R);
   Solve (R2);
   Solve (R3);
end Main;
