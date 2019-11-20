with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

with Langkit_Support.Images; use Langkit_Support.Images;

--  Check that vars are defined and have correct values after Solve_First

procedure Main is
   use T_Solver; use Refs;

   procedure Solve (R : Relation);

   X : constant Raw_Var := Create ("X");
   Y : constant Raw_Var := Create ("Y");
   Z : constant Raw_Var := Create ("Z");

   R  : constant Relation :=
     (X = 1 and Y = 1)
     or (X = 3 and Y = 12);
   R2 : constant Relation := Y = 3;
   R3 : constant Relation := Y = Z and Y = 2;

   -----------
   -- Solve --
   -----------

   procedure Solve (R : Relation) is
   begin
      if Solve_First (R) then
         for V of Raw_Logic_Var.Var_Array'(X, Y, Z) loop
            Put_Line
              (Image (V) & " = "
               & (if Is_Defined (V)
                 then Stripped_Image (Get_Value (V))
                 else "<undefined>"));
         end loop;
      end if;
   end Solve;
begin
   Solve (R);
   Solve (R2);
   Solve (R3);
end Main;
