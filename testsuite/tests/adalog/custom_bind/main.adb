with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test propagation with a custom bind operator

procedure Main is
   use T_Solver, Refs;

   X : Raw_Var := Create ("X");
   Y : Raw_Var := Create ("Y");

   function Square (S : Integer) return Integer is (S ** 2);
   R : constant Relation :=
     ((Domain (X, (1, 2, 3, 4, 5, 6)))
      and (Propagate (X, Y, Converter (Square'Access, "Square"))));

   R2 : constant Relation :=
     ((Domain (X, (1, 2, 3, 4, 5, 6))
      and (Propagate (X, Y, Converter (Square'Access, "Square"))))
      and Y = 36);
begin
   Solve_All (R, Show_Relation => True);
   Solve_All (R2, Show_Relation => True);
end Main;


