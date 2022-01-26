--  Check that the constant folding done on relations during the preparation
--  for the solver work correctly on Any/All relations that contain a single
--  sub-relation.

with Langkit_Support.Adalog; use Langkit_Support.Adalog;
with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

procedure Main is
   use T_Solver;
   use Refs;

   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");
   Z : constant Refs.Logic_Var := Create ("X");
begin
   Cst_Folding_Trace.Set_Active (True);

   Solve_All
     (R_All
        ((R_Any ((1 => R_All ((X = 1, Y = 2)))),
          Z = 3)));

   Solve_All
     (R_Any
        ((R_All ((1 => R_Any ((X = 1, X = 2)))),
          X = 3)));
end Main;
