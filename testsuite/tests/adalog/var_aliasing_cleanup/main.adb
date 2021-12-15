with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that Unify works correctly, specially that variable aliasing is well
--  reset after evaluating a solution.

procedure Main is
   use T_Solver;
   use Refs;

   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");
   R : constant Relation :=
     ((X = 12 and Y = 15)
      or (X = 18 and X = Y)
      or (X = 2 and Y = 3 and X = Y))
     and Domain (Y, (15, 16, 16, 18, 19));
begin
   Solve_All (R);
end Main;
