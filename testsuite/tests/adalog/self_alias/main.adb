with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Check that topological sort in the solver works even if a var is defined
--  twice in a solution.

procedure Main is
   use T_Solver; use Refs;

   X : constant Raw_Var := Create ("X");
   Y : constant Raw_Var := Create ("Y");
   R : constant Relation := X = X and X = 1 and Y = 1;
begin
   Solve_All (R);
end Main;
