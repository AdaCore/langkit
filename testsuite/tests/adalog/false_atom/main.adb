with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that an atomic boolean rel solves correctly

procedure Main is
   use T_Solver;
   R : constant Relation := Logic_False;
begin
   Solve_All (R);
end Main;
