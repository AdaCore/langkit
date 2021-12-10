with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test true relation

procedure Main is
   use T_Solver, Refs;

   X : constant Raw_Var := Create ("X");
   R : constant Relation := Domain (X, (1, 2, 3, 4, 5, 6)) and +Create_True;
begin
   Solve_All (R);
end Main;
