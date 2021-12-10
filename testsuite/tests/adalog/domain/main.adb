with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test domain primitive

procedure Main is
   use T_Solver; use Refs;

   X : constant Raw_Var := Create ("X");
   R : constant Relation := Domain (X, (1, 2, 3, 4, 5, 6));
begin
   Solve_All (R);
end Main;
