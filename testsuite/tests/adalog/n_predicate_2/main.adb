with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

with Support; use Support;

--  Test N predicates - in this case a predicate applying on two logic
--  variables.

procedure Main is

   use T_Solver, Refs, Solver_Ifc;

   X  : constant Raw_Var := Create ("X");
   Y  : constant Raw_Var := Create ("Y");
   R3 : constant Relation :=
     R_All ((X = 2, Y = 1, +Create_N_Predicate ((X, Y), Pred_Singleton)));
begin
   Solve_All (R3);
end Main;
