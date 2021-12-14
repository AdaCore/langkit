with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that indirect (chained) aliasing works correctly

procedure Main is
   use T_Solver;
   use Refs;

   X     : constant Refs.Logic_Var := Create("X");
   Y     : constant Refs.Logic_Var := Create("Y");
   Y_Ind : constant Refs.Logic_Var := Create("Y_Ind");
   Expr  : constant Refs.Logic_Var := Create("Expr");

   R : constant Relation :=
       R_All (
         (X = Y,
          Y_Ind = 2,
          Y = Y_Ind,
          Expr = 1,
          X = Expr));
begin
   Solve_All (R);
end Main;
