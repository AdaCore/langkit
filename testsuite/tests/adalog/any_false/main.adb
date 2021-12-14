with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that an Any containing a logic false and another rel:
--  1. Expands directly to the other rel
--  2. Solves correctly.

procedure Main is
   use T_Solver;
   use Refs;

   X : constant Refs.Logic_Var := Create ("X");
   R : constant Relation := R_Any ((Logic_False, X = 12));
begin
   Solve_All (R);
end Main;
