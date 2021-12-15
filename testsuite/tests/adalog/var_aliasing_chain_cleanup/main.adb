with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that Unify works correctly, specially that variable aliasing is well
--  reset after evaluating a solution for chained aliases.

procedure Main is
   use T_Solver;
   use Refs;

   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");
   Z : constant Refs.Logic_Var := Create ("Z");

   R : constant Relation :=
     X = Y
     and ((X = Z and Z = 1)
          or (X = 10 and Z = 11));
begin
   Solve_All (R);
end Main;
