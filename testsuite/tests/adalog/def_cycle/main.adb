--  Check that the solver correctly handles an equation that involve a cycle in
--  variable definitions, when that cycle can be resolved anyway.

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");
   R : constant Relation :=
     R_All ((Propagate (X, Y),
             Propagate (Y, X),
             Assign (X, 1)));
begin
   Solve_All (R);
end Main;
