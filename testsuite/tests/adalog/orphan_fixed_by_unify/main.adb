--  Check that the solver is able to generate a correct contradiction for
--  a topo sort failure, when the only way to resolve the missing dependencies
--  is to include a block that unifies the unset variable to another variable.
--  This used to only work if the unify atom would have the unset variable
--  appear in its right-hand side. Now, this will also work if it appears on the
--  left-hand side.

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");
   Z : constant Refs.Logic_Var := Create ("Z");

   R : constant Relation :=
     R_All ((Propagate (Y, X),
             R_Any ((Logic_True,
                     R_All ((Unify (Y, Z), Assign (Z, 1)))))));
begin
   Solve_All (R);
end Main;
