--  Check that the contradiction generated for the topo-sort failure in the
--  equation below correctly takes into account all cyclic dependencies between
--  the variables. The nested cycle "W <- Y, Y <- Z, Z <- W" used to be ignored
--  when first encountering the cycle "V <- W, W <-X, X <-V", which caused the
--  algorithm to create a contradiction preventing the overall solution to be
--  found.

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   V : constant Refs.Logic_Var := Create ("V");
   W : constant Refs.Logic_Var := Create ("W");
   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");
   Z : constant Refs.Logic_Var := Create ("Z");

   R : constant Relation :=
     R_All ((Propagate (W, V),
             Propagate (X, W),
             Propagate (V, X),
             Propagate (Y, W),
             Propagate (Z, Y),
             Propagate (W, Z),
             R_Any ((Logic_True,
                     Assign (Z, 1)))));
begin
   Solve_All (R);
end Main;
