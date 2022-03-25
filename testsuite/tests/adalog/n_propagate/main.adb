--  Check that the solver correctly handles an equation that involve a cycle in
--  variable definitions, when that cycle cannot be resolved.

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   function Add (Vals : Value_Array) return Integer
   is (Vals (1) + Vals (2));

   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");
   Z : constant Refs.Logic_Var := Create ("Z");

   --  Simple case: a N_Propagate relation depends on two variables defined by
   --  two separate relations.

   R1 : constant Relation :=
     R_All ((N_Propagate (Z, Combiner (Add'Access, 2, "Add"), (X, Y)),
             Assign (X, 1),
             Assign (Y, 2)));

   --  More complex case: the N_Propagate relation depends on itself, but an
   --  assignment allows the scheduler to break the cycle.

   R2 : constant Relation :=
     R_All ((N_Propagate (Z, Combiner (Add'Access, 2, "Add"), (X, Y)),
             Propagate (Z, Y),
             Assign (Z, 1),
             Assign (X, 0)));
begin
   Solve_All (R1);
   Solve_All (R2);
end Main;
