--  Check that the solver has correctly assigned ids to any variable
--  referenced in any atom kind that can have dependencies, when no other atom
--  gives them a value. If not done correctly, the solver will fail with a
--  CONSTRAINT_ERROR during toposort, trying to fetch a variable with a id 0
--  in a 1-based array.

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   function Prop (Dummy : Integer) return Integer is (0);
   --  Dummy function used as Propagate

   function N_Prop (Dummy : Value_Array) return Integer is (0);
   --  Dummy function used as N_Propagate

   function Pred (Dummy : Integer) return Boolean is (True);
   --  Dummy function used as Predicate

   function N_Pred (Dummy : Value_Array) return Boolean is (True);
   --  Dummy function used as N_Predicate

   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");
   Z : constant Refs.Logic_Var := Create ("Z");

   R_Unify : constant Relation := Unify (X, Y);

   R_Prop : constant Relation :=
     Propagate (X, Y, Converter (Prop'Access, "Prop"));

   R_N_Prop : constant Relation :=
     N_Propagate (X, Combiner (N_Prop'Access, 2, "N_Prop"), (Y, Z));

   R_Pred : constant Relation :=
     Predicate (X, Predicate (Pred'Access, "Pred"));

   R_N_Pred : constant Relation :=
     N_Predicate ((X, Y), N_Predicate (N_Pred'Access, 2, "N_Pred"));

begin
   Solve_All (R_Unify);
   Solve_All (R_Prop);
   Solve_All (R_N_Prop);
   Solve_All (R_Pred);
   Solve_All (R_N_Pred);
end Main;
