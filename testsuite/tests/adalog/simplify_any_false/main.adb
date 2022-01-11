--  Check that when the Simplify optimization replaces an alternative of the
--  top-level Any relation with a False atom, it actually removes this
--  alternative.
--
--  It used to leave the False atom while the solver assumes that Any relations
--  cannot contain False atoms alternatives.

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   function LT_100 (Value : Integer) return Boolean
   is (Value < 100);

   P_LT_100 : constant Predicate_Type'Class :=
     Predicate (LT_100'Access, "lt-100");

   X : constant Refs.Logic_Var := Create ("X");
   R : constant Relation :=
     (X = 120 and Predicate (X, P_LT_100)) or Logic_True;
begin
   Solve_All (R);
end Main;
