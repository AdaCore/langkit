--  Check that predicates are evaluated in the same order as they appear in the
--  original relation tree.

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   function Is_Positive (Value : Integer) return Boolean
   is (Value > 0);

   function LT_100 (Value : Integer) return Boolean
   is (100 / Value >= 1);

   P_Is_Positive : constant Predicate_Type'Class :=
     Predicate (Is_Positive'Access, "is_positive");
   P_LT_100 : constant Predicate_Type'Class :=
     Predicate (LT_100'Access, "lt-100");

   X : constant Refs.Logic_Var := Create ("X");

   --  P_Is_Positive should reject X = 0, and thus should avoid a crash in
   --  P_LT_100. If predicates are re-ordered, the crash will happen.

   R : constant Relation :=
     R_All ((Domain (X, (-1, 0, 27, 100)),
             Predicate (X, P_Is_Positive),
             Predicate (X, P_LT_100)));
begin
   Solve_All (R);
end Main;
