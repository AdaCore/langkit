--  Check that topo sort properly cleans up its re-used data structures between
--  two runs. It used not to, which could trigger a crash.

with Langkit_Support.Adalog;
with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   function Is_Positive (Value : Integer) return Boolean
   is (Value > 0);

   P_Is_Positive : constant Predicate_Type'Class :=
     Predicate (Is_Positive'Access, "is_positive");

   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");

   All_1 : constant Relation :=
     R_All ((Predicate (X, P_Is_Positive), Y = 1));
   All_2 : constant Relation :=
     R_All ((Predicate (X, P_Is_Positive), Y = 2, X = 3));
begin
   Solve_All (R_Any ((All_1, All_2)));
end Main;
