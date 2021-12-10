with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

with Support;

--  Test a combination of features at the same time:
--  * Predicates
--  * Custom bind
--  * Domains.

procedure Main is

   use T_Solver, Refs, Solver_Ifc;

   X : constant Raw_Var := Create ("X");
   Y : constant Raw_Var := Create ("Y");

   function Is_Even (Val : Integer) return Boolean is (Val mod 2 = 0);

   R3 : constant Relation :=
     Domain (X, (1, 2, 3, 4, 5, 6))
     and Propagate (X, Y, Conv => Support.Transformer_Singleton)
     and Predicate (X, Predicate(Is_Even'Access, "Is_Even"))
     and Domain (Y, (12, 18));

begin
   Solve_All (R3);
end Main;
