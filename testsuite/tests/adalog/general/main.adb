with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

with Support; use Support;

--  Test a combination of features at the same time:
--  * Predicates
--  * Custom bind
--  * Domains.

procedure Main is

   use T_Solver, Refs;

   X : Raw_Var := Create ("X");
   Y : Raw_Var := Create ("Y");

   function Is_Even (Val : Integer) return Boolean is (Val mod 2 = 0);

   R3 : constant Relation :=
     "and" (Domain (X, (1, 2, 3, 4, 5, 6)),
            "and" (Propagate (X, Y, Conv => Support.Transformer'(null record)),
                     "and" (
                Predicate (X, Predicate(Is_Even'Access, "Is_Even")),
                Domain (Y, (12, 18)))));

begin
   Solve_All (R3, Show_Relation => True);
end Main;
