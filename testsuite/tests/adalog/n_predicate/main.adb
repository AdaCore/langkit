with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

with Support; use Support;

--  Test N predicates - in this case a predicate applying on two logic
--  variables.

procedure Main is

   use Int_Solver, Refs;

   X : Raw_Var := Create ("X");
   Y : Raw_Var := Create ("Y");
   R3   : constant Relation :=
     "and" (+Create_N_Predicate ((X, Y), Pred'(null record)),
            "and" (Domain (X, (1, 2, 3, 4, 5, 6)),
              Domain (Y, (1, 2, 3, 4, 5, 6))));
begin
   Solve_All (R3, Show_Relation => True);
end Main;
