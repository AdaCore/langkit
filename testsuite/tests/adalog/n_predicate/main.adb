with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test N predicates - in this case a predicate applying on two logic
--  variables.

procedure Main is

   use T_Solver, Refs;

   function Times_3 (Val : Integer) return Integer
   is (Val * 3);

   function Double_Of (Vals : T_Solver.Value_Array) return Boolean
   is (Vals (1) = Vals (2) * 2);

   X : Raw_Var := Create ("X");
   Y : Raw_Var := Create ("Y");

   R3   : constant Relation :=
     "and" (+Create_N_Predicate 
             ((X, Y),
              N_Predicate (Double_Of'Access, "Double_Of")),
            "and" (Domain (X, (1, 2, 3, 4, 5, 6)),
              Domain (Y, (1, 2, 3, 4, 5, 6))));
begin
   Solve_All (R3, Show_Relation => True);
end Main;
