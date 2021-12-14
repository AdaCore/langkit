with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test N predicates - in this case a predicate applying on two logic
--  variables.

procedure Main is

   use T_Solver, Refs, Solver_Ifc;

   function Double_Of (Vals : Value_Array) return Boolean
   is (Vals (1) = Vals (2) * 2);

   X : constant Refs.Logic_Var := Create ("X");
   Y : constant Refs.Logic_Var := Create ("Y");

   R3   : constant Relation :=
     "and" (+Create_N_Predicate
             ((X, Y),
              N_Predicate (Double_Of'Access, "Double_Of")),
            "and" (Domain (X, (1, 2, 3, 4, 5, 6)),
              Domain (Y, (1, 2, 3, 4, 5, 6))));
begin
   Solve_All (R3);
end Main;
