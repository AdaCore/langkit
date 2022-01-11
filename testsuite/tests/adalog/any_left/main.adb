--  Check that the "any left" warning works as expected

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

procedure Main is
   use T_Solver, Refs, Solver_Ifc;

   function Is_Even (I : Integer) return Boolean
   is (I mod 2 = 0);
   P_Is_Even : constant Predicate_Type'Class :=
     Predicate (Is_Even'Access, "Is_Even");

   X : constant Refs.Logic_Var := Create ("X");

   R1 : constant Relation :=
     R_All ((X = 12, Predicate (X, P_Is_Even)));
   --  No Any from the beginning

   R2 : constant Relation :=
     R_All ((Domain (X, (1, 2, 3)), Predicate (X, P_Is_Even)));
   --  No Any left after simplification

   R3 : constant Relation :=
     R_All ((Domain (X, (1, 2, 3, 4)), Predicate (X, P_Is_Even)));
   --  One Any left after simplification

   R4 : constant Relation :=
     Domain (X, (1, 2, 3, 4));
   --  Only one root any

begin
   Solve_All (R1);
   Solve_All (R2);
   Solve_All (R3);
   Solve_All (R4);
end Main;
