with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Check that topological sort in the solver works even if a var is defined
--  twice in a solution.

procedure Main is
   use T_Solver; use Refs;

   function Is_Odd (V : Integer) return Boolean is (V mod 2 = 1);

   X : Raw_Var := Create ("X");
   R : constant Relation := R_All
     ((Domain (X, (1, 2, 3, 4, 5, 6)),
       Assign (X, 1),
       Predicate (X, Predicate (Is_Odd'Access, "Is_Odd"))));
begin
   Solve_All (R, Show_Relation => True);
end Main;
