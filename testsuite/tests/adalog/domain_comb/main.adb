with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that Unify works correctly, specially that variable aliasing is well
--  reset after evaluating a solution.

procedure Main is
   use T_Solver;
   use Refs;
begin
   declare
      X : Refs.Raw_Var := Create ("X");
      R : constant Relation :=
        Domain (X, (1, 2, 3, 4, 5, 6)) and Domain (X, (3, 4, 5));
   begin
      Solve_All (R, Show_Relation => True);
   end;
end Main;
