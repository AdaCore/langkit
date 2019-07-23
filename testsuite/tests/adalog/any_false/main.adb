with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that an Any containing a logic false and another rel:
--  1. Expands directly to the other rel
--  2. Solves correctly

procedure Main is
   use T_Solver;
   use Refs;
begin
   declare
      X : Refs.Raw_Var := Create ("X");
      R : constant Relation :=
        R_Any ((Create_False, X = 12));
   begin
      Solve_All (R, Show_Relation => True);
   end;
end Main;
