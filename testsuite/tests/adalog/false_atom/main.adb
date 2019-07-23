with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that an atomic boolean rel solves correctly

procedure Main is
   use T_Solver;
   use Refs;
   R : constant Relation := Create_False;
begin
   Solve_All (R, Show_Relation => True);
end Main;
