with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNATCOLL.Traces;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that indirect (chained) aliasing works correctly

procedure Main is
   use T_Solver;
   use Refs;

   X     : constant Raw_Var := Create("X");
   Y     : constant Raw_Var := Create("Y");
   Y_Ind : constant Raw_Var := Create("Y_Ind");
   Expr  : constant Raw_Var := Create("Expr");

   R : constant Relation :=
       R_All (
         (X = Y,
          Y_Ind = 2,
          Y = Y_Ind,
          Expr = 1,
          X = Expr));
begin
   GNATCOLL.Traces.Parse_Config_File;
   Solve_All (R);
exception
   when E : others =>
      Put_Line (Exception_Message (E));
end Main;
