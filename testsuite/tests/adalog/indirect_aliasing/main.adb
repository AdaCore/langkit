with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that an atomic boolean rel solves correctly. No !

procedure Main is
   use T_Solver;
   use Refs;

   X: Raw_Var := Create("X");
   Y   : Raw_Var := Create("Y");
   Y_Ind : Raw_Var := Create("Y_Ind");
   Expr : Raw_Var := Create("Expr");

   R : constant Relation :=
       R_All (
         (X = Y, 
          Y_Ind = 2,
          Y = Y_Ind,
          Expr = 1,
          X = Expr));
begin
   GNATCOLL.Traces.Parse_Config_File;
   Solve_All (R, Show_Relation => True);
exception
   when E : others =>
      Put_Line (Exception_Message (E));
end Main;
