with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that an atomic boolean rel solves correctly. No!

procedure Main is
   use T_Solver;
   use Refs;

   X : Raw_Var := Create("X");
   Y : Raw_Var := Create("Y");
   Z : Raw_Var := Create("Z");
   A : Raw_Var := Create("A");
   B : Raw_Var := Create("B");
   C : Raw_Var := Create("C");
   D : Raw_Var := Create("D");
   E : Raw_Var := Create("E");

   R : constant Relation :=
       R_All ((X = Y, 
               Y = Z,
               A = Z,
               E = A,
               B = C,
               A = C,
               D = X,
               X = 1));
begin
   GNATCOLL.Traces.Parse_Config_File;
   Solve_All (R, Show_Relation => True);
exception
   when E : others =>
      Put_Line (Exception_Message (E));
end Main;
