with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that an atomic boolean rel solves correctly

procedure Main is
   use T_Solver;
   use Refs;

   X : Raw_Var := Create ("X");

   function Is_Even (V : Integer) return Boolean is (V mod 2 = 0);

   R : constant Relation :=
     (Predicate (X, Predicate(Is_Even'Access, "Is_Even")));
begin
   Solve_All (R, Show_Relation => True);
exception
   when E : others =>
      Put_Line (Exception_Message (E));
end Main;
