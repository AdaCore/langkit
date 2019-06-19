with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that the solver works properly when you have a predicate applied to a
--  variable *before* this variable can have a value, either through a domain
--  or through a bind/equality operation.

procedure Main is

   use Int_Solver, Refs;

   X : Raw_Var := Create ("X");

   R3 : constant Relation :=
     (Predicate (X, Is_Even)
      and Domain (X, (1, 2, 3, 4, 5, 6)));

begin
   Solve_All (R3, Show_Relation => True);
end Main;
