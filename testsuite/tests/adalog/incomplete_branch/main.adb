with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that an equation resolves correctly even if not all variables are
--  defined in all branches.

procedure Main is
   use Int_Solver;
   use Refs;
begin
   GNATCOLL.Traces.Parse_Config_File;
    declare
      X : Raw_Var := Create ("X");
      Y : Raw_Var := Create ("Y");

      R : constant Relation :=
        ((X = 1 or (Predicate (X, Is_Even) and Y = 1)));

   begin
      Solve_All (R, Show_Relation => True);
   end;
end Main;
