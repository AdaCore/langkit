with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that Member primitives goes along correctly with the "=" operator

procedure Main is
   use T_Solver;
   use Refs;
begin
   GNATCOLL.Traces.Parse_Config_File;
   declare
      X : Raw_Var := Create ("X");
      R : constant Relation :=
        (Domain (X, (1, 2, 3, 4, 5, 6)) or X = 7 or X = 8);
   begin
      Solve_All (R);
   end;

   declare
      X : Raw_Var := Create ("X");
      Y : Raw_Var := Create ("Y");

      X_Constraint : constant Relation :=
        X = 1 or X = 2 or X = 3 or X = 4 or X = 5 or X = 6;

      Y_Constraint : constant Relation :=
         Y = 3 or Y = 2 or Y = 1;

      R : constant Relation :=
         X_Constraint and X = Y and Y_Constraint;
   begin
      Solve_All (R);
   end;

end Main;
