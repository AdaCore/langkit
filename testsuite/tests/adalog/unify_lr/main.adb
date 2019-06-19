with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

procedure Main is
   use Int_Solver; use Refs;

   X : Raw_Var := Create ("X");
   Y : Raw_Var := Create ("Y");

   function Is_Even (V : Raw_Var) return Relation is
     (Predicate (V, Langkit_Support.Adalog.Main_Support.Is_Even));

   Relations : constant array (Positive range <>) of Relation :=
     ("and" (Domain (X, (1, 2)),
              "and" (Domain (Y, (2, 3)), X = Y)),
      "and" (Domain (X, (1, 2)), X = Y),
      "and" (Domain (X, (1, 2)), Y = X),
      "and" (Domain (X, (1, 2)),
              "and" (Is_Even (Y), X = Y)),
      "and" (Domain (Y, (1, 2)),
              "and" (Is_Even (X), X = Y)));
begin
   GNATCOLL.Traces.Parse_Config_File;
   for R of Relations loop
      Put_Line ((1 .. 72 => '='));
      New_Line;
      Solve_All (R, Show_Relation => True);
   end loop;
end Main;
