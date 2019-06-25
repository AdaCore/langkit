with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

with Support; use Support;

procedure Main is
   use T_Solver, Refs;

   X : Raw_Var := Create ("X");
   Y : Raw_Var := Create ("Y");

   Relations : constant array (Positive range <>) of Relation :=
     (R_All ((Domain (X, (6, 9)),
              Domain (Y, (9, 16)),
              Square (X, 3),
              Square (4, Y))),

      "and" (Domain (Y, (2, 3)), Square (X, 4)),

      R_All ((Support.Is_Even (X),
              Square (X, 3),
        Square (X, Y))),

      R_All ((Domain (X, (1, 2, 3, 4, 5, 6, 7, 8)),
              Square (X, Y)))
     );
begin
   for R of Relations loop
      Put_Line ((1 .. 72 => '='));
      New_Line;
      Solve_All (R, Show_Relation => True);
   end loop;
end Main;
