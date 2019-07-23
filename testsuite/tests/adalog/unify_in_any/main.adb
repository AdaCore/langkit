with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test that Unify works correctly, specially that variable aliasing is well
--  reset after evaluating a solution.

procedure Main is
   use T_Solver;
   use Refs;

   function Abs_Equal (Vals : T_Solver.Value_Array) return Boolean is
     (abs Vals (1) = abs Vals (2));

   X : Refs.Raw_Var := Create ("X");
   Y : Refs.Raw_Var := Create ("Y");
   R : constant Relation :=
     R_All ((
            R_Any ((X = Y, Create_True)),
            R_Any ((Domain (X, (1, 2)), Create_True)),
            R_Any ((Domain (Y, (-1, -2)), Create_True)),
            N_Predicate ((X, Y), N_Predicate (Abs_Equal'Access))
     ));
begin
   GNATCOLL.Traces.Parse_Config_File;
   Solve_All (R, Show_Relation => True);
end Main;
