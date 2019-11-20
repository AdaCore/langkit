with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Check that ``Unify`` is properly reset across disjunction branches

procedure Main is
   use T_Solver; use Refs;

   function Is_Odd (V : Integer) return Boolean is (V mod 2 = 1);

   X : Raw_Var := Create ("X");
   Y : Raw_Var := Create ("Y");

   R : constant Relation :=
     (X = Y and X = 1 and Y = 1)
     or (X = 3 and Y = 12);
begin
   Solve_All (R, Show_Relation => True);
end Main;
