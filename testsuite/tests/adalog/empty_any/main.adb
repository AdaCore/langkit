with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;

--  Test domain primitive

procedure Main is
   use T_Solver;
   R : constant Relation := R_Any (No_Relation_Array);
begin
   if not Solve_First (R) then
      Put_Line ("FAILED!");
   end if;
end Main;
