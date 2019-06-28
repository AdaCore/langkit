with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support;
with GNATCOLL.Traces;

--  Test domain primitive

procedure Main is
   use T_Solver; use Refs;
   R : constant Relation := R_All (No_Relation_Array);
begin
   GNATCOLL.Traces.Parse_Config_File;
   if not Solve_First (R) then
      Put_Line ("FAILED!");
   end if;
end Main;
