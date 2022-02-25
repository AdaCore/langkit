with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Main_Support;
use Langkit_Support.Adalog.Main_Support.T_Solver;

procedure Main is
   Null_Rel : Relation;
begin
   Inc_Ref (Null_Rel);
   Put_Line (Image (Null_Rel));
   Dec_Ref (Null_Rel);
end Main;
