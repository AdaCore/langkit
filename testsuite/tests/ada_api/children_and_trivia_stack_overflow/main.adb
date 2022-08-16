with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   U : constant Analysis_Unit :=
      Create_Context.Get_From_File ("main.txt");

   Child_Counter, Trivia_Counter : Natural := 0;
begin
   --  This call to Children_And_Trivia raises a STORAGE_ERROR (stack overflow)
   --  if the nodes are stored on the stack.

   for N of U.Root.Children_And_Trivia loop
      if N.Kind = Child then
         Child_Counter := Child_Counter + 1;
      else
         Trivia_Counter := Trivia_Counter + 1;
      end if;
   end loop;
   Put_Line ("Child:" & Child_Counter'Image);
   Put_Line ("Trivia:" & Trivia_Counter'Image);
   Put_Line ("main.adb: Done.");
end Main;
