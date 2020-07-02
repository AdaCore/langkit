with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   U : constant Analysis_Unit := Create_Context.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "a # foo"
                   & ASCII.LF & "b error # bar");
begin
   for C of U.Root.Children loop
      Put_Line (C.Image);

      for CC of C.Children_And_Trivia loop
         if CC.Kind = Child then
            Put_Line ("  Node: " & CC.Node.Image);
         else
            Put_Line
              ("  Trivia: " & Image (Text (CC.Trivia), With_Quotes => True));
         end if;
      end loop;

      New_Line;
   end loop;
   Put_Line ("main.adb: Done.");
end Main;
