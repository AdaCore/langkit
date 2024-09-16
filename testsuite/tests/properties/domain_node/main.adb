with Ada.Text_IO;    use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "main.txt", Buffer  => "example");

   Node  : constant Example := U.Root.As_Example;
   Dummy : Boolean;
begin
   if U.Has_Diagnostics then
      Put_Line ("Parsing errors...");
      return;
   end if;
   Dummy := Node.P_Test;
   Put_Line ("main.adb: Done.");
end Main;
