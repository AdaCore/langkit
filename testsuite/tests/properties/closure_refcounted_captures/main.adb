with Ada.Text_IO;    use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "example", Buffer  => "example");
begin
   Put_Line ("main.adb: Running...");
   Put_Line ("resolved : " & U.Root.As_Example.P_Resolve'Image);
   Put_Line ("main.adb: Done.");
end Main;
