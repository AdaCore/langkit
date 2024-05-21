with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "example example");
begin
   GNATCOLL.Traces.Parse_Config_File;
   Put_Line ("main.adb: Running...");
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;

   declare
      A_Block      : constant Block := U.Root.As_Block;
      An_Example_1 : constant Example1 := A_Block.F_Example1;
      An_Example_2 : constant Example2 := A_Block.F_Example2;
   begin
      --  The property call should only appear once in the trace
      Put_Line (Integer'Image (A_Block.P_Foo (1)));
      Put_Line (Integer'Image (A_Block.P_Foo (1)));

      --  The property call should appear twice in the trace
      Put_Line (Integer'Image (An_Example_1.P_Foo (1)));
      Put_Line (Integer'Image (An_Example_1.P_Foo (1)));

      --  The property call should only appear once in the trace
      Put_Line (Integer'Image (An_Example_2.P_Foo (1)));
      Put_Line (Integer'Image (An_Example_2.P_Foo (1)));
   end;

   Put_Line ("main.adb: Done.");
end Main;
