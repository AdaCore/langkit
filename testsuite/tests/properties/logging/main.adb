with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "a(c) b(a c) +c(a)");
begin
   GNATCOLL.Traces.Parse_Config_File;
   Put_Line ("main.adb: Running...");
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;

   U.Populate_Lexical_Env;
   Put_Line ("Calling P_Entity_Items..");

   declare
      Dummy : Ref_Array := U.Root.Child (1).As_Decl.P_Entity_Items;
   begin
      null;
   end;

   Put_Line ("main.adb: Done.");
end Main;
