with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with Prettier_Ada.Documents; use Prettier_Ada.Documents;

with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Unparsing;
use Langkit_Support.Generic_API.Unparsing;

with Libfoolang.Generic_API; use Libfoolang.Generic_API;

procedure Default_Config is
   Context   : constant Lk_Context := Create_Context (Self_Id);
   Unit      : constant Lk_Unit := Context.Get_From_File ("example.txt");
   Config    : constant Unparsing_Configuration :=
     Default_Unparsing_Configuration (Self_Id);
   Doc       : Document_Type;
begin
   if Unit.Has_Diagnostics then
      for D of Unit.Diagnostics loop
         Put_Line (Unit.Format_GNU_Diagnostic (D));
      end loop;
      raise Program_Error;
   end if;

   Doc := Unparse_To_Prettier (Unit.Root, Config);
   Put_Line (Format (Doc));
   New_Line;

   Put_Line ("main.adb: done");
end Default_Config;
