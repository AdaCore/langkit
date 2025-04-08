with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with Prettier_Ada.Documents; use Prettier_Ada.Documents;

with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Unparsing;
use Langkit_Support.Generic_API.Unparsing;

with Libfoolang.Generic_API; use Libfoolang.Generic_API;

procedure Main is
   Config : constant Unparsing_Configuration :=
     Default_Unparsing_Configuration (Self_Id);
   Buffer : constant String :=
      "def {1};" & ASCII.LF
      & "def (a);" & ASCII.LF;

   Ctx : constant Lk_Context := Create_Context (Self_Id);
   U   : constant Lk_Unit :=
     Ctx.Get_From_Buffer ("main.txt", Buffer => Buffer);
begin
   Put_Line ("main.adb: starting...");

   if U.Has_Diagnostics then
      Put_Line ("Errors:");
      for D of U.Diagnostics loop
         Put_Line (U.Format_GNU_Diagnostic (D));
      end loop;
      raise Program_Error;
   end if;

   declare
      Doc       : constant Document_Type :=
        Unparse_To_Prettier (U.Root, Config);
      Formatted : constant Unbounded_String :=
        Format (Doc, Default_Format_Options);
   begin
      Put_Line (Formatted);
   end;
   New_Line;
   Put_Line ("main.adb: done.");
end Main;
