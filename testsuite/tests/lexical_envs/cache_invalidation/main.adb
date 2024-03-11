with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;

with GNATCOLL.Traces;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx    : constant Analysis_Context := Create_Context;
   Unit   : Analysis_Unit;
   Node   : Example;
   Symbol : Unbounded_Text_Type := To_Unbounded_Text ("foo");
begin
   GNATCOLL.Traces.Parse_Config_File;

   Put_Line ("main.adb: Starting...");

   Unit := Ctx.Get_From_Buffer (Filename => "example", Buffer => "example");
   if Unit.Has_Diagnostics then
      raise Program_Error;
   end if;

   Node := Unit.Root.As_Example;

   for I in 1 .. 100 loop
      declare
         Lookup_Result : constant Example := Node.P_Lookup (Symbol);
      begin
         if Lookup_Result /= Node then
            raise Program_Error with "Unexpected lookup result";
         end if;
         Append (Symbol, "o");
      end;
   end loop;

   Put_Line ("main.adb: Done.");
end Main;
