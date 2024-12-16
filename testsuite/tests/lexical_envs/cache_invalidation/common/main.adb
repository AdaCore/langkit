with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;

with GNATCOLL.Traces;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx    : constant Analysis_Context := Create_Context;
   Unit   : Analysis_Unit;

   procedure Perform_Lookups (Count : Natural);
   --  Call the ``P_Lookup`` property on the root node of ``Unit`` for the
   --  given amount of times.

   ---------------------
   -- Perform_Lookups --
   ---------------------

   procedure Perform_Lookups (Count : Natural) is
      Node   : constant Example := Unit.Root.As_Example;
      Symbol : Unbounded_Text_Type := To_Unbounded_Text ("foo");
   begin
      for I in 1 .. Count loop
         declare
            Lookup_Result : constant Example := Node.P_Lookup (Symbol);
         begin
            if Lookup_Result /= Node then
               raise Program_Error with "Unexpected lookup result";
            end if;
            Append (Symbol, "o");
         end;
      end loop;
   end Perform_Lookups;
begin
   GNATCOLL.Traces.Parse_Config_File;

   Put_Line ("main.adb: Starting...");

   Unit := Ctx.Get_From_Buffer (Filename => "example", Buffer => "example");
   if Unit.Has_Diagnostics then
      raise Program_Error;
   end if;

   Perform_Lookups (100);

   Put_Line ("main.adb: Reparsing...");

   --  Check that reparsing correctly clears the lexical env lookup caches
   --  *before* destroying the unit. If it is not the case, valgrind will
   --  catch an invalid read at this point.
   Unit.Reparse (Buffer => "example ");

   --  Also check that cache collection works fine on the reparsed unit
   Perform_Lookups (40);

   Put_Line ("main.adb: Done.");
end Main;
