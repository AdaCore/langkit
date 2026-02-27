with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with Prettier_Ada.Documents; use Prettier_Ada.Documents;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Unparsing;
use Langkit_Support.Generic_API.Unparsing;

with Libfoolang.Generic_API; use Libfoolang.Generic_API;
with Libfoolang.Generic_API.Unparsing;

procedure Builtin_Overriding is

   package Unp renames Libfoolang.Generic_API.Unparsing;

   Context     : constant Lk_Context := Create_Context (Self_Id);
   Unit        : constant Lk_Unit :=
     Context.Get_From_File ("overridings/example.txt");
   Diagnostics : Diagnostics_Vectors.Vector;
   Config      : constant Unparsing_Configuration :=
     Load_Unparsing_Config
       (Language    => Self_Id,
        Filename    => Unp.Default_Configuration_Filename,
        Diagnostics => Diagnostics,
        Overridings => (1 => Unp.Builtin_Overridings.Some_Overriding));
   Doc         : Document_Type;
begin
   if Unit.Has_Diagnostics then
      for D of Unit.Diagnostics loop
         Put_Line (Unit.Format_GNU_Diagnostic (D));
      end loop;
      raise Program_Error;
   end if;
   if not Diagnostics.Is_Empty then
      Print (Diagnostics);
      raise Program_Error;
   end if;

   Doc := Unparse_To_Prettier (Unit.Root, Config);
   Put_Line (Format (Doc));
   New_Line;

   Put_Line ("main.adb: done");
end Builtin_Overriding;
