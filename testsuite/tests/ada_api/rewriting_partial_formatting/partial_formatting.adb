with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Diagnostics;           use Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Rewriting;
use Langkit_Support.Generic_API.Rewriting;
with Langkit_Support.Generic_API.Unparsing;
use Langkit_Support.Generic_API.Unparsing;
with Langkit_Support.Text;                  use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

with Libfoolang.Generic_API; use Libfoolang.Generic_API;

with Prettier_Ada.Documents;

procedure Partial_Formatting is
   procedure Test (Title : String; Options : Rewriting_Options);
   --  Run the main test procedure using the given rewriting options.
   --  Start by printing the given Title.

   ----------
   -- Test --
   ----------

   procedure Test (Title : String; Options : Rewriting_Options) is
      Ctx     : constant Analysis_Context := Create_Context;
      U       : constant Analysis_Unit :=
        Get_From_File (Ctx, "partial_formatting.txt");
      RH      : Rewriting_Handle;
      Dummy_R : Apply_Result;
   begin
      Put_Line (Title);
      Put_Line ("===========================");
      New_Line;

      if Has_Diagnostics (U) then
         Put_Line ("Errors:");
         for D of Diagnostics (U) loop
            Put_Line (Format_GNU_Diagnostic (U, D));
         end loop;
         return;
      end if;

      RH := Start_Rewriting (To_Generic_Context (Ctx), Options);

      Put_Line ("Adding new definition ...");
      Insert_After
        (First_Child (Handle (To_Generic_Node (U.Root))),
         Create_From_Template
           (RH,
            "def f1(a: Int, c: Int = 0): Int {{ print(a, c); }}",
            (1 .. 0 => <>),
            To_Generic_Grammar_Rule (Fun_Decl_Rule)));

      declare
         Last_Child_RH : constant Node_Rewriting_Handle :=
           Last_Child (Handle (To_Generic_Node (U.Root)));
      begin
         Remove_Child (Last_Child_RH);
         Insert_Before
           (Last_Child (Handle (To_Generic_Node (U.Root))), Last_Child_RH);
      end;

      New_Line;
      Put_Line ("Applying the diff...");
      Dummy_R := Apply (RH);
      if not Dummy_R.Success then
         Print (Dummy_R.Diagnostics);
      end if;

      New_Line;
      Put_Line ("Quoting source buffer for rewritten unit...");
      Put_Line (Encode (Text (U), "ASCII"));
      New_Line;
   end Test;

   Diagnostics : Diagnostics_Vectors.Vector;
   Config      : Unparsing_Configuration;
begin
   Config := Load_Unparsing_Config (Self_Id, "config.json", Diagnostics);
   if Config = No_Unparsing_Configuration then
      Print (Diagnostics);
      raise Program_Error;
   end if;

   --  Check first the output when no rewriting options are specified:
   --  original formatting should be preserved as much as possible on the
   --  nodes have a source correspondance. However the new nodes won't be
   --  formatted.

   Test ("No rewriting options", No_Rewriting_Options);

   --  Use the default unparsing config and formatting options: the default
   --  config is written without any "independent_lines" list: this implies
   --  that a single rewriting tile will span the whole unit, and thus it
   --  will be completely reformatted (no original formatting will be
   --  preserved).
   --
   --  Moreover, since default format options are defined in the config with a
   --  small line width, the output should not contain long lines.

   Test ("Default rewriting options", Default_Rewriting_Options (Self_Id));

   --  Use a custom unparsing configuration and format options that should make
   --  the rewritten parts of the tree look good, while leaving the untouched
   --  ones with their original formatting.

   Test
     ("Custom rewriting options",
      Custom_Rewriting_Options
        (Config, Prettier_Ada.Documents.Default_Format_Options));

   Put_Line ("partial_formatting.adb: Done.");
end Partial_Formatting;
