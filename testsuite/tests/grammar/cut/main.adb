with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   procedure Start (Label : String; New_Rule : Grammar_Rule);
   procedure Check (Label, Buffer : String);
   Rule : Grammar_Rule;

   -----------
   -- Start --
   -----------

   procedure Start (Label : String; New_Rule : Grammar_Rule) is
   begin
      Put_Line ((1 .. 79 => '#'));
      Put_Line ("# " & Label);
      Put_Line ((1 .. 79 => '#'));
      New_Line;
      Rule := New_Rule;
   end Start;

   -----------
   -- Check --
   -----------

   procedure Check (Label, Buffer : String) is
      U : constant Analysis_Unit := Create_Context.Get_From_Buffer
        (Filename => "buffer", Buffer => Buffer, Rule => Rule);
   begin
      Put_Line ("== " & Label & ": " & Buffer & " ==");
      New_Line;
      for D of U.Diagnostics loop
         Put (Image (D.Sloc_Range));
         Put (": ");
         Put_Line (Image (To_Text (D.Message)));
      end loop;
      U.Root.Print;
      New_Line;
   end Check;
begin
   Put_Line ("main.adb: Starting");
   New_Line;

   Start ("diag_cleanup", Diag_Cleanup_Root_Rule);
   Check ("diag_cleanup", "var foo()");

   Start ("multi", Multi_Root_Rule);
   Check ("complete case 1", "def a");
   Check ("complete case 2", "def a (b)");
   Check ("complete case 3", "def a (b) {c}");
   Check ("complete case 4", "var a");
   Check ("complete case 5", "var a (b)");
   Check ("complete case 6", "var a (b, c, d)");
   Check ("complete case 7", ". a (b)");
   Check ("complete case 8", ". a (b) {c}");
   Check ("complete case 9", ", a b");
   Check ("complete case 10", "(a) , b c");

   --  The def and var rules check that incomplete results are produced
   --  regarding the presence of several cut parsers.

   Check ("incomplete case 1", "def");
   Check ("incomplete case 2", "def a (b");
   Check ("incomplete case 3", "def a (b) {c");
   Check ("incomplete case 4", "def a (");
   Check ("incomplete case 5", "def a (b) {");
   Check ("incomplete case 6", "def a ( {");
   Check ("incomplete case 7", "def a (b {c");
   Check ("incomplete case 8", "var");
   Check ("incomplete case 9", "var a (");
   Check ("incomplete case 10", "var a ()");
   Check ("incomplete case 11", "var a (b, c, d");

   --  The dot rule checks that an incomplete result is produced if only the
   --  optional part can set the no_backtracing variable.

   Check ("incomplete case 12", ". a (b");
   Check ("incomplete case 13", ". a (b) {");
   Check ("incomplete case 14", ". a ( {");

   --  The comma rule is similar to the dot one but the optional part is at the
   --  beginning of the rule.
   Check ("incomplete case 15", ", b");
   Check ("incomplete case 16", "(a) , b");
   Check ("incomplete case 17", "(a , b");

   Start ("list", List_Root_Rule);
   Check ("list", "(");
   Check ("list", "( .");
   Check ("list", "( .a");
   Check ("list", "( .a )");
   Check ("list", "( .a .");
   Check ("list", "( .a .b");
   Check ("list", "( .a .b )");

   Put_Line ("main.adb: Done");
end Main;
