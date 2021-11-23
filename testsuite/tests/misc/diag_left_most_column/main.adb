with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

with Langkit_Support.Diagnostics.Output;
use Langkit_Support.Diagnostics;
use Langkit_Support.Diagnostics.Output;


procedure Main is
   U : constant Analysis_Unit := Create_Context.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "test" & ASCII.LF);

   function Visit (N : Foo_Node'Class) return Visit_Status;
   --  Callback for Libfoolang.Analysis.Traverse

   -----------
   -- Visit --
   -----------

   function Visit (N : Foo_Node'Class) return Visit_Status is
      Diag : constant Diagnostic := Create
        (Sloc_Range => N.Sloc_Range,
         Message    => "All clear!");
   begin
      Print_Diagnostic (Diag, N.Unit, "main.txt");
      return Over;
   end Visit;

begin
   Put_Line ("Printing a diagnostic which ends at column 1 shouldn't crash:");
   U.Root.Traverse (Visit'Access);
   Put_Line ("main.adb: Done.");
end Main;
