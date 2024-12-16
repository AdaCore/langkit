with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Filename : constant String := "main.txt";

   Buffer_A : constant String := "example";
   Buffer_B : constant String := "example example";

   Ctx : constant Analysis_Context := Create_Context;

   procedure Run (Buffer : String);

   procedure Run (Buffer : String) is
      Dummy : Analysis_Unit;
   begin
      Put_Line ("About to parse: " & Buffer);
      Dummy := Ctx.Get_From_Buffer (Filename, Buffer => Buffer);
      New_Line;
   end Run;

begin
   --  Check twice that 1) reparsing happens when the source buffer changes and
   --  that 2) reparsing does not happen when the source buffer does not
   --  change.

   Run (Buffer_A);
   Run (Buffer_A);
   Run (Buffer_B);
   Run (Buffer_B);
   Run (Buffer_A);

   --  Check that reparsing happens even when the source buffer apparently does
   --  not change (is empty) because there is a reading/decoding error.

   declare
      Dummy : Analysis_Unit;
   begin
      Put_Line ("Reparse from a non-existing file #1");
      Dummy := Ctx.Get_From_File (Filename, Reparse => True);
      Put_Line ("Reparse from a non-existing file #2");
      Dummy := Ctx.Get_From_File (Filename, Reparse => True);
      New_Line;
   end;

   Put_Line ("main.adb: Done.");
end Main;
