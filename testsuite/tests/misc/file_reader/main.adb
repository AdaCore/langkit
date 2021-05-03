with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Errors;       use Langkit_Support.Errors;
with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;
with Langkit_Support.Text;         use Langkit_Support.Text;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;

with Pkg; use Pkg;

procedure Main is
   Ctx : Analysis_Context;
   U   : Analysis_Unit;

   procedure Put_Title (Label : String);

   procedure Parse (Filename, Charset : String);
   --  Helper to parse a unit and print its diagnostic/its text buffer

   ---------------
   -- Put_Title --
   ---------------

   procedure Put_Title (Label : String) is
   begin
      Put_Line (Label);
      Put_Line ((1 .. Label'Length => '='));
      New_Line;
   end Put_Title;

   -----------
   -- Parse --
   -----------

   procedure Parse (Filename, Charset : String) is
   begin
      Put_Title ("Parsing " & Filename);
      U := Ctx.Get_From_File (Filename, Charset);
      if U.Has_Diagnostics then
         Put_Line ("Errors:");
         for D of U.Diagnostics loop
            Put_Line ("  " & U.Format_GNU_Diagnostic (D));
         end loop;
      else
         Put_Line ("Success: " & Image (U.Text, With_Quotes => True));
      end if;
      New_Line;
   end Parse;

begin
   Put_Line ("main.adb: Starting...");

   --  Create a context with our file reader

   declare
      FR : constant File_Reader_Reference :=
        Create_File_Reader_Reference (My_File_Reader'(null record));
   begin
      Ctx := Create_Context (File_Reader => FR);
   end;

   --  Check the file reader is used appropriately when reading files, and that
   --  common errors are properly reported.

   Parse ("foo.txt", "");
   Parse ("error.txt", "some-charset");
   Parse ("direct-ok.txt", "ascii");
   Parse ("direct-no-such-file.txt", "ascii");
   Parse ("direct-bad-charset.txt", "some-charset");
   Parse ("direct-decoding-error.txt", "ascii");

   --  Check that the use of parsing APIs with buffers is rejected

   Put_Title ("Using buffer-based parsing APIs");

   Put_Line ("Get_From_Buffer:");
   begin
      U := Ctx.Get_From_Buffer
        (Filename => "from_buffer.txt", Buffer => "example");
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Precondition_Failure: " & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("Reparse:");
   begin
      U.Reparse (Buffer => "example");
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Precondition_Failure: " & Exception_Message (Exc));
   end;
   New_Line;

   --  Check that the use of the rewriting API is rejected

   Put_Title ("Using the rewriting API");
   Put_Line ("Start_Rewriting:");
   declare
      Dummy : Rewriting_Handle;
   begin
      Dummy := Start_Rewriting (Ctx);
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Precondition_Failure: " & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("main.adb: Done.");
end Main;
