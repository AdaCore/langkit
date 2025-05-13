with Ada.Directories;       use Ada.Directories;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Langkit_Support.Errors;       use Langkit_Support.Errors;
with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;
with Langkit_Support.Slocs;        use Langkit_Support.Slocs;
with Langkit_Support.Text;         use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Pkg;      use Libfoolang.Pkg;

procedure Main is
   Ctx : Analysis_Context;
   U   : Analysis_Unit;

   procedure Put_Title (Label : String);

   procedure Dump (Unit : Analysis_Unit);
   --  Print Unit's diagnostics or its text buffer (if there is no diagnostic)

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

   ----------
   -- Dump --
   ----------

   procedure Dump (Unit : Analysis_Unit) is
   begin
      if Unit.Has_Diagnostics then
         Put_Line ("Errors:");
         for D of Unit.Diagnostics loop
            Put ("  ");
            Put (Simple_Name (Unit.Get_Filename));
            if D.Sloc_Range /= No_Source_Location_Range then
               Put (':');
               Put (Image (Start_Sloc (D.Sloc_Range)));
            end if;
            Put (": ");
            Put_Line (To_UTF8 (To_Text (D.Message)));
         end loop;
      else
         Put_Line ("Success: " & Image (Unit.Text, With_Quotes => True));
      end if;
      New_Line;
   end Dump;

   -----------
   -- Parse --
   -----------

   procedure Parse (Filename, Charset : String) is
   begin
      Put_Title ("Parsing " & Filename);
      Dump (Ctx.Get_From_File (Filename, Charset));
   end Parse;

begin
   Put_Line ("main.adb: Starting...");

   --  Create a context with our file reader

   Ctx := Create_Context (File_Reader => Create_My_File_Reader);

   --  Check the file reader is used appropriately when reading files, and that
   --  common errors are properly reported.

   Parse ("foo.txt", "");
   Parse ("error.txt", "some-charset");
   Parse ("direct-ok.txt", "ascii");
   Parse ("direct-no-such-file.txt", "ascii");
   Parse ("direct-bad-charset.txt", "some-charset");
   Parse ("direct-decoding-error.txt", "ascii");

   --  Check that we can load the internal unit, which is supposed to bypass
   --  the file reader, to read from a memory buffer.

   Put_Title ("Parsing the internal unit");
   U := Get_Internal_Unit (Ctx);
   Dump (U);

   --  Check that we can fetch the internal unit with Get_From_File in
   --  non-reparsing mode only.

   if U /= Ctx.Get_From_File (Internal_Unit_Name) then
      raise Program_Error;
   end if;

   Put_Title ("Reparsing the internal unit");
   begin
      U := Ctx.Get_From_File (Internal_Unit_Name, Reparse => True);
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Precondition_Failure: " & Exception_Message (Exc));
   end;
   New_Line;

   --  Check that the use of parsing APIs with buffers is rejected

   Put_Title ("Using buffer-based parsing APIs");

   Put_Line ("First, create the from_buffer.txt unit...");
   U := Ctx.Get_From_File ("from_buffer.txt");
   New_Line;

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

   Put_Title ("Stubbing file fetcher");
   declare
      function "+"
        (S : String) return Unbounded_String renames To_Unbounded_String;

      Store   : constant File_Stub_Store := Create_File_Stub_Store;
      Fetcher : constant File_Fetcher_Reference :=
        Create_Stubbing_Fetcher (Store);

      D_Ok, Not_Existing : Analysis_Unit;
   begin
      Ctx := Create_Context
        (File_Reader => Create_File_Reader_Reference
                          (Fetcher, Empty_File_Refiner_Array));

      Put_Line ("Parsing direct-ok.txt");
      D_Ok := Ctx.Get_From_File ("direct-ok.txt");
      Dump (D_Ok);

      Put_Line ("Stubbing direct-ok.txt");
      Stub_File (Store, "direct-ok.txt", +"example # stubbed direct-ok.txt");

      Put_Line ("Re-parsing direct-ok.txt");
      D_Ok.Reparse;
      Dump (D_Ok);

      Put_Line ("Parsing not-existing.txt");
      Not_Existing := Ctx.Get_From_File ("not-existing.txt");
      Dump (Not_Existing);

      Put_Line ("Stubbing not-existing.txt");
      Stub_File
        (Store, "not-existing.txt", +"example # stubbed not-existing.txt");

      Put_Line ("Re-parsing not-existing.txt");
      Not_Existing.Reparse;
      Dump (Not_Existing);

      Put_Line ("Resetting direct-ok.txt");
      Reset_File (Store, "direct-ok.txt");

      Put_Line ("Re-parsing direct-ok.txt");
      D_Ok.Reparse;
      Dump (D_Ok);
   end;

   Put_Line ("main.adb: Done.");
end Main;
