with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Langkit_Support.Internal.Analysis;
with Langkit_Support.Text;  use Langkit_Support.Text;
with Langkit_Support.Types; use Langkit_Support.Types;

with Libfoolang.Common;            use Libfoolang.Common;
with Libfoolang.Implementation;    use Libfoolang.Implementation;
with Libfoolang.Public_Converters; use Libfoolang.Public_Converters;

package body Libfoolang.Pkg is

   Internal_Unit_Source : aliased constant String :=
     "example # internal";

   function Is_Direct_Filename (Filename : String) return Boolean
   is (Filename'Length > 7
       and then Filename (Filename'First .. Filename'First + 6) = "direct-");

   -----------
   -- Fetch --
   -----------

   overriding procedure Fetch
     (Self        : My_File_Fetcher;
      Filename    : String;
      Contents    : out File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      Fn : constant String := Simple_Name (Filename);
   begin
      Put_Line ("My_File_Fetcher.Fetch:");
      Put_Line ("  Filename: " & Fn);

      if Fn = "error.txt" then
         Append (Diagnostics,
                 Sloc_Range => (1, 2, 3, 4),
                 Message    => "error from the file fetcher");
         Contents := Create_File_Contents ("");

      elsif Is_Direct_Filename (Fn) then
         Self.Filesystem_Fetcher.Unchecked_Get.Fetch
           (Fn, Contents, Diagnostics);

      else
         --  Make sure the explicit bounds are used, and not the buffer's
         --  internal ones: add garbage content before and after the actual
         --  buffer.

         Contents.Buffer :=
            new String'("zexample" & ASCII.LF & "example{#");
         Contents.First := Contents.Buffer'First + 1;
         Contents.Last := Contents.Buffer'Last - 2;
      end if;
   end Fetch;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Self : in out My_File_Fetcher) is
   begin
      Put_Line ("My_File_Fetcher.Do_Release");
   end Release;

   ------------
   -- Refine --
   ------------

   overriding procedure Refine
     (Self        : My_File_Refiner;
      Filename    : String;
      Contents    : in out File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      Fn : constant String := Simple_Name (Filename);
   begin
      Put_Line ("My_File_Refiner.Refine:");
      Put_Line ("  Filename: " & Fn);

      if Fn = "error.txt" then
         Append (Diagnostics,
                 Sloc_Range => (1, 2, 3, 4),
                 Message    => "error from the file refiner");
         Contents.First := 1;
         Contents.Last := 0;

      else
         --  Add padding after the buffer to check that bounds are correctly
         --  used.

         declare
            S : constant String_Access :=
              new String (Contents.Buffer'First .. Contents.Buffer'Last + 1);
         begin
            S.all (S'First .. S'Last - 1) := Contents.Buffer.all;
            S.all (S'Last) := '|';
            Free (Contents.Buffer);
            Contents.Buffer := S;
            Contents.First := Contents.First;
            Contents.Last := Contents.Last;
         end;
      end if;
   end Refine;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Self : in out My_File_Refiner) is
   begin
      Put_Line ("My_File_Refiner.Do_Release");
   end Release;

   ---------------------------
   -- Create_My_File_Reader --
   ---------------------------

   function Create_My_File_Reader return File_Reader_Reference is
      FF : constant My_File_Fetcher :=
        (Filesystem_Fetcher => Create_Filesystem_Fetcher);
      FR : constant My_File_Refiner := (null record);
   begin
      return Create_File_Reader_Reference
        (Create_File_Fetcher_Reference (FF),
         (1 => Create_File_Refiner_Reference (FR)));
   end Create_My_File_Reader;

   -----------------------
   -- Get_Internal_Unit --
   -----------------------

   function Get_Internal_Unit (Context : Analysis_Context) return Analysis_Unit
   is
      Ctx    : constant Internal_Context := Unwrap_Context (Context);
      Result : constant Internal_Unit := Get_Unit
        (Context     => Ctx,
         Filename    => Internal_Unit_Name,
         Charset     => "ascii",
         Reparse     => False,
         Input       =>
           (Kind        => Bytes_Buffer,
            Charset     => To_Unbounded_String ("ascii"),
            Read_BOM    => False,
            Bytes       => Internal_Unit_Source'Address,
            Bytes_Count => Internal_Unit_Source'Length),
         Rule        => Default_Grammar_Rule,
         Is_Internal => True);
   begin
      return Wrap_Unit (Result);
   end Get_Internal_Unit;

end Libfoolang.Pkg;
