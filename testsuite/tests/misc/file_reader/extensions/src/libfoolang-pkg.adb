with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Common;            use Libfoolang.Common;
with Libfoolang.Implementation;    use Libfoolang.Implementation;
with Libfoolang.Public_Converters; use Libfoolang.Public_Converters;

package body Libfoolang.Pkg is

   Internal_Unit_Source : aliased constant String :=
     "example # internal";

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self        : My_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      Fn : constant String := Simple_Name (Filename);
   begin
      Put_Line ("My_File_Reader.Read:");
      Put_Line ("  Filename: " & Fn);
      Put_Line ("  Charset: " & Charset);
      Put_Line ("  Read_BOM: " & (if Read_BOM then "True" else "False"));

      if Fn = "error.txt" then
         Append (Diagnostics,
                 Sloc_Range => (1, 2, 3, 4),
                 Message    => "this is an error message");
         Contents := Create_Decoded_File_Contents ("");

      elsif Fn'Length > 7 and then Fn (Fn'First .. Fn'First + 6) = "direct-"
      then
         Direct_Read (Fn, Charset, Read_BOM, Contents, Diagnostics);

      else
         --  Make sure the explicit bounds are used, and not the buffer's
         --  internal ones: add garbage content before and after the actual
         --  buffer.

         Contents.Buffer :=
            new Text_Type'("zexample" & Chars.LF & "example{#");
         Contents.First := Contents.Buffer'First + 1;
         Contents.Last := Contents.Buffer'Last - 2;
      end if;
   end Read;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Self : in out My_File_Reader) is
   begin
      Put_Line ("My_File_Reader.Do_Release");
   end Release;

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
         Input       => (Kind        => Bytes_Buffer,
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
