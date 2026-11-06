with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.Opt_Parse;       use GNATCOLL.Opt_Parse;
with Liblktlang.Generic_API;   use Liblktlang.Generic_API;
with Liblktlang_Support.Generic_API.Analysis;
use Liblktlang_Support.Generic_API.Analysis;
with Liblktlang_Support.Generic_API.Unparsing;
use Liblktlang_Support.Generic_API.Unparsing;
with Liblktlang_Support.Slocs; use Liblktlang_Support.Slocs;
with Liblktlang_Support.Text;  use Liblktlang_Support.Text;
with Prettier_Ada.Documents;   use Prettier_Ada.Documents;

procedure Lktformat is

   Parser : Argument_Parser := Create_Argument_Parser
     (Help => "Reformat Lkt source files");

   package Source_Files is new Parse_Positional_Arg_List
     (Parser   => Parser,
      Name     => "source-files",
      Help     => "List of source files to reformat.",
      Arg_Type => Unbounded_String);

   Config  : constant Unparsing_Configuration :=
     Default_Unparsing_Configuration (Self_Id);
   Context : constant Lk_Context := Create_Context (Self_Id);
   Options : constant Prettier_Ada.Documents.Format_Options_Type :=
     (Width       => 79,
      Indentation =>
        (Kind         => Spaces,
         Width        => 4,
         Continuation => 4,
         Offset       => <>),
      End_Of_Line => LF);

   procedure Process_Source_File (Filename : String);
   --  Process (reformat) the given source file

   procedure Process_Error (Node : Lk_Node; Message : String);
   --  Callback for Unparse_To_Prettier: display the diagnostic on the standard
   --  output.

   -------------------------
   -- Process_Source_File --
   -------------------------

   procedure Process_Source_File (Filename : String) is
      Unit      : constant Lk_Unit := Context.Get_From_File (Filename);
      Doc       : Prettier_Ada.Documents.Document_Type;
      Formatted : Unbounded_String;
      F         : File_Type;
   begin
      --  Abort processing for this file if it does not parse correctly

      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;
         Set_Exit_Status (Failure);
         return;
      end if;

      Doc := Unparse_To_Prettier (Unit.Root, Config, Process_Error'Access);
      Formatted := Prettier_Ada.Documents.Format (Doc, Options);
      Create (F, Name => Filename);

      --  Transcode Formatted from UTF-8 (the encoding for Prettier's output)
      --  to the same charset as the original source file.
      --
      --  Calls to Write_Encoded should never raise GNATCOLL.Iconv exceptions
      --  since, if we got here, we managed to successfully parse the source
      --  file using Unit.Charset.

      Write_Encoded (F, Formatted, "utf-8", Unit.Charset);
      Close (F);
   end Process_Source_File;

   -------------------
   -- Process_Error --
   -------------------

   procedure Process_Error (Node : Lk_Node; Message : String) is
   begin
      Put (Node.Unit.Filename);
      Put (":");
      Put (Image (Start_Sloc (Node.Sloc_Range)));
      Put (": ");
      Put_Line (Message);
   end Process_Error;

begin
   if not Parser.Parse then
      return;
   end if;

   --  Process each input source files in sequence

   for Filename of Source_Files.Get loop
      Process_Source_File (To_String (Filename));
   end loop;
end Lktformat;
