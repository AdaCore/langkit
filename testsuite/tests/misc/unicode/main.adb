with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Strings;  use GNAT.Strings;
with GNATCOLL.Mmap; use GNATCOLL.Mmap;

with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;
with Langkit_Support.Slocs;        use Langkit_Support.Slocs;
with Libfoolang.Analysis;          use Libfoolang.Analysis;
with Libfoolang.Common;            use Libfoolang.Common;

with Support; use Support;

procedure Main is

   Empty_File     : constant String := "empty.txt";
   Empty_Buffer   : aliased constant String := "";

   Example_File   : constant String := "main-iso-8859-1.txt";
   Example_Buffer : String_Access := Read_Whole_File (Example_File);

   procedure Check
     (From_Buffer      : Boolean := False;
      Empty_File       : Boolean := False;
      Wrong_Encoding   : Boolean := False;
      With_File_Reader : Boolean := False);

   -----------
   -- Check --
   -----------

   procedure Check
     (From_Buffer      : Boolean := False;
      Empty_File       : Boolean := False;
      Wrong_Encoding   : Boolean := False;
      With_File_Reader : Boolean := False)
   is
      Charset  : constant String :=
        (if Wrong_Encoding then "utf-8" else "iso-8859-1");
      Filename : constant String :=
        (if Empty_File then Main.Empty_File else Example_File);
      Buffer   : constant access constant String :=
        (if Empty_File then Empty_Buffer'Access else Example_Buffer);

      Ctx : Analysis_Context;
      U   : Analysis_Unit;
   begin
      --  Put some label for this check

      Put ("== ");
      Put (if From_Buffer then "buffer" else "file");
      Put (" | ");
      Put (if Empty_File then "empty-file" else "example-file");
      Put (" | ");
      Put (if Wrong_Encoding then "wrong-encoding" else "correct-encoding");
      Put (" | ");
      Put (if With_File_Reader then "file-reader" else "default");
      Put_Line (" ==");
      New_Line;

      --  Parse the source according to requested settings

      Ctx := Create_Context
        (File_Reader => (if With_File_Reader
                         then Get_File_Reader
                         else No_File_Reader_Reference));
      if From_Buffer then
         U := Ctx.Get_From_Buffer
           (Filename => Filename,
            Charset  => Charset,
            Buffer   => Buffer.all);
      else
         U := Ctx.Get_From_File
           (Filename => Filename, Charset => Charset);
      end if;

      --  Display parsing errors, if any

      if U.Has_Diagnostics then
         Put_Line ("Errors:");
         for D of U.Diagnostics loop
            Put_Line ("  " & U.Format_GNU_Diagnostic (D));
         end loop;
         New_Line;
      end if;

      --  Summarize the content of the parsed unit

      if U.Root.Is_Null then
         Put_Line ("No root node");
      else
         Put_Line ("Root node children:" & U.Root.Children_Count'Image);
         declare
            D : constant Token_Data_Type := Data (U.First_Token);
         begin
            Put_Line
              ("First token: "
               & Kind (D)'Image
               & " at " & Image (Sloc_Range (D)));
         end;
         declare
            D : constant Token_Data_Type := Data (U.Last_Token);
         begin
            Put_Line
              ("Last token:  "
               & Kind (D)'Image
               & " at " & Image (Sloc_Range (D)));
         end;
      end if;
      New_Line;
   end Check;

begin
   --  Get_From_File

   Check;
   Check (With_File_Reader => True);

   Check (Empty_File => True);
   Check (Empty_File => True, With_File_Reader => True);

   Check (Wrong_Encoding => True);
   Check (Wrong_Encoding => True, With_File_Reader => True);

   --  Get_From_Buffer

   Check (From_Buffer => True);
   Check (From_Buffer => True, Empty_File => True);
   Check (From_Buffer => True, Wrong_Encoding => True);

   Free (Example_Buffer);
end Main;
