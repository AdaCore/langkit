--
--  Copyright (C) 2020-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Langkit_Support.Images; use Langkit_Support.Images;

package body Langkit_Support.Diagnostics.Output is

   Term_Info   : Terminal_Info;
   Colors_Init : Boolean := False;

   procedure Print_Source_Listing
     (Sloc_Range      : Source_Location_Range;
      Buffer          : Text_Buffer_Ifc'Class;
      Output_File     : File_Type := Standard_Output;
      Caretting_Color : ANSI_Color);
   --  Print a source listing.
   --
   --  ``Sloc_Range`` determines range of source code to print.
   --
   --  ``Buffer`` is used to get access to the source code.
   --
   --  The source listing is written to ``Output_File``.
   --
   --  ``Caretting_Color`` is the style used to display the carets to highlight
   --  the slice of source code designated by ``Sloc_Range``.

   procedure Reset_Colors;
   --  Reset the state of colors for ``Term_Info``

   ------------------
   -- Reset_Colors --
   ------------------

   procedure Reset_Colors is
   begin
      Set_Color (Term_Info, Foreground => Reset);
      Set_Style (Term_Info, Reset_All);
   end Reset_Colors;

   --------------------------
   -- Print_Source_Listing --
   --------------------------

   procedure Print_Source_Listing
     (Sloc_Range      : Source_Location_Range;
      Buffer          : Text_Buffer_Ifc'Class;
      Output_File     : File_Type := Standard_Output;
      Caretting_Color : ANSI_Color)
   is
      procedure Line_Starting (Line_Nb : Natural);
      --  Print the "NN |" source listing prefix. ``Line_Nb`` is the line
      --  number in a natural integer. If ``Line_Nb`` is 0 then the line
      --  number "NN" is blank.

      Start_Line : constant Natural := Natural (Sloc_Range.Start_Line);
      Start_Col  : constant Natural := Natural (Sloc_Range.Start_Column);
      End_Line   : constant Natural := Natural (Sloc_Range.End_Line);
      End_Col    : constant Natural := Natural (Sloc_Range.End_Column);
      Line_Nb    : constant Natural := End_Line - Start_Line + 1;
      Col_Size   : constant Natural :=
         To_Text (Stripped_Image (End_Line))'Length;

      ----------------
      -- Start_Line --
      ----------------

      procedure Line_Starting (Line_Nb : Natural) is
         Num_Col : String (1 .. Col_Size) := (others => ' ');
      begin
         Set_Color (Term_Info, Foreground => Blue);

         if Line_Nb >= 1 then
            declare
               Line_Nb_Img : constant String := Stripped_Image (Line_Nb);
            begin
               Num_Col (1 .. Line_Nb_Img'Length) := Line_Nb_Img;
            end;
         end if;

         Put (Output_File, Num_Col);
         Put (Output_File, " |");
         Reset_Colors;
      end Line_Starting;
   begin
      Set_Style (Term_Info, Bright);

      --  If the number of line to display is 1
      if Line_Nb = 1 then
         declare
            Caret_Line : String (1 .. End_Col - 1) := (others => ' ');
         begin
            Caret_Line (Start_Col .. End_Col - 1) := (others => '^');
            Line_Starting (Start_Line);
            Put_Line
              (Output_File, " " & To_UTF8 (Get_Line (Buffer, Start_Line)));
            if Start_Col /= End_Col then
               Line_Starting (0);
               Set_Color (Term_Info, Foreground => Caretting_Color);
               Put_Line (Output_File, " " & Caret_Line);
            end if;
         end;

      --  Else display the multiline style
      else
         declare
            Diff            : constant Natural := Line_Nb - 2;
            Start_Underline : constant String
               (1 .. Start_Col) := (others => '_');
            End_Undeline    : constant String
               (1 .. Integer'Max (End_Col - 1, 1)) := (others => '_');
         begin
            Line_Starting (Start_Line);
            Put_Line
              (Output_File,
               "  " & To_UTF8 (Get_Line (Buffer, Start_Line)));
            Line_Starting (0);
            Set_Color (Term_Info, Foreground => Caretting_Color);
            Put_Line (Output_File, " " & Start_Underline & "^");

            if Diff > 0 then
               Line_Starting (0);
               Set_Color (Term_Info, Foreground => Caretting_Color);
               Put_Line (Output_File, "|");
               Line_Starting (0);
               Set_Color (Term_Info, Foreground => Caretting_Color);
               Put (Output_File, "|");
               Put_Line (Output_File,
                         " ~~~ " & Stripped_Image (Diff)
                         & " other lines ~~~");
               Line_Starting (0);
               Set_Color (Term_Info, Foreground => Caretting_Color);
               Put_Line (Output_File, "|");
            end if;

            Line_Starting (End_Line);
            Set_Color (Term_Info, Foreground => Caretting_Color);
            Put (Output_File, "|");
            Reset_Colors;
            Put_Line
              (Output_File,
               " " & To_UTF8 (Get_Line (Buffer, End_Line)));
            Line_Starting (0);
            Set_Color (Term_Info, Foreground => Caretting_Color);
            Put_Line (Output_File, "|" & End_Undeline & "^");
         end;
      end if;
      Reset_Colors;
   end Print_Source_Listing;

   ----------------------
   -- Print_Diagnostic --
   ----------------------

   procedure Print_Diagnostic
     (Self        : Diagnostic;
      Buffer      : Text_Buffer_Ifc'Class;
      Path        : String;
      Style       : Diagnostic_Style := Default_Diagnostic_Style;
      Output_File : File_Type := Standard_Output)
   is
   begin
      if not Colors_Init then
         Init_For_Stdout (Term_Info);
         Colors_Init := True;
      end if;

      --  Put `file_name.ext:line:col: error:`

      Set_Style (Term_Info, Bright);
      Put (Output_File,
           Path & ":"
           & Stripped_Image (Integer (Self.Sloc_Range.Start_Line)) & ":"
           & Stripped_Image
             (Integer (Self.Sloc_Range.Start_Column)) & ":");

      Set_Color (Term_Info, Foreground => Style.Color);
      Put (Output_File, " " & To_UTF8 (To_Text (Style.Label)) & ": ");
      Reset_Colors;

      --  Put the error message

      declare
         In_Lang_Entity : Boolean := False;
      begin
         for C of To_UTF8 (To_Text (Self.Message)) loop

            --  Style backtick parts: put everything in `Bright` inbetween
            --  backticks.

            if C = '`' then
               if In_Lang_Entity then
                  Reset_Colors;
               else
                  Set_Style (Term_Info, Bright);
                  In_Lang_Entity := False;
               end if;
               In_Lang_Entity := not In_Lang_Entity;
            end if;
            Put (Output_File, C);
         end loop;
      end;
      New_Line (Output_File);

      --  Put the source sample

      Print_Source_Listing (Self.Sloc_Range, Buffer,
                            Output_File     => Output_File,
                            Caretting_Color => Style.Color);
      New_Line (Output_File);
   end Print_Diagnostic;

end Langkit_Support.Diagnostics.Output;
