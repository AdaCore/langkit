------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                         --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;
with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;       use Ada.Wide_Wide_Text_IO;

with GNATCOLL.Terminal;           use GNATCOLL.Terminal;

with Langkit_Support.Images;      use Langkit_Support.Images;

package body Langkit_Support.Diagnostics.Output is

   Term_Info   : Terminal_Info;
   Colors_Init : Boolean := False;

   use Ada.Containers;

   procedure Print_Source_Listing
     (Sloc_Range  : Source_Location_Range;
      Buffer      : in out Text_Buffer;
      Lines_After : Natural := 0);
   --  Print a source listing

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

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Self        : in out Text_Buffer;
      Line_Number : Positive) return Text_Type
   is
      T      : Text_Type renames Self.Text_Ptr.all;

      Idx : Natural := 0;
      --  Index of the currently processed line's newline char. It starts at 0
      --  because we start the search for the next newline char from Idx + 1
      --  in the call to `Index` below.

   begin
      --  If there are line spans, then the start index is the index of the
      --  last computed line span.
      if Self.Line_Spans.Length > 0 then
         Idx := Self.Line_Spans.Last_Element;
      end if;

      --  Compute every line span between the last computed line span and the
      --  queried one.
      for I in Positive (Self.Line_Spans.Length + 1) .. Line_Number loop

         --  Search the index of the next newline char, starting just after
         --  the index of the previous newline char.
         Idx := Index (T, Chars.LF & "", Idx + 1);

         --  If no next newline found, Index is still at its starting value of
         --  0: Put the next index just after the end of the string, at the
         --  (virtual) "$" EOF character.
         if Idx = 0 then
            Idx := T'Last + 1;
         end if;
         Self.Line_Spans.Append (Idx);
      end loop;

      --  Return slice from...
      return  T
        (
          --  The character just after the found newline character to...
          (if Line_Number = 1
           then 0
           else Self.Line_Spans (Line_Number - 1)) + 1

         ..

         --  The character just before the next newline character
         Self.Line_Spans (Line_Number) - 1);

   end Get_Line;

   --------------------------
   -- Print_Source_Listing --
   --------------------------

   procedure Print_Source_Listing
     (Sloc_Range  : Source_Location_Range;
      Buffer      : in out Text_Buffer;
      Lines_After : Natural := 0)
   is
      procedure Start_Line (Line_Nb : String := "");

      Line_Nb      : constant Positive := Positive (Sloc_Range.Start_Line);
      Start_Offset : constant Positive := Positive (Sloc_Range.Start_Column);
      End_Offset   : constant Positive := Positive (Sloc_Range.End_Column);

      Line_Nb_Width : constant Positive :=
        Positive'Image (Line_Nb + Lines_After)'Length - 1;

      -----------------
      -- Append_Line --
      -----------------

      procedure Start_Line (Line_Nb : String := "") is
      begin
         Set_Style (Term_Info, Bright);
         Set_Color (Term_Info, Foreground => Blue);
         Put (Line_Nb);
         Ada.Text_IO.Put ((1 .. Line_Nb_Width - Line_Nb'Length => ' '));
         Ada.Text_IO.Put (" | ");
         Reset_Colors;
      end Start_Line;
   begin
      --  Append the line containing the sloc
      Start_Line (Stripped_Image (Line_Nb));
      Put_Line (Get_Line (Buffer, Line_Nb));

      --  Append the line caretting the sloc in the line above
      Start_Line ("");
      Set_Style (Term_Info, Bright);
      Set_Color (Term_Info, Foreground => Red);
      declare
         Caret_Line : Text_Type (1 .. End_Offset) := (others => ' ');
      begin
         Caret_Line (Start_Offset .. End_Offset - 1) := (others => '^');
         Put_Line (Caret_Line);
      end;
      Reset_Colors;

      --  TODO??? Missing the printing of lines after, because so far it was
      --  never used.
   end Print_Source_Listing;

   ----------------------
   -- Print_Diagnostic --
   ----------------------

   procedure Print_Diagnostic
     (Self   : Diagnostic;
      Buffer : in out Text_Buffer;
      Path   : String) is
   begin
      if not Colors_Init then
         Init_For_Stdout (Term_Info);
         Colors_Init := True;
      end if;

      --  Put `file_name.ext:line:col: error:`
      Set_Style (Term_Info, Bright);
      Put (Path & ":"
           & Stripped_Image (Integer (Self.Sloc_Range.Start_Line)) & ":"
           & Stripped_Image (Integer (Self.Sloc_Range.Start_Column)) & ":");
      Set_Color (Term_Info, Foreground => Red);
      Ada.Text_IO.Put (" error: ");
      Reset_Colors;

      --  Put the error message

      declare
         In_Lang_Entity : Boolean := False;
      begin
         for C of To_Text (Self.Message) loop
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
            Put (C);
         end loop;
      end;
      Ada.Text_IO.New_Line;
      Print_Source_Listing (Self.Sloc_Range, Buffer);
      Ada.Text_IO.New_Line;
   end Print_Diagnostic;

   ------------
   -- Create --
   ------------

   function Create (Text : Text_Cst_Access) return Text_Buffer is
   begin
      return Text_Buffer'(Text_Ptr => Text, others => <>);
   end Create;

end Langkit_Support.Diagnostics.Output;
