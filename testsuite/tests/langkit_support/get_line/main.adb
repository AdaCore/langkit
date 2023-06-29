--  Check that Langkit_Support.Token_Data_Handlers.Get_Line/Get_Sloc work as
--  expected.

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Text_IO;    use Ada.Text_IO;

with System;

with Langkit_Support.Slocs;   use Langkit_Support.Slocs;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

procedure Main is

   Syms : Symbol_Table := Create_Symbol_Table;

   type Offset_Array is array (Positive range <>) of Positive;

   procedure Check
     (Label : String; Buffer : Text_Type; Offsets : Offset_Array);
   --  Using the given source buffer, check Get_Line on relevant line numbers
   --  and Check_Sloc for all given offsets.

   -----------
   -- Check --
   -----------

   procedure Check
     (Label   : String;
      Buffer  : Text_Type;
      Offsets : Offset_Array)
   is
      Lines_Count : constant Natural :=
        Ada.Strings.Wide_Wide_Fixed.Count (Buffer, (1 => Chars.LF));
      TDH         : Token_Data_Handler;
   begin
      Put_Line ("== " & Label & " ==");

      --  Create a token data handler for Buffer for Get_Line/Get_Sloc calls

      Initialize (TDH, Syms, System.Null_Address);
      Reset (TDH, new Text_Type'(Buffer), Buffer'First, Buffer'Last);

      --  Check that Get_Line works correctly even for out-of-bounds line
      --  numbers.

      for I in 1 .. Lines_Count + 2 loop
         Put ("  Line" & I'Image & ": ");
         begin
            declare
               L : constant Text_Type := Get_Line (TDH, I);
            begin
               Put_Line (Image (L, With_Quotes => True));
            end;
         exception
            when E : Constraint_Error =>
               Put_Line ("Constraint_Error: " & Exception_Message (E));
         end;
      end loop;
      New_Line;

      --  Check that Get_Sloc works as expected for the given buffer offsets

      for Offset of Offsets loop
         Put ("  Offset" & Offset'Image & ": ");
         if Offset not in Buffer'Range then
            Put_Line ("<out-of-bounds>");
         else
            Put_Line
              (Image (Buffer (Buffer'First .. Offset), With_Quotes => True));
         end if;
         Put ("    ");
         declare
            S : Source_Location;
         begin
            S := Get_Sloc (TDH, Offset);
            Put_Line (Langkit_Support.Slocs.Image (S));
         exception
            when E : Constraint_Error =>
              Put_Line ("Constraint_Error: " & Exception_Message (E));
         end;
      end loop;
      if Offsets'Length > 0 then
         New_Line;
      end if;

      Free (TDH);
   end Check;

begin
   --  Test various valid offsets, and much after the last character

   Check
     (Label   => "Basic",
      Buffer  => "Line 1" & Chars.LF
                 & "Line 2" & Chars.LF
                 & "Line 3" & Chars.LF,
      Offsets => (3, 7, 8, 9, 19, 21, 22, 23, 250));

   --  Various tests with empty lines

   Check
     (Label   => "Empty lines 1/2",
      Buffer  => "" & Chars.LF
                 & "Line 2" & Chars.LF
                 & "Line 3",
      Offsets => (1 .. 0 => <>));

   Check
     (Label   => "Empty lines 2/2",
      Buffer  => "" & Chars.LF
                 & Chars.LF
                 & "Line 3" & Chars.LF
                 & Chars.LF,
      Offsets => (1 .. 0 => <>));

   Check
     (Label   => "Empty source buffer",
      Buffer  => "",
      Offsets => (1, 2, 3));

   --  Check the sloc for the "end of buffer" offset in the absence of trailing
   --  newline codepoint.

   Check
     (Label   => "No newline",
      Buffer  => "abc" & Chars.HT & Chars.HT,
      Offsets => (1, 2, 3, 4, 5, 6, 7));

   --  Check that column numbers are correct when dealing with non-ASCII
   --  codepoints.

   declare
      Eacute  : constant Text_Type :=
        ((1 => Character_Type'Val (16#e9#)));
      Emoji   : constant Text_Type :=
        ((1 => Character_Type'Val (16#1f642#)));
      Buffer  : constant Text_Type :=
        "a" & Eacute & "b" & Emoji & "c" & Chars.LF;
      Offsets : Offset_Array (1 .. Buffer'Length + 1);
   begin
      for I in Offsets'Range loop
         Offsets (I) := I;
      end loop;
      Check ("Unicode", Buffer, Offsets);
   end;

   Destroy (Syms);
   Put_Line ("Done.");
end Main;
