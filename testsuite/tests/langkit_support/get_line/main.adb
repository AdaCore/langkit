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

   procedure Check (Label : String; TDH : Token_Data_Handler);
   function Create (Buffer : Text_Type) return Token_Data_Handler;

   ------------
   -- Create --
   ------------

   function Create (Buffer : Text_Type) return Token_Data_Handler is
      B   : constant Text_Access := new Text_Type'(Buffer);
      TDH : Token_Data_Handler;
   begin
      Initialize (TDH, Syms, System.Null_Address);
      Reset (TDH, B, Buffer'First, Buffer'Last);
      return TDH;
   end Create;

   -----------
   -- Check --
   -----------

   procedure Check (Label : String; TDH : Token_Data_Handler) is
      Lines_Count : constant Natural :=
        Ada.Strings.Wide_Wide_Fixed.Count
          (TDH.Source_Buffer.all, (1 => Chars.LF));
   begin
      Put_Line ("== " & Label & " ==");
      for I in 1 .. Lines_Count + 2 loop
         begin
            declare
               L : constant Text_Type := Get_Line (TDH, I);
            begin
               Put_Line
                 ("  Line" & I'Image & ": " & Image (L, With_Quotes => True));
            end;
         exception
            when E : Constraint_Error =>
               Put_Line ("  Constraint_Error: " & Exception_Message (E));
         end;
      end loop;
      New_Line;
   end Check;

   --------------------------
   -- Show_Sloc_For_Offset --
   --------------------------

   procedure Show_Sloc_For_Offset
     (Label  : String;
      TDH    : Token_Data_Handler;
      Offset : Positive)
   is
      S : Source_Location;
   begin
      Put_Line ("== " & Label & " offset" & Offset'Image & " ==");
      S := Get_Sloc (TDH, Offset);
      Put_Line (Langkit_Support.Slocs.Image (S));
   exception
      when E : Constraint_Error =>
        Put_Line ("  Constraint_Error: " & Exception_Message (E));
   end Show_Sloc_For_Offset;

   B1 : Token_Data_Handler := Create
     ("Line 1" & Chars.LF & "Line 2" & Chars.LF & "Line 3" & Chars.LF);
   B2 : Token_Data_Handler := Create
     ("" & Chars.LF & "Line 2" & Chars.LF & "Line 3");
   B3 : Token_Data_Handler := Create
     ("" & Chars.LF & Chars.LF & "Line 3" & Chars.LF & Chars.LF);
begin
   Check ("Buffer 1", B1);
   Check ("Buffer 2", B2);
   Check ("Buffer 3", B3);

   Show_Sloc_For_Offset ("Buffer 1", B1, 3);
   Show_Sloc_For_Offset ("Buffer 1", B1, 7);
   Show_Sloc_For_Offset ("Buffer 1", B1, 8);
   Show_Sloc_For_Offset ("Buffer 1", B1, 9);
   Show_Sloc_For_Offset ("Buffer 1", B1, 19);
   Show_Sloc_For_Offset ("Buffer 1", B1, 21);

   --  After the last character
   Show_Sloc_For_Offset ("Buffer 1", B1, 250);

   Destroy (Syms);
   Free (B1);
   Free (B2);
   Free (B3);
end Main;
