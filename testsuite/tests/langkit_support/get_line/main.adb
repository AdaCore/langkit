with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Diagnostics.Output;
use Langkit_Support.Diagnostics.Output;
with Langkit_Support.Symbols;
with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Token_Data_Handlers;

procedure Main is

   type Dummy_Enum is (Dummy);

   function Precomputed_Symbol
     (Index : Dummy_Enum) return Text_Type
   is ("");

   package Symbols is new Langkit_Support.Symbols (Dummy_Enum);
   package My_TDH is new Langkit_Support.Token_Data_Handlers (Symbols);

   use My_TDH;

   Symbol_Table : Symbols.Symbol_Table := Symbols.Create_Symbol_Table;

   procedure Check (Label : String; Buffer : Text_Type);

   -----------
   -- Check --
   -----------

   procedure Check (Label : String; Buffer : Text_Type) is
      B           : Text_Access := new Text_Type'(Buffer);

      TDH         : Token_Data_Handler;

      Lines_Count : constant Natural :=
         Ada.Strings.Wide_Wide_Fixed.Count (Buffer, (1 => Chars.LF));


   begin
      Initialize (TDH, Symbol_Table);
      Reset (TDH, B, Buffer'First, Buffer'Last);

      Put_Line ("== " & Label & " ==");
      for I in 1 .. Lines_Count + 2 loop
         begin
            declare
               L : Text_Type := Get_Line (TDH, I);
            begin
               Put_Line
                 ("  Line" & I'Image & ": " & Image (L, With_Quotes => True));
            end;
         exception
            when E : Constraint_Error =>
               Put_Line ("  Constraint_Error: " & Exception_Message (E));
         end;
      end loop;
      Free (B);
      New_Line;
   end Check;

begin
   Check ("Buffer 1",
          "Line 1" & Chars.LF & "Line 2" & Chars.LF & "Line 3" & Chars.LF);
   Check ("Buffer 2",
          "" & Chars.LF & "Line 2" & Chars.LF & "Line 3");
   Check ("Buffer 3",
          "" & Chars.LF & Chars.LF & "Line 3" & Chars.LF & Chars.LF);

   Symbols.Destroy (Symbol_Table);
end Main;
