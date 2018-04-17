with Ada.Unchecked_Deallocation;

package Langkit_Support.Text is

   subtype Character_Type is Wide_Wide_Character;
   subtype Text_Type is Wide_Wide_String;
   --  All our strings are encoded in UTF-32 (native endinannness). This type,
   --  which is not a subtype of String, makes it obvious when conversions are
   --  needed.

   function To_Text (S : String) return Text_Type;
   --  Convenience converter for pure ASCII strings. Raise a Constraint_Error
   --  if a non-ASCII character is met.

   function Image
     (T : Text_Type; With_Quotes : Boolean := False)
      return String;
   --  Return a Python-style escaped string for T. If With_Quote is True, the
   --  return string will include boundary quotes.

   type Text_Access is access all Text_Type;
   type Text_Cst_Access is access constant Text_Type;

   procedure Free is new Ada.Unchecked_Deallocation (Text_Type, Text_Access);

   package Chars is
      NUL : constant Character_Type :=
         Wide_Wide_Character'Val (Character'Pos (ASCII.NUL));
      LF : constant Character_Type :=
         Wide_Wide_Character'Val (Character'Pos (ASCII.LF));
      HT : constant Character_Type :=
         Wide_Wide_Character'Val (Character'Pos (ASCII.HT));
      ESC : constant Character_Type :=
         Wide_Wide_Character'Val (Character'Pos (ASCII.ESC));
   end Chars;

end Langkit_Support.Text;
