with Ada.Unchecked_Deallocation;

package Langkit_Support.Text is

   subtype Character_Type is Wide_Wide_Character;
   subtype Text_Type is Wide_Wide_String;
   --  All our strings are encoded in UTF-32 (native endinannness). This type,
   --  which is not a subtype of String, makes it obvious when conversions are
   --  needed.

   function Text_Charset return String;
   --  Return the name of the charset used to encode Text_Type values

   function To_Text (S : String) return Text_Type;
   --  Convenience converter for pure ASCII strings. Raise a Constraint_Error
   --  if a non-ASCII character is met.

   function Image
     (T : Text_Type; With_Quotes : Boolean := False)
      return String;
   --  Return a Python-style escaped string for T. If With_Quote is True, the
   --  return string will include boundary quotes.

   procedure Process_As_String
     (Text : Text_Type;
      Proc : access procedure (S : String));
   --  Call Proc, passing to it Text as a String. This is useful to call APIs
   --  that work on mere strings to do conversions, such as GNATCOLL.Iconv.

   function Transcode (Text : Text_Type; Charset : String) return String;
   --  Use GNATCOLL.Iconv to convert Text into a String encoded using the given
   --  Charset. Note that this is only a convenience wrapper around
   --  GNATCOLL.Iconv: for instance, if performance needs dictatet to avoid
   --  secondary stack usage, please use directly GNATCOLL.Iconv.

   type Text_Access is access all Text_Type;
   type Text_Cst_Access is access constant Text_Type;

   procedure Free is new Ada.Unchecked_Deallocation (Text_Type, Text_Access);

   package Chars is
      NUL : constant Character_Type :=
         Wide_Wide_Character'Val (Character'Pos (ASCII.NUL));
      LF : constant Character_Type :=
         Wide_Wide_Character'Val (Character'Pos (ASCII.LF));
      CR : constant Character_Type :=
         Wide_Wide_Character'Val (Character'Pos (ASCII.CR));
      HT : constant Character_Type :=
         Wide_Wide_Character'Val (Character'Pos (ASCII.HT));
      FF : constant Character_Type :=
         Wide_Wide_Character'Val (Character'Pos (ASCII.FF));
      ESC : constant Character_Type :=
         Wide_Wide_Character'Val (Character'Pos (ASCII.ESC));
   end Chars;

end Langkit_Support.Text;
