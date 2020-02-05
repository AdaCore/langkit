------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2020, AdaCore                     --
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

with Ada.Strings.UTF_Encoding;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Deallocation;

package Langkit_Support.Text is

   subtype Character_Type is Wide_Wide_Character;
   subtype Text_Type is Wide_Wide_String;
   subtype Unbounded_Text_Type is
      Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
   --  All our strings are encoded in UTF-32 (native endinannness). This type,
   --  which is not a subtype of String, makes it obvious when conversions are
   --  needed.

   function Text_Charset return String;
   --  Return the name of the charset used to encode Text_Type values

   function To_Text (S : String) return Text_Type;
   --  Convenience converter for pure ASCII strings. Raise a Constraint_Error
   --  if a non-ASCII character is met.

   function To_Text (UT : Unbounded_Text_Type) return Text_Type
      renames Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String;

   function To_Unbounded_Text (T : Text_Type) return Unbounded_Text_Type
      renames Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String;

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

   function Encode (Text : Text_Type; Charset : String) return String;
   --  Use GNATCOLL.Iconv to convert Text into a String encoded using the given
   --  Charset. Note that this is only a convenience wrapper around
   --  GNATCOLL.Iconv: for instance, if performance needs dictate to avoid
   --  secondary stack usage, please use directly GNATCOLL.Iconv.

   function Decode (S : String; Charset : String) return Text_Type;
   --  Likewise, but convert a string to text

   function To_UTF8
     (Text : Text_Type) return Ada.Strings.UTF_Encoding.UTF_8_String;
   --  Encode the given text into an UTF-8 string

   function From_UTF8
     (S : Ada.Strings.UTF_Encoding.UTF_8_String) return Text_Type;
   --  Decode the given UTF-8 string into text

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
