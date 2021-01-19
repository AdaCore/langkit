------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Wide_Wide_Characters.Handling;
with Interfaces;            use Interfaces;
with System;

with GNATCOLL.Iconv;

package body Langkit_Support.Text is

   ------------------
   -- Text_Charset --
   ------------------

   function Text_Charset return String is
      use GNATCOLL.Iconv, System;
   begin
      if Default_Bit_Order = Low_Order_First then
         return UTF32LE;
      else
         return UTF32BE;
      end if;
   end Text_Charset;

   -------------
   -- To_Text --
   -------------

   function To_Text (S : String) return Text_Type is
      Result : Text_Type (1 .. S'Length);
   begin
      for I in Result'Range loop
         declare
            C : constant Character := S (S'First + I - 1);
         begin
            if C in ASCII.NUL .. Character'Val (16#7f#) then
               Result (I) := Wide_Wide_Character'Val (Character'Pos (C));
            else
               raise Constraint_Error with "Invalid ASCII character";
            end if;
         end;
      end loop;
      return Result;
   end To_Text;

   -----------
   -- Image --
   -----------

   function Image (T : Text_Type; With_Quotes : Boolean := False) return String
   is
      subtype Hex_Byte is String (1 .. 2);
      --  Couple of hexadecimal digits, used to represent a byte

      function Byte_Image (B : Unsigned_8) return Hex_Byte;
      --  Given a byte, return the corresponding two-chars hexadecimal image

      ----------------
      -- Byte_Image --
      ----------------

      function Byte_Image (B : Unsigned_8) return Hex_Byte
      is
         type Digits_Type is array (Unsigned_8 range 0 .. 15) of Character;
         D : constant Digits_Type := "0123456789abcdef";
      begin
         return D (B / 16) & D (B mod 16);
      end Byte_Image;

      Result : Unbounded_String;
      W      : Unsigned_32;
   begin
      if With_Quotes then
         Append (Result, '"');
      end if;

      for C of T loop

         --  Determine how to output each character:
         --
         --    - Escape backslashes and escape quotes if With_Quotes.
         --    - Output other ASCII chars as-is.
         --    - Escape non-ASCII small chars with \xXX sequences.
         --    - Escape other medium chars with \uXXXX sequences.
         --    - Escape the rest with \UXXXXXXXX sequences.

         W := Wide_Wide_Character'Pos (C);
         if (With_Quotes and then C = '"') or else C = '\' then
            Append (Result, '\');
            Append (Result, Character'Val (W));
         elsif 16#20# <= W and then W <= 16#7f# then
            Append (Result, Character'Val (W));
         elsif W <= 16#ff# then
            Append (Result, "\x" & Byte_Image (Unsigned_8 (W)));
         elsif W <= 16#ffff# then
            Append
              (Result,
               "\u"
               & Byte_Image (Unsigned_8 (W / 16#100#))
               & Byte_Image (Unsigned_8 (W mod 16#100#)));
         else
            Append
              (Result,
               "\U"
               & Byte_Image (Unsigned_8 (W / 16#100_0000#))
               & Byte_Image (Unsigned_8 (W / 16#1_0000# mod 16#100#))
               & Byte_Image (Unsigned_8 (W / 16#100# mod 16#100#))
               & Byte_Image (Unsigned_8 (W mod 16#100#)));
         end if;
      end loop;

      if With_Quotes then
         Append (Result, '"');
      end if;
      return To_String (Result);
   end Image;

   -----------------------
   -- Process_As_String --
   -----------------------

   procedure Process_As_String
     (Text : Text_Type;
      Proc : access procedure (S : String))
   is
      S : String (1 .. 4 * Text'Length)
         with Import  => True,
              Address => Text'Address;
   begin
      Proc.all (S);
   end Process_As_String;

   ------------
   -- Encode --
   ------------

   function Encode (Text : Text_Type; Charset : String) return String is
      S : String (1 .. 4 * Text'Length) with Import, Address => Text'Address;
   begin
      return GNATCOLL.Iconv.Iconv
        (Input     => S,
         To_Code   => Charset,
         From_Code => Text_Charset);
   end Encode;

   ------------
   -- Decode --
   ------------

   function Decode (S : String; Charset : String) return Text_Type is
      Result : constant String := GNATCOLL.Iconv.Iconv
        (Input     => S,
         To_Code   => Text_Charset,
         From_Code => Charset);
      pragma Assert (Result'Length mod 4 = 0);

      Text_Result : constant Text_Type (1 .. Result'Length / 4)
         with Import, Address => Result'Address;
   begin
      return Text_Result;
   end Decode;

   -------------
   -- To_UTF8 --
   -------------

   function To_UTF8
     (Text : Text_Type) return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      return Encode (Text, "UTF-8");
   end To_UTF8;

   ---------------
   -- From_UTF8 --
   ---------------

   function From_UTF8
     (S : Ada.Strings.UTF_Encoding.UTF_8_String) return Text_Type is
   begin
      return Decode (S, "UTF-8");
   end From_UTF8;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (C : Character_Type) return Character_Type is
      subtype WWC is Wide_Wide_Character;

      First_ASCII : constant WWC :=
         WWC'Val (Character'Pos (ASCII.NUL));
      Last_ASCII  : constant WWC :=
         WWC'Val (Character'Pos (ASCII.DEL));
      subtype WWC_ASCII is WWC range First_ASCII .. Last_ASCII;
   begin
      if C in 'A' .. 'Z' then
         return WWC'Val (WWC'Pos (C) - WWC'Pos ('A') + WWC'Pos ('a'));
      elsif C in WWC_ASCII'Range then
         return C;
      else
         return Ada.Wide_Wide_Characters.Handling.To_Lower (C);
      end if;
   end To_Lower;

end Langkit_Support.Text;
