--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with GNATCOLL.Strings; use GNATCOLL.Strings;

package body Langkit_Support.Images is

   --------------------
   -- Stripped_Image --
   --------------------

   function Stripped_Image (I : Integer) return String is
      Result : constant String := Integer'Image (I);
   begin
      return (if Result (Result'First) = ' '
              then Result (Result'First + 1 .. Result'Last)
              else Result);
   end Stripped_Image;

   -----------------
   -- Array_Image --
   -----------------

   function Array_Image
     (Self : Array_Type; Limit : Positive := 80) return String
   is
      Images : XString_Array (1 .. Self'Length);
      Len    : Natural := 0;
      Sep    : XString;
      Ret    : XString;
      J      : Positive := 1;
   begin
      for I in Self'Range loop
         Images (J) := To_XString (Image (Self (I)));
         Len := Len + Images (J).Length;
         J := J + 1;
      end loop;

      if Len > Limit then
         Sep := To_XString (ASCII.LF & " ");
      else
         Sep := To_XString (", ");
      end if;

      Ret.Append ("[");
      Ret.Append (Sep.Join (Images));
      Ret.Append ("]");

      return Ret.To_String;
   end Array_Image;

end Langkit_Support.Images;
