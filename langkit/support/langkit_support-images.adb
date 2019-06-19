------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
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
     (Self : Array_Type; Limit : Positive := 80) return String is
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
         Sep := To_XString (ASCII.LF & "  ");
      else
         Sep := To_XString (", ");
      end if;

      Ret.Append ("[");
      Ret.Append (Sep.Join (Images));
      Ret.Append ("]");

      return Ret.To_String;

   end Array_Image;

end Langkit_Support.Images;
