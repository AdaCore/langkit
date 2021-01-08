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

with Ada.Unchecked_Conversion;

with System.Storage_Elements;

with Interfaces;

package body Langkit_Support.Hashes is

   -------------
   -- Combine --
   -------------

   function Combine (L, R : Hash_Type) return Hash_Type is
      use Interfaces;

      --  The following is a translation from Boost's uint32_t hash function

      C1 : constant Unsigned_32 := 16#cc9e2d51#;
      C2 : constant Unsigned_32 := 16#1b873593#;
      H1 : Unsigned_32 := Unsigned_32 (L);
      K1 : Unsigned_32 := Unsigned_32 (R);
   begin
      K1 := K1 * C1;
      K1 := Rotate_Left (K1, 15);
      K1 := K1 * C2;

      H1 := H1 xor K1;
      H1 := Rotate_Left (H1, 13);
      H1 := H1 * 5 + 16#e6546b64#;

      return Hash_Type (H1);
   end Combine;

   -------------
   -- Combine --
   -------------

   function Combine (Hashes : Hash_Array) return Hash_Type is
      Result : Hash_Type := Initial_Hash;
   begin
      for H of Hashes loop
         Result := Combine (Result, H);
      end loop;
      return Result;
   end Combine;

   ------------------
   -- Hash_Address --
   ------------------

   function Hash_Address (Addr : System.Address) return Hash_Type is
      use System, System.Storage_Elements;

      Result : constant Integer_Address :=
         To_Integer (Addr) / (2 ** Ignored_LSB);
   begin
      return Hash_Type'Mod (Result);
   end Hash_Address;

   -----------------
   -- Hash_Access --
   -----------------

   function Hash_Access (Acc : Object_Access) return Hash_Type is
      use System;

      function Convert is new Ada.Unchecked_Conversion
        (Object_Access, System.Address);
      function Hash is new Hash_Address (Word_Size / Storage_Unit);

   begin
      return Hash (Convert (Acc));
   end Hash_Access;

end Langkit_Support.Hashes;
