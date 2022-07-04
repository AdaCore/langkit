--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

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
