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
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.   See the  GNU  General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with this software;  see file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Containers; use Ada.Containers;

with System;

--  Various helpers to work with hashes for standard containers

package Langkit_Support.Hashes is

   Initial_Hash : constant Hash_Type;

   type Hash_Array is array (Positive range <>) of Hash_Type;

   function Combine (L, R : Hash_Type) return Hash_Type;
   --  Combine two hashes into a single one

   function Combine (Hashes : Hash_Array) return Hash_Type;
   --  Combine several hashes into a single one. This is like hashing a tuple.

   generic
      Ignored_LSB : Natural;
      --  Amount of least significant bits to ignore for hashing
   function Hash_Address (Addr : System.Address) return Hash_Type;
   --  Compute the hash of an address, ignoring the given amount of least
   --  significant bits.

   generic
      type Object_Type (<>) is limited private;
      type Object_Access is access Object_Type;
   function Hash_Access (Acc : Object_Access) return Hash_Type;
   --  Generic access hash function

private

   Initial_Hash : constant Hash_Type := 0;

end Langkit_Support.Hashes;
