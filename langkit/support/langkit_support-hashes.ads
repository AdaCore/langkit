with Ada.Containers; use Ada.Containers;

--  Various helpers to work with hashes for standard containers

package Langkit_Support.Hashes is

   Initial_Hash : constant Hash_Type;

   type Hash_Array is array (Positive range <>) of Hash_Type;

   function Combine (L, R : Hash_Type) return Hash_Type;
   --  Combine two hashes into a single one

   function Combine (Hashes : Hash_Array) return Hash_Type;
   --  Combine several hashes into a single one. This is like hashing a tuple.

   generic
      type Object_Type (<>) is limited private;
      type Object_Access is access Object_Type;
   function Hash_Access (Acc : Object_Access) return Hash_Type;
   --  Generic access hash function

private

   Initial_Hash : constant Hash_Type := 0;

end Langkit_Support.Hashes;
