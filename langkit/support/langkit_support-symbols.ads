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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Sets;

with GNAT.String_Hash;

with Langkit_Support.Text; use Langkit_Support.Text;

--  Provide a symbol table for text (Text_Type) identifiers

generic

   type Precomputed_Symbol_Index is (<>);
   --  Indexes for symbols to pre-compute in each symbol table

   with function Precomputed_Symbol
     (Index : Precomputed_Symbol_Index) return Text_Type is <>;
   --  Return the symbol corresponding to the precomputed symbol Index

package Langkit_Support.Symbols is

   type Symbol_Type is new Text_Cst_Access;

   function Image (S : Symbol_Type) return Text_Type;
   function Image
     (S : Symbol_Type; With_Quotes : Boolean := False) return String;

   type Symbol_Table is private;
   --  The actual symbol table type to use

   No_Symbol_Table : constant Symbol_Table;
   --  Value to use as a default for unallocated symbol tables

   function Create_Symbol_Table return Symbol_Table;
   --  Allocate a new symbol table and return it

   function Precomputed_Symbol
     (ST : Symbol_Table; Index : Precomputed_Symbol_Index) return Symbol_Type
      with Inline;
   --  Return the precomputed symbol corresponding to Index

   function Find
     (ST     : Symbol_Table;
      T      : Text_Type;
      Create : Boolean := True) return Symbol_Type with Inline;
   --  Look for an entry for the T text in the ST symbol table. If there is
   --  such an entry, return it. Otherwise, create it and return it if Create
   --  is true. Elsewise, return null.
   --
   --  Non-null returned accesses are guaranteed to be the same for all equal
   --  Text_Type.

   procedure Destroy (ST : in out Symbol_Table);
   --  Deallocate a symbol table and all the text returned by the corresponding
   --  calls to Find.

   function Hash (ST : Symbol_Type) return Hash_Type;
   --  Default hash function for symbols.
   --  WARNING: It assumes that you don't mix symbols from different symbol
   --  tables, but doesn't verify it!

private

   function Hash is new GNAT.String_Hash.Hash
     (Char_Type => Wide_Wide_Character,
      Key_Type  => Text_Type,
      Hash_Type => Ada.Containers.Hash_Type);

   function String_Hash (T : Symbol_Type) return Ada.Containers.Hash_Type is
     (Hash (T.all));

   function Key_Equal (L, R : Symbol_Type) return Boolean is (L.all = R.all);

   package Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Symbol_Type,
      Hash                => String_Hash,
      Equivalent_Elements => Key_Equal,
      "="                 => "=");

   type Precomputed_Symbol_Array is
      array (Precomputed_Symbol_Index) of Symbol_Type;

   type Symbol_Table_Record is record
      Symbols     : Sets.Set;
      Precomputed : Precomputed_Symbol_Array;
   end record;

   type Symbol_Table is access Symbol_Table_Record;

   No_Symbol_Table : constant Symbol_Table := null;

end Langkit_Support.Symbols;
