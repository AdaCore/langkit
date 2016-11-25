with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Sets;

with GNAT.String_Hash;

with Langkit_Support.Text; use Langkit_Support.Text;

--  Provide a symbol table for text (Text_Type) identifiers

package Langkit_Support.Symbols is

   type Symbol_Type is new Text_Access;

   function Image
     (S           : Symbol_Type;
      With_Quotes : Boolean := False)
      return String
   is
     (Image (S.all, With_Quotes));

   type Symbol_Table is private;
   --  The actual symbol table type to use

   No_Symbol_Table : constant Symbol_Table;
   --  Value to use as a default for unallocated symbol tables

   function Create return Symbol_Table;
   --  Allocate a new symbol table and return it

   function Find
     (ST     : Symbol_Table;
      T      : Text_Type;
      Create : Boolean := True)
      return Symbol_Type
      with Inline;
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

   type Symbol_Table is access Sets.Set;

   No_Symbol_Table : constant Symbol_Table := null;

end Langkit_Support.Symbols;
