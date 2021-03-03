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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;

with GNAT.String_Hash;

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Vectors;

--  This package provides a symbol table for text (Text_Type) identifiers. This
--  is used in Langkit for the interning of symbols.
--
--  The main interest is to:
--
--  1. Use less memory by interning strings.
--
--  2. Be faster when using the symbol as map keys for example, because hashing
--     is faster.

package Langkit_Support.Symbols is

   type Symbol_Table_Record is tagged private;

   type Symbol_Table is access all Symbol_Table_Record'Class;
   --  Represents a symbol table. The symbol table is the holder of the memory
   --  allocated for each symbol, and serves as a single access point if you
   --  want to find back an existing symbol.

   No_Symbol_Table : constant Symbol_Table;
   --  Value to use as a default for unallocated symbol tables

   type Symbol_Type is new Text_Cst_Access;
   --  Main symbol type.
   --
   --  WARNING: For usability reasons, we use the access to the string as a
   --  symbol type. This is very convenient because you can access the text of
   --  a symbol without a reference to the symbol table, but is also unsafe,
   --  because if the symbol table has been freed, the symbol will be a
   --  dangling pointer.

   function Image (S : Symbol_Type) return Text_Type;
   --  Return the text associated to this symbol

   function Image
     (S : Symbol_Type; With_Quotes : Boolean := False) return String;
   --  Return the text associated with this symbol, as a string

   type Thin_Symbol is private;
   --  Thin symbol type. This type is a bit heavier to use than the main symbol
   --  type, because you need a reference to the symbol table to get the text
   --  of the symbol, but:
   --
   --  1. It consumes less memory (which is the primary reason it is used in
   --     Langkit).
   --
   --  2. It is safer, as long as you never store ``Symbol_Type`` instances
   --     returned by ``Get_Symbol`` you should be safe.
   --
   --  TODO: See if we can get rid of the intermediate operation that returns a
   --  ``Symbol_Type``.

   No_Thin_Symbol : constant Thin_Symbol;

   function Get_Symbol
     (Self : Symbol_Table; TS : Thin_Symbol) return Symbol_Type;
   --  Return the Symbol for this ``Thin_Symbol`` instance

   function Create_Symbol_Table return Symbol_Table;
   --  Allocate a new symbol table and return it

   function Find
     (ST     : Symbol_Table;
      T      : Text_Type;
      Create : Boolean := True) return Thin_Symbol with Inline;
   --  Look for an entry for the T text in the ST symbol table. If there is
   --  such an entry, return it. Otherwise, create it and return it if Create
   --  is true. Elsewise, return null.
   --
   --  Non-null returned accesses are guaranteed to be the same for all equal
   --  Text_Type.

   function Find
     (ST     : Symbol_Table;
      T      : Text_Type;
      Create : Boolean := True) return Symbol_Type
   is
      (Get_Symbol (ST, Find (ST, T, Create))) with Inline;
   --  Overload of ``Find`` which returns a ``Symbol`` directly

   procedure Destroy (ST : in out Symbol_Table);
   --  Deallocate a symbol table and all the text returned by the corresponding
   --  calls to Find.

   function Hash (ST : Symbol_Type) return Hash_Type;
   --  Default hash function for symbols.
   --  WARNING: It assumes that you don't mix symbols from different symbol
   --  tables, but doesn't verify it!

   -----------------------------
   -- Symbol canonicalization --
   -----------------------------

   type Symbolization_Result (Success : Boolean; Size : Natural) is record
      case Success is
         when True  =>
            Symbol : Text_Type (1 .. Size);
            --  Text for successfully symbolized identifiers

         when False =>
            Error_Message : Text_Type (1 .. Size);
            --  Message describing why symbolization failed
      end case;
   end record;
   --  Holder for results of the symbolization process, conditionned by whether
   --  this process was successful.

   function Create_Symbol (Name : Text_Type) return Symbolization_Result is
     ((Success => True, Size => Name'Length, Symbol => Name));
   --  Shortcut to create successful symbolization results

   function Create_Error (Message : Text_Type) return Symbolization_Result is
     ((Success => False, Size => Message'Length, Error_Message => Message));
   --  Shortcut to create failed symbolization results

   function Fold_Case (Name : Text_Type) return Symbolization_Result;
   --  Convert Name to lowercase (cannot fail).
   --
   --  This is the default symbol canonicalizer when case insensitivity is
   --  enabled.

private

   type Thin_Symbol is mod 2 ** 32;

   function Hash is new GNAT.String_Hash.Hash
     (Char_Type => Wide_Wide_Character,
      Key_Type  => Text_Type,
      Hash_Type => Ada.Containers.Hash_Type);

   function String_Hash (T : Symbol_Type) return Ada.Containers.Hash_Type is
     (Hash (T.all));

   function Key_Equal (L, R : Symbol_Type) return Boolean is (L.all = R.all);

   package Maps is new Ada.Containers.Hashed_Maps
     (Key_Type            => Symbol_Type,
      Element_Type        => Thin_Symbol,
      Hash                => String_Hash,
      Equivalent_Keys     => Key_Equal,
      "="                 => "=");

   package Symbol_Vectors
   is new Langkit_Support.Vectors (Symbol_Type);

   type Symbol_Table_Record is tagged record
      Symbols_Map : Maps.Map;
      Symbols     : Symbol_Vectors.Vector;
   end record;

   No_Symbol_Table : constant Symbol_Table := null;

   No_Thin_Symbol  : constant Thin_Symbol := 0;

end Langkit_Support.Symbols;
