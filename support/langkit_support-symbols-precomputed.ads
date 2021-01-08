------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

generic
   type Precomputed_Symbol_Index is (<>);
   --  Indexes for symbols to pre-compute in each symbol table

   with function Precomputed_Symbol
     (Index : Precomputed_Symbol_Index) return Text_Type is <>;
   --  Return the symbol corresponding to the precomputed symbol Index

package Langkit_Support.Symbols.Precomputed is

   type Precomputed_Symbol_Table_Record
   is new Symbol_Table_Record with private;

   type Precomputed_Symbol_Table
   is access all Precomputed_Symbol_Table_Record'Class;

   function Precomputed_Symbol
     (ST    : Precomputed_Symbol_Table;
      Index : Precomputed_Symbol_Index) return Thin_Symbol
     with Inline;
   function Precomputed_Symbol
     (ST    : Precomputed_Symbol_Table;
      Index : Precomputed_Symbol_Index) return Symbol_Type
     with Inline;
   --  Return the precomputed symbol corresponding to Index

   function Create_Symbol_Table return Precomputed_Symbol_Table;
   --  Allocate a new symbol table and return it

private
   type Precomputed_Symbol_Array is
     array (Precomputed_Symbol_Index) of Thin_Symbol;

   type Precomputed_Symbol_Table_Record is new Symbol_Table_Record with record
      Precomputed : Precomputed_Symbol_Array;
   end record;
end Langkit_Support.Symbols.Precomputed;
