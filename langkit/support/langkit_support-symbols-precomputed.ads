--
--  Copyright (C) 2020-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

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
