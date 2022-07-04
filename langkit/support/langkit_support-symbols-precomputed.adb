--
--  Copyright (C) 2020-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

package body Langkit_Support.Symbols.Precomputed is

   -------------------------
   -- Create_Symbol_Table --
   -------------------------

   function Create_Symbol_Table return Precomputed_Symbol_Table is
   begin
      return Result : constant Precomputed_Symbol_Table
        := new Precomputed_Symbol_Table_Record do
         for I in Precomputed_Symbol_Index'Range loop
            Result.Precomputed (I) := Find
              (Symbol_Table (Result), Precomputed_Symbol (I));
         end loop;
      end return;
   end Create_Symbol_Table;

   ------------------------
   -- Precomputed_Symbol --
   ------------------------

   function Precomputed_Symbol
     (ST    : Precomputed_Symbol_Table;
      Index : Precomputed_Symbol_Index) return Symbol_Type
   is
   begin
      return Get_Symbol (Symbol_Table (ST), Precomputed_Symbol (ST, Index));
   end Precomputed_Symbol;

   ------------------------
   -- Precomputed_Symbol --
   ------------------------

   function Precomputed_Symbol
     (ST    : Precomputed_Symbol_Table;
      Index : Precomputed_Symbol_Index) return Thin_Symbol
   is
   begin
      return ST.Precomputed (Index);
   end Precomputed_Symbol;

end Langkit_Support.Symbols.Precomputed;
