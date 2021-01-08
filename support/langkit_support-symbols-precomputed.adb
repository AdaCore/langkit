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
