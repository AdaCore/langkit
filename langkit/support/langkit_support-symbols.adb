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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package body Langkit_Support.Symbols is

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Symbol_Table_Record, Symbol_Table);

   -----------
   -- Image --
   -----------

   function Image (S : Symbol_Type) return Text_Type is
   begin
      return (if S = null then "<no symbol>" else S.all);
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (S : Symbol_Type; With_Quotes : Boolean := False) return String is
   begin
      if S = null then
         return "<no symbol>";
      else
         return Image (S.all, With_Quotes);
      end if;
   end Image;

   -------------------------
   -- Create_Symbol_Table --
   -------------------------

   function Create_Symbol_Table return Symbol_Table is
   begin
      return Result : constant Symbol_Table := new Symbol_Table_Record do
         for I in Precomputed_Symbol_Index'Range loop
            Result.Precomputed (I) := Find (Result, Precomputed_Symbol (I));
         end loop;
      end return;
   end Create_Symbol_Table;

   ------------------------
   -- Precomputed_Symbol --
   ------------------------

   function Precomputed_Symbol
     (ST : Symbol_Table; Index : Precomputed_Symbol_Index) return Symbol_Type
   is
   begin
      --  For languages that carry no precomputed symbols, Index can have no
      --  value, so we have noisy but useless warning.
      pragma Warnings (Off, "value not in range");
      return ST.Precomputed (Index);
      pragma Warnings (On, "value not in range");
   end Precomputed_Symbol;

   ----------
   -- Find --
   ----------

   function Find
     (ST     : Symbol_Table;
      T      : Text_Type;
      Create : Boolean := True)
      return Symbol_Type
   is
      use Sets;

      T_Acc  : Symbol_Type := T'Unrestricted_Access;
      Result : constant Cursor := ST.Symbols.Find (T_Acc);
   begin
      --  If we already have such a symbol, return the access we already
      --  internalized. Otherwise, give up if asked to.

      if Has_Element (Result) then
         return Element (Result);
      elsif not Create then
         return null;
      end if;

      --  At this point, we know we have to internalize a new symbol

      T_Acc := new Text_Type'(T);
      ST.Symbols.Insert (T_Acc);
      return T_Acc;
   end Find;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (ST : in out Symbol_Table) is
      use Sets;
      C : Cursor := ST.Symbols.First;
   begin
      while Has_Element (C) loop
         declare
            --  We keep Symbol_Type to be a constant access everywhere for
            --  simplification, but we know symbol tables are the only owners
            --  of these, so stripping the "constant" attribute away here is
            --  known to be safe.

            function Convert is new Ada.Unchecked_Conversion
              (Symbol_Type, Text_Access);
            To_Free : Text_Access := Convert (Element (C));
         begin
            Next (C);
            Free (To_Free);
         end;
      end loop;
      Deallocate (ST);
   end Destroy;

   ----------
   -- Hash --
   ----------

   function Hash (ST : Symbol_Type) return Hash_Type is

   begin
      if ST = null then
         return Hash_Type (0);
      else
         return Hash_Type'Mod (To_Integer (ST.all'Address));
      end if;
   end Hash;

end Langkit_Support.Symbols;
