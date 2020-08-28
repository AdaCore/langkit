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
      return Get_Symbol (ST, Precomputed_Symbol (ST, Index));
      pragma Warnings (On, "value not in range");
   end Precomputed_Symbol;

   ------------------------
   -- Precomputed_Symbol --
   ------------------------

   function Precomputed_Symbol
     (ST : Symbol_Table; Index : Precomputed_Symbol_Index) return Thin_Symbol
   is
   begin
      return ST.Precomputed (Index);
   end Precomputed_Symbol;

   ----------
   -- Find --
   ----------

   function Find
     (ST     : Symbol_Table;
      T      : Text_Type;
      Create : Boolean := True)
      return Thin_Symbol
   is
      use Maps;

      T_Acc  : Symbol_Type := T'Unrestricted_Access;
      Result : constant Cursor := ST.Symbols_Map.Find (T_Acc);
   begin
      --  If we already have such a symbol, return the access we already
      --  internalized. Otherwise, give up if asked to.

      if Has_Element (Result) then
         return Element (Result);
      elsif not Create then
         return No_Thin_Symbol;
      end if;

      --  At this point, we know we have to internalize a new symbol

      T_Acc := new Text_Type'(T);
      ST.Symbols.Append (T_Acc);

      ST.Symbols_Map.Insert (T_Acc, Thin_Symbol (ST.Symbols.Last_Index));
      return Thin_Symbol (ST.Symbols.Last_Index);
   end Find;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (ST : in out Symbol_Table) is
      use Maps;
      To_Free : Text_Access;
   begin
      ST.Symbols_Map.Clear;

      for El of ST.Symbols loop
         To_Free := Text_Access'(El.all'Unrestricted_Access);
         Free (To_Free);
      end loop;

      ST.Symbols.Destroy;
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

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Self : Symbol_Table; TS : Thin_Symbol) return Symbol_Type is
   begin
      return Self.Symbols.Get (Positive (TS));
   end Get_Symbol;

end Langkit_Support.Symbols;
