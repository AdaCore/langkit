--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Deallocation;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package body Langkit_Support.Symbols is

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Symbol_Table_Record'Class, Symbol_Table);

   -----------
   -- Image --
   -----------

   function Image (S : Symbol_Type) return Text_Type is
   begin
      return (if S = No_Symbol
              then "<no symbol>"
              else Get (S).all);
   end Image;

   ---------
   -- Get --
   ---------

   function Get (S : Symbol_Type) return Text_Access is
   begin
      if S = No_Symbol then
         return null;
      else
         return Get (S.Table, S.TS);
      end if;
   end Get;

   -----------
   -- Image --
   -----------

   function Image
     (S : Symbol_Type; With_Quotes : Boolean := False) return String is
   begin
      if S = No_Symbol then
         return "<no symbol>";
      else
         return Image (Get (S).all, With_Quotes);
      end if;
   end Image;

   -------------------------
   -- Create_Symbol_Table --
   -------------------------

   function Create_Symbol_Table return Symbol_Table is
   begin
      return new Symbol_Table_Record;
   end Create_Symbol_Table;

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

      T_Acc  : Text_Access := T'Unrestricted_Access;
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
      if ST = No_Symbol then
         return Hash_Type (0);
      else
         return Hash_Type'Mod (To_Integer (Get (ST).all'Address));
      end if;
   end Hash;

   ---------------
   -- To_Symbol --
   ---------------

   function To_Symbol
     (Self : Symbol_Table; TS : Thin_Symbol) return Symbol_Type is
   begin
      if TS = No_Thin_Symbol then
         return No_Symbol;
      else
         return (TS, Self);
      end if;
   end To_Symbol;

   ---------
   -- Get --
   ---------

   function Get
     (Self : Symbol_Table; TS : Thin_Symbol) return Text_Access is
   begin
      if TS = No_Thin_Symbol then
         return null;
      else
         return Self.Symbols.Get (Positive (TS));
      end if;
   end Get;

   ---------------
   -- Fold_Case --
   ---------------

   function Fold_Case (Name : Text_Type) return Symbolization_Result is
   begin
      return Result : Symbolization_Result
        (Success => True, Size => Name'Length)
      do
         for I in 1 .. Result.Size loop
            Result.Symbol (I) := To_Lower (Name (Name'First + I - 1));
         end loop;
      end return;
   end Fold_Case;

   ----------
   -- Find --
   ----------

   function Find
     (ST     : Symbol_Table;
      T      : Text_Type;
      Create : Boolean := True) return Symbol_Type
   is
   begin
      return (Find (ST, T, Create), ST);
   end Find;

end Langkit_Support.Symbols;
