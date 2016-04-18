with Ada.Unchecked_Deallocation;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

package body Langkit_Support.Symbols is

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Sets.Set, Symbol_Table);

   ------------
   -- Create --
   ------------

   function Create return Symbol_Table is
   begin
      return new Sets.Set;
   end Create;

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
      Result : constant Cursor := ST.Find (T_Acc);
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
      ST.Insert (T_Acc);
      return T_Acc;
   end Find;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (ST : in out Symbol_Table) is
      use Sets;
      C       : Cursor := ST.First;
   begin
      while Has_Element (C) loop
         declare
            To_Free : Symbol_Type := Element (C);
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
      S : constant Storage_Elements.Integer_Address :=
        To_Integer (ST.all'Address);
   begin
      return Hash_Type'Mod (S);
   end Hash;

end Langkit_Support.Symbols;
