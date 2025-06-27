--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package body Liblktlang_AdaSAT is

   procedure Free_Clause is new Ada.Unchecked_Deallocation
     (Literal_Array, Clause);

   ---------
   -- "+" --
   ---------

   function "+" (V : Variable) return Literal is
     (Literal (V));

   ---------
   -- "-" --
   ---------

   function "-" (V : Variable) return Literal is
     (-Literal (V));

   -----------
   -- "abs" --
   -----------

   function Get_Var (L : Literal) return Variable is
   begin
      return Variable (Literal'(abs L));
   end Get_Var;

   -----------
   -- Image --
   -----------

   function Image (C : Clause) return String is
      use Ada.Strings.Unbounded;

      Res : Unbounded_String;
   begin
      Append (Res, "(");
      for I in C.all'Range loop
         declare
            Raw : constant String := C (I)'Image;
            Suf : constant String := Raw (Raw'First + 1 .. Raw'Last);
         begin
            if C (I) < 0 then
               Append (Res, "¬");
            end if;
            Append (Res, Suf);
            if I < C.all'Last then
               Append (Res, " | ");
            end if;
         end;
      end loop;
      Append (Res, ")");
      return To_String (Res);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (M : Model) return String is
      use Ada.Strings.Unbounded;

      Res   : Unbounded_String;
      First : Boolean := True;
   begin
      for I in M'Range loop
         if M (I) in True then
            if First then
               First := False;
            else
               Append (Res, " & ");
            end if;
            Append (Res, I'Image);
         end if;
      end loop;
      return To_String (Res);
   end Image;

   ----------
   -- Free --
   ----------

   procedure Free (C : in out Clause) is
   begin
      Free_Clause (C);
   end Free;
end Liblktlang_AdaSAT;
