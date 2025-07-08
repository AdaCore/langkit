--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded;

package body Liblktlang_AdaSAT.Formulas is

   -----------
   -- Image --
   -----------

   function Image (F : Formula) return String is
      use Ada.Strings.Unbounded;

      Res : Unbounded_String;
   begin
      for I in 1 .. F.Length loop
         Append (Res, Image (F.Get (I)));
         if I < F.Length then
            Append (Res, " & ");
         end if;
      end loop;
      return To_String (Res);
   end Image;

   ---------------
   -- Satisfies --
   ---------------

   function Satisfies (F : Formula; M : Model) return SAT_Result is
      OK : Variable_Value := False;
   begin
      for Dis of F loop
         for L of Dis.all loop
            declare
               Model_Value : constant Variable_Value := M (Get_Var (L));
               Required    : constant Variable_Value :=
                 (if L < 0 then False else True);
            begin
               if Model_Value in Unset then
                  OK := Unset;
               elsif Model_Value = Required then
                  OK := True;
                  exit;
               end if;
            end;
         end loop;

         case OK is
            when True =>
               OK := False;
            when False =>
               return UNSAT;
            when Unset =>
               return UNKNOWN;
         end case;
      end loop;
      return SAT;
   end Satisfies;

   --------------
   -- Free_All --
   --------------

   procedure Free_All (F : in out Formula) is
      Mutable_C : Clause;
   begin
      for C of F loop
         Mutable_C := C;
         Free (Mutable_C);
      end loop;
      F.Destroy;
   end Free_All;
end Liblktlang_AdaSAT.Formulas;
