--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body Liblktlang_AdaSAT.Decisions is

   ----------------------
   -- First_Unassigned --
   ----------------------

   function First_Unassigned
     (M : Model; First_Unset : in out Variable) return Variable_Or_Null
   is
   begin
      for I in First_Unset .. M'Last loop
         if M (I) in Unset then
            --  We can update ``First_Unset`` since we were exactly looking for
            --  the first unset variable.
            First_Unset := I + 1;
            return I;
         end if;
      end loop;
      return 0;
   end First_Unassigned;
end Liblktlang_AdaSAT.Decisions;
