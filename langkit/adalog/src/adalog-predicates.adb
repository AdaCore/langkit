------------------------------------------------------------------------------
--                               A D A L O G                                --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

package body Adalog.Predicates is

   ---------------
   -- Predicate --
   ---------------

   package body Predicate is

      ----------
      -- Free --
      ----------

      procedure Free (Inst : in out Predicate_Logic) is
      begin
         Remove_Predicate (Inst.Ref, Inst'Unrestricted_Access);
         Free (Inst.Pred);
      end Free;

      -----------
      -- Solve --
      -----------

      overriding function Apply
        (Inst : in out Predicate_Logic) return Boolean
      is
      begin
         if Is_Defined (Inst.Ref) then
            return Call (Inst.Pred, GetL (Inst.Ref));
         else
            --  If the variable is not set, then predicate will return True all
            --  the time, and we register the predicate to be called at a later
            --  time.
            Add_Predicate (Inst.Ref, Inst'Unchecked_Access);
            return True;
         end if;
      end Apply;

      -----------
      -- Reset --
      -----------

      procedure Revert (Inst : in out Predicate_Logic) is
      begin
         Remove_Predicate (Inst.Ref, Inst'Unchecked_Access);
      end Revert;

   end Predicate;

end Adalog.Predicates;
