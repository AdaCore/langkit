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
      -- Apply --
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

      ------------
      -- Revert --
      ------------

      procedure Revert (Inst : in out Predicate_Logic) is
      begin
         Remove_Predicate (Inst.Ref, Inst'Unchecked_Access);
      end Revert;

   end Predicate;

   -----------------
   -- N_Predicate --
   -----------------

   package body N_Predicate is

      ----------
      -- Free --
      ----------

      procedure Free (Inst : in out Predicate_Logic) is
      begin
         for Ref of Inst.Refs loop
            Remove_Predicate (Ref, Inst'Unrestricted_Access);
         end loop;
         Free (Inst.Pred);
      end Free;

      -----------
      -- Apply --
      -----------

      overriding function Apply
        (Inst : in out Predicate_Logic) return Boolean
      is
      begin
         if (for all Ref of Inst.Refs => Is_Defined (Ref)) then
            declare
               Vals : Val_Array (1 .. Arity);
            begin
               for I in Inst.Refs'Range loop
                  Vals (I) := GetL (Inst.Refs (I));
               end loop;

               return Call (Inst.Pred, Vals);
            end;
         else
            for Ref of Inst.Refs loop
               Add_Predicate (Ref, Inst'Unchecked_Access);
            end loop;

            return True;
         end if;
      end Apply;

      ------------
      -- Revert --
      ------------

      procedure Revert (Inst : in out Predicate_Logic) is
      begin
         for Ref of Inst.Refs loop
            Remove_Predicate (Ref, Inst'Unchecked_Access);
         end loop;
      end Revert;

   end N_Predicate;

   package body Predicate_2 is

      procedure Free (Self : Predicate_Wrapper) is
      begin
         Free (Self.T);
      end Free;

      ------------
      -- Create --
      ------------

      function Create
        (L, R : Var.Var; Pred : Predicate_Type) return Relation
      is
      begin
         return Predicate_2_Internal.Create
           ((L, R), Predicate_Wrapper'(Pred, L, R));
      end Create;

   end Predicate_2;

end Adalog.Predicates;
