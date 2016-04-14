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

   package body Predicate is

      use Var;

      -----------
      -- Apply --
      -----------

      function Call (Inst : in out Predicate_Logic) return Boolean is
        (Is_Defined (Inst.Ref)
         and then Predicate (GetL (Inst.Ref)));

      ------------
      -- Revert --
      ------------

      procedure Reset (Inst : in out Predicate_Logic) is
      begin
         null;
      end Reset;

      ------------
      -- Create --
      ------------

      function Create (R : Var.Var) return Predicate_Logic is
      begin
         return Predicate_Logic'(Ref => R);
      end Create;

   end Predicate;

   package body Dyn_Predicate is

      use Var;

      -----------
      -- Apply --
      -----------

      function Call (Inst : in out Predicate_Logic) return Boolean is
        (Is_Defined (Inst.Ref)
         and then Inst.P (GetL (Inst.Ref)));

      ------------
      -- Revert --
      ------------

      procedure Reset (Inst : in out Predicate_Logic) is
      begin
         null;
      end Reset;

      ------------
      -- Create --
      ------------

      function Create
        (R    : Var.Var;
         Pred : Predicate_Access)
         return Predicate_Logic
      is
      begin
         return Predicate_Logic'(Ref => R, P => Pred);
      end Create;

   end Dyn_Predicate;

end Adalog.Predicates;
