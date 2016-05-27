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

with Adalog.Eq_Same;
with Adalog.Predicates;        use Adalog.Predicates;

package Adalog.Main_Support is
   type My_Rec is record
      A : Integer;
      B : Integer;
   end record;

   function Convert (R : My_Rec) return Integer is (R.A) with Inline_Always;

   package Eq_Int is new Eq_Same (Integer);
   package Eq_Rec is new Eq_Same (My_Rec);

   package Pred_Int is
     new Dyn_Predicate (Integer, Eq_Int.Refs.Raw_Logic_Var);
end Adalog.Main_Support;
