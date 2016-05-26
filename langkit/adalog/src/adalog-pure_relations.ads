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

with Adalog.Relation; use Adalog.Relation;

package Adalog.Pure_Relations is

   --------------------
   -- False_Relation --
   --------------------

   type False_Relation_Rec is null record;
   function Apply (Inst : in out False_Relation_Rec) return Boolean is (False);
   procedure Free (Inst : in out False_Relation_Rec) is null;
   package False_Relation is new Pure_Relation (Ty => False_Relation_Rec);

   -------------------
   -- True_Relation --
   -------------------

   type True_Relation_Rec is null record;
   function Apply (Inst : in out True_Relation_Rec) return Boolean is (True);
   procedure Free (Inst : in out True_Relation_Rec) is null;
   package True_Relation is new Pure_Relation (Ty => True_Relation_Rec);

   ----------------------
   -- Boolean_Relation --
   ----------------------

   type Boolean_Relation_Rec is record
      Result : Boolean;
   end record;

   function Apply (Inst : in out Boolean_Relation_Rec) return Boolean
   is (Inst.Result);

   procedure Free (Inst : in out Boolean_Relation_Rec) is null;

   package Boolean_Relation is
     new Pure_Relation (Ty => Boolean_Relation_Rec);

end Adalog.Pure_Relations;
