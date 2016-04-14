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

with Adalog.Abstract_Relation; use Adalog.Abstract_Relation;
with Adalog.Logic_Var;
with Adalog.Pure_Relations; use Adalog.Pure_Relations;
with Adalog.Relation;       use Adalog.Relation;
with Adalog.Unify_One_Side;

--  This package makes the unification algorithms of Adalog accessible under
--  the form of equality operators, and a membership function. Equality works
--  between logic variables or between logic variables and real values. It is
--  the base building block of constructing equations in Adalog.

--  See Eq_Same for an example of use

generic
   type L_Type is private;
   type R_Type is private;
   with function "=" (L : L_Type; R : R_Type) return Boolean is <>;
   with function Convert (From : R_Type) return L_Type is <>;
   with function Convert (From : L_Type) return R_Type is <>;

   with package Left_Var is new Logic_Var
     (Element_Type => L_Type, others => <>);

   with package Right_Var is new Logic_Var
     (Element_Type => R_Type, others => <>);

package Adalog.Unify is

   function Equals (R : R_Type; L : L_Type) return Boolean
   is (L = R) with Inline_Always;

   function Equals (L : L_Type; R : R_Type) return Boolean
   is (L = R) with Inline_Always;

   package Unify_Left is new Unify_One_Side
     (L_Type => L_Type, R_Type => R_Type, Var => Left_Var);

   package Unify_Right is new Unify_One_Side
     (L_Type => R_Type, R_Type => L_Type, Var => Right_Var);

   --------------
   -- Unify_LR --
   --------------

   type LR_State is (No_Change, Left_Changed, Right_Changed);
   type Unify_LR is record
      Left  : Left_Var.Var;
      Right : Right_Var.Var;
      State : LR_State := No_Change;
   end record;

   function Apply (Self : in out Unify_LR) return Boolean;
   procedure Revert (Self : in out Unify_LR);

   function Create
     (Left : Left_Var.Var; Right : Right_Var.Var) return Unify_LR
   is ((Left => Left, Right => Right, State => No_Change));

   package Unify_LR_Rel is
     new Relation.Stateful_Relation (Unify_LR);

   ------------------
   -- Eq predicate --
   ------------------

   function "="
     (L : Left_Var.Var; R : Right_Var.Var) return Unify_LR_Rel.Rel
   is ((Rel => Create (L, R), others => <>)) with Inline;

   function "=" (L : L_Type; R : R_Type) return Boolean_Relation.Rel
   is ((Rel => (Result => L = R), others => <>)) with Inline;

   function "=" (L : Left_Var.Var; R : R_Type) return Unify_Left.Rel.Rel
   is ((Rel => Unify_Left.Create (L, R), others => <>)) with Inline;

   function "=" (L : L_Type; R : Right_Var.Var) return Unify_Right.Rel.Rel
   is ((Rel => Unify_Right.Create (R, L), others => <>)) with Inline;

   -----------------------
   --  Dynamic wrappers --
   -----------------------

   function "=" (L : Left_Var.Var; R : R_Type) return Rel
   is (Unify_Left.Rel.Impl.Dynamic (L = R)) with Inline;

   function "=" (L : L_Type; R : Right_Var.Var) return Rel
   is (Unify_Right.Rel.Impl.Dynamic (L = R)) with Inline;

   function "=" (L : L_Type; R : R_Type) return Rel
   is (Boolean_Relation.Impl.Dynamic (L = R)) with Inline;

   function "=" (L : Left_Var.Var; R : Right_Var.Var) return Rel
   is (Unify_LR_Rel.Impl.Dynamic (L = R)) with Inline;

   ------------
   -- Member --
   ------------

   function Member
     (R    : Left_Var.Var;
      Vals : Unify_Left.R_Type_Array) return Unify_Left.Member_T
         renames Unify_Left.Member;

   function Member
     (R    : Left_Var.Var;
      Vals : Unify_Left.R_Type_Array) return Rel renames Unify_Left.Member;

end Adalog.Unify;
