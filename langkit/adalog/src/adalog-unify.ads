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

with Adalog.Abstract_Relation;  use Adalog.Abstract_Relation;
with Adalog.Logic_Var;
with Adalog.Pure_Relations;     use Adalog.Pure_Relations;
with Adalog.Unify_LR;
with Adalog.Unify_One_Side;

--  This package makes the unification algorithms of Adalog accessible under
--  the form of equality operators, and a membership function. Equality works
--  between logic variables or between logic variables and real values. It is
--  the base building block of constructing equations in Adalog.

--  See Eq_Same for an example of use

generic
   type L_Type is private;
   type R_Type is private;

   with function Convert (From : R_Type) return L_Type is <>;
   with function Convert (From : L_Type) return R_Type is <>;

   with package Left_Var is new Logic_Var
     (Element_Type => L_Type, others => <>);

   with package Right_Var is new Logic_Var
     (Element_Type => R_Type, others => <>);

package Adalog.Unify is

   --  TODO HACK FIXME??? P418-022 Removing the body for this package causes a
   --  generic instantiation error.
   procedure What;

   type R_Convert_Data is null record;
   type L_Convert_Data is null record;
   Dummy_L : L_Convert_Data;
   Dummy_R : R_Convert_Data;

   function Convert (Data : R_Convert_Data; From : R_Type) return L_Type
   is
     (Convert (From));

   function Convert (Data : L_Convert_Data; From : L_Type) return R_Type
   is
     (Convert (From));

   package Simple_Unify is new Adalog.Unify_LR
     (L_Type, R_Type, L_Convert_Data, R_Convert_Data,
      Convert, Convert, Left_Var, Right_Var);
   use Simple_Unify;

   function Equals (R : R_Type; L : L_Type) return Boolean
   is (Convert (L) = R) with Inline_Always;

   function Equals (L : L_Type; R : R_Type) return Boolean
   is (Convert (L) = R) with Inline_Always;

   package Unify_Left is new Unify_One_Side
     (L_Type, R_Type, Equals, Convert, Left_Var);

   package Unify_Right is new Unify_One_Side
     (R_Type, L_Type, Equals, Convert, Right_Var);

   ------------------
   -- Eq predicate --
   ------------------

   function "="
     (L : Left_Var.Var; R : Right_Var.Var) return Unify_LR_Rel.Rel
   is ((Rel => Create (L, R, Dummy_L, Dummy_R), others => <>)) with Inline;

   function "=" (L : L_Type; R : R_Type) return Boolean_Relation.Rel
   is ((Rel => (Result => Equals (L, R)), others => <>)) with Inline;

   function "=" (L : Left_Var.Var; R : R_Type) return Unify_Left.Rel.Rel
   is ((Rel => Unify_Left.Create (L, R), others => <>)) with Inline;

   function "=" (L : L_Type; R : Right_Var.Var) return Unify_Right.Rel.Rel
   is ((Rel => Unify_Right.Create (R, L), others => <>)) with Inline;

   -----------------------
   --  Dynamic wrappers --
   -----------------------

   function "=" (L : Left_Var.Var; R : R_Type) return Relation
   is (Unify_Left.Rel.Impl.Dynamic (L = R)) with Inline;

   function "=" (L : L_Type; R : Right_Var.Var) return Relation
   is (Unify_Right.Rel.Impl.Dynamic (L = R)) with Inline;

   function "=" (L : L_Type; R : R_Type) return Relation
   is (Boolean_Relation.Impl.Dynamic (L = R)) with Inline;

   function "=" (L : Left_Var.Var; R : Right_Var.Var) return Relation
   is (Unify_LR_Rel.Impl.Dynamic (L = R)) with Inline;

   function Equals (L : Left_Var.Var; R : R_Type) return Relation renames "=";

   function Equals (L : L_Type; R : Right_Var.Var) return Relation renames "=";

   function Equals (L : L_Type; R : R_Type) return Relation renames "=";

   function Equals
     (L : Left_Var.Var; R : Right_Var.Var) return Relation renames "=";

   ------------
   -- Member --
   ------------

   function Member
     (R    : Left_Var.Var;
      Vals : Unify_Left.R_Type_Array) return Unify_Left.Member_T
         renames Unify_Left.Member;

   function Member
     (R    : Left_Var.Var;
      Vals : Unify_Left.R_Type_Array) return Relation
      renames Unify_Left.Member;

end Adalog.Unify;
