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

with Adalog.Abstract_Relation;   use Adalog.Abstract_Relation;
with Adalog.Logic_Var;
with Adalog.Logic_Var_Predicate; use Adalog.Logic_Var_Predicate;
with Adalog.Relations;           use Adalog.Relations;

package Adalog.Predicates is

   --  Generic predicate package, that is applied on one logic variable.
   --
   --  Applying a predicate on a logic variable ensures that in all the
   --  solutions to the equation, this predicate will be satisfied.
   --
   --  For flexibility, the predicate that the user passes to this package is a
   --  type with a Call procedure, so that you can store state along with your
   --  predicate.

   generic
      type El_Type is private;
      with package Var is new Logic_Var
        (Element_Type => El_Type, others => <>);

      type Predicate_Type is private;

      with function Call
        (Self : Predicate_Type; L : El_Type) return Boolean is <>;

      with procedure Free (Self : Predicate_Type) is null;

   package Predicate is

      function Create
        (R : Var.Var; Pred : Predicate_Type) return access I_Relation'Class;
      --  Return a predicate relation, where Pred is the actual implementation
      --  of the predicate logic. Pred will be called on the value of R when
      --  appropriate.

   private

      use Var;

      type Predicate_Logic is new Var_Predicate_Type with record
         Ref  : Var.Var;
         Pred : Predicate_Type;
      end record;
      --  This is the internal predicate type, that will be stored along the
      --  variable if necessary. The Apply operation is idempotent, eg. always
      --  return the same result (provided the provided predicate satisfies
      --  this invariant).

      function Apply
        (Inst : in out Predicate_Logic) return Boolean;

      procedure Revert (Inst : in out Predicate_Logic);
      procedure Free (Inst : in out Predicate_Logic);

      package Impl is new Stateful_Relation (Ty => Predicate_Logic);
      --  This package contains the I_Relation wrapper that is actually to
      --  be used by the clients when constructing equations. So as to not
      --  yield solutions for ever, the implementation is wrapped into a
      --  Stateful_Relation, that will return evaluation of the predicate
      --  only once, until it is reverted.

      function Create
        (R : Var.Var; Pred : Predicate_Type) return access I_Relation'Class
      is (new Impl.Rel'
            (Rel    => Predicate_Logic'(Ref => R, Pred => Pred),
             others => <>));

   end Predicate;

   generic
      type El_Type is private;
      with package Var is new Logic_Var
        (Element_Type => El_Type, others => <>);
   package Dyn_Predicate is

      type Predicate_Access is access function (L : El_Type) return Boolean;

      type Predicate_Logic is record
         Ref : Var.Var;
         P   : access function (L : El_Type) return Boolean;
      end record;

      function Apply (Inst : in out Predicate_Logic) return Boolean;
      procedure Revert (Inst : in out Predicate_Logic);
      procedure Free (Inst : in out Predicate_Logic) is null;

      package Impl is new Stateful_Relation (Ty => Predicate_Logic);

      function Create
        (R    : Var.Var; Pred : Predicate_Access)
         return access I_Relation'Class
      is
        (new Impl.Rel'(Rel => Predicate_Logic'(Ref => R, P => Pred),
                       others => <>));

   end Dyn_Predicate;

end Adalog.Predicates;
