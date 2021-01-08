------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                         --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Logic_Var;
with Langkit_Support.Adalog.Relations;
use Langkit_Support.Adalog.Relations;

package Langkit_Support.Adalog.Predicates is

   ---------------
   -- Predicate --
   ---------------

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

      with procedure Free (Self : in out Predicate_Type) is null;

      with function Image (Self : Predicate_Type) return String is <>;

   package Predicate is

      function Create
        (R : Var.Var; Pred : Predicate_Type) return Relation;
      --  Return a predicate relation, where Pred is the actual implementation
      --  of the predicate logic. Pred will be called on the value of R when
      --  appropriate.

   private

      use Var;

      type Predicate_Logic is record
         Ref  : Var.Var;
         Pred : Predicate_Type;
      end record;
      --  This is the internal predicate type, that will be stored along the
      --  variable if necessary. The Apply operation is idempotent, eg. always
      --  return the same result (provided the provided predicate satisfies
      --  this invariant).

      function Apply (Self : in out Predicate_Logic) return Solving_State;

      procedure Revert (Self : in out Predicate_Logic);
      procedure Free (Self : in out Predicate_Logic);

      function Custom_Image (Self : Predicate_Logic) return String
      is ("Predicate " & Image (Self.Pred) & " on " & Var.Image (Self.Ref));

      package Impl is new Stateful_Relation (Ty => Predicate_Logic);
      --  This package contains the Base_Relation that is actually to
      --  be used by the clients when constructing equations. So as to not
      --  yield solutions for ever, the implementation is wrapped into a
      --  Stateful_Relation, that will return evaluation of the predicate
      --  only once, until it is reverted.

      function Create
        (R : Var.Var; Pred : Predicate_Type) return Relation
      is (new Impl.Rel'
            (Rel    => Predicate_Logic'(Ref => R, Pred => Pred),
             others => <>));

   end Predicate;

   -------------------
   -- Dyn_Predicate --
   -------------------

   --  Convenience package to create predicates from access to functions. Not
   --  used by langkit, meant for general purpose use of the library.

   generic
      type El_Type is private;
      with package Var is new Logic_Var
        (Element_Type => El_Type, others => <>);
   package Dyn_Predicate is

      function Create
        (R    : Var.Var;
         Pred : access function (L : El_Type) return Boolean) return Relation;

   private

      type Predicate_Holder is record
         Pred : access function (L : El_Type) return Boolean;
      end record;

      function Call (Self : Predicate_Holder; L : El_Type) return Boolean
      is (Self.Pred.all (L));

      function Image (Dummy_Self : Predicate_Holder) return String is ("");

      package Internal_Pred is new Predicate (El_Type, Var, Predicate_Holder);

      function Create
        (R : Var.Var; Pred : access function (L : El_Type) return Boolean)
         return Relation
      is (Internal_Pred.Create (R, (Pred => Pred'Unrestricted_Access.all)));

   end Dyn_Predicate;

   -----------------
   -- N_Predicate --
   -----------------

   --  N_Predicate is like Predicate, except that instead of being a predicate
   --  on the value of one logic variable, it is a predicate on the value of
   --  N logic variables. While this package can be used directly, it is not
   --  practical, and is mainly done to decouple the logic. Proxy packages
   --  (See Predicate_2 below) will be added when needed.

   generic
      type El_Type is private;
      with package Var is new Logic_Var
        (Element_Type => El_Type, others => <>);

      Arity : Natural;

      type Predicate_Type is private;

      with function Call
        (Self : Predicate_Type; Vals : Var.Val_Array) return Boolean is <>;

      with function Image (Self : Predicate_Type) return String is <>;

      with procedure Free (Self : in out Predicate_Type) is null;

   package N_Predicate is

      function Create
        (R    : Var.Var_Array;
         Pred : Predicate_Type) return Relation;
      --  Return a predicate relation, where Pred is the actual implementation
      --  of the predicate logic. Pred will be called on the value of R when
      --  appropriate.

   private

      use Var;

      type Predicate_Logic is record
         Refs  : Var_Array (1 .. Arity);
         Pred  : Predicate_Type;
      end record;
      --  This is the internal predicate type, that will be stored along the
      --  variable if necessary. The Apply operation is idempotent, eg. always
      --  return the same result (provided the provided predicate satisfies
      --  this invariant).

      function Apply (Self : in out Predicate_Logic) return Solving_State;

      procedure Revert (Self : in out Predicate_Logic);
      procedure Free (Self : in out Predicate_Logic);

      function Img (Refs : Var_Array) return String
      is
        (Var.Image (Refs (Refs'First))
         & (if Refs'Length > 1
            then Img (Refs (Refs'First + 1 .. Refs'Last))
            else ""));

      function Custom_Image (Self : Predicate_Logic) return String
      is ("PREDICATE " & Image (Self.Pred) & " ON " & Img (Self.Refs));

      package Impl is new Stateful_Relation (Ty => Predicate_Logic);
      --  This package contains the Base_Relation that is actually to
      --  be used by the clients when constructing equations. So as to not
      --  yield solutions for ever, the implementation is wrapped into a
      --  Stateful_Relation, that will return evaluation of the predicate
      --  only once, until it is reverted.

      function Create (R : Var_Array; Pred : Predicate_Type) return Relation
      is (new Impl.Rel'
            (Rel    => Predicate_Logic'(Refs => R, Pred => Pred),
             others => <>));

   end N_Predicate;

   -----------------
   -- Predicate_2 --
   -----------------

   --  Predicates that will act on two logic variables

   generic
      type El_Type is private;
      with package Var is new Logic_Var
        (Element_Type => El_Type, others => <>);

      type Predicate_Type is private;

      with function Call
        (Self : Predicate_Type; L, R : El_Type) return Boolean is <>;

      with function Image (Self : Predicate_Type) return String is <>;

      with procedure Free (Self : in out Predicate_Type) is null;

   package Predicate_2 is

      function Create
        (L, R : Var.Var; Pred : Predicate_Type) return Relation;
      --  Return a predicate relation, where Pred is the actual implementation
      --  of the predicate logic. Pred will be called on the value of L and R
      --  when appropriate.

   private

      type Predicate_Wrapper is record
         T    : Predicate_Type;
         L, R : Var.Var;
      end record;

      function Call
        (P : Predicate_Wrapper; Vals : Var.Val_Array) return Boolean
      is (Call (P.T, Vals (1), Vals (2)));

      function Image (Self : Predicate_Wrapper) return String
      is (Image (Self.T));

      procedure Free (Self : in out Predicate_Wrapper);

      package Predicate_2_Internal is new N_Predicate
        (El_Type, Var, 2, Predicate_Wrapper, Call, Image, Free);

   end Predicate_2;

   generic package Predicate_1 renames Predicate;
   --  Renaming for Predicate, to help with code generation

end Langkit_Support.Adalog.Predicates;
