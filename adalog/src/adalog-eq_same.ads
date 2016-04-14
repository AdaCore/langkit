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
with Adalog.Logic_Ref;
with Adalog.Unify;

--  Convenience wrapper generic package that, from a type implementing
--  equality, will instantiate all the needed stuff to create logical
--  equations containing that type, namely:
--
--  * A Logic_Ref instantiation so that you can have logical variables holding
--  values of the type.
--
--  * An instantiation of Unify, so that you can do logical equality comparison
--  between logic references and real instances of the type.
--
--  Since you're gonna want to express more complex logical predicates using
--  logical And/Or operations, you can either:
--
--  1. Use Adalog.Dynamic_Ops for that, together with the dynamic wrappers
--  provided in Unify.
--
--  2. Create your own instantiations of Adalog.Operations, if you need the
--  extra speed.
--
--  Please note that 2. is not without compromises, it will make your code much
--  harder to understand, due to the absence of implicit generic instantiation
--  in Ada, and the size of the generated code might explode.

generic
   type LR_Type is private;
   with function "=" (L, R : LR_Type) return Boolean is <>;
package Adalog.Eq_Same is

   function Convert (From : LR_Type) return LR_Type
   is (From) with Inline_Always;

   package Refs is new Logic_Ref (LR_Type);

   package Refcounted_Impl is new Unify
     (LR_Type, LR_Type,
      Left_Var  => Refs.Refcounted_Logic_Var,
      Right_Var => Refs.Refcounted_Logic_Var);

   package Raw_Impl is new Unify
     (LR_Type, LR_Type,
      Left_Var  => Refs.Raw_Logic_Var,
      Right_Var => Refs.Raw_Logic_Var);

   subtype Refcounted_Member_Array is Refcounted_Impl.Unify_Left.R_Type_Array;
   subtype Raw_Member_Array is Raw_Impl.Unify_Left.R_Type_Array;

end Adalog.Eq_Same;
