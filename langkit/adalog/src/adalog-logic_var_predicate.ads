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

with Langkit_Support.Cheap_Sets;

--  This package defines a support interface for logic variables. Logic
--  variables can have predicates associated to them. This is the interface
--  that such predicates must implement. It is quite close to the relation
--  interface generally, but a distinction is made because the inner workings
--  should not be the same.
--
--  In particular, it is expected that Var_Predicates be idempotent, eg. the
--  Apply operation should always return the same result for the same value
--  of the logic variable it is linked to.
--
--  It is also expected that predicates keep a reference to the logic var
--  they're linked to at construction time, which is why operations on
--  predicates do not take a logic var formal.

package Adalog.Logic_Var_Predicate is
   type Var_Predicate_Type is abstract tagged null record;

   function Apply
     (Inst : in out Var_Predicate_Type) return Boolean is abstract;
   --  Apply the predicate, and return whether it succeeded or not

   type Var_Predicate is access all Var_Predicate_Type'Class;
   --  Access type that is meant to be used by clients. Predicates are meant to
   --  be manipulated through accesses.

   package Pred_Sets
   is new Langkit_Support.Cheap_Sets (Var_Predicate, null);
   --  Logic variables will want to manipulate sets of predicates associated to
   --  them.

end Adalog.Logic_Var_Predicate;
