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

with Langkit_Support.Adalog.Logic_Var;

generic
   with package Logic_Vars is new Langkit_Support.Adalog.Logic_Var (<>);
package Langkit_Support.Adalog.Solver_Interface is

   use Logic_Vars;

   -------------------
   -- Functor types --
   -------------------

   --  The solver contains a number of abstract functor types, that are meant
   --  to be derived by the client to provide functionality.
   --
   --  The reason functor types are exposed is if you need to store state along
   --  with your function. If you don't, there are convenience constructors
   --  that take access to functions.

   type Base_Functor_Type is abstract tagged record
      Ref_Count : Natural;
      --  Functors are generally dynamically allocated and handled through
      --  access types. This tracks the number of references to a functor, so
      --  that we know when to deallocate it.
   end record;
   procedure Destroy (Self : in out Base_Functor_Type) is null;

   --------------------
   -- Predicate_Type --
   --------------------

   type Predicate_Type is abstract new Base_Functor_Type with null record;
   function Call
     (Self : Predicate_Type; Val : Value_Type) return Boolean is abstract;
   function Image (Self : Predicate_Type) return String is ("");
   function Full_Image
     (Self : Predicate_Type; Dummy_Var : Logic_Vars.Logic_Var) return String
   is ("");
   --  A predicate encapsulates the logic of applying a boolean predicate to a
   --  value, returning whether the predicate succeeds.

   ----------------------
   -- N_Predicate_Type --
   ----------------------

   type N_Predicate_Type is abstract new Base_Functor_Type with null record;
   function Call
     (Self : N_Predicate_Type; Vals : Logic_Vars.Value_Array) return Boolean
   is abstract;
   function Image (Self : N_Predicate_Type) return String is ("");
   function Full_Image
     (Self : N_Predicate_Type; Dummy_Vars : Logic_Var_Array) return String
   is ("");
   --  A predicate encapsulates the logic of applying a boolean predicate to a
   --  list of values, returning whether the predicate succeeds.

   -------------------
   -- Comparer_Type --
   -------------------

   type Comparer_Type is abstract new Base_Functor_Type with null record;
   function Compare
     (Self : Comparer_Type; L, R : Value_Type) return Boolean is abstract;
   function Image (Self : Comparer_Type) return String is ("");
   --  Type to compare two values, returning whether they are equal

   function No_Comparer return Comparer_Type'Class;
   --  Return a special comparer which considers two values are always
   --  different.

   --------------------
   -- Converter_Type --
   --------------------

   type Converter_Type is abstract new Base_Functor_Type with null record;
   function Convert
     (Self : Converter_Type; From : Value_Type) return Value_Type
      is abstract;
   function Image (Self : Converter_Type) return String is ("");
   --  Type to convert one value to another

   function No_Converter return Converter_Type'Class;
   --  Return a special converter that just raises a ``Program_Error`` when
   --  called.

   -------------------------------------
   -- Stateless functors constructors --
   -------------------------------------

   --  Those constructors are a shortcut to avoid creating custom functor types
   --  when you have no state to store.

   function Predicate
     (Pred      : access function (V : Value_Type) return Boolean;
      Pred_Name : String := "Predicate")
      return Predicate_Type'Class;
   --  Create a Predicate relation. A Predicate relation will solve
   --  successfully if the ``Predicate`` applied to the value of
   --  ``Logic_Var`` yields ``True``.

   function N_Predicate
     (Pred      : access function (V : Value_Array) return Boolean;
      Pred_Name : String := "N_Predicate")
      return N_Predicate_Type'Class;

   function Comparer
     (Pred      : access function (L, R : Value_Type) return Boolean;
      Pred_Name : String := "Comparer") return Comparer_Type'Class;

   function Converter
     (Pred      : access function (V : Value_Type) return Value_Type;
      Pred_Name : String := "Converter") return Converter_Type'Class;

   --------------------------
   -- Solving options type --
   --------------------------

   type Solve_Options_Type is record
      Simplify : Boolean := True;
      --  Try to split Any relations in ``Self`` looking for contradictions in
      --  its atoms through a depth first traversal.

      Cut_Dead_Branches : Boolean := True;
      --  Whether to enable an optimization that will cut branches that
      --  necessarily contain falsy solutions.
   end record;

   Default_Options : constant Solve_Options_Type := (others => <>);

end Langkit_Support.Adalog.Solver_Interface;
