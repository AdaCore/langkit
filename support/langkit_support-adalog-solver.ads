------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Eq_Same;
with Langkit_Support.Adalog.Solver_Interface;
private with Langkit_Support.Adalog.Symbolic_Solver;

generic
   with package Solver_Ifc
     is new Langkit_Support.Adalog.Solver_Interface (<>);
   Debug : Boolean := False;
package Langkit_Support.Adalog.Solver is

   use Solver_Ifc;
   use Logic_Vars;

   type Relation is private;
   --  Type for a relation. A relation is either an atomic relation, or a
   --  compound relation, which can represent a tree of relations of unbounded
   --  depth. Solving a relation will assign values to every logic variable
   --  involved in the relation or fail.
   --
   --  A relation is manually ref-counted, and has an ownership share of every
   --  sub-relation. When the ref-count of a root relation reaches 0, the
   --  ownership share of each of its sub-relations is destroyed.

   No_Relation : constant Relation;

   procedure Inc_Ref (Self : Relation);
   --  Increments the reference count of Self

   procedure Dec_Ref (Self : in out Relation);
   --  Decrements the reference count of Self. If no reference left, deallocate
   --  ``Self.all`` and sets ``Self`` to ``null``.

   subtype Logic_Var_Array is Var_Array;

   procedure Solve
     (Self              : Relation;
      Solution_Callback : access function
        (Vars : Logic_Var_Array) return Boolean;
      Solve_Options     : Solve_Options_Type := Default_Options);
   --  Tries to solve ``Self``. For every solution, ``Solution_Callback`` will
   --  be called.

   function Solve_First
     (Self          : Relation;
      Solve_Options : Solve_Options_Type := Default_Options) return Boolean;
   --  Tries to solve ``Self``. If there is at least one valid solution to the
   --  relation, stops solving and return True. Else, return False.

   ---------------------------
   -- Relation constructors --
   ---------------------------

   type Relation_Array is array (Positive range <>) of Relation;
   No_Relation_Array : constant Relation_Array;

   function Create_Predicate
     (Logic_Var    : Var;
      Pred         : Predicate_Type'Class;
      Debug_String : String_Access := null) return Relation;
   --  Create a Predicate relation. A Predicate relation will solve
   --  successfully if the ``Predicate`` applied to the value of ``Logic_Var``
   --  yields ``True``.

   function Create_N_Predicate
     (Logic_Vars   : Variable_Array;
      Pred         : N_Predicate_Type'Class;
      Debug_String : String_Access := null) return Relation;

   function Create_Assign
     (Logic_Var    : Var;
      Value        : Value_Type;
      Conv         : Converter_Type'Class := No_Converter;
      Eq           : Comparer_Type'Class := No_Comparer;
      Debug_String : String_Access := null) return Relation;
   --  Create an Assign relation. An Assign relation will solve successfully if
   --  we can assign the value ``Value`` to ``Logic_Var``.

   function Create_Unify
     (From, To     : Var;
      Debug_String : String_Access := null) return Relation;
   --  Create an Unify relation. An Unify relation will solve successfully if
   --  either the assignment of ``From.Value`` to ``To`` is successful, either
   --  the opposite assignment is successful.

   function Create_Propagate
     (From, To     : Var;
      Conv         : Converter_Type'Class := No_Converter;
      Eq           : Comparer_Type'Class := No_Comparer;
      Debug_String : String_Access := null) return Relation;
   --  Create a Propagate relation. A Propagate relation will solve
   --  successfully if the assignment of ``From.Value`` to ``To`` is
   --  successful.

   function Create_Domain
     (Logic_Var    : Var;
      Domain       : Value_Array;
      Debug_String : String_Access := null) return Relation;
   --  Create a Domain relation. A Domain relation is a shortcut such that:
   --
   --  ``Domain (Var, (A, B, ...))`` is equivalent to
   --  ``Any (Assign (Var, A), Assign (Var, B), ...)``.

   function Create_Any
     (Relations    : Relation_Array;
      Debug_String : String_Access := null) return Relation;
   --  Create an Any relation. An Any relation solves successfully if any of
   --  its sub-relations solves successfully.

   function Create_All
     (Relations    : Relation_Array;
      Debug_String : String_Access := null) return Relation;
   --  Create an All relation. An All relation solves successfully if all of
   --  its sub-relation solves successfully.

   function Create_Or
     (L, R         : Relation;
      Debug_String : String_Access := null) return Relation
   is (Create_Any ((L, R), Debug_String));
   --  Create an Or relation. An Or relation is just a binary Any

   function Create_And
     (L, R         : Relation;
      Debug_String : String_Access := null) return Relation
   is (Create_All ((L, R), Debug_String));
   --  Create an And relation. An And relation is just a binary All

   function Create_True (Debug_String : String_Access := null) return Relation;
   --  Create ``True`` relation

   function Create_False
     (Debug_String : String_Access := null) return Relation;
   --  Create ``False`` relation

   function Image (Self : Relation) return String;
   --  Return an image of the relation as a string.
   --
   --  .. warning:: This is not implemented for the state machine solver, where
   --      only ``Print_Relation`` is implemented.

   procedure Print_Relation (Self : Relation);
   --  Print the relation

   Default_Solver_Kind : Valid_Solver_Kind := State_Machine;
   --  Global variable that defines the default solver kind

   procedure Set_Kind (Kind : Solver_Kind);
   --  Set the kind of the solver

private
   package Sym_Solve is new Langkit_Support.Adalog.Symbolic_Solver
     (Solver_Ifc);
   package SSM_Solve is new Langkit_Support.Adalog.Eq_Same (Solver_Ifc);

   use Ada.Strings;

   package Var_Sets is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Var, Hash, "=", "=");
   type Ref_Counted_Var_Set is record
      Vars      : Var_Sets.Map;
      Ref_Count : Natural;
   end record;
   type Var_Set is access all Ref_Counted_Var_Set;

   type Relation (Kind : Solver_Kind := Symbolic) is record
      case Kind is
         when Symbolic =>
            Symbolic_Relation : Sym_Solve.Relation;

         when State_Machine =>
            SSM_Relation : Abstract_Relation.Relation;
            Vars         : Var_Set := null;
            --  This vector is used to keep track of logic vars in the old
            --  solver, since it doesn't and we need them to implement the
            --  version of ``Solve`` that takes the array of variables in
            --  the callback. If ``Debug`` is False, it isn't used.

         when None => null;
      end case;
   end record;

   No_Relation : constant Relation := (Kind => None);

   No_Relation_Array : constant Relation_Array (1 .. 0) := (others => <>);

end Langkit_Support.Adalog.Solver;
