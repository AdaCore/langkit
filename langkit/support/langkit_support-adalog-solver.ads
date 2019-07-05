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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Adalog.Logic_Var;
with Langkit_Support.Vectors;

generic
   with package Logic_Vars
     is new Langkit_Support.Adalog.Logic_Var (<>);
package Langkit_Support.Adalog.Solver is

   subtype Value_Type is Logic_Vars.Element_Type;

   use Logic_Vars;

   subtype Value_Array is Logic_Vars.Val_Array;
   subtype Variable_Array is Logic_Vars.Var_Array;

   type Relation_Type is private;
   type Relation is access all Relation_Type;
   --  Type for a relation. A relation is either an atomic relation, or a
   --  compound relation, which can represent a tree of relations of unbounded
   --  depth. Solving a relation will assign values to every logic variable
   --  involved in the relation or fail.
   --
   --  A relation is manually ref-counted, and owns every sub-relation. When
   --  the ref-count reaches 0, every sub-relation is destroyed.

   procedure Inc_Ref (Self : Relation);
   --  Increments the reference count of Self

   procedure Dec_Ref (Self : in out Relation);
   --  Decrements the reference count of Self. If no reference left, deallocate
   --  ``Self.all`` and sets ``Self`` to ``null``.

   type Solve_Options_Type is record
      Cut_Dead_Branches : Boolean := True;
   end record;

   Default_Options : constant Solve_Options_Type := (others => <>);

   procedure Solve
     (Self              : Relation;
      Solution_Callback : access function return Boolean;
      Solve_Options     : Solve_Options_Type := Default_Options);
   --  Tries to solve ``Self``. For every solution, ``Solution_Callback`` will
   --  be called.

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

   -------------------
   -- Functor types --
   -------------------

   --  The solver contains a number of abstract functor types, that are meant
   --  to be derived by the client to provide functionality.
   --
   --  The reason functor types are exposed is if you need to store state along
   --  with your function. If you don't ,there are convenience constructors
   --  that take access to functions.

   type Base_Functor_Type is abstract tagged null record;
   procedure Destroy (Self : in out Base_Functor_Type) is null;

   --------------------
   -- Predicate_Type --
   --------------------

   type Predicate_Type is abstract new Base_Functor_Type with null record;
   function Call
     (Self : Predicate_Type; Val : Value_Type) return Boolean is abstract;
   function Image (Self : Predicate_Type) return String is ("");
   function Full_Image (Self : Predicate_Type; Dummy_Var : Var) return String
   is ("");
   --  A predicate encapsulates the logic of applying a boolean predicate to a
   --  value, returning whether the predicate succeeds.

   ----------------------
   -- N_Predicate_Type --
   ----------------------

   type N_Predicate_Type is abstract new Base_Functor_Type with null record;
   function Call
     (Self : N_Predicate_Type; Vals : Logic_Vars.Val_Array) return Boolean
      is abstract;
   function Image (Self : N_Predicate_Type) return String is ("");
   function Full_Image
     (Self : N_Predicate_Type; Dummy_Vars : Var_Array) return String
   is ("");
   --  A predicate encapsulates the logic of applying a boolean predicate to a
   --  value, returning whether the predicate succeeds.

   -------------------
   -- Comparer_Type --
   -------------------

   type Comparer_Type is abstract new Base_Functor_Type with null record;
   function Compare
     (Self : Comparer_Type; L, R : Value_Type) return Boolean is abstract;
   --  Type to compare two values of Value_TYpe, returning whether they're
   --  equal or not.
   function Image (Self : Comparer_Type) return String is ("");

   function No_Comparer return Comparer_Type'Class;

   --------------------
   -- Converter_Type --
   --------------------

   type Converter_Type is abstract new Base_Functor_Type with null record;
   function Convert
     (Self : Converter_Type; From : Value_Type) return Value_Type
         is abstract;
   --  Type to convert one value to another
   function Image (Self : Converter_Type) return String is ("");
   function No_Converter return Converter_Type'Class;

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
     (Pred : access function (L, R : Value_Type) return Boolean;
      Pred_Name : String := "Comparer") return Comparer_Type'Class;

   function Converter
     (Pred : access function (V : Value_Type) return Value_Type;
      Pred_Name : String := "Converter") return Converter_Type'Class;

   ---------------------------
   -- Relation constructors --
   ---------------------------

   package Relation_Vectors is new Langkit_Support.Vectors (Relation);
   subtype Relation_Array is Relation_Vectors.Elements_Array;
   No_Relation_Array : Relation_Array
     renames Relation_Vectors.Empty_Array;

   function Create_Predicate
     (Logic_Var    : Var;
      Pred         : Predicate_Type'Class;
      Debug_String : String_Access := null) return Relation;
   --  Create a Predicate relation. A Predicate relation will solve
   --  successfully if the ``Predicate`` applied to the value of
   --  ``Logic_Var`` yields ``True``.

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
   --  Create an Unify relation. An Unify relation will solve successfully if
   --  the assignment of ``From.Value`` to ``To`` is successful.

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

   function Create_And
     (L, R         : Relation;
     Debug_String : String_Access := null) return Relation
   is (Create_All ((L, R), Debug_String));

   function Create_True (Debug_String : String_Access := null) return Relation;
   function Create_False
     (Debug_String : String_Access := null) return Relation;

   function Image (Self : Relation; Level : Natural := 0) return String;

   function Relation_Image (Self : Relation) return String;
   procedure Print_Relation (Self : Relation);

private

   type Converter_Access is access all Converter_Type'Class;
   type Comparer_Access is access all Comparer_Type'Class;
   type Predicate_Access is access all Predicate_Type'Class;
   type N_Predicate_Access is access all N_Predicate_Type'Class;

   procedure Free is new
     Ada.Unchecked_Deallocation (Comparer_Type'Class, Comparer_Access);
   procedure Free is new
     Ada.Unchecked_Deallocation (Converter_Type'Class, Converter_Access);
   procedure Free is new
     Ada.Unchecked_Deallocation (Predicate_Type'Class, Predicate_Access);
   procedure Free is new
     Ada.Unchecked_Deallocation (N_Predicate_Type'Class, N_Predicate_Access);

   package Logic_Var_Vectors
   is new Langkit_Support.Vectors (Var);
   subtype Logic_Var_Vector is Logic_Var_Vectors.Vector;
   type Logic_Var_Vector_Access is access Logic_Var_Vector;

   ---------------------
   -- Atomic_Relation --
   ---------------------

   type Atomic_Kind is (Propagate, Unify, Assign, Predicate,
                        N_Predicate, True, False);

   type Atomic_Relation (Kind : Atomic_Kind := Propagate) is record
      Target : Logic_Vars.Var;
      --  Every atomic relation has a target. Depending on the relation, it
      --  will be *used* or *defined*.

      case Kind is
         when Assign | Propagate =>
            Conv     : Converter_Access := null;
            --  An access to the projection co nverter, if there is one

            Can_Fail : Boolean := False;

            case Kind is
               when Assign =>
                  Val      : Value_Type;
                  --  The value we want to assign to ``Target``

               when Propagate =>
                  From     : Logic_Vars.Var;
                  --  The variable from which we want to propagate

               when others => null;
            end case;
         when Predicate =>
            Pred     : Predicate_Access;
            --  The predicate that will be applied as part of this relation
         when N_Predicate =>
            Vars     : Logic_Var_Vector;
            N_Pred   : N_Predicate_Access;
            --  The predicate that will be applied as part of this relation

         when Unify =>
            Unify_From : Logic_Vars.Var;
         when True | False => null;
      end case;
   end record;
   --  An atomic relation is a relation that has no children. When we get
   --  to solve a specific solution, we expect to have a set of only atomic
   --  relations.
   --
   --  Atomic relations can be either Assign, ``Propagate, or ``Predicate``.
   --  Semantics of those are defined in the public, in the list of relation
   --  constructors.

   function Solve_Atomic (Self : Atomic_Relation) return Boolean;
   --  Solve this atomic relation

   function Image (Self : Atomic_Relation) return String;

   procedure Destroy (Self : in out Atomic_Relation);
   --  Destroy this atomic relation

   ----------------------------------------
   --  Atomic relations dependency graph --
   ----------------------------------------

   --  In this section, we'll define types and operations on the graph of
   --  dependencies between atomic relations. This is what will allow us to:
   --
   --  1. Sort a list of atomic relations topologically, so that they form an
   --  executable list of instructions.
   --
   --  2. Define a map from vars to atomic rels where for every var `V`, the
   --  map maps `V -> [R1, R2, R3, ...]` where `Used_Var (RN) = V`. This map
   --  will allow us cut some branches of the solution tree early.

   type Var_Or_Null (Exists : Boolean := False) is record
      case Exists is
         when True => Logic_Var : Var;
         when False => null;
      end case;
   end record;
   --  Option type for a Var. Used to express dependencies from an atomic
   --  relation to a logic variable.

   Null_Var : Var_Or_Null := (Exists => False);

   function Is_Defined_Or_Null (Logic_Var : Var_Or_Null) return Boolean
   is
     ((not Logic_Var.Exists) or else Is_Defined (Logic_Var.Logic_Var));
   --  Shortcut predicate. Returns whether a variable is defined or is null

   function Used_Var (Self : Atomic_Relation) return Var_Or_Null;
   --  Return the variable that this atomic relation uses, if there is one

   function Defined_Var (Self : Atomic_Relation) return Var_Or_Null;
   --  Return the variable that this atomic relation defines, if there is one

   -----------------------
   -- Compound relation --
   -----------------------

   type Compound_Kind is (Kind_All, Kind_Any);

   type Compound_Relation is record
      Kind : Compound_Kind;
      Rels : Relation_Vectors.Vector;
   end record;

   procedure Destroy (Self : in out Compound_Relation);
   function Image
     (Self         : Compound_Relation;
      Level        : Natural := 0;
      Debug_String : String_Access := null) return String;

   --------------
   -- Relation --
   --------------

   type Relation_Kind is (Atomic, Compound);

   type Relation_Type (Kind : Relation_Kind := Atomic) is record
      Ref_Count  : Integer range -1 .. Integer'Last := 1;
      Debug_Info : Ada.Strings.Unbounded.String_Access := null;
      case Kind is
         when Atomic   => Atomic_Rel   : Atomic_Relation;
         when Compound => Compound_Rel : Compound_Relation;
      end case;
   end record;

   procedure Destroy (Self : Relation);

end Langkit_Support.Adalog.Solver;
