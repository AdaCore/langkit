--
--  Copyright (C) 2019-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Adalog.Solver_Interface;
with Langkit_Support.Vectors;

generic
   with package Solver_Ifc
     is new Langkit_Support.Adalog.Solver_Interface (<>);
package Langkit_Support.Adalog.Solver is

   use Solver_Ifc;
   use Logic_Vars;

   type Relation_Type (<>) is private;
   type Relation is access all Relation_Type;
   --  Type for a relation. A relation is either an atomic relation, or a
   --  compound relation, which can represent a tree of relations of unbounded
   --  depth. Solving a relation will assign values to every logic variable
   --  involved in the relation or fail.
   --
   --  A relation is manually ref-counted, and has an ownership share of every
   --  sub-relation. When the ref-count of a root relation, reaches 0, the
   --  ownership share of each of its sub-relations is destroyed.

   No_Relation : constant Relation := null;

   procedure Inc_Ref (Self : Relation);
   --  Increment the reference count of Self

   procedure Dec_Ref (Self : in out Relation);
   --  Decrement the reference count of Self. If no reference is left,
   --  deallocate ``Self.all`` and set ``Self`` to ``null``.

   procedure Solve
     (Self              : Relation;
      Solution_Callback : access function
        (Vars : Logic_Var_Array) return Boolean;
      Solve_Options     : Solve_Options_Type := Default_Options;
      Timeout           : Natural := 0);
   --  Run the solver on the ``Self`` relation. For every solution found, call
   --  ``Solution_Callback`` with the variables involved in ``Self``, and
   --  continue looking for other solutions iff it returns True. See
   --  ``Solve_Options Type`` for the available way to configure the resolution
   --  process.
   --
   --  ``Timeout`` determines the maximum of times we evaluate atoms before
   --  aborting the solver. If left to 0, no timeout applies.

   function Solve_First
     (Self          : Relation;
      Solve_Options : Solve_Options_Type := Default_Options;
      Timeout       : Natural := 0) return Boolean;
   --  Run the solver on the ``Self`` relation. Return whether there is at
   --  least one valid solution. See ``Solve_Options Type`` for the available
   --  way to configure the resolution process.
   --
   --  ``Timeout`` determines the maximum of times we evaluate atoms before
   --  aborting the solver. If left to 0, no timeout applies.

   function Image (Self : Relation) return String;
   --  Return a textual representation of ``Self`` as a multi-line string

   ---------------------------
   -- Relation constructors --
   ---------------------------

   package Relation_Vectors is new Langkit_Support.Vectors
     (Relation, Small_Vector_Capacity => 16);
   subtype Relation_Array is Relation_Vectors.Elements_Array;
   No_Relation_Array : Relation_Array renames Relation_Vectors.Empty_Array;

   --  In all constructor functions below, ``Debug_String`` is an optional
   --  string attached to the returned relation, to be used in the result of
   --  the ``Image`` function. Relations do not manage their lifetimes: it is
   --  up to users to free them when appropriate.

   function Create_Predicate
     (Logic_Var    : Logic_Vars.Logic_Var;
      Pred         : Predicate_Type'Class;
      Debug_String : String_Access := null) return Relation;
   --  Create a relation that will solve successfully when calling ``Pred`` on
   --  the value of ``Logic_Var`` returns ``True``.

   function Create_N_Predicate
     (Logic_Vars   : Logic_Var_Array;
      Pred         : N_Predicate_Type'Class;
      Debug_String : String_Access := null) return Relation;
   --  Create a relation that will solve successfully when calling ``Pred`` on
   --  the values of all variables in ``Logic_Vars`` returns ``True``.

   function Create_Assign
     (Logic_Var    : Logic_Vars.Logic_Var;
      Value        : Value_Type;
      Conv         : Converter_Type'Class := No_Converter;
      Debug_String : String_Access := null) return Relation;
   --  Create a relation that will solve successfully if it is possible to
   --  assign the given ``Value`` to ``Logic_Var``. Two attempts to assign
   --  different values to the same logic variable will make the relation
   --  always fail.
   --
   --  If ``Conv`` is provided, the actually assigned value is the result of
   --  ``Conv`` when called on ``Value``.

   function Create_Unify
     (Left, Right  : Logic_Vars.Logic_Var;
      Debug_String : String_Access := null) return Relation;
   --  Create a relation that will solve successfully if ``Left`` and ``Right``
   --  can be assigned the same value.

   function Create_Propagate
     (From, To     : Logic_Vars.Logic_Var;
      Conv         : Converter_Type'Class := No_Converter;
      Debug_String : String_Access := null) return Relation;
   --  Create a relation that will solve successfully if it is possible to
   --  assign the value in ``From`` to the ``To`` variable.
   --
   --  If ``Conv`` is provided, the actually assigned value is the result of
   --  ``Conv`` when called on ``From``'s value.

   function Create_N_Propagate
     (To           : Logic_Var;
      Comb         : Combiner_Type'Class;
      Logic_Vars   : Logic_Var_Array;
      Debug_String : String_Access := null) return Relation;
   --  Create a relation that will solve successfully if it is possible to
   --  assign to ``To`` the value computed by calling ``Comb`` on the given
   --  ``Logic_Vars``.

   function Create_Domain
     (Logic_Var    : Logic_Vars.Logic_Var;
      Domain       : Value_Array;
      Debug_String : String_Access := null) return Relation;
   --  Create a relation that will solve successfully if it is possible to
   --  assign one value in ``Domain`` to ``Logic_Var``.
   --
   --  This is a shortcut: ``Domain (Var, (A, B, ...))`` is equivalent to
   --  ``Any (Assign (Var, A), Assign (Var, B), ...)``.

   function Create_Any
     (Relations    : Relation_Array;
      Debug_String : String_Access := null) return Relation;
   --  Create a relation that will solve successfully when at least one of the
   --  the relations in ``Relations`` solves successfully.

   function Create_All
     (Relations    : Relation_Array;
      Debug_String : String_Access := null) return Relation;
   --  Create a relation that will solve successfully when all of the
   --  relations in ``Relations`` solve successfully.

   function Create_Or
     (L, R         : Relation;
      Debug_String : String_Access := null) return Relation
   is (Create_Any ((L, R), Debug_String));

   function Create_And
     (L, R         : Relation;
      Debug_String : String_Access := null) return Relation
   is (Create_All ((L, R), Debug_String));

   function Create_True (Debug_String : String_Access := null) return Relation;
   --  Return a relation that always solves successfully

   function Create_False
     (Debug_String : String_Access := null) return Relation;
   --  Return a relation that never solves successfully

private

   type Converter_Access is access all Converter_Type'Class;
   type Combiner_Access is access all Combiner_Type'Class;
   type Predicate_Access is access all Predicate_Type'Class;
   type N_Predicate_Access is access all N_Predicate_Type'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Converter_Type'Class, Converter_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (Combiner_Type'Class, Combiner_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (Predicate_Type'Class, Predicate_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (N_Predicate_Type'Class, N_Predicate_Access);

   package Logic_Var_Vectors is new Langkit_Support.Vectors (Logic_Var);
   subtype Logic_Var_Vector is Logic_Var_Vectors.Vector;
   type Logic_Var_Vector_Access is access Logic_Var_Vector;

   ---------------------
   -- Atomic_Relation --
   ---------------------

   type Atomic_Kind is (Propagate, N_Propagate, Unify, Assign, Predicate,
                        N_Predicate, True, False);

   type Atomic_Relation_Type (Kind : Atomic_Kind := Propagate) is record
      Target : Logic_Var;
      --  What is the Target and whether it is considered as a "used" or
      --  "defined" logic variable depends on the kind of relation.  See the
      --  "Atomic relations dependency graph" section for more information.

      case Kind is
         when Assign | Propagate =>
            Conv : Converter_Access := null;
            --  Conversion function for the value to assign/propagate. If left
            --  to null, use the value itself.

            case Kind is
               when Assign =>
                  Val : Value_Type;
                  --  The value we want to assign to ``Target``

               when Propagate =>
                  From : Logic_Var;
                  --  The variable from which we want to propagate to
                  --  ``Target``.

               when others => null;
            end case;

         when N_Propagate =>
            Comb_Vars : Logic_Var_Vector;
            --  List of logic variables used by the converter

            Comb : Combiner_Access;
            --  Combiner function to assign a value to Target that is computed
            --  from the value of N_Propagate_Vars.

         when Predicate =>
            Pred : Predicate_Access;
            --  The predicate that will be applied as part of this relation

         when N_Predicate =>
            Vars : Logic_Var_Vector;
            --  List of logic variables used by the predicate

            N_Pred : N_Predicate_Access;
            --  The predicate that will be applied as part of this relation

         when Unify =>
            Unify_From : Logic_Var;

         when True | False =>
            null;
      end case;
   end record;
   --  An atomic relation is a relation that has no children. When we get to
   --  solve a specific solution, we expect to have a set of only atomic
   --  relations.
   --
   --  Atomic relations can be either ``Assign``, ``Propagate, or
   --  ``Predicate``. Their semantics are defined in the corresponding public
   --  relation constructors.

   function Image (Self : Atomic_Relation_Type) return String;
   --  Helper for the ``Image`` primitive of ``Relation``

   procedure Destroy (Self : in out Atomic_Relation_Type);
   --  Destroy this atomic relation

   ----------------------------------------
   --  Atomic relations dependency graph --
   ----------------------------------------

   --  This section defines operations to explore the dependency graph between
   --  atomic relations. They are used to:
   --
   --  1. Sort a list of atomic relations topologically, so that they form an
   --     executable sequence of instructions.
   --
   --  2. Define a map from vars to atomic rels where for every logic variable
   --     ``V``, the map maps ``V -> [R1, R2, R3, ...]`` where
   --     ``Used_Var (Rn) = V``. This map will allow us to cut some branches of
   --     the solution tree early.
   --
   --  To compute dependencies, we consider for each relation which variable it
   --  defines (sets a value: see the ``Defined_Var`` function below) or which
   --  variable it uses (copies/checks the value associated to this variable:
   --  see the ``Used_Var`` below).
   --
   --  TODO??? Some relations actually use multiple logic variables
   --  (N_Predicate), while Unify does not really use/define any, but actually
   --  treats both variables as aliases. This dataflow analysis probably
   --  deserves a refactoring to clarify this.

   function Is_Defined_Or_Null (Var : Logic_Var) return Boolean
   is
     (Var = null or else Is_Defined (Var));
   --  Shortcut predicate. Returns whether a variable is defined or is null

   function Defined_Var (Self : Atomic_Relation_Type) return Logic_Var;
   --  Return the variable that this atomic relation defines, if there is one

   -----------------------
   -- Compound relation --
   -----------------------

   type Compound_Kind is (Kind_All, Kind_Any);

   type Compound_Relation_Type is record
      Kind : Compound_Kind;
      Rels : Relation_Vectors.Vector;
   end record;

   procedure Destroy (Self : in out Compound_Relation_Type);

   --------------
   -- Relation --
   --------------

   type Relation_Kind is (Atomic, Compound);

   type Relation_Type (Kind : Relation_Kind) is record
      Ref_Count  : Natural;
      --  Number of ownership shares for this relation. When it drops to zero,
      --  it must be deallocated.

      Id : Natural;
      --  Id for this relation, for debugging purposes: dealing with small
      --  integers is much easier for humans than dealing with addresses, or
      --  even with relation images when one wants to quickly know if two
      --  relations are the same in the logs.
      --
      --  Ids are assigned right before starting relation
      --  simplification/solving, and are as much as possible propagated when
      --  creating a new relation to replace a previous one.
      --
      --  0 means "Id unassigned".

      Debug_Info : Ada.Strings.Unbounded.String_Access := null;
      case Kind is
         when Atomic   => Atomic_Rel   : Atomic_Relation_Type;
         when Compound => Compound_Rel : Compound_Relation_Type;
      end case;
   end record;

   procedure Destroy (Self : Relation);

   --  TODO??? Use predicates once the GNAT bug is fixed.
   --
   --  .. code::
   --
   --     subtype Atomic_Relation is Relation
   --       with Predicate => Atomic_Relation = null
   --                         or else Atomic_Relation.Kind = Atomic;
   --
   --     subtype Compound_Relation is Relation
   --       with Predicate => Compound_Relation = null
   --                         or else Compound_Relation.Kind = Compound;
   --
   --     subtype Any_Rel is Compound_Relation
   --       with Predicate => Any_Rel.Compound_Rel.Kind = Kind_Any;

   subtype Atomic_Relation is Relation;
   subtype Compound_Relation is Relation;
   subtype Any_Rel is Relation;

   function Solve_Atomic (Self : Atomic_Relation) return Boolean;
   --  Solve this atomic relation, return if we have found a valid solution.
   --  Note that this assumes that all "input" logic variables in Self are
   --  already defined.

end Langkit_Support.Adalog.Solver;
