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

with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;

with GNATCOLL.Strings; use GNATCOLL.Strings;

with Langkit_Support.Images;

pragma Warnings (Off, "attribute Update");
--  Attribute update is obsolescent in Ada 2022, but we don't yet want to use
--  delta aggregates because they won't be supported on old compilers, so just
--  silence the warning.
--
--  TODO??? Remove this and consistently use delta aggregates once the oldest
--  GNAT supported decently supports them.

package body Langkit_Support.Adalog.Symbolic_Solver is

   ----------------------
   -- Supporting types --
   ----------------------

   package Atomic_Relation_Vectors is new Langkit_Support.Vectors
     (Atomic_Relation);
   subtype Atomic_Relation_Vector is Atomic_Relation_Vectors.Vector;
   --  Vectors of atomic relations

   function Image is new Langkit_Support.Images.Array_Image
     (Atomic_Relation,
      Positive,
      Atomic_Relation_Vectors.Elements_Array);
   function Image (Self : Atomic_Relation_Vector) return String;

   package Any_Relation_Vectors is new Langkit_Support.Vectors (Any_Rel);
   subtype Any_Relation_Vector is Any_Relation_Vectors.Vector;
   --  Vectors of Any relations

   function Image is new Langkit_Support.Images.Array_Image
     (Any_Rel,
      Positive,
      Any_Relation_Vectors.Elements_Array);
   function Image (Self : Any_Relation_Vector) return String;

   --------------------------
   -- Supporting functions --
   --------------------------

   procedure Destroy_Rels (Self : in out Relation_Vectors.Vector);
   --  Delete one ownership share for all relations in ``Self`` and destroy
   --  the ``Self`` vector itself.

   function Create_Propagate
     (From, To     : Logic_Var;
      Conv         : Converter_Access := null;
      Debug_String : String_Access := null) return Relation;
   --  Helper function to create a Propagate relation

   function Create_Compound
     (Relations    : Relation_Array;
      Cmp_Kind     : Compound_Kind;
      Debug_String : String_Access := null) return Relation;
   --  Helper to create a compound relationship

   function Image_Header (Self : Relation) return String;
   --  Return a single-line image for ``Self``

   function Internal_Image
     (Self : Relation; Level : Natural := 0) return String;
   --  Return an image of ``Self`` with each line indented with ``Level``
   --  spaces.

   type Callback_Type is
     access function (Vars : Logic_Var_Array) return Boolean;
   --  Callback to invoke when a valid solution has been found. Takes the logic
   --  variables involved in the relation in arguments, returns whether to
   --  continue the exploration of valid solutions.
   --
   --  TODO??? This should make more data accessible, like the numbers of
   --  solutions tried so far... But would this be really useful?

   type Atom_And_Index is record
      Atom       : Atomic_Relation;
      Atom_Index : Positive;
   end record;
   --  Simple record storing an atom along with its index. Used to construct
   --  the dependency graph during topo sort: ``Atom_Index`` is the index in
   --  ``Topo_Sort.Atoms`` where ``Atom`` lives.

   package Atom_Vectors is new Langkit_Support.Vectors (Atom_And_Index);
   type Atom_Vector_Array is array (Positive range <>) of Atom_Vectors.Vector;
   type Atom_Vector_Array_Access is access all Atom_Vector_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Atom_Vector_Array, Atom_Vector_Array_Access);

   type Sort_Context is record
      Using_Atoms : Atom_Vector_Array_Access;
      --  Map each logic var Id to the list of atoms that use that variable

      Working_Set : Atom_Vectors.Vector;
      --  Working set of atoms. Used as a temporary list to store atoms in the
      --  graph that need to be subsequently added: at all points, the atoms in
      --  the working set have all their dependencies already in the result of
      --  the topo sort.

      N_Preds : Atom_Vectors.Vector;
      --  List of N_Predicates, to be applied at the end of solving. TODO??? we
      --  could apply this policy for all predicates, which would simplify the
      --  code a bit.

      Has_Contradiction_Counter : Natural;
      --  During the Simplify optimization, number of times
      --  ``Has_Contradiction`` was called. Used for logging/debugging
      --  purposes.
   end record;
   --  Data used when doing a topological sort (used only in
   --  Solving_Context.Sort_Ctx), when we reach a complete potential solution.

   type Logic_Var_Array_Access is access all Logic_Var_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Logic_Var_Array, Logic_Var_Array_Access);

   type Prepared_Relation is record
      Rel  : Relation;
      Vars : Logic_Var_Array_Access;
   end record;
   --  Relation that is prepared for solving (see the ``Prepare_Relation``
   --  function below).

   function Prepare_Relation
     (Self : Relation; Opts : Solve_Options_Type) return Prepared_Relation;
   --  Prepare a relation for the solver: simplify it and create a list of all
   --  the logic variables it references, assigning an Id to each.
   --
   --  TODO??? Due to Aliasing, a logic variable can have several ids.
   --  Consequently, the exponential resolution optimization is not complete.

   procedure Create_Aliases
     (Vars : Logic_Var_Array; Unifies : Atomic_Relation_Vector);
   --  Create alias information for variables in ``Vars`` according to Unify
   --  relations in ``Unifies``. Also reset all variables, so that we are ready
   --  to evaluate a sequence of atoms.

   procedure Cleanup_Aliases (Vars : Logic_Var_Array);
   --  Remove alias information for all variables in ``Vars``

   function Topo_Sort
     (Atoms, Unifies : Atomic_Relation_Vector;
      Vars           : Logic_Var_Array;
      Sort_Ctx       : in out Sort_Context;
      Has_Orphan     : out Boolean)
      return Atomic_Relation_Vectors.Elements_Array;
   --  Do a topological sort of the atomic relations in ``Atoms``. Atoms with
   --  no dependencies will come first. Then, atoms will be sorted according to
   --  their dependencies. Finally, ``N_Predicate``s will come last, because
   --  they have multiple dependencies but nothing can depend on them.
   --
   --  ``Unifies`` must be the ``Unify`` atoms to consider for variables
   --  aliasing. ``Atoms`` must not contain any ``Unify`` atom.
   --
   --  ``Vars`` must be the array of all variable referenced in the relation we
   --  are trying to solve.
   --
   --  ``Sort_Ctx`` is a cache for the data structure used to run the topo
   --  sort.
   --
   --  ``Has_Orphan`` is set to whether at least one atom is an "orphan", that
   --  is to say it is not part of the resulting sorted collection.

   function Evaluate_Atoms
     (Sorted_Atoms : Atomic_Relation_Vectors.Elements_Array) return Boolean;
   --  Evaluate the given sequence of sorted atoms (see ``Topo_Sort``) and
   --  return whether they are all satisfied: if they are, the logic variables
   --  are assigned values, so it is possible to invoke the user callback for
   --  solutions.

   function Simplify
     (Self     : Relation;
      Vars     : Logic_Var_Array;
      Sort_Ctx : in out Sort_Context) return Relation;
   --  Try to split Any relations in ``Self`` looking for contradictions in its
   --  atoms through a depth first traversal. Return the simplified relation.

   function Has_Contradiction
     (Atoms, Unifies : Atomic_Relation_Vector;
      Vars           : Logic_Var_Array;
      Sort_Ctx       : in out Sort_Context) return Boolean;
   --  Return whether the given sequence of atoms contains a contradiction,
   --  i.e. if two or more of its atoms make each other unsatisfied. This
   --  function works even for incomplete sequences, for instance when one atom
   --  uses a variable that no atom defines.

   type Solving_Context is record
      Cb : Callback_Type;
      --  User callback, to be called when a solution is found. Returns whether
      --  to continue exploring the solution space.

      Vars : Logic_Var_Array_Access;
      --  List of all logic variables referenced in the top-level relation.
      --
      --  Indexes in this array are the same as Ids for the corresponding
      --  variables, i.e. ``for all I in Vars.all => I = Id (Vars.all (I))``
      --
      --  Computed once (before starting the solver), used to pass all
      --  variables to the user callback and to reset aliasing information when
      --  leaving a branch.

      Unifies : Atomic_Relation_Vector;
      --  Accumulator in ``Solve_Compound`` to hold the current list of
      --  ``Unify`` in the recursive relation traversal: for each relation
      --  leaf, ``Unifies`` will contain all the ``Unify`` atoms necessary to
      --  use in order to interpret the remaining ``Atoms``.

      Atoms : Atomic_Relation_Vector;
      --  Accumulator in ``Solve_Compound`` to hold the current list of atoms
      --  (minus ``Unify`` atoms) in the recursive relation traversal: for each
      --  relation leaf, ``Unifies`` + ``Atoms`` will contain an autonomous
      --  relation to solve (this is a solver "branch").

      Anys : Any_Relation_Vector;
      --  Remaining list of ``Any`` relations to traverse

      Cut_Dead_Branches : Boolean := False;
      --  Optimization that will cut branches that necessarily contain falsy
      --  solutions.

      Sort_Ctx : Sort_Context;
      --  Context used for the topological sort, when reaching a complete
      --  potential solution. Stored once in the context to save ourselves
      --  from reallocating data structures everytime.

      Tried_Solutions : Natural;
      --  Number of tried solutions. Stored for analytics purpose, and
      --  potentially for timeout.
   end record;
   --  Context for the solving of a compound relation

   function Create (Vars : Logic_Var_Array) return Sort_Context;
   --  Create a new sorting context. Use ``Destroy`` to free allocated
   --  resources.

   procedure Destroy (Sort_Ctx : in out Sort_Context);
   --  Free resources for the sorting context

   function Create
     (Cb   : Callback_Type;
      Vars : Logic_Var_Array_Access) return Solving_Context;
   --  Create a new instance of a solving context. The data will be cleaned up
   --  and deallocated by a call to ``Destroy``.

   procedure Destroy (Ctx : in out Solving_Context);
   --  Destroy a solving context, and associated data

   function Solve_Compound
     (Self : Compound_Relation; Ctx : in out Solving_Context) return Boolean;
   --  Look for valid solutions in ``Self`` & ``Ctx``. Return whether to
   --  continue looking for other solutions.

   procedure Any_Left_Warn (Self : Relation);
   --  If ``Self`` contains a non-root ``Any`` relation, warn about it on
   --  ``Any_Left_Trace``.

   ------------------
   -- Destroy_Rels --
   ------------------

   procedure Destroy_Rels (Self : in out Relation_Vectors.Vector) is
      Mutable_R : Relation;
   begin
      for R of Self loop
         Mutable_R := R;
         Dec_Ref (Mutable_R);
      end loop;
      Self.Destroy;
   end Destroy_Rels;

   ------------
   -- Create --
   ------------

   function Create (Vars : Logic_Var_Array) return Sort_Context is
   begin
      return
        (Using_Atoms               => new Atom_Vector_Array'
           (Vars'Range => Atom_Vectors.Empty_Vector),
         Working_Set               => Atom_Vectors.Empty_Vector,
         N_Preds                   => Atom_Vectors.Empty_Vector,
         Has_Contradiction_Counter => 0);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Cb   : Callback_Type;
      Vars : Logic_Var_Array_Access) return Solving_Context is
   begin
      return Ret : Solving_Context do
         Ret.Cb := Cb;
         Ret.Vars := Vars;
         Ret.Atoms := Atomic_Relation_Vectors.Empty_Vector;
         Ret.Unifies := Atomic_Relation_Vectors.Empty_Vector;
         Ret.Anys := Any_Relation_Vectors.Empty_Vector;
         Ret.Sort_Ctx := Create (Vars.all);
         Ret.Tried_Solutions := 0;
      end return;
   end Create;

   ----------------------
   -- Prepare_Relation --
   ----------------------

   function Prepare_Relation
     (Self : Relation; Opts : Solve_Options_Type) return Prepared_Relation
   is
      --  For determinism, collect variables in the order in which they appear
      --  in the equation.
      Vec : Logic_Var_Vectors.Vector;

      Next_Id : Positive := 1;
      --  Id to assign to the next processed relation

      function Is_Atom (Self : Relation; Kind : Atomic_Kind) return Boolean
      is (Self.Kind = Atomic and then Self.Atomic_Rel.Kind = Kind);

      procedure Add (Var : Logic_Var);
      --  Add ``Var`` to ``Vec`` and assign an Id to this variable

      procedure Process_Atom (Self : Atomic_Relation_Type);
      --  Collect variables from ``Self``

      function Fold_And_Track_Vars (Self : Relation) return Relation;
      --  Return a relation (with its dedicated ownership share) with
      --  True/False relations folded. Add to ``Vec`` the variables reference
      --  in ``Self`` during the traversal.

      ---------
      -- Add --
      ---------

      procedure Add (Var : Logic_Var) is
      begin
         if Var /= null and then Id (Var) = 0 then
            Vec.Append (Var);
            Set_Id (Var, Vec.Length);
         end if;
      end Add;

      ------------------
      -- Process_Atom --
      ------------------

      procedure Process_Atom (Self : Atomic_Relation_Type) is
      begin
         case Self.Kind is
            when Propagate =>
               Add (Self.Target);
               Add (Self.From);
            when N_Predicate =>
               Add (Self.Target);
               for V of Self.Vars loop
                  Add (V);
               end loop;
            when Unify =>
               Add (Self.Unify_From);
               Add (Self.Target);
            when others =>
               Add (Self.Target);
         end case;
      end Process_Atom;

      -------------------------
      -- Fold_And_Track_Vars --
      -------------------------

      function Fold_And_Track_Vars (Self : Relation) return Relation is
      begin
         Self.Id := Next_Id;
         Next_Id := Next_Id + 1;

         --  For atomic relations, just add the vars it contains. For compound
         --  relations, just recurse over sub-relations.

         case Self.Kind is
         when Atomic =>
            Process_Atom (Self.Atomic_Rel);
            Inc_Ref (Self);
            return Self;

         when Compound =>
            declare
               Comp_Kind : constant Compound_Kind := Self.Compound_Rel.Kind;
               Last_Rel  : Relation;

               Rels : Relation_Vectors.Vector := Relation_Vectors.Empty_Vector;
               --  Vector of subrelations for the returned compound

               Is_Different : Boolean := False;
               --  Whether the compound relation we are about to return is
               --  different from ``Self``. Used for a small optimization: do
               --  not create a new relation when we can just return the
               --  existing one.

               Neutral : constant Atomic_Kind :=
                 (case Comp_Kind is
                  when Kind_All => True,
                  when Kind_Any => False);
               --  Neutral element for this compound relation, i.e. element
               --  which can just be removed without changing the semantics.

               Absorbing : constant Atomic_Kind := False;
               --  Absorbing element for this compound relation, i.e. element
               --  which, when present as an item in the compound relation, can
               --  replace the whole compound relation without changing the
               --  semantics.
               --
               --  Note that we don't consider True as absorbing for Any
               --  relations on purpose (i.e. we will not fold ``Any (X = 1,
               --  True)`` into ``True``). The reason for this is that we
               --  support solutions with undefined variables. In the example
               --  above, this means that we need to give two solutions: ``X =
               --  1`` and ``X is undefined``, thus we need to refrain from
               --  folding absorbing elements in Any relations.
            begin
               for R of Self.Compound_Rel.Rels loop
                  Last_Rel := Fold_And_Track_Vars (R);

                  --  If we got a neutral or absorbing relation, simplify the
                  --  returned compound.

                  if Is_Atom (Last_Rel, Neutral) then

                     --  No need to add the neutral element to the result, and
                     --  thus the result will necessarily be different from
                     --  ``Self``.

                     Dec_Ref (Last_Rel);
                     Is_Different := True;

                  elsif Comp_Kind = Kind_All
                        and then Is_Atom (Last_Rel, Absorbing)
                  then
                     --  The whole compound can be replaced with the absorbing
                     --  relation: cleanup our temporary vector and return
                     --  that.

                     Destroy_Rels (Rels);
                     return Last_Rel;

                  elsif Last_Rel.Kind = Compound
                        and then Last_Rel.Compound_Rel.Kind = Comp_Kind
                  then
                     --  The sub-relation has become a compound of the same
                     --  kind as ``Self``: we must inline it into ``Self`` to
                     --  preserve our invariants.

                     for R of Last_Rel.Compound_Rel.Rels loop
                        Rels.Append (R);
                        Inc_Ref (R);
                     end loop;
                     Dec_Ref (Last_Rel);
                     Is_Different := True;

                  --  Past here, we just add this sub-relation to the result.
                  --  Just make sure we update ``Is_Different`` when
                  --  appropriate.

                  else
                     Rels.Append (Last_Rel);
                     Is_Different := Is_Different or else Last_Rel /= R;
                  end if;
               end loop;

               --  Now that we have processed each sub-relation, prepare the
               --  result.

               if Rels.Is_Empty then

                  --  All sub-relations were simplified to neutral elements: we
                  --  can replace the whole compound relation with the neutral
                  --  element itself.

                  Rels.Destroy;
                  return (if Neutral = True
                          then Create_True (Self.Debug_Info)
                          else Create_False (Self.Debug_Info));

               elsif Rels.Length = 1 then

                  --  Only one sub-relation is left: it already has a new
                  --  ownership share, so just return it.

                  Last_Rel := Rels.Get (1);
                  Rels.Destroy;
                  return Last_Rel;

               elsif Is_Different then

                  --  We have more than one sub-relation, and the set of
                  --  sub-relations is different than the one in ``Self``, so
                  --  return a new compound relation.

                  return Result : constant Relation := new Relation_Type'
                    (Kind         => Compound,
                     Ref_Count    => 1,
                     Id           => Self.Id,
                     Debug_Info   => Self.Debug_Info,
                     Compound_Rel => (Kind => Self.Compound_Rel.Kind,
                                      Rels => Relation_Vectors.Empty_Vector))
                  do
                     Result.Compound_Rel.Rels.Concat (Rels);
                     Rels.Destroy;
                  end return;

               else
                  --  We are returning ``Self``: destroy the sub-relation
                  --  owership shares created for ``Rels`` as we do not create
                  --  a new compound relation.

                  Destroy_Rels (Rels);
                  Inc_Ref (Self);
                  return Self;
               end if;
            end;
         end case;
      end Fold_And_Track_Vars;

      --  Fold True/False atoms in the input relation and add all variables to
      --  ``Vars`` in the same pass.

      Result          : Prepared_Relation;
      Folded_Relation : Relation := Fold_And_Track_Vars (Self);
   begin
      if Cst_Folding_Trace.Is_Active then
         Cst_Folding_Trace.Trace ("After constant folding:");
         Cst_Folding_Trace.Trace (Image (Folded_Relation));
      end if;

      --  Convert the ``Vars`` vector into the ``Result.Vars`` array and
      --  assign Ids to all variables.

      Result.Vars := new Logic_Var_Array (1 .. Vec.Length);
      for I in Result.Vars.all'Range loop
         declare
            V : Logic_Var renames Result.Vars.all (I);
         begin
            V := Vec.Get (I);
            Set_Id (V, I);
         end;
      end loop;
      Vec.Destroy;

      --  If requested, simplify the relation, trying to find contradictions
      --  between related atoms with a recursive search.

      if Opts.Simplify then
         declare
            Sort_Ctx : Sort_Context := Create (Result.Vars.all);
         begin
            Result.Rel :=
              Simplify (Folded_Relation, Result.Vars.all, Sort_Ctx);
            Dec_Ref (Folded_Relation);
            Destroy (Sort_Ctx);
         end;
      else
         Result.Rel := Folded_Relation;
      end if;

      return Result;
   end Prepare_Relation;

   --------------------
   -- Create_Aliases --
   --------------------

   procedure Create_Aliases
     (Vars : Logic_Var_Array; Unifies : Atomic_Relation_Vector) is
   begin
      for V of Vars loop
         Reset (V);
      end loop;

      for U of Unifies loop
         declare
            Atom : Atomic_Relation_Type renames U.Atomic_Rel;
         begin
            if Verbose_Trace.Active then
               Verbose_Trace.Trace
                 ("Aliasing var " & Image (Atom.Unify_From)
                  & " to " & Image (Atom.Target));
            end if;

            Alias (Atom.Unify_From, Atom.Target);
         end;
      end loop;
   end Create_Aliases;

   ---------------------
   -- Cleanup_Aliases --
   ---------------------

   procedure Cleanup_Aliases (Vars : Logic_Var_Array) is
   begin
      for V of Vars loop
         Unalias (V);
      end loop;
   end Cleanup_Aliases;

   ---------------
   -- Topo_Sort --
   ---------------

   function Topo_Sort
     (Atoms, Unifies : Atomic_Relation_Vector;
      Vars           : Logic_Var_Array;
      Sort_Ctx       : in out Sort_Context;
      Has_Orphan     : out Boolean)
      return Atomic_Relation_Vectors.Elements_Array
   is
      Sorted_Atoms : Atomic_Relation_Vectors.Elements_Array
        (1 .. Atoms.Length);
      --  Array of topo-sorted atoms (i.e. the result). All items in ``Atoms``
      --  should be eventually transferred to ``Sorted_Atoms``.

      Last_Atom_Index : Natural := 0;
      --  Index of the last atom appended to ``Sorted_Atoms``

      Defined_Vars : array (Vars'Range) of Boolean := (others => False);
      --  For each logic variable, whether at least one atom in
      --  ``Sorted_Atoms`` defines it.
      --
      --  Note that because of the way aliasing is implemented, this can be
      --  False for an alias when the aliased variable is actually defined. For
      --  this reason, use the ``Is_Defined`` function below to check a
      --  variable (or its alias, if applicable).

      procedure Append (Atom : Atomic_Relation);
      --  Append Atom to Sorted_Atoms

      Appended : array (Sorted_Atoms'Range) of Boolean := (others => False);
      --  ``Appended (I)`` indicates whether the ``Atoms (I)`` atom was
      --  appended to ``Sorted_Atoms``.
      --
      --  TODO??? It actually says that the atom does not need to be appended
      --  to the result (for instance it's true for ``Unify`` atoms even though
      --  these are not to be part of the result). We should probably rename
      --  this.

      function Id (S : Var_Or_Null) return Natural
      is (if S.Exists then Id (S.Logic_Var) else 0);
      --  Return the Id for the ``S`` variable, or 0 if there is no variable

      function Defined (S : Atomic_Relation_Type) return Natural
      is (Id (Defined_Var (S)));
      --  Return the Id for the variable that ``S`` defines, or 0 if it
      --  contains no definition.

      function Is_Defined (V : Logic_Var) return Boolean
      is (Defined_Vars (Id (V)));
      --  Return whether ``V`` is defined according to the atoms collected in
      --  ``Sorted_Atoms`` so far.

      ------------
      -- Append --
      ------------

      procedure Append (Atom : Atomic_Relation) is
      begin
         Last_Atom_Index := Last_Atom_Index + 1;
         Sorted_Atoms (Last_Atom_Index) := Atom;
      end Append;

      Using_Atoms : Atom_Vector_Array renames Sort_Ctx.Using_Atoms.all;
      N_Preds     : Atom_Vectors.Vector renames Sort_Ctx.N_Preds;
      Working_Set : Atom_Vectors.Vector renames Sort_Ctx.Working_Set;
   begin
      Has_Orphan := False;

      --  Step 1, process Unify atoms so that the processing of other atoms
      --  correctly handles aliased variables.

      Create_Aliases (Vars, Unifies);

      --  Step 2: create:
      --
      --    1. A map of vars to all atoms that use them.
      --
      --    2. The base working set for the topo sort, constituted of all atoms
      --       with no dependencies.

      for I in reverse Atoms.First_Index .. Atoms.Last_Index loop
         declare
            Current_Rel  : constant Atomic_Relation := Atoms.Get (I);
            Current_Atom : Atomic_Relation_Type renames Current_Rel.Atomic_Rel;
            Working_Item : constant Atom_And_Index := (Current_Rel, I);

            --  Resolve the Id of the var used. If the var aliases to another
            --  var, resolve to the aliased var's Id.
            Used_Logic_Var : constant Var_Or_Null :=
              Used_Var (Current_Atom);
            Used_Id        : constant Natural :=
              (if Used_Logic_Var.Exists
               then Id (Used_Logic_Var.Logic_Var)
               else 0);
         begin
            if Current_Atom.Kind = N_Predicate then
               --  N_Predicates are appended at the end separately

               N_Preds.Append (Working_Item);

            elsif Used_Id = 0 then
               --  Put atoms with no dependency in the working set

               Working_Set.Append (Working_Item);

            elsif Current_Atom.Kind /= Unify then
               --  For other atoms, put them in the ``Using_Atoms`` map, which
               --  represents the edges of the dependency graph.

               Using_Atoms (Used_Id).Append (Working_Item);

            else
               --  Aliasing processing prior to the topo sort is supposed to
               --  take care of Unifys, which should not appear in ``Atoms``.

               raise Program_Error with "unreachable code";
            end if;
         end;
      end loop;

      --  Step 3: Do the topo sort

      while not Working_Set.Is_Empty loop
         --  The dependencies of all atoms in the working set are already in
         --  the topo sort result (this is the invariant of
         --  ``Sort_Context_Type.Working_Set``): we can just take the first one
         --  and put it in the result too.
         declare
            Atom    : constant Atom_And_Index := Working_Set.Pop;
            Defd_Id : constant Natural := Defined (Atom.Atom.Atomic_Rel);
         begin
            Append (Atom.Atom);
            Appended (Atom.Atom_Index) := True;

            --  If this atom defines a variable, put all the atoms that use
            --  this variable in the working set, as their dependencies are now
            --  satisfied.
            if Defd_Id /= 0 then
               for El of Using_Atoms (Defd_Id) loop
                  Working_Set.Append (El);
               end loop;

               --  Remove items from Using_Atoms, so that they're not appended
               --  again to the working set.
               Using_Atoms (Defd_Id).Clear;

               Defined_Vars (Defd_Id) := True;
            end if;
         end;
      end loop;

      --  Append at the end all N_Predicates for which all input variables are
      --  defined.
      for N_Pred of N_Preds loop
         if (for all V of N_Pred.Atom.Atomic_Rel.Vars => Is_Defined (V))
         then
            Append (N_Pred.Atom);
            Appended (N_Pred.Atom_Index) := True;
         end if;
      end loop;

      --  Check that all atoms are in the result. If not, we have orphans, and
      --  thus the topo sort failed.
      if Last_Atom_Index /= Sorted_Atoms'Last then

         --  If requested, log all orphan atoms
         if Solv_Trace.Is_Active then
            for I in Appended'Range loop
               if not Appended (I) then
                  Solv_Trace.Trace
                    ("Orphan relation: " & Image (Atoms.Get (I)));
               end if;
            end loop;
         end if;

         Has_Orphan := True;
      end if;

      for Atoms of Sort_Ctx.Using_Atoms.all loop
         Atoms.Clear;
      end loop;
      Working_Set.Clear;
      N_Preds.Clear;
      return Sorted_Atoms (1 .. Last_Atom_Index);
   end Topo_Sort;

   --------------------
   -- Evaluate_Atoms --
   --------------------

   function Evaluate_Atoms
     (Sorted_Atoms : Atomic_Relation_Vectors.Elements_Array) return Boolean is
   begin
      for Atom of Sorted_Atoms loop
         if not Solve_Atomic (Atom) then
            if Solv_Trace.Is_Active then
               Solv_Trace.Trace ("Failed on " & Image (Atom));
            end if;

            return False;
         end if;
      end loop;

      return True;
   end Evaluate_Atoms;

   --------------
   -- Simplify --
   --------------

   function Simplify
     (Self     : Relation;
      Vars     : Logic_Var_Array;
      Sort_Ctx : in out Sort_Context) return Relation
   is
      Iter_Count : Natural := 0;
      --  Number of times we went through the loop in ``Simplify.Process``, for
      --  logging purposes.

      Atoms, Unifies : Atomic_Relation_Vector :=
        Atomic_Relation_Vectors.Empty_Vector;
      --  Non-Unify and Unify atoms that must be considered in ``Process`` when
      --  looking for contradictions in ``Self``.

      procedure Cut_Atoms (Atoms_Mark, Unifies_Mark : Natural);
      --  Remove items in ``Atoms`` past the ``Atoms_Mark`` index and in
      --  ``Unifies`` past the ``Unifies_Mark`` index. Destroy the
      --  corresponding ownership shares.

      function Process (Self : Relation) return Relation
        with Pre => Self.Kind = Compound
                    and then Self.Compound_Rel.Kind = Kind_All;
      --  Try to simplify ``Self``, and return a new relation (or ``Self`` with
      --  a new ownership share). Set ``Continue`` to True if at least one
      --  simplification occurred.

      ---------------
      -- Cut_Atoms --
      ---------------

      procedure Cut_Atoms (Atoms_Mark, Unifies_Mark : Natural) is
      begin
         for Dummy in reverse Atoms_Mark + 1 .. Atoms.Length loop
            declare
               R : Relation := Atoms.Pop;
            begin
               Dec_Ref (R);
            end;
         end loop;

         for Dummy in reverse Unifies_Mark + 1 .. Unifies.Length loop
            declare
               R : Relation := Unifies.Pop;
            begin
               Dec_Ref (R);
            end;
         end loop;
      end Cut_Atoms;

      -------------
      -- Process --
      -------------

      function Process (Self : Relation) return Relation is
         Atoms_Mark   : constant Natural := Atoms.Length;
         Unifies_Mark : constant Natural := Unifies.Length;
         --  Length for these vectors at the point of this call to ``Process``.
         --  This call and its recursions may append atoms to these vectors, so
         --  we must restore their original length before returning to our
         --  caller.

         Anys : Relation_Vectors.Vector;
         --  List of Any to consider as direct sub-relations in ``Self``

         Atoms_Changed : Boolean := True;
         --  We run a fixpoint algorithm: as long as atoms (including Unify
         --  ones) keeps growing, look for new contradictions in ``Anys``.
         --  Everytime the simplification of an ``Any`` relation adds items to
         --  ``Atoms`` or ``Unifies``, new opportunities to find contradictions
         --  in ``Anys` may aride. This boolean keeps track of whether
         --  ``Atoms`` or ``Unifies`` growed since the last iteration.

         procedure Add
           (Self : Relation; Update_Atoms_Changed : Boolean := True);
         --  Append ``Self`` to ``Anys`` if it is an Any relation, to
         --  ``Unifies`` if it is a Unify relation, or to ``Atoms`` otherwise.
         --
         --  Set ``Atoms_Changed`` to True if 1) ``Update_Atoms_Changed`` is
         --  True and 2) this modifies ``Atoms`` or ``Unifies``.
         --
         --  Calling this on an All relation is invalid.

         procedure Cleanup;
         --  Restore the ``Atoms`` and ``Unifies`` vectors, plus free the
         --  ``Anys`` vector.

         ---------
         -- Add --
         ---------

         procedure Add
           (Self : Relation; Update_Atoms_Changed : Boolean := True) is
         begin
            if Self.Kind = Compound then
               case Self.Compound_Rel.Kind is
                  when Kind_Any =>
                     Inc_Ref (Self);
                     Anys.Append (Self);
                  when Kind_All =>
                     for R of Self.Compound_Rel.Rels loop
                        Add (R);
                     end loop;
               end case;

            elsif Self.Atomic_Rel.Kind = Unify then
               Inc_Ref (Self);
               Unifies.Append (Self);
               if Update_Atoms_Changed then
                  Atoms_Changed := True;
               end if;

            else
               Inc_Ref (Self);
               Atoms.Append (Self);
               if Update_Atoms_Changed then
                  Atoms_Changed := True;
               end if;
            end if;
         end Add;

         -------------
         -- Cleanup --
         -------------

         procedure Cleanup is
         begin
            --  Remove the atoms/unifies we have added in this instance of
            --  ``Process``, and destroy the ownership share we have for them.

            Cut_Atoms (Atoms_Mark, Unifies_Mark);

            --  Likewise, free the Any relations we tracked

            for A of Anys loop
               declare
                  R : Relation := A;
               begin
                  Dec_Ref (R);
               end;
            end loop;
            Anys.Destroy;

            if Simplify_Trace.Is_Active then
               Simplify_Trace.Decrease_Indent;
            end if;
         end Cleanup;

      begin
         if Simplify_Trace.Is_Active then
            Simplify_Trace.Increase_Indent ("Running on:");
            Simplify_Trace.Trace (Image (Self));
         end if;

         --  Decompose ``Self`` into atoms, Unify and Any nodes

         declare
            Subrels : Relation_Vectors.Vector renames Self.Compound_Rel.Rels;
         begin
            for I in 1 .. Subrels.Length loop
               Add (Subrels.Get (I));
            end loop;
         end;

         --  Now run our fixpoint

         while Atoms_Changed loop
            Atoms_Changed := False;
            Iter_Count := Iter_Count + 1;
            if Simplify_Trace.Is_Active then
               Simplify_Trace.Trace ("Iteration count:" & Iter_Count'Image);
            end if;

            --  If can find a contradiction just looking at the atoms that are
            --  direct sub-relations of ``Self``, we can replace ``Self`` with
            --  a logic False.

            if Has_Contradiction (Atoms, Unifies, Vars, Sort_Ctx) then
               Cleanup;
               return Create_False;
            end if;

            --  Go through all alternatives in all Any relations and try to
            --  find contradictions there to simplify/remove these Any
            --  relations.
            --
            --  For both iterations, go through relations in reverse order so
            --  that we can remove the item being processing from its vector
            --  without breaking the iteration.

            for Any_Idx in reverse 1 .. Anys.Length loop
               declare
                  Any         : Relation := Anys.Get (Any_Idx);
                  Any_Subrels : Relation_Vectors.Vector renames
                    Any.Compound_Rel.Rels;

                  function Any_Img return String
                  is (Image_Header (Any) & " [Any" & Any_Idx'Image & "]");
               begin
                  if Simplify_Trace.Is_Active then
                     Simplify_Trace.Trace ("Trying to simplify " & Any_Img);
                  end if;

                  Alt_Loop : for Alt_Idx in reverse 1 .. Any_Subrels.Length
                  loop
                     declare
                        Alt : Relation := Any_Subrels.Get (Alt_Idx);

                        function Alt_Img return String
                        is (Image_Header (Alt)
                            & " alt" & Alt_Idx'Image & " of " & Any_Img);
                     begin
                        if Simplify_Trace.Is_Active then
                           Simplify_Trace.Trace
                             ("Trying to simplify " & Alt_Img);
                        end if;

                        if Alt.Kind = Compound then

                           --  The only compound relations that Any relations
                           --  can have are All relations.

                           pragma Assert (Alt.Compound_Rel.Kind = Kind_All);

                           Alt := Process (Alt);

                        else
                           --  Look for a contradiction with this ``Alt`` atom.
                           --  To do this, we must add ``Alt`` to our knowledge
                           --  base just the time to check for contradictions,
                           --  and rollback the knowledge base right after
                           --  that. This is why we must not update
                           --  ``Atoms_Changed``: this operation should not
                           --  trigger another inner loop iteration.

                           declare
                              Atoms_Mark   : constant Natural := Atoms.Length;
                              Unifies_Mark : constant Natural :=
                                Unifies.Length;
                           begin
                              Add (Alt, Update_Atoms_Changed => False);

                              if Has_Contradiction
                                (Atoms, Unifies, Vars, Sort_Ctx)
                              then
                                 Alt := Create_False;
                              else
                                 --  Increase ``Alt``'s reference count so that
                                 --  we have always have a new ownership share
                                 --  for ``Alt`` in the code below: other
                                 --  branches contain assignments to ``Alt``
                                 --  which also create an ownership share.

                                 Inc_Ref (Alt);
                              end if;

                              Cut_Atoms (Atoms_Mark, Unifies_Mark);
                           end;
                        end if;

                        --  Destroy the ownership share that ``Any`` has on
                        --  this alternative, as we are going to replace it
                        --  with ``Alt`` (or even remove it).

                        declare
                           R : Relation := Any_Subrels.Get (Alt_Idx);
                        begin
                           Dec_Ref (R);
                        end;

                        --  If this alternative leads to a contradiction, we
                        --  can remove it from the ``Any`` relation we are
                        --  currently processing.

                        if Alt.Kind = Atomic
                           and then Alt.Atomic_Rel.Kind = False
                        then
                           Dec_Ref (Alt);
                           Any_Subrels.Remove_At (Alt_Idx);

                           --  Moreover, if there is only one alternative left
                           --  in this ``Any``, we can get rid of it and
                           --  migrate its only alternative to
                           --  ``Atoms``/``Unifies``/``Anys``.

                           if Any_Subrels.Length = 1 then
                              Add (Any.Compound_Rel.Rels.Get (1));
                              Dec_Ref (Any);
                              Anys.Remove_At (Any_Idx);
                              exit Alt_Loop;
                           end if;

                        elsif Alt.Kind = Compound
                              and then Alt.Compound_Rel.Kind = Kind_Any
                        then
                           --  Simplification replaced an All relation with an
                           --  Any one, however we cannot have an Any relation
                           --  in another Any relation: inline the former into
                           --  the latter.

                           declare
                              Nested_Subrels : Relation_Vectors.Vector renames
                                Alt.Compound_Rel.Rels;
                           begin
                              --  Move ``Alt`` sub-relations to ``Any`` (so no
                              --  ownership share modification) and get rid of
                              --  ``Alt``.

                              for R of Nested_Subrels loop
                                 Any_Subrels.Append (R);
                              end loop;
                              Nested_Subrels.Clear;

                              Dec_Ref (Alt);
                              Any_Subrels.Remove_At (Alt_Idx);

                              --  TODO??? In principle there should not be
                              --  empty Any relations hanging around, so given
                              --  that we appended at least one relations to
                              --  ``Any_Subrels``, that vector should not be
                              --  empty after the call to ``Remove_At``.

                              pragma Assert (Any_Subrels.Length /= 0);
                           end;

                        else
                           --  We could not split this alternative, but at
                           --  least we can use its simplified version.

                           Any_Subrels.Set (Alt_Idx, Alt);
                        end if;
                     end;
                  end loop Alt_Loop;
               end;
            end loop;
         end loop;

         --  Build the result from ``Anys`` plus all items in
         --  ``Atoms``/``Unifies`` which we added during this call.

         return Result : Relation do
            declare
               Count : constant Positive :=
                 (Atoms.Length - Atoms_Mark)
                 + (Unifies.Length - Unifies_Mark)
                 + Anys.Length;

               Items : Relation_Array (1 .. Count);
               Last  : Natural := 0;
            begin
               --  The ``Items`` array just borrows relations: we just create
               --  one ownership share for ``Result`` at the end, and the call
               --  to ``Cleanup`` will then destroy the shares assigned to
               --  ``Atoms``, ``Unifies`` and ``Anys``.

               for I in Atoms_Mark + 1 .. Atoms.Length loop
                  Last := Last + 1;
                  Items (Last) := Atoms.Get (I);
               end loop;

               for I in Unifies_Mark + 1 .. Unifies.Length loop
                  Last := Last + 1;
                  Items (Last) := Unifies.Get (I);
               end loop;

               for A of Anys loop
                  Last := Last + 1;
                  Items (Last) := A;
               end loop;

               if Count = 1 then
                  Result := Items (1);
                  Inc_Ref (Result);
               else
                  Result := Create_All (Items, Self.Debug_Info);
                  Result.Id := Self.Id;
               end if;
            end;

            Cleanup;
         end return;
      end Process;

      Result : Relation;
   begin
      --  We can only simplify compound relations, so return atoms unchanged

      if Self.Kind = Atomic then
         Inc_Ref (Self);
         return Self;

      elsif Self.Compound_Rel.Kind = Kind_Any then

         --  Simplify each alternative separately: there can be no interaction
         --  between alternatives of a root conjuction.
         --
         --  As usual, process alternatives in the reverse order so that we can
         --  remove the current element without breaking the iteration.

         declare
            Subrels : Relation_Vectors.Vector renames Self.Compound_Rel.Rels;
            Alt     : Relation;
            New_Alt : Relation;
         begin
            for I in reverse 1 .. Subrels.Length loop
               Alt := Subrels.Get (I);
               New_Alt := Simplify (Alt, Vars, Sort_Ctx);
               Dec_Ref (Alt);

               --  Remove alternatives that are simplified to a False atom, to
               --  maintain the solver input invariants.

               if New_Alt.Kind = Atomic
                  and then New_Alt.Atomic_Rel.Kind = False
               then
                  Subrels.Remove_At (I);
                  Dec_Ref (New_Alt);
               else
                  Subrels.Set (I, New_Alt);
               end if;
            end loop;

            Inc_Ref (Self);
            return Self;
         end;
      end if;

      --  At this point, we know we are dealing with an All relation.  Create
      --  an ownership share for the tentative result: the input All relation
      --  itself.

      Result := Self;
      Inc_Ref (Result);

      declare
         New_Result : constant Relation := Process (Result);
      begin
         Dec_Ref (Result);
         Result := New_Result;
      end;

      if Simplify_Trace.Is_Active then
         Simplify_Trace.Trace ("Simplification completed");
         Simplify_Trace.Trace (Image (Result));
         Simplify_Trace.Trace ("Iterations:" & Iter_Count'Image);
      end if;

      Atoms.Destroy;
      Unifies.Destroy;
      return Result;
   end Simplify;

   -----------------------
   -- Has_Contradiction --
   -----------------------

   function Has_Contradiction
     (Atoms, Unifies : Atomic_Relation_Vector;
      Vars           : Logic_Var_Array;
      Sort_Ctx       : in out Sort_Context) return Boolean
   is
      Had_Exception : Boolean := False;
      Exc           : Exception_Occurrence;

      Result : Boolean;
   begin
      Sort_Ctx.Has_Contradiction_Counter :=
        Sort_Ctx.Has_Contradiction_Counter + 1;

      if Simplify_Trace.Is_Active then
         Simplify_Trace.Increase_Indent
           ("Looking for a contradiction (number"
            & Sort_Ctx.Has_Contradiction_Counter'Image & ")");
         Simplify_Trace.Trace (Image (Atoms));
      end if;

      declare
         use Atomic_Relation_Vectors;
         Dummy        : Boolean;
         Sorted_Atoms : constant Elements_Array :=
           Topo_Sort (Atoms, Unifies, Vars, Sort_Ctx, Dummy);
      begin
         if Simplify_Trace.Is_Active then
            Simplify_Trace.Trace ("After partial topo sort");
            Simplify_Trace.Trace (Image (Sorted_Atoms));
         end if;

         --  Once the partial topological sort has been done, we can just
         --  run the linear evaluator to check if there is a contradiction.
         --
         --  Note that we must catch and hide here all exceptions that
         --  predicates/converters might raise during the evaluation: while it
         --  is ok during the relation solving to let them abort the
         --  resolution, ``Has_Contradiction`` is used to simplify the
         --  relation: we do not want to abort the simplification process. In
         --  this case, even though we know that the solver will later fail
         --  evaluating the same atom, we cannot optimize it out to preserve
         --  the order in which the solver finds solutions.

         begin
            Result := not Evaluate_Atoms (Sorted_Atoms);
         exception
            when E : others =>
               Save_Occurrence (Exc, E);
               Had_Exception := True;
               Result := False;
         end;

         if Simplify_Trace.Is_Active then
            if Had_Exception then
               Simplify_Trace.Trace
                 (Exc,
                  "Got an exception, considering no contradiction was found:"
                  & ASCII.LF);
            elsif Result then
               Simplify_Trace.Trace ("Contradiction found");
            else
               Simplify_Trace.Trace ("No contradiction found");
            end if;
         end if;

         if Simplify_Trace.Is_Active then
            Simplify_Trace.Decrease_Indent;
         end if;

         Cleanup_Aliases (Vars);
         return Result;
      end;
   end Has_Contradiction;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Sort_Ctx : in out Sort_Context) is
   begin
      Sort_Ctx.N_Preds.Destroy;
      Sort_Ctx.Working_Set.Destroy;
      for Atoms of Sort_Ctx.Using_Atoms.all loop
         Atoms.Destroy;
      end loop;
      Free (Sort_Ctx.Using_Atoms);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Ctx : in out Solving_Context) is
   begin
      Ctx.Unifies.Destroy;
      Ctx.Atoms.Destroy;
      Ctx.Anys.Destroy;
      Destroy (Ctx.Sort_Ctx);

      --  Cleanup logic vars for future solver runs using them. Note that no
      --  aliasing information is supposed to be left at this stage.

      for V of Ctx.Vars.all loop
         Reset (V);
         Set_Id (V, 0);
      end loop;
      Free (Ctx.Vars);
   end Destroy;

   --------------
   -- Used_Var --
   --------------

   function Used_Var (Self : Atomic_Relation_Type) return Var_Or_Null
   is
      --  We handle Unify here, even though it is not strictly treated in the
      --  dependency graph, so that the Unify_From variable is registered in
      --  the list of variables of the equation. TODO??? Might be cleaner to
      --  have a separate function to return all variables a relation uses?
     (case Self.Kind is
         when Assign | True | False | N_Predicate => Null_Var,
         when Propagate => (True, Self.From),
         when Predicate => (True, Self.Target),
         when Unify     => (True, Self.Unify_From));

   -----------------
   -- Defined_Var --
   -----------------

   function Defined_Var (Self : Atomic_Relation_Type) return Var_Or_Null
   is
      --  We handle Unify here, even though it is not strictly treated in the
      --  dependency graph, so that the Target variable is registered in
      --  the list of variables of the equation. TODO??? Might be cleaner to
      --  have a separate function to return all variables a relation defines?
     (case Self.Kind is
         when Assign | Propagate | Unify             => (True, Self.Target),
         when Predicate | True | False | N_Predicate => Null_Var);

   -----------------
   -- To_Relation --
   -----------------

   function To_Relation
     (Inner        : Atomic_Relation_Type;
      Debug_String : String_Access := null) return Relation
   is
     (new Relation_Type'
        (Kind       => Atomic,
         Ref_Count  => 1,
         Id         => 0,
         Debug_Info => Debug_String,
         Atomic_Rel => Inner));

   -----------------
   -- To_Relation --
   -----------------

   function To_Relation
     (Inner        : Compound_Relation_Type;
      Debug_String : String_Access := null) return Relation
   is
     (new Relation_Type'
        (Kind         => Compound,
         Ref_Count    => 1,
         Id           => 0,
         Debug_Info   => Debug_String,
         Compound_Rel => Inner));

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : Relation) is
   begin
      if Self /= null then
         Self.Ref_Count := Self.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Relation) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Relation_Type, Relation);
   begin
      if Self = null then
         return;
      elsif Self.Ref_Count = 1 then
         Destroy (Self);
         Unchecked_Free (Self);
      else
         Self.Ref_Count := Self.Ref_Count - 1;
      end if;
      Self := null;
   end Dec_Ref;

   --------------------
   -- Solve_Compound --
   --------------------

   function Solve_Compound
     (Self : Compound_Relation; Ctx : in out Solving_Context) return Boolean
   is
      Comp : Compound_Relation_Type renames Self.Compound_Rel;

      function Try_Solution (Atoms : Atomic_Relation_Vector) return Boolean;
      --  Try to solve the given sequence of atoms. Return whether no valid
      --  solution was found (so return False on success).

      function Process_Atom (Self : Atomic_Relation) return Boolean;
      --  Process one atom, whether we are in an ``All`` or ``Any`` branch.
      --  Returns whether we should abort current path or not, in the case of
      --  an ``All`` relation.

      function Recurse return Boolean;
      --  If ``Ctx.Anys`` is empty, just try to evaluate ``Ctx.Atoms`` and
      --  return the result. Otherwise, recurse on ``Any``'s head, keeping the
      --  tail for the recursion.

      function Cleanup (Val : Boolean) return Boolean;
      --  Cleanup helper to call before exitting ``Solve_Compound``

      procedure Branch_Cleanup;
      --  Cleanup helper to call after having processed an ``Any``
      --  sub-relation.

      procedure Create_Aliases;
      --  Shortcut to create aliases from ``Ctx.Unifies``

      procedure Cleanup_Aliases;
      --  Shortcut to cleanup aliases in ``Ctx.Vars``

      ------------------
      -- Try_Solution --
      ------------------

      function Try_Solution (Atoms : Atomic_Relation_Vector) return Boolean is

         function Cleanup (Val : Boolean) return Boolean;
         --  Helper for early abort: cancel the indentation increase in
         --  Solv_Trace and return Val.

         -------------
         -- Cleanup --
         -------------

         function Cleanup (Val : Boolean) return Boolean is
         begin
            Solv_Trace.Decrease_Indent;
            return Val;
         end Cleanup;

      begin
         if Solv_Trace.Is_Active then
            Solv_Trace.Increase_Indent ("In try solution");
            Solv_Trace.Trace (Image (Atoms));
         end if;
         Ctx.Tried_Solutions := Ctx.Tried_Solutions + 1;

         Sol_Trace.Trace ("Tried solutions: " & Ctx.Tried_Solutions'Image);

         declare
            use Atomic_Relation_Vectors;
            Sorting_Error : Boolean;
            Sorted_Atoms  : constant Elements_Array :=
              Topo_Sort (Atoms,
                         Ctx.Unifies,
                         Ctx.Vars.all,
                         Ctx.Sort_Ctx,
                         Sorting_Error);
         begin
            --  There was an error in the topo sort: continue to next potential
            --  solution.
            if Sorting_Error then
               return Cleanup (True);
            end if;

            if Solv_Trace.Is_Active then
               Solv_Trace.Trace ("After topo sort");
               Solv_Trace.Trace (Image (Sorted_Atoms));
            end if;

            --  Once the topological sort has been done, we just have to solve
            --  every relation in order. Abort if one doesn't solve.
            if not Evaluate_Atoms (Sorted_Atoms) then
               return Cleanup (True);
            end if;

            if Sol_Trace.Is_Active then
               Sol_Trace.Trace ("Valid solution");
               Sol_Trace.Trace (Image (Sorted_Atoms));
            end if;

            --  All atoms have correctly solved: we have found a solution: let
            --  the user defined callback know and decide if we should continue
            --  exploring the solution space.
            return Cleanup (Ctx.Cb (Ctx.Vars.all));
         end;
      end Try_Solution;

      Initial_Atoms_Length   : Natural renames Ctx.Atoms.Last_Index;
      Initial_Unifies_Length : Natural renames Ctx.Unifies.Last_Index;
      Initial_Anys_Length    : Natural renames Ctx.Anys.Last_Index;

      --------------------
      -- Create_Aliases --
      --------------------

      procedure Create_Aliases is
      begin
         Create_Aliases (Ctx.Vars.all, Ctx.Unifies);
      end Create_Aliases;

      ---------------------
      -- Cleanup_Aliases --
      ---------------------

      procedure Cleanup_Aliases is
      begin
         Cleanup_Aliases (Ctx.Vars.all);
      end Cleanup_Aliases;

      --------------------
      -- Branch_Cleanup --
      --------------------

      procedure Branch_Cleanup is
      begin
         Ctx.Atoms.Cut (Initial_Atoms_Length);
         Ctx.Unifies.Cut (Initial_Unifies_Length);
         Ctx.Anys.Cut (Initial_Anys_Length);
      end Branch_Cleanup;

      -------------
      -- Cleanup --
      -------------

      function Cleanup (Val : Boolean) return Boolean is
      begin
         --  Unalias every var that was aliased
         Cleanup_Aliases;

         Branch_Cleanup;
         Trav_Trace.Decrease_Indent;
         return Val;
      end Cleanup;

      ------------------
      -- Process_Atom --
      ------------------

      function Process_Atom (Self : Atomic_Relation) return Boolean is
         Atom : Atomic_Relation_Type renames Self.Atomic_Rel;
      begin
         if Atom.Kind = Unify then
            if Atom.Unify_From /= Atom.Target then
               Ctx.Unifies.Append (Self);
            end if;
            return True;

         elsif Atom.Kind = True then
            return True;

         elsif Atom.Kind = False then
            return False;
         end if;

         Ctx.Atoms.Append (Self);
         return True;
      end Process_Atom;

      -------------
      -- Recurse --
      -------------

      function Recurse return Boolean is
         Head   : Any_Rel;
         Result : Boolean;
      begin
         if Trav_Trace.Is_Active then
            Trav_Trace.Trace ("Before recursing in Solve_Recurse");
            Trav_Trace.Trace (Image (Ctx.Atoms));
            Trav_Trace.Trace (Image (Ctx.Anys));
         end if;

         if not Ctx.Anys.Is_Empty then

            --  The relation we are trying to solve here is the equivalent of:
            --
            --     Ctx.Atoms & All (Ctx.Anys)
            --
            --  Exploring solutions for this complex relation is not linear: we
            --  need recursion. Start with the head of ``Ctx.Anys``:
            --
            --     Ctx.Atoms & Head (Ctx.Anys)
            --
            --  And leave the rest for later:
            --
            --     Ctx.Atoms & Tail (Ctx.Anys)

            --  Temporarily remove the head of the Anys to pass it to
            --  ``Solve_Compound`` and restore it before returning.

            Head := Ctx.Anys.Pop;
            begin
               Result := Solve_Compound (Head, Ctx);
            exception
               when others =>
                  Ctx.Anys.Append (Head);
                  raise;
            end;
            Ctx.Anys.Append (Head);

         else
            --  We are currently exploring only one alternative: just
            --  look for a solution in ``Ctx.Atoms``.

            begin
               Result := Try_Solution (Ctx.Atoms);
            exception
               when others =>
                  Cleanup_Aliases;
                  raise;
            end;
            Cleanup_Aliases;
         end if;
         return Result;
      end Recurse;

   begin
      Trav_Trace.Increase_Indent ("In Solve_Compound " & Self.Kind'Image);

      case Comp.Kind is

      --  This is a conjunction: We want to *inline* every possible combination
      --  of relations contained by disjunctions, to get to every possible
      --  solution. We're going to do that by:
      --
      --  1. Add atoms from this ``All`` relation to our already accumulated
      --     list of atoms.
      --
      --  2. Add disjunctions from this relation to our list of disjunctions
      --     that we need to explore.
      --
      --  Explore every possible alternative created by disjunctions, by
      --  recursing on them.

      when Kind_All =>
         --  First step: gather ``Any`` relations and atoms in their own
         --  vectors (``Anys`` and ``Ctx.Atoms``)

         for Sub_Rel of Comp.Rels loop
            case Sub_Rel.Kind is
            when Compound =>
               --  The ``Create_All`` inlines the sub-relations of ``All``
               --  relations passed to it in the relation it returns. For
               --  instance:
               --
               --     Create_All ((Create_All ((A, B)), C))
               --
               --  is equivalent to:
               --
               --     Create_All ((A, B, C))
               --
               --  ``Self`` is an ``All`` relation, so ``Sub_Rel`` cannot be an
               --  ``All`` as well, so it if is compound, it must be an
               --  ``Any``.
               pragma Assert (Sub_Rel.Compound_Rel.Kind = Kind_Any);
               Ctx.Anys.Append (Sub_Rel);

            when Atomic =>
               if not Process_Atom (Sub_Rel) then
                  return Cleanup (True);
               end if;
            end case;
         end loop;

         if Ctx.Cut_Dead_Branches then
            --  Exponential resolution optimization: check if we have a
            --  contradiction in the list of atoms we have accumulated so far.
            --
            --  TODO??? PROBLEM: While this avoids exponential resolutions, it
            --  also makes the default algorithm quadratic (?), since we
            --  re-iterate on all atoms at every depth of the recursion.  What
            --  we could do is:
            --
            --  1. Either not activate this opt for certain trees.
            --
            --  2. Either try to check only for new atoms. This seems
            --     hard/impossible since new constraints are added at every
            --     recursion, so old atoms need to be checked again for
            --     completeness. But maybe there is a way. Investigate later.

            Create_Aliases;
            if Has_Contradiction
              (Ctx.Atoms, Ctx.Unifies, Ctx.Vars.all, Ctx.Sort_Ctx)
            then
               if Solv_Trace.Active then
                  Solv_Trace.Trace ("Aborting due to exp res optim");
               end if;
               return Cleanup (True);
            end if;
            Cleanup_Aliases;
         end if;

         return Cleanup (Recurse);

      when Kind_Any =>
         --  Recurse for each ``Any`` alternative (i.e. sub-relation)

         for Sub_Rel of Comp.Rels loop
            case Sub_Rel.Kind is
               when Atomic =>
                  pragma Assert (Sub_Rel.Atomic_Rel.Kind /= False);

                  --  Add ``Sub_Rel`` to ``Ctx.Atoms``

                  declare
                     Dummy : Boolean := Process_Atom (Sub_Rel);
                  begin
                     null;
                  end;

                  if not Recurse then
                     return Cleanup (False);
                  end if;

               when Compound =>
                  --  See the corresponding assertion in the ``Kind_All``
                  --  section.
                  pragma Assert (Sub_Rel.Compound_Rel.Kind = Kind_All);

                  if not Solve_Compound (Sub_Rel, Ctx) then
                     return Cleanup (False);
                  end if;
            end case;

            Branch_Cleanup;
         end loop;

         return Cleanup (True);
      end case;
   exception
      when others =>
         declare
            Dummy : Boolean := Cleanup (True);
         begin
            raise;
         end;
   end Solve_Compound;

   -------------------
   -- Any_Left_Warn --
   -------------------

   procedure Any_Left_Warn (Self : Relation) is

      function Contains_Any (Self : Relation) return Boolean;
      --  Return whether ``Self`` is an Any relation or if one of its
      --  sub-relations contains an Any relation.

      function Contains_Any_Subrel
        (Self : Compound_Relation_Type) return Boolean;
      --  Return whether one of ``Self``'s sub-relations contains an Any
      --  relation.

      -------------------------
      -- Contains_Any_Subrel --
      -------------------------

      function Contains_Any_Subrel
        (Self : Compound_Relation_Type) return Boolean
      is
        (for some Subrel of Self.Rels => Contains_Any (Subrel));

      ------------------
      -- Contains_Any --
      ------------------

      function Contains_Any (Self : Relation) return Boolean
      is
        (Self.Kind = Compound
         and then (Self.Compound_Rel.Kind = Kind_Any
                   or else Contains_Any_Subrel (Self.Compound_Rel)));

      Has_Any : constant Boolean :=
        (if Self.Kind = Compound and then Self.Compound_Rel.Kind = Kind_Any
         then Contains_Any_Subrel (Self.Compound_Rel)
         else Contains_Any (Self));

   begin
      if Has_Any then
         Any_Left_Trace.Trace ("Non-root Any relations left before solving");
      end if;
   end Any_Left_Warn;

   -----------
   -- Solve --
   -----------

   procedure Solve
     (Self              : Relation;
      Solution_Callback : access function
        (Vars : Logic_Var_Array) return Boolean;
      Solve_Options     : Solve_Options_Type := Default_Options)
   is
      PRel   : Prepared_Relation;
      Rel    : Relation renames PRel.Rel;
      Ctx    : Solving_Context;
      Ignore : Boolean;

      procedure Cleanup;
      --  Cleanup helper to call before exitting Solve

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup is
      begin
         Destroy (Ctx);
         Dec_Ref (Rel);
      end Cleanup;

   begin
      if Solver_Trace.Is_Active then
         Solver_Trace.Trace ("Solving equation:");
         Solver_Trace.Trace (Image (Self));
      end if;

      PRel := Prepare_Relation (Self, Solve_Options);
      Ctx := Create (Solution_Callback'Unrestricted_Access.all, PRel.Vars);
      Ctx.Cut_Dead_Branches := Solve_Options.Cut_Dead_Branches;

      if Any_Left_Trace.Is_Active then
         Any_Left_Warn (PRel.Rel);
      end if;

      case Rel.Kind is
         when Compound =>
            Ignore := Solve_Compound (Rel, Ctx);

         when Atomic =>
            --  We want to use a single entry point for the solver:
            --  ``Solve_Compound``. Create a trivial compound relation to wrap
            --  the given atom.

            declare
               C : Relation := Create_All ((1 => Rel));
            begin
               Ignore := Solve_Compound (C, Ctx);
               Dec_Ref (C);
            exception
               when others =>
                  Dec_Ref (C);
                  raise;
            end;
      end case;

      Cleanup;
   exception
      when E : others =>
         Solver_Trace.Trace ("Exception during solving... Cleaning up");
         if Verbose_Trace.Is_Active then
            Verbose_Trace.Trace (Symbolic_Traceback (E));
         end if;

         --  There is nothing to clean up if we do not have a prepared relation
         --  yet, as we build a context only after getting one.

         if PRel.Rel /= null then
            Cleanup;
         end if;
         raise;
   end Solve;

   -----------------
   -- Solve_First --
   -----------------

   function Solve_First
     (Self          : Relation;
      Solve_Options : Solve_Options_Type := Default_Options) return Boolean
   is
      Ret : Boolean := False;

      function Callback (Vars : Logic_Var_Array) return Boolean;
      --  Simple callback that will stop on first solution

      type Tracked_Var is record
         Var     : Logic_Var;
         Defined : Boolean;
         Value   : Value_Type;
      end record;
      type Tracked_Var_Array is array (Positive range <>) of Tracked_Var;
      type Tracked_Vars_Access is access all Tracked_Var_Array;
      procedure Free is new Ada.Unchecked_Deallocation
        (Tracked_Var_Array, Tracked_Vars_Access);
      Tracked_Vars : Tracked_Vars_Access;
      --  Track variables and their state at the point ``Callback`` is invoked

      --------------
      -- Callback --
      --------------

      function Callback (Vars : Logic_Var_Array) return Boolean is
      begin
         Ret := True;
         Tracked_Vars := new Tracked_Var_Array (Vars'Range);
         for I in Vars'Range loop
            declare
               TV      : Tracked_Var renames Tracked_Vars (I);
               V       : Logic_Var renames Vars (I);
               Defined : constant Boolean := Is_Defined (V);
            begin
               TV.Var := V;
               TV.Defined := Defined;
               if Defined then
                  TV.Value := Get_Value (Vars (I));
               end if;
            end;
         end loop;
         return False;
      end Callback;

   begin
      Solve (Self, Callback'Access, Solve_Options);
      if Tracked_Vars /= null then
         for TV of Tracked_Vars.all loop
            if TV.Defined then
               Set_Value (TV.Var, TV.Value);
            else
               Reset (TV.Var);
            end if;
         end loop;
         Free (Tracked_Vars);
      end if;
      return Ret;
   end Solve_First;

   -----------------
   -- Create_True --
   -----------------

   function Create_True (Debug_String : String_Access := null) return Relation
   is (To_Relation (Atomic_Relation_Type'(True, Target => <>),
                    Debug_String => Debug_String));

   ------------------
   -- Create_False --
   ------------------

   function Create_False (Debug_String : String_Access := null) return Relation
   is (To_Relation (Atomic_Relation_Type'(False, Target => <>),
                    Debug_String => Debug_String));

   ----------------------
   -- Create_Predicate --
   ----------------------

   function Create_Predicate
     (Logic_Var    : Logic_Vars.Logic_Var;
      Pred         : Predicate_Type'Class;
      Debug_String : String_Access := null) return Relation is
   begin
      return To_Relation
        (Atomic_Relation_Type'
           (Kind   => Predicate,
            Target => Logic_Var,
            Pred   => new Predicate_Type'Class'(Pred)),
         Debug_String => Debug_String);
   end Create_Predicate;

   ----------------------
   -- Create_Predicate --
   ----------------------

   function Create_N_Predicate
     (Logic_Vars   : Logic_Var_Array;
      Pred         : N_Predicate_Type'Class;
      Debug_String : String_Access := null) return Relation
   is
      Vars_Vec : Logic_Var_Vector := Logic_Var_Vectors.Empty_Vector;
   begin
      Vars_Vec.Concat (Logic_Var_Vectors.Elements_Array (Logic_Vars));
      return To_Relation
        (Atomic_Relation_Type'
           (Kind   => N_Predicate,
            N_Pred => new N_Predicate_Type'Class'(Pred),
            Vars   => Vars_Vec,
            Target => <>),
         Debug_String => Debug_String);
   end Create_N_Predicate;

   -------------------
   -- Create_Assign --
   -------------------

   function Create_Assign
     (Logic_Var    : Logic_Vars.Logic_Var;
      Value        : Value_Type;
      Conv         : Converter_Type'Class := No_Converter;
      Eq           : Comparer_Type'Class := No_Comparer;
      Debug_String : String_Access := null) return Relation
   is
      Conv_Ptr : Converter_Access := null;
   begin
      if not Is_No_Comparer (Eq) then
         raise Unsupported_Error with
           "Comparer_Type not supported with the symbolic solver";
      end if;

      if not Is_No_Converter (Conv) then
         Conv_Ptr := new Converter_Type'Class'(Conv);
      end if;

      return To_Relation
        (Atomic_Relation_Type'
           (Kind     => Assign,
            Conv     => Conv_Ptr,
            Val      => Value,
            Target   => Logic_Var),
         Debug_String => Debug_String);
   end Create_Assign;

   ------------------
   -- Create_Unify --
   ------------------

   function Create_Unify
     (Left, Right  : Logic_Var;
      Debug_String : String_Access := null) return Relation is
   begin
      return To_Relation
        (Atomic_Relation_Type'(Kind       => Unify,
                               Target     => Right,
                               Unify_From => Left),
         Debug_String => Debug_String);
   end Create_Unify;

   ----------------------
   -- Create_Propagate --
   ----------------------

   function Create_Propagate
     (From, To     : Logic_Var;
      Conv         : Converter_Access := null;
      Debug_String : String_Access := null) return Relation is
   begin
      return To_Relation (Atomic_Relation_Type'(Kind   => Propagate,
                                                Conv   => Conv,
                                                From   => From,
                                                Target => To),
                          Debug_String => Debug_String);
   end Create_Propagate;

   ----------------------
   -- Create_Propagate --
   ----------------------

   function Create_Propagate
     (From, To     : Logic_Var;
      Conv         : Converter_Type'Class := No_Converter;
      Eq           : Comparer_Type'Class := No_Comparer;
      Debug_String : String_Access := null) return Relation
   is
      Conv_Ptr : Converter_Access := null;
   begin
      if not Is_No_Comparer (Eq) then
         raise Unsupported_Error with
           "Comparer_Type not supported with the symbolic solver";
      end if;

      if not Is_No_Converter (Conv) then
         Conv_Ptr := new Converter_Type'Class'(Conv);
      end if;

      return Create_Propagate
        (From, To, Conv_Ptr, Debug_String => Debug_String);
   end Create_Propagate;

   -------------------
   -- Create_Domain --
   -------------------

   function Create_Domain
     (Logic_Var    : Logic_Vars.Logic_Var;
      Domain       : Value_Array;
      Debug_String : String_Access := null) return Relation
   is
      Rels : Relation_Array (Domain'Range);
   begin
      for I in Domain'Range loop
         Rels (I) := Create_Assign
           (Logic_Var, Domain (I), Debug_String => Debug_String);
      end loop;

      return R : constant Relation := Create_Any
        (Rels, Debug_String => Debug_String)
      do
         for Rel of Rels loop
            Dec_Ref (Rel);
         end loop;
      end return;
   end Create_Domain;

   ---------------------
   -- Create_Compound --
   ---------------------

   function Create_Compound
     (Relations    : Relation_Array;
      Cmp_Kind     : Compound_Kind;
      Debug_String : String_Access := null) return Relation
   is
      Rels : Relation_Vectors.Vector;

      procedure Append (R : Relation);
      --  If ``R`` is an All, inline its relations inside ``Rels``. Else, just
      --  append ``R`` to Rels``.

      ------------
      -- Append --
      ------------

      procedure Append (R : Relation) is
      begin
         if R.Kind = Compound and then R.Compound_Rel.Kind = Cmp_Kind then
            --  Inline Anys in toplevel Any and Alls in toplevel All
            for El of R.Compound_Rel.Rels loop
               Append (El);
            end loop;

         else
            --  Create an ownership share for every relation added to Rels
            Inc_Ref (R);
            Rels.Append (R);
         end if;
      end Append;

   begin
      for El of Relations loop
         Append (El);
      end loop;

      return To_Relation (Compound_Relation_Type'(Cmp_Kind, Rels),
                          Debug_String => Debug_String);
   end Create_Compound;

   ----------------
   -- Create_Any --
   ----------------

   function Create_Any
     (Relations    : Relation_Array;
      Debug_String : String_Access := null) return Relation
   is
     (Create_Compound (Relations, Kind_Any, Debug_String));

   ----------------
   -- Create_All --
   ----------------

   function Create_All
     (Relations    : Relation_Array;
      Debug_String : String_Access := null) return Relation
   is
     (Create_Compound (Relations, Kind_All, Debug_String));

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Atomic_Relation_Type) is
   begin
      case Self.Kind is
         when Assign | Propagate =>
            if Self.Conv /= null then
               Destroy (Self.Conv.all);
            end if;
            Free (Self.Conv);

         when Predicate =>
            Destroy (Self.Pred.all);
            Free (Self.Pred);

         when N_Predicate =>
            Self.Vars.Destroy;
            Destroy (Self.N_Pred.all);
            Free (Self.N_Pred);

         when True | False | Unify =>
            null;
      end case;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Compound_Relation_Type) is
   begin
      for Rel of Self.Rels loop
         declare
            R : Relation := Rel;
         begin
            Dec_Ref (R);
         end;
      end loop;
      Self.Rels.Destroy;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : Relation) is
   begin
      case Self.Kind is
         when Atomic   => Destroy (Self.Atomic_Rel);
         when Compound => Destroy (Self.Compound_Rel);
      end case;
   end Destroy;

   ------------------
   -- Solve_Atomic --
   ------------------

   function Solve_Atomic (Self : Atomic_Relation) return Boolean is
      Atom : Atomic_Relation_Type renames Self.Atomic_Rel;

      function Assign_Val (Val : Value_Type) return Boolean;
      --  Tries to assign ``Val`` to ``Atom.Target`` and return True either if
      --  ``Atom.Target`` already has a value compatible with ``Val``, or if
      --  it had no value and the assignment succeeded.
      --
      --  This assumes that ``Self`` is either an ``Assign`` or a `Propagate``
      --  relation.

      ----------------
      -- Assign_Val --
      ----------------

      function Assign_Val (Val : Value_Type) return Boolean is
         Conv_Val : constant Value_Type :=
           (if Atom.Conv /= null
            then Atom.Conv.Convert_Wrapper (Val)
            else Val);
      begin
         if Is_Defined (Atom.Target) then
            return Conv_Val = Get_Value (Atom.Target);
         else
            Set_Value (Atom.Target, Conv_Val);
            return True;
         end if;
      end Assign_Val;

      Ret : Boolean;
   begin
      --  If the logic variable that ``Self`` uses is not defined, raise an
      --  error.
      --
      --  Note that this cannot happen when called from ``Solve_Compound`` as
      --  the topological sort makes sure all variables are defined before they
      --  are used (and abort the resolution if it is not possible), so the
      --  condition below will succeed only when ``Solve_Atomic`` is called
      --  from ``Solve`` when called on an atom directly.
      --
      --  TODO??? Maybe we should always go through ``Solve_Compound`` to avoid
      --  this redundant check, and more generally have a unique way to solve
      --  relations, and unique way to deal with errors (return no solution
      --  or raise ``Early_Binding_Error``.
      if not Is_Defined_Or_Null (Used_Var (Atom)) then
         raise Early_Binding_Error with
           "Relation " & Image (Atom)
           & " needs var " & Image (Used_Var (Atom).Logic_Var)
           & " to be defined";
      end if;

      case Atom.Kind is
         when Assign =>
            Ret := Assign_Val (Atom.Val);

         when Propagate =>
            pragma Assert (Is_Defined (Atom.From));
            Ret := Assign_Val (Get_Value (Atom.From));

         when Predicate =>
            pragma Assert (Is_Defined (Atom.Target));
            Ret := Atom.Pred.Call_Wrapper (Get_Value (Atom.Target));

         when N_Predicate =>

            for V of Atom.Vars loop
               if not Is_Defined (V) then
                  if Solv_Trace.Active then
                     Solv_Trace.Trace
                       ("Trying to apply " & Image (Atom)
                        & ", but " & Image (V) & " is not defined");
                  end if;
                  return False;
               end if;
               if Solv_Trace.Active then
                  Solv_Trace.Trace
                    (Image (V) & " = " & Value_Image (Get_Value (V)));
               end if;
            end loop;

            declare
               Vals : Value_Array (1 .. Atom.Vars.Length);
            begin
               for I in Atom.Vars.First_Index .. Atom.Vars.Last_Index loop
                  Vals (I) := Get_Value (Atom.Vars.Get (I));
               end loop;

               Ret := Atom.N_Pred.Call_Wrapper (Vals);
            end;

         when True  => Ret := True;
         when False => Ret := False;

         when Unify => raise Assertion_Error with "Should never happen";
      end case;

      if not Ret and then Solv_Trace.Active then
         Solv_Trace.Trace ("Solving " & Image (Atom) & " failed!");
      end if;

      return Ret;
   end Solve_Atomic;

   -----------
   -- Image --
   -----------

   function Image (Self : Atomic_Relation_Type) return String is

      function Right_Image (Right : String) return String
      is
        (if Self.Conv /= null
         then Self.Conv.Image & "(" & Right & ")"
         else Right);

      function Prop_Image (Left, Right : String) return String
      is
        (Left & " <- " & Right_Image (Right));
   begin
      case Self.Kind is
         when Propagate =>
            return Prop_Image (Image (Self.Target), Image (Self.From));

         when Assign =>
            return Prop_Image
              (Image (Self.Target), Logic_Vars.Value_Image (Self.Val));

         when Predicate =>
            declare
               Full_Img : constant String :=
                 Self.Pred.Full_Image (Self.Target);
            begin
               return
                 (if Full_Img /= "" then Full_Img
                  else Self.Pred.Image & "?(" & Image (Self.Target) & ")");
            end;

         when N_Predicate =>
            declare
               Full_Img : constant String :=
                 Self.N_Pred.Full_Image (Logic_Var_Array (Self.Vars.To_Array));
               Vars_Image : XString_Array (1 .. Self.Vars.Length);
            begin
               if Full_Img /= "" then
                  return Full_Img;
               end if;
               for I in Vars_Image'Range loop
                  Vars_Image (I) := To_XString (Image (Self.Vars.Get (I)));
               end loop;
               return Self.N_Pred.Image
                 & "?(" & To_XString (", ").Join (Vars_Image).To_String & ")";
            end;

         when True =>
            return "True";

         when False =>
            return "False";

         when Unify =>
            return Image (Self.Unify_From) & " <-> " & Image (Self.Target);
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Self : Atomic_Relation_Vector) return String is
   begin
      return Image (Self.To_Array);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Self : Any_Relation_Vector) return String is
   begin
      return Image (Self.To_Array);
   end Image;

   ------------------
   -- Image_Header --
   ------------------

   function Image_Header (Self : Relation) return String is
      Prefix : constant String :=
        (if Self.Id = 0
         then ""
         else "[" & Langkit_Support.Images.Stripped_Image (Self.Id) & "] ");
      Suffix : constant String :=
        (if Self.Debug_Info /= null and then Self.Debug_Info.all /= ""
         then " " & Self.Debug_Info.all
         else "");
   begin
      case Self.Kind is
         when Compound =>
            return
              Prefix
              & (case Self.Compound_Rel.Kind is
                 when Kind_All => "All:",
                 when Kind_Any => "Any:")
              & Suffix;

         when Atomic =>
            return Prefix & Image (Self.Atomic_Rel) & Suffix;
      end case;
   end Image_Header;

   -----------
   -- Image --
   -----------

   function Internal_Image
     (Self : Relation; Level : Natural := 0) return String
   is
      Result : XString;
   begin
      case Self.Kind is
         when Compound =>
            Result.Append (Image_Header (Self) & ASCII.LF);
            for Rel of Self.Compound_Rel.Rels loop
               Result.Append
                 ((1 .. Level + 4 => ' ')
                  & Internal_Image (Rel, Level + 4) & ASCII.LF);
            end loop;
            return Result.To_String;

         when Atomic =>
            return Image_Header (Self);
      end case;
   end Internal_Image;

   --------------------
   -- Relation_Image --
   --------------------

   function Image (Self : Relation) return String
   is
     (Internal_Image (Self));

end Langkit_Support.Adalog.Symbolic_Solver;
