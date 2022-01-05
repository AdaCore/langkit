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

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;

with GNATCOLL.Strings; use GNATCOLL.Strings;

with Langkit_Support.Functional_Lists;
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
   type Atoms_Vector_Access is access all Atomic_Relation_Vector;
   --  Vectors of atomic relations

   function Image is new Langkit_Support.Images.Array_Image
     (Atomic_Relation,
      Positive,
      Atomic_Relation_Vectors.Elements_Array);
   function Image (Self : Atomic_Relation_Vector) return String;

   subtype Any_Rel is Compound_Relation
     with Predicate => Any_Rel.Kind = Kind_Any;
   --  Helper subtype. Allows us to check that we only have ``Any`` relations
   --  in ``Any_Relation_Vectors.Vector``.

   package Any_Relation_Lists is new Langkit_Support.Functional_Lists
     (Any_Rel);
   subtype Any_Relation_List is Any_Relation_Lists.List;
   --  Lists of ``Any`` relations

   function Image (Self : Any_Relation_List) return String;

   package Atomic_Relation_Lists is new Langkit_Support.Functional_Lists
     (Atomic_Relation);
   --  Lists of atomic relations

   package Var_Ids_To_Atoms_Vectors is new Langkit_Support.Vectors
     (Atomic_Relation_Lists.List);
   subtype Var_Ids_To_Atoms is Var_Ids_To_Atoms_Vectors.Vector;
   --  Vector mapping logic var ids to atomic relations

   --------------------------
   -- Supporting functions --
   --------------------------

   procedure Reserve (V : in out Var_Ids_To_Atoms; Size : Positive);
   --  Reserve ``N`` elements in ``V``, creating new lists for each new item

   procedure Merge_Vars
     (Self : in out Var_Ids_To_Atoms; From : Natural; To : Positive);
   --  Transfer the list of atoms in ``Self`` corresponding to the ``From``
   --  variable to the list of atoms corresponding to the ``To`` variable. For
   --  convenience, a null Id for ``From`` is accepted: in this case, this is a
   --  no-op.

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

   function Internal_Image
     (Self : Relation; Level : Natural := 0) return String;
   --  Internal image function for a relation

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

   package Atom_Lists is new Langkit_Support.Functional_Lists (Atom_And_Index);
   package Atom_Lists_Vectors is new Langkit_Support.Vectors (Atom_Lists.List);
   --  Vectors of lists of ``Atom_And_Index``. Used to construct the
   --  dependency graph during topo sort.

   type Sort_Context_Type is record
      Using_Atoms : Atom_Lists_Vectors.Vector;
      --  Map each logic var Id to the list of atoms that use that variable.
      --  TODO??? we could use a vector rather than a list as the inner storing
      --  type. Would be more efficient.

      Working_Set : Atom_Lists.List;
      --  Working set of atoms. Used as a temporary list to store atoms in the
      --  graph that need to be subsequently added: at all points, the atoms in
      --  the working set have all their dependencies already in the result of
      --  the topo sort.

      N_Preds : Atom_Lists.List;
      --  List of N_Predicates, to be applied at the end of solving. TODO??? we
      --  could apply this policy for all predicates, which would simplify the
      --  code a bit.
   end record;
   --  Data used when doing a topological sort (used only in
   --  Solving_Context.Sort_Ctx), when we reach a complete potential solution.

   type Sort_Context is access all Sort_Context_Type;

   type Nat_Access is access all Natural;

   type Logic_Var_Array_Access is access all Logic_Var_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Logic_Var_Array, Logic_Var_Array_Access);

   type Prepared_Relation is record
      Rel  : Relation;
      Vars : Logic_Var_Array_Access;
   end record;
   --  Relation that is prepared for solving (see the ``Prepare_Relation``
   --  function below).

   function Prepare_Relation (Self : Relation) return Prepared_Relation;
   --  Prepare a relation for the solver: simplify it and create a list of all
   --  the logic variables it references, assigning an Id to each.
   --
   --  TODO??? Due to Aliasing, a logic variable can have several ids.
   --  Consequently, the exponential resolution optimization is not complete.

   procedure Create_Aliases
     (Vars          : Logic_Var_Array;
      Unifies       : Atomic_Relation_Vector;
      Vars_To_Atoms : access Var_Ids_To_Atoms := null);
   --  Create alias information for variables in ``Vars`` according to Unify
   --  relations in ``Unifies``. Also reset all variables, so that we are ready
   --  to evaluate a sequence of atoms.
   --
   --  If ``Vars_To_Atoms`` is not null, also merge the lists of atoms for the
   --  canonical variable and the aliased variable.

   procedure Cleanup_Aliases (Vars : Logic_Var_Array);
   --  Remove alias information for all variables in ``Vars``

   function Topo_Sort
     (Atoms, Unifies : Atomic_Relation_Vector;
      Vars           : Logic_Var_Array;
      Sort_Ctx       : Sort_Context;
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

      Unifies : Atoms_Vector_Access;
      --  Accumulator in ``Solve_Compound`` to hold the current list of
      --  ``Unify`` in the recursive relation traversal: for each relation
      --  leaf, ``Unifies`` will contain all the ``Unify`` atoms necessary to
      --  use in order to interpret the remaining ``Atoms``.

      Atoms : Atoms_Vector_Access;
      --  Accumulator in ``Solve_Compound`` to hold the current list of atoms
      --  (minus ``Unify`` atoms) in the recursive relation traversal: for each
      --  relation leaf, ``Unifies`` + ``Atoms`` will contain an autonomous
      --  relation to solve (this is a solver "branch").

      Anys : Any_Relation_List := Any_Relation_Lists.No_List;
      --  Remaining list of ``Any`` relations to traverse

      Vars_To_Atoms : Var_Ids_To_Atoms;
      --  Stores a mapping of variables to:
      --
      --  1. ``Predicate`` atoms that use it;
      --  2. ``Assign`` atoms that set it.
      --
      --  Used for exponential resolution optimization.
      --
      --  TODO???
      --
      --  1. Store an array rather than a vector (traversing the equation first
      --     to find out all variables).
      --
      --  2. Store vectors rather than lists, to have a more bounded memory
      --     behavior.
      --
      --  3. Try to not re-iterate on every atoms in the optimization.

      Cut_Dead_Branches : Boolean := False;
      --  Optimization that will cut branches that necessarily contain falsy
      --  solutions.

      Sort_Ctx : Sort_Context;
      --  Context used for the topological sort, when reaching a complete
      --  potential solution. Stored once in the context to save ourselves
      --  from reallocating data structures everytime.

      Tried_Solutions : Nat_Access;
      --  Number of tried solutions. Stored for analytics purpose, and
      --  potentially for timeout.
   end record;
   --  Context for the solving of a compound relation

   procedure Clear (Sort_Ctx : Sort_Context);
   --  Clear the data of the sorting context

   function Create return Solving_Context;
   --  Create a new instance of a solving context. The data will be cleaned up
   --  and deallocated by a call to ``Destroy``.

   procedure Destroy (Ctx : in out Solving_Context);
   --  Destroy a solving context, and associated data

   function Solve_Compound
     (Self : Compound_Relation; Ctx : Solving_Context) return Boolean;
   --  Look for valid solutions in ``Self`` & ``Ctx``. Return whether to
   --  continue looking for other solutions.

   -----------
   -- Clear --
   -----------

   procedure Clear (Sort_Ctx : Sort_Context) is
   begin
      Atom_Lists.Clear (Sort_Ctx.N_Preds);
      Atom_Lists.Clear (Sort_Ctx.Working_Set);
      for I in Sort_Ctx.Using_Atoms.First_Index
               .. Sort_Ctx.Using_Atoms.Last_Index
      loop
         Atom_Lists.Clear (Sort_Ctx.Using_Atoms.Get_Access (I).all);
      end loop;
   end Clear;

   ------------
   -- Create --
   ------------

   function Create return Solving_Context is
   begin
      return Ret : Solving_Context do
         Ret.Sort_Ctx := new Sort_Context_Type;
         Ret.Tried_Solutions := new Natural'(0);
      end return;
   end Create;

   ----------------------
   -- Prepare_Relation --
   ----------------------

   function Prepare_Relation (Self : Relation) return Prepared_Relation is
      --  For determinism, collect variables in the order in which they appear
      --  in the equation.
      Vec : Logic_Var_Vectors.Vector;

      function Is_Atom (Self : Relation; Kind : Atomic_Kind) return Boolean
      is (Self.Kind = Atomic and then Self.Atomic_Rel.Kind = Kind);

      procedure Add (Var : Logic_Var);
      --  Add ``Var`` to ``Vec``/``Set``

      procedure Process_Atom (Self : Atomic_Relation);
      --  Collect variables from ``Self``

      function Process (Self : Relation) return Relation;
      --  Return a relation (with its dedicated ownership share) that is
      --  possibly a simplified version of ``Self``. Add to ``Vec`` the
      --  variables reference in ``Self`` during the traversal.

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

      procedure Process_Atom (Self : Atomic_Relation) is
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

      -------------
      -- Process --
      -------------

      function Process (Self : Relation) return Relation is
      begin
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

               Rels     : Relation_Array (1 .. Self.Compound_Rel.Rels.Length);
               Last_Rel : Natural := 0;
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
                  Last_Rel := Last_Rel + 1;
                  Rels (Last_Rel) := Process (R);

                  --  If we got a neutral or absorbing relation, simplify the
                  --  returned compound.

                  if Is_Atom (Rels (Last_Rel), Neutral) then

                     --  No need to add the neutral element to the result, and
                     --  thus the result will necessarily be different from
                     --  ``Self``.

                     Dec_Ref (Rels (Last_Rel));
                     Last_Rel := Last_Rel - 1;
                     Is_Different := True;

                  elsif Comp_Kind = Kind_All
                        and then Is_Atom (Rels (Last_Rel), Absorbing)
                  then

                     --  The whole compound can be replaced with the absorbing
                     --  relation: cleanup our temporary vector and return
                     --  that.

                     for R of Rels (1 .. Last_Rel - 1) loop
                        Dec_Ref (R);
                     end loop;
                     return Rels (Last_Rel);

                  --  Past here, we just add this sub-relation to the result.
                  --  Just make sure we update ``Is_Different`` when
                  --  appropriate.

                  elsif Rels (Last_Rel) /= R then
                     Is_Different := True;
                  end if;
               end loop;

               --  Now that we have processed each sub-relation, prepare the
               --  result.

               if Last_Rel = 0 then

                  --  All sub-relations were simplified to neutral elements: we
                  --  can replace the whole compound relation with the neutral
                  --  element itself.

                  return (if Neutral = True
                          then Create_True (Self.Debug_Info)
                          else Create_False (Self.Debug_Info));

               elsif Last_Rel = 1 then

                  --  Only one sub-relation is left: it already has a new
                  --  ownership share, so just return it.

                  return Rels (Last_Rel);

               elsif Is_Different then

                  --  We have more than one sub-relation, and the set of
                  --  sub-relations is different than the one in ``Self``, so
                  --  return a new compound relation.

                  return Result : constant Relation := new Relation_Type'
                    (Kind         => Compound,
                     Ref_Count    => 1,
                     Debug_Info   => Self.Debug_Info,
                     Compound_Rel => (Kind => Self.Compound_Rel.Kind,
                                      Rels => Relation_Vectors.Empty_Vector))
                  do
                     Result.Compound_Rel.Rels.Concat (Rels (1 .. Last_Rel));
                  end return;

               else
                  --  We are returning ``Self``: destroy the sub-relation
                  --  owership shares created for ``Rels`` as we do not create
                  --  a new compound relation.

                  for R of Rels loop
                     Dec_Ref (R);
                  end loop;
                  Inc_Ref (Self);
                  return Self;
               end if;
            end;
         end case;
      end Process;

      --  Simplify the input relation and add all variables to ``Vars`` in the
      --  same pass.

      Simplified_Relation : constant Relation := Process (Self);
   begin
      if Cst_Folding_Trace.Is_Active then
         Cst_Folding_Trace.Trace ("After constant folding:");
         Cst_Folding_Trace.Trace (Image (Simplified_Relation));
      end if;

      return Result : constant Prepared_Relation :=
        (Rel  => Simplified_Relation,
         Vars => new Logic_Var_Array (1 .. Vec.Length))
      do
         --  Convert the ``Vars`` vector into the ``Result.Vars`` array and
         --  assign Ids to all variables.

         for I in Result.Vars.all'Range loop
            declare
               V : Logic_Var renames Result.Vars.all (I);
            begin
               V := Vec.Get (I);
               Set_Id (V, I);
            end;
         end loop;
         Vec.Destroy;
      end return;
   end Prepare_Relation;

   --------------------
   -- Create_Aliases --
   --------------------

   procedure Create_Aliases
     (Vars          : Logic_Var_Array;
      Unifies       : Atomic_Relation_Vector;
      Vars_To_Atoms : access Var_Ids_To_Atoms := null)
   is
      Old_Unify_From_Id, Old_Target_Id : Positive;
   begin
      for V of Vars loop
         Reset (V);
      end loop;

      for U of Unifies loop
         if Verbose_Trace.Active then
            Verbose_Trace.Trace
              ("Aliasing var " & Image (U.Unify_From)
               & " to " & Image (U.Target));
         end if;

         if Vars_To_Atoms /= null then
            Old_Unify_From_Id := Id (U.Unify_From);
            Old_Target_Id := Id (U.Target);
         end if;

         Alias (U.Unify_From, U.Target);

         if Vars_To_Atoms /= null then
            --  After the aliasing, either the Id of ``U.Unify_From`` has
            --  changed, either it's the ID of ``U.Target``. Update the
            --  ``Var_Ids_To_Atoms`` map accordingly.

            if Id (U.Unify_From) /= Old_Unify_From_Id then
               Merge_Vars
                 (Vars_To_Atoms.all, Old_Unify_From_Id, Old_Target_Id);
            elsif Id (U.Target) /= Old_Target_Id then
               Merge_Vars
                 (Vars_To_Atoms.all, Old_Target_Id, Old_Unify_From_Id);
            end if;
         end if;
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
      Sort_Ctx       : Sort_Context;
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

      use Atom_Lists;

      function Id (S : Var_Or_Null) return Natural
      is (if S.Exists then Id (S.Logic_Var) else 0);
      --  Return the Id for the ``S`` variable, or 0 if there is no variable

      function Defined (S : Atomic_Relation) return Natural
      is (Id (Defined_Var (S)));
      --  Return the Id for the variable that ``S`` defines, or 0 if it
      --  contains no definition.

      ------------
      -- Append --
      ------------

      procedure Append (Atom : Atomic_Relation) is
      begin
         Last_Atom_Index := Last_Atom_Index + 1;
         Sorted_Atoms (Last_Atom_Index) := Atom;
      end Append;

      Using_Atoms : constant Atom_Lists_Vectors.Vector := Sort_Ctx.Using_Atoms;
      N_Preds     : Atom_Lists.List := Sort_Ctx.N_Preds;
      Working_Set : Atom_Lists.List := Sort_Ctx.Working_Set;
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
            Current_Atom : constant Atomic_Relation := Atoms.Get (I);

            --  Resolve the Id of the var used. If the var aliases to another
            --  var, resolve to the aliased var's Id.
            Used_Logic_Var : constant Var_Or_Null :=
              Used_Var (Current_Atom);
            Used_Id        : constant Natural :=
              (if Used_Logic_Var.Exists
               then (if Get_Alias (Used_Logic_Var.Logic_Var) /= No_Logic_Var
                     then Id (Get_Alias (Used_Logic_Var.Logic_Var))
                     else Id (Used_Logic_Var.Logic_Var))
               else 0);
         begin
            if Current_Atom.Kind = N_Predicate then
               --  N_Predicates are appended at the end separately

               N_Preds := (Current_Atom, I) & N_Preds;

            elsif Used_Id = 0 then
               --  Put atoms with no dependency in the working set

               Working_Set := (Current_Atom, I) & Working_Set;

            elsif Current_Atom.Kind /= Unify then
               --  For other atoms, put them in the ``Using_Atoms`` map, which
               --  represents the edges of the dependency graph.

               Push (Using_Atoms.Get_Access (Used_Id).all,
                     (Current_Atom, I));

            else
               --  Aliasing processing prior to the topo sort is supposed to
               --  take care of Unifys, which should not appear in ``Atoms``.

               raise Program_Error with "unreachable code";
            end if;
         end;
      end loop;

      --  Step 3: Do the topo sort

      while Has_Element (Working_Set) loop
         --  The dependencies of all atoms in the working set are already in
         --  the topo sort result (this is the invariant of
         --  ``Sort_Context_Type.Working_Set``): we can just take the first one
         --  and put it in the result too.
         declare
            Atom    : constant Atom_And_Index := Pop (Working_Set);
            Defd_Id : constant Natural := Defined (Atom.Atom);
         begin
            Append (Atom.Atom);
            Appended (Atom.Atom_Index) := True;

            --  If this atom defines a variable, put all the atoms that use
            --  this variable in the working set, as their dependencies are now
            --  satisfied.
            if Defd_Id /= 0 then
               for El of Using_Atoms.Get (Defd_Id) loop
                  Working_Set := El & Working_Set;
               end loop;

               --  Remove items from Using_Atoms, so that they're not appended
               --  again to the working set.
               Clear (Using_Atoms.Get_Access (Defd_Id).all);

               Defined_Vars (Defd_Id) := True;
            end if;
         end;
      end loop;

      --  Append at the end all N_Predicates for which all input variables are
      --  defined.
      for N_Pred of N_Preds loop
         if (for all V of N_Pred.Atom.Vars => Defined_Vars (Id (V))) then
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

      Clear (Working_Set);
      Clear (N_Preds);

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

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Ctx : in out Solving_Context) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Atomic_Relation_Vector, Atoms_Vector_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Sort_Context_Type, Sort_Context);
      procedure Free is new Ada.Unchecked_Deallocation
        (Natural, Nat_Access);
   begin
      Ctx.Unifies.Destroy;
      Ctx.Atoms.Destroy;
      Any_Relation_Lists.Destroy (Ctx.Anys);
      Ctx.Vars_To_Atoms.Destroy;
      Free (Ctx.Unifies);
      Free (Ctx.Atoms);
      Free (Ctx.Tried_Solutions);

      Atom_Lists.Destroy (Ctx.Sort_Ctx.N_Preds);
      Atom_Lists.Destroy (Ctx.Sort_Ctx.Working_Set);
      for I in  Ctx.Sort_Ctx.Using_Atoms.First_Index ..
        Ctx.Sort_Ctx.Using_Atoms.Last_Index
      loop
         Atom_Lists.Destroy (Ctx.Sort_Ctx.Using_Atoms.Get_Access (I).all);
      end loop;
      Ctx.Sort_Ctx.Using_Atoms.Destroy;

      --  Cleanup logic vars for future solver runs using them. Note that no
      --  aliasing information is supposed to be left at this stage.

      for V of Ctx.Vars.all loop
         Reset (V);
         Set_Id (V, 0);
      end loop;
      Free (Ctx.Vars);

      Free (Ctx.Sort_Ctx);
   end Destroy;

   -------------
   -- Reserve --
   -------------

   procedure Reserve (V : in out Var_Ids_To_Atoms; Size : Positive) is
   begin
      while V.Length < Size loop
         V.Append (Atomic_Relation_Lists.Create);
      end loop;
   end Reserve;

   ----------------
   -- Merge_Vars --
   ----------------

   procedure Merge_Vars
     (Self : in out Var_Ids_To_Atoms; From : Natural; To : Positive)
   is
      use Atomic_Relation_Lists;
   begin
      --  If the source list is conceptually empty, there is nothing to do

      if From = 0 or else From > Self.Length then
         return;
      end if;

      --  Since ``Self`` is resized on demand, it is possible to have a
      --  non-empty list for ``From`` but have no list allocated for ``To``
      --  yet: make sure the vector is big enough to hold the destination list.

      Reserve (Self, To);

      --  Finally do the merge: just pop all items from ``From`` and push them
      --  to ``To``.

      declare
         From_List : Atomic_Relation_Lists.List renames
           Self.Get_Access (From).all;
         To_List   : Atomic_Relation_Lists.List renames
           Self.Get_Access (To).all;
      begin
         while Has_Element (From_List) loop
            Push (To_List, Pop (From_List));
         end loop;
      end;
   end Merge_Vars;

   --------------
   -- Used_Var --
   --------------

   function Used_Var (Self : Atomic_Relation) return Var_Or_Null
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

   function Defined_Var (Self : Atomic_Relation) return Var_Or_Null is
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
     (Inner        : Atomic_Relation;
      Debug_String : String_Access := null) return Relation
   is
     (new Relation_Type'
        (Atomic,
         Atomic_Rel => Inner,
         Debug_Info => Debug_String,
         Ref_Count  => 1));

   -----------------
   -- To_Relation --
   -----------------

   function To_Relation
     (Inner        : Compound_Relation;
      Debug_String : String_Access := null) return Relation
   is
     (new Relation_Type'
        (Compound,
         Compound_Rel => Inner,
         Debug_Info   => Debug_String,
         Ref_Count    => 1));

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
     (Self : Compound_Relation; Ctx : Solving_Context) return Boolean
   is
      function Try_Solution (Atoms : Atomic_Relation_Vector) return Boolean;
      --  Try to solve the given sequence of atoms. Return whether no valid
      --  solution was found (so return False on success).

      function Process_Atom (Atom : Atomic_Relation) return Boolean;
      --  Process one atom, whether we are in an ``All`` or ``Any`` branch.
      --  Returns whether we should abort current path or not, in the case of
      --  an ``All`` relation.

      function Cleanup (Val : Boolean) return Boolean;
      --  Cleanup helper to call before exitting ``Solve_Compound``

      procedure Branch_Cleanup;
      --  Cleanup helper to call after having processed an ``Any``
      --  sub-relation.

      procedure Create_Aliases;
      --  Shortcut to create aliases from ``Ctx.Unifies``

      procedure Cleanup_Aliases;
      --  Shortcut to cleanup aliases in ``Ctx.Vars``

      use Any_Relation_Lists;
      use Atomic_Relation_Lists;

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
         Clear (Ctx.Sort_Ctx);

         Ctx.Tried_Solutions.all := Ctx.Tried_Solutions.all + 1;

         Sol_Trace.Trace ("Tried solutions: " & Ctx.Tried_Solutions.all'Image);

         declare
            use Atomic_Relation_Vectors;
            Sorting_Error : Boolean;
            Sorted_Atoms  : constant Elements_Array :=
              Topo_Sort (Atoms,
                         Ctx.Unifies.all,
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

      Vars_To_Atoms          : aliased Var_Ids_To_Atoms :=
        Ctx.Vars_To_Atoms.Copy;
      Initial_Atoms_Length   : Natural renames Ctx.Atoms.Last_Index;
      Initial_Unifies_Length : Natural renames Ctx.Unifies.Last_Index;

      --------------------
      -- Create_Aliases --
      --------------------

      procedure Create_Aliases is
      begin
         Create_Aliases
           (Ctx.Vars.all,
            Ctx.Unifies.all,
            (if Ctx.Cut_Dead_Branches then Vars_To_Atoms'Access else null));
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

         Vars_To_Atoms.Destroy;
         Vars_To_Atoms := Ctx.Vars_To_Atoms.Copy;
      end Branch_Cleanup;

      -------------
      -- Cleanup --
      -------------

      function Cleanup (Val : Boolean) return Boolean is
      begin
         --  Unalias every var that was aliased
         Cleanup_Aliases;

         Branch_Cleanup;
         Vars_To_Atoms.Destroy;
         Trav_Trace.Decrease_Indent;
         return Val;
      end Cleanup;

      ------------------
      -- Process_Atom --
      ------------------

      function Process_Atom (Atom : Atomic_Relation) return Boolean is
      begin
         if Atom.Kind = Unify then
            if Atom.Unify_From /= Atom.Target then
               Reserve (Vars_To_Atoms, Id (Atom.Unify_From));
               Ctx.Unifies.Append (Atom);
            end if;
            return True;

         elsif Atom.Kind = True then
            return True;

         elsif Atom.Kind = False then
            return False;
         end if;

         Ctx.Atoms.Append (Atom);

         --  Exponential resolution optimization: if relevant, add the atomic
         --  relation to the mappings of vars to atoms.
         if Ctx.Cut_Dead_Branches and then Atom.Kind in Predicate | Assign then
            if Solv_Trace.Is_Active then
               Solv_Trace.Trace
                 ("== Appending " & Image (Atom) & " to Vars_To_Atoms");
            end if;

            declare
               V    : constant Var_Or_Null := (if Atom.Kind = Predicate
                                               then Used_Var (Atom)
                                               else Defined_Var (Atom));
               V_Id : constant Natural := Id (V.Logic_Var);
            begin
               Reserve (Vars_To_Atoms, V_Id);
               Push (Vars_To_Atoms.Get_Access (V_Id).all, Atom);
            end;
         end if;

         return True;
      end Process_Atom;

   begin
      Trav_Trace.Increase_Indent ("In Solve_Compound " & Self.Kind'Image);

      case Self.Kind is

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

         declare
            Anys : Any_Relation_List := Ctx.Anys;
            --  List of direct sub-relations of ``Self`` that are ``Any``
         begin
            for Sub_Rel of Self.Rels loop
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
                  --  ``Self`` is an ``All`` relation, so ``Sub_Rel`` cannot be
                  --  an ``All`` as well, so it if is compound, it must be an
                  --  ``Any``.
                  pragma Assert (Sub_Rel.Compound_Rel.Kind = Kind_Any);
                  Anys := Sub_Rel.Compound_Rel & Anys;

               when Atomic =>
                  if not Process_Atom (Sub_Rel.Atomic_Rel) then
                     return Cleanup (True);
                  end if;
               end case;
            end loop;

            if Ctx.Cut_Dead_Branches then
               --  Exponential resolution optimization: check if any atom
               --  *defines* the value of a var that is *used* by another atom
               --  in that solution branch.
               --
               --  TODO??? PROBLEM: While this avoids exponential resolutions,
               --  it also makes the default algorithm quadratic (?), since we
               --  re-iterate on all atoms at every depth of the recursion.
               --  What we could do is:
               --
               --  1. Either not activate this opt for certain trees.
               --
               --  2. Either try to check only for new atoms. This seems
               --     hard/impossible since new constraints are added at every
               --     recursion, so old atoms need to be checked again for
               --     completeness. But maybe there is a way. Investigate
               --     later.

               Create_Aliases;
               for Atom of Ctx.Atoms.all loop
                  if Atom.Kind = Assign then
                     declare
                        V : constant Var_Or_Null := Defined_Var (Atom);

                        --  TODO??? with aliasing, a variable can have several
                        --  ids.
                        V_Id : constant Positive := Id (V.Logic_Var);

                        Dummy : Boolean;
                     begin
                        pragma Assert (Vars_To_Atoms.Length >= V_Id);

                        --  If there are atomic relations which use this
                        --  variable, try to solve them: if at least one fails,
                        --  then there is no way we can find a valid solution
                        --  in this branch: we can return early to avoid
                        --  recursions.
                        if Length (Vars_To_Atoms.Get (V_Id)) > 0 then
                           Dummy := Solve_Atomic (Atom);
                           for User of Vars_To_Atoms.Get (V_Id) loop
                              if not Solve_Atomic (User) then
                                 if Solv_Trace.Active then
                                    Solv_Trace.Trace
                                      ("Aborting due to exp res optim");
                                    Solv_Trace.Trace
                                      ("Current atoms: "
                                       & Image (Ctx.Atoms.all));
                                    Solv_Trace.Trace
                                      ("Stored atom: " & Image (User));
                                    Solv_Trace.Trace
                                      ("Current atom: " & Image (Atom));
                                 end if;
                                 Reset (V.Logic_Var);
                                 return Cleanup (True);
                              end if;
                           end loop;

                           --  Else, reset the value of var for further solving
                           Reset (V.Logic_Var);
                        end if;
                     end;
                  end if;
               end loop;
               Cleanup_Aliases;
            end if;

            if Has_Element (Anys) then
               --  The relation we are trying to solve in this instance of
               --  ``Solve_Compound`` is the equivalent of:
               --
               --     Ctx.Atoms & All (Anys)
               --
               --  Exploring solutions for this complex relation is not linear:
               --  we need recursion. Start with the head of ``Anys``:
               --
               --     Ctx.Atoms & Head (Anys)
               --
               --  And leave the rest for later:
               --
               --     Ctx.Atoms & Tail (Anys)
               if Trav_Trace.Is_Active then
                  Trav_Trace.Trace ("Before recursing in solve All");
                  Trav_Trace.Trace (Image (Ctx.Atoms.all));
                  Trav_Trace.Trace (Image (Anys));
               end if;

               return Cleanup
                 (Solve_Compound
                    (Head (Anys),
                     Ctx'Update (Anys          => Tail (Anys),
                                 Vars_To_Atoms => Vars_To_Atoms)));

            else
               --  We don't have any Any relation left, so we have a flat list
               --  of atoms to solve.
               return Cleanup (Try_Solution (Ctx.Atoms.all));
            end if;
         end;

      when Kind_Any =>
         --  Recurse for each ``Any`` alternative (i.e. sub-relation)

         for Sub_Rel of Self.Rels loop
            case Sub_Rel.Kind is
               when Atomic =>
                  pragma Assert (Sub_Rel.Atomic_Rel.Kind /= False);

                  --  Add ``Sub_Rel`` to ``Ctx.Atoms``

                  declare
                     Dummy : Boolean := Process_Atom (Sub_Rel.Atomic_Rel);
                  begin
                     null;
                  end;

                  if Has_Element (Ctx.Anys) then
                     --  Assuming ``Ctx.Anys`` is not empty, we need to find
                     --  solutions for:
                     --
                     --     Ctx.Atoms & Ctx.Anys
                     --
                     --  As usual, try first to solve:
                     --
                     --     Ctx.Atoms & Head (Ctx.Anys)
                     --
                     --  Leaving the following for the recursion:
                     --
                     --     Ctx.Atoms & Tail (Ctx.Anys)
                     if not Solve_Compound
                       (Head (Ctx.Anys),
                        Ctx'Update (Anys          => Tail (Ctx.Anys),
                                    Vars_To_Atoms => Vars_To_Atoms))
                     then
                        return Cleanup (False);
                     end if;

                  else
                     --  We are currently exploring only one alternative: just
                     --  look for a solution in ``Ctx.Atoms``.
                     if not Try_Solution (Ctx.Atoms.all) then
                        return Cleanup (False);
                     end if;
                     Cleanup_Aliases;
                  end if;

               when Compound =>
                  --  See the corresponding assertion in the ``Kind_All``
                  --  section.
                  pragma Assert (Sub_Rel.Compound_Rel.Kind = Kind_All);

                  if not Solve_Compound
                    (Sub_Rel.Compound_Rel,
                     Ctx'Update (Vars_To_Atoms => Vars_To_Atoms))
                  then
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
      Ctx    : Solving_Context := Create;
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
      Ctx.Cut_Dead_Branches := Solve_Options.Cut_Dead_Branches;

      if Solver_Trace.Is_Active then
         Solver_Trace.Trace ("Solving equation:");
         Solver_Trace.Trace (Image (Self));
      end if;

      PRel := Prepare_Relation (Self);

      Ctx.Cb := Solution_Callback'Unrestricted_Access.all;
      Ctx.Vars := PRel.Vars;
      Ctx.Atoms := new Atomic_Relation_Vector;
      Ctx.Unifies := new Atomic_Relation_Vector;

      --  Initialize the ``Using_Atoms`` vector of lists for topo sort to have
      --  one entry per variable.

      for Dummy in Ctx.Vars.all'Range loop
         Ctx.Sort_Ctx.Using_Atoms.Append (Atom_Lists.Create);
      end loop;

      case Rel.Kind is
         when Compound =>
            Ignore := Solve_Compound (Rel.Compound_Rel, Ctx);

         when Atomic =>
            --  We want to use a single entry point for the solver:
            --  ``Solve_Compound``. Create a trivial compound relation to wrap
            --  the given atom.

            declare
               C : Compound_Relation := (Kind => Kind_All, Rels => <>);
            begin
               C.Rels.Append (Rel);
               Ignore := Solve_Compound (C, Ctx);
               C.Rels.Destroy;
            exception
               when others =>
                  C.Rels.Destroy;
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
         Cleanup;
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

      type Var_Array_Access is access all Logic_Var_Array;
      type Val_Array_Access is access all Value_Array;

      Last_Vars : Var_Array_Access := null;
      Last_Vals : Val_Array_Access := null;

      procedure Free is new Ada.Unchecked_Deallocation
        (Logic_Var_Array, Var_Array_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Value_Array, Val_Array_Access);

      --------------
      -- Callback --
      --------------

      function Callback (Vars : Logic_Var_Array) return Boolean is
      begin
         Ret := True;
         Last_Vals := new Value_Array (Vars'Range);
         Last_Vars := new Logic_Var_Array'(Vars);
         for I in  Vars'Range loop
            Last_Vals (I) := Get_Value (Vars (I));
         end loop;
         return False;
      end Callback;

   begin
      Solve (Self, Callback'Access, Solve_Options);
      if Last_Vars /= null then
         for I in Last_Vars'Range loop
            Set_Value (Last_Vars (I), Last_Vals (I));
         end loop;
      end if;
      Free (Last_Vars);
      Free (Last_Vals);
      return Ret;
   end Solve_First;

   -----------------
   -- Create_True --
   -----------------

   function Create_True (Debug_String : String_Access := null) return Relation
   is (To_Relation (Atomic_Relation'(True, Target => <>),
                    Debug_String => Debug_String));

   ------------------
   -- Create_False --
   ------------------

   function Create_False (Debug_String : String_Access := null) return Relation
   is (To_Relation (Atomic_Relation'(False, Target => <>),
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
        (Atomic_Relation'
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
        (Atomic_Relation'
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
      if Eq /= No_Comparer then
         raise Unsupported_Error with
           "Comparer_Type not supported with the symbolic solver";
      end if;

      if Conv /= No_Converter then
         Conv_Ptr := new Converter_Type'Class'(Conv);
      end if;

      return To_Relation
        (Atomic_Relation'
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
        (Atomic_Relation'(Kind => Unify, Target => Right, Unify_From => Left),
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
      return (To_Relation (Atomic_Relation'(Kind   => Propagate,
                                            Conv   => Conv,
                                            From   => From,
                                            Target => To),
                           Debug_String => Debug_String));
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
      if Eq /= No_Comparer then
         raise Unsupported_Error with
           "Comparer_Type not supported with the symbolic solver";
      end if;

      if Conv /= No_Converter then
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

      return To_Relation
        (Compound_Relation'(Cmp_Kind, Rels), Debug_String => Debug_String);
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

   procedure Destroy (Self : in out Atomic_Relation) is
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

   procedure Destroy (Self : in out Compound_Relation) is
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
      function Assign_Val (Val : Value_Type) return Boolean;
      --  Tries to assign ``Val`` to ``Self.Target`` and return True either if
      --  ``Self.Target`` already has a value compatible with ``Val``, or if
      --  it had no value and the assignment succeeded.
      --
      --  This assumes that ``Self`` is either an ``Assign`` or a `Propagate``
      --  relation.

      ----------------
      -- Assign_Val --
      ----------------

      function Assign_Val (Val : Value_Type) return Boolean is
         Conv_Val : constant Value_Type :=
           (if Self.Conv /= null
            then Self.Conv.Convert (Val)
            else Val);
      begin
         if Is_Defined (Self.Target) then
            return Conv_Val = Get_Value (Self.Target);
         else
            Set_Value (Self.Target, Conv_Val);
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
      if not Is_Defined_Or_Null (Used_Var (Self)) then
         raise Early_Binding_Error with
           "Relation " & Image (Self)
           & " needs var " & Image (Used_Var (Self).Logic_Var)
           & " to be defined";
      end if;

      case Self.Kind is
         when Assign =>
            Ret := Assign_Val (Self.Val);

         when Propagate =>
            pragma Assert (Is_Defined (Self.From));
            Ret := Assign_Val (Get_Value (Self.From));

         when Predicate =>
            pragma Assert (Is_Defined (Self.Target));
            Ret := Self.Pred.Call (Get_Value (Self.Target));

         when N_Predicate =>

            for V of Self.Vars loop
               if not Is_Defined (V) then
                  if Solv_Trace.Active then
                     Solv_Trace.Trace
                       ("Trying to apply " & Image (Self)
                        & ", but " & Image (V) & " is not defined");
                  end if;
                  return False;
               end if;
               if Solv_Trace.Active then
                  Solv_Trace.Trace ("Var = " & Value_Image (Get_Value (V)));
               end if;
            end loop;

            declare
               Vals : Value_Array (1 .. Self.Vars.Length);
            begin
               for I in Self.Vars.First_Index .. Self.Vars.Last_Index loop
                  Vals (I) := Get_Value (Self.Vars.Get (I));
               end loop;

               Ret := Self.N_Pred.Call (Vals);
            end;

         when True  => Ret := True;
         when False => Ret := False;

         when Unify => raise Assertion_Error with "Should never happen";
      end case;

      if not Ret and then Solv_Trace.Active then
         Solv_Trace.Trace ("Solving " & Image (Self) & " failed!");
      end if;

      return Ret;
   end Solve_Atomic;

   -----------
   -- Image --
   -----------

   function Image (Self : Atomic_Relation) return String is

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

   function Image (Self : Any_Relation_List) return String is

      function Img (Rel : Any_Rel) return String is
        (Image (Rel));

      function Anys_Array_Image is new Langkit_Support.Images.Array_Image
        (Any_Rel,
         Positive,
         Any_Relation_Lists.T_Array,
         Image => Img);
   begin
      return Anys_Array_Image (Any_Relation_Lists.To_Array (Self));
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Self         : Compound_Relation;
      Level        : Natural := 0;
      Debug_String : String_Access := null) return String
   is
      Ret : XString;
   begin
      Ret.Append
        ((case Self.Kind is
            when Kind_All => "All:",
            when Kind_Any => "Any:")
         & (if Debug_String /= null and then Debug_String.all /= ""
            then " " & Debug_String.all
            else "")
         & ASCII.LF);

      for Rel of Self.Rels loop
         Ret.Append ((1 .. Level + 4 => ' ')
                     & Internal_Image (Rel, Level + 4) & ASCII.LF);
      end loop;

      return Ret.To_String;
   end Image;

   -----------
   -- Image --
   -----------

   function Internal_Image
     (Self : Relation; Level : Natural := 0) return String is
   begin
      case Self.Kind is
         when Compound =>
            return Image (Self.Compound_Rel, Level, Self.Debug_Info);
         when Atomic => return
              Image (Self.Atomic_Rel)
              & (if Self.Debug_Info /= null and then Self.Debug_Info.all /= ""
                 then " " & Self.Debug_Info.all
                 else "");
      end case;
   end Internal_Image;

   --------------------
   -- Relation_Image --
   --------------------

   function Image (Self : Relation) return String
   is
     (Internal_Image (Self));

end Langkit_Support.Adalog.Symbolic_Solver;
