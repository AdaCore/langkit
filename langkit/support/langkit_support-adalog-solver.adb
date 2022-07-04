--
--  Copyright (C) 2019-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Assertions; use Ada.Assertions;
with Ada.Calendar;   use Ada.Calendar;
with Ada.Exceptions; use Ada.Exceptions;

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;

with GNATCOLL.Strings; use GNATCOLL.Strings;

with Langkit_Support.Images;

package body Langkit_Support.Adalog.Solver is

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

   package Positive_Vectors is new Langkit_Support.Vectors (Positive);
   type Positive_Vector_Array is
     array (Positive range <>) of Positive_Vectors.Vector;
   type Positive_Vector_Array_Access is access all Positive_Vector_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Positive_Vector_Array, Positive_Vector_Array_Access);

   type Sort_Context is record
      Defining_Atoms : Positive_Vector_Array_Access;
      --  For each logic variable, list of atoms indexes for atoms that define
      --  this variable.

      Has_Contradiction_Counter : Natural;
      --  Number of times ``Has_Contradiction`` was called. Used for
      --  logging/debugging purposes.
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

   function Prepare_Relation (Self : Relation) return Prepared_Relation;
   --  Prepare a relation for the solver: simplify it and create a list of all
   --  the logic variables it references, assigning an Id to each.

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

   type Solving_Context is record
      Cb : Callback_Type;
      --  User callback, to be called when a solution is found. Returns whether
      --  to continue exploring the solution space.

      Vars : Logic_Var_Array_Access;
      --  List of all logic variables referenced in the top-level relation.
      --
      --  Indexes in this array are the same as Ids for the corresponding
      --  variables, i.e. ``for all I in Vars.all => I = Id (Vars.all (I))``.
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

      Timeout : Natural;
      --  Number of times left we allow ourselves to evaluate an atom before
      --  aborting the solver. If 0, no timeout applies.

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

   function Evaluate_Atoms
     (Ctx          : in out Solving_Context;
      Sorted_Atoms : Atomic_Relation_Vectors.Elements_Array) return Boolean;
   --  Evaluate the given sequence of sorted atoms (see ``Topo_Sort``) and
   --  return whether they are all satisfied: if they are, the logic variables
   --  are assigned values, so it is possible to invoke the user callback for
   --  solutions.

   function Has_Contradiction
     (Atoms, Unifies : Atomic_Relation_Vector;
      Vars           : Logic_Var_Array;
      Ctx            : in out Solving_Context) return Boolean;
   --  Return whether the given sequence of atoms contains a contradiction,
   --  i.e. if two or more of its atoms make each other unsatisfied. This
   --  function works even for incomplete sequences, for instance when one atom
   --  uses a variable that no atom defines.

   function Solve_Compound
     (Self : Compound_Relation; Ctx : in out Solving_Context) return Boolean;
   --  Look for valid solutions in ``Self`` & ``Ctx``. Return whether to
   --  continue looking for other solutions.

   procedure Trace_Timing (Label : String; Start : Time);
   --  Log ``Start .. Clock`` as the time it took to run ``Label``

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
        (Defining_Atoms            => new Positive_Vector_Array'
           (Vars'Range => Positive_Vectors.Empty_Vector),
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

   function Prepare_Relation (Self : Relation) return Prepared_Relation is
      --  For determinism, collect variables in the order in which they appear
      --  in the equation.
      Vec : Logic_Var_Vectors.Vector;

      Next_Id : Positive := 1;
      --  Id to assign to the next processed relation

      function Is_Atom (Self : Relation; Kind : Atomic_Kind) return Boolean
      is (Self.Kind = Atomic and then Self.Atomic_Rel.Kind = Kind);

      procedure Add (Var : Logic_Var);
      --  Add ``Var`` to ``Vec``/``Set``

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
      Start           : constant Time := Clock;
      Folded_Relation : constant Relation := Fold_And_Track_Vars (Self);
   begin
      Trace_Timing ("Constant folding", Start);

      if Cst_Folding_Trace.Is_Active then
         Cst_Folding_Trace.Trace ("After constant folding:");
         Cst_Folding_Trace.Trace (Image (Folded_Relation));
      end if;

      if Stats_Trace.Is_Active then
         declare
            All_Count, Any_Count, Atoms_Count : Natural := 0;

            procedure Traverse (Self : Relation);

            --------------
            -- Traverse --
            --------------

            procedure Traverse (Self : Relation) is
            begin
               case Self.Kind is
                  when Atomic =>
                     Atoms_Count := Atoms_Count + 1;

                  when Compound =>
                     case Self.Compound_Rel.Kind is
                        when Kind_All =>
                           All_Count := All_Count + 1;
                        when Kind_Any =>
                           Any_Count := Any_Count + 1;
                     end case;
                     for Sub_R of Self.Compound_Rel.Rels loop
                        Traverse (Sub_R);
                     end loop;
               end case;
            end Traverse;

         begin
            Traverse (Self);
            Stats_Trace.Trace ("All relations:" & All_Count'Image);
            Stats_Trace.Trace ("Any relations:" & Any_Count'Image);
            Stats_Trace.Trace ("Atoms:" & Atoms_Count'Image);
         end;
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

      Result.Rel := Folded_Relation;
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

      Defining_Atoms : Positive_Vector_Array renames
        Sort_Ctx.Defining_Atoms.all;

      function Append_Definition (Var : Logic_Var) return Boolean;
      --  Try to append an atom that defines ``Var`` instead. This returns
      --  False if there is no atom that defines ``Var`` or if dependency cyles
      --  prevent us from appending a sequence of atoms to achieve that.
      --  Otherwise (on success), return True.

      function Append_Definitions (Vars : Logic_Var_Vector) return Boolean;
      --  Likewise but try to append definitions for all the given variables.
      --  Return False if we fail to add a definition for at least one
      --  variable.

      function Append (Atom_Index : Positive) return Boolean;
      --  Try to append ``Atoms (Atom_Index)`` (and its dependencies) to
      --  ``Sorted_Atoms``. Return whether successful.

      Appended : array (Sorted_Atoms'Range) of Boolean := (others => False);
      --  ``Appended (I)`` indicates whether the ``Atoms (I)`` atom was
      --  appended to ``Sorted_Atoms``.
      --
      --  TODO??? It actually says that the atom does not need to be appended
      --  to the result (for instance it's true for ``Unify`` atoms even though
      --  these are not to be part of the result). We should probably rename
      --  this.

      Visiting : array (Sorted_Atoms'Range) of Boolean := (others => False);
      --  ``Visiting (I)`` indicates whether our recursive traversal of the
      --  dependency graph of all atoms is currently visiting the ``Atoms (I)``
      --  atom.
      --
      --  If one atom is being visited but not yet appended to the result, i.e.
      --  if ``Visiting (I) and not Appended (I)``, then we have found a
      --  dependency cycle.

      function Id_Or_Null (Var : Logic_Var) return Natural
      is (if Var = null then 0 else Id (Var));
      --  Return the Id for the ``Var`` variable, or 0 if there is no variable

      function Defined (S : Atomic_Relation_Type) return Natural
      is (Id_Or_Null (Defined_Var (S)));
      --  Return the Id for the variable that ``S`` defines, or 0 if it
      --  contains no definition.

      -----------------------
      -- Append_Definition --
      -----------------------

      function Append_Definition (Var : Logic_Var) return Boolean is
      begin
         for Definition of Defining_Atoms (Id (Var)) loop
            if Append (Definition) then
               return True;
            end if;
         end loop;
         return False;
      end Append_Definition;

      ------------------------
      -- Append_Definitions --
      ------------------------

      function Append_Definitions (Vars : Logic_Var_Vector) return Boolean is
      begin
         for V of Vars loop
            if not Append_Definition (V) then
               return False;
            end if;
         end loop;
         return True;
      end Append_Definitions;

      ------------
      -- Append --
      ------------

      function Append (Atom_Index : Positive) return Boolean is
      begin
         if Appended (Atom_Index) then

            --  If we already appended this atom to the result, there is
            --  nothing to do.

            return True;

         elsif Visiting (Atom_Index) then

            --  We are trying to append this atom as a consequence of another
            --  attempt to append this atom: we have found a dependency cycle.
            --  It is not possible to add this atom right now.

            return False;

         else
            Visiting (Atom_Index) := True;
            declare
               Rel  : constant Atomic_Relation := Atoms.Get (Atom_Index);
               Atom : Atomic_Relation_Type renames Rel.Atomic_Rel;

               --  Try to satisfy all dependencies for this atom

               Deps_Satisfied : constant Boolean :=
                 (case Atom.Kind is
                  when Assign | True | False => True,

                  when Propagate   => Append_Definition (Atom.From),
                  when N_Propagate => Append_Definitions (Atom.Comb_Vars),
                  when Predicate   => Append_Definition (Atom.Target),
                  when N_Predicate => Append_Definitions (Atom.Vars),

                  --  All Unify relations should be in the Unifies list: no
                  --  Unify relation should be in Atoms.

                  when Unify => raise Program_Error);
            begin
               --  If all dependencies are satisfied, we can append this atom

               if Deps_Satisfied then
                  Last_Atom_Index := Last_Atom_Index + 1;
                  Sorted_Atoms (Last_Atom_Index) := Rel;
                  Appended (Atom_Index) := True;
               end if;

               Visiting (Atom_Index) := False;
               return Deps_Satisfied;
            end;
         end if;
      end Append;

   begin
      Has_Orphan := False;

      --  Step 1, process Unify atoms so that the processing of other atoms
      --  correctly handles aliased variables.

      Create_Aliases (Vars, Unifies);

      --  Step 2, create a map of vars to the first atom that defines it

      for I in Atoms.First_Index .. Atoms.Last_Index loop
         declare
            Rel : constant Atomic_Relation := Atoms.Get (I);
            Def : constant Natural := Defined (Rel.Atomic_Rel);
         begin
            if Def /= 0 then
               Defining_Atoms (Def).Append (I);
            end if;
         end;
      end loop;

      --  Step 3, go through all atoms and recurse to add its dependencies and
      --  the atom itself to the sorted list.

      for I in Atoms.First_Index .. Atoms.Last_Index loop
         if not Append (I) then
            Has_Orphan := True;

            --  If requested, log all orphan atoms

            if Solv_Trace.Is_Active then
               Solv_Trace.Trace ("Orphan relation: " & Image (Atoms.Get (I)));
            end if;
         end if;
      end loop;

      --  Clean up the Defining_Atoms shared data structure for the next topo
      --  sort.

      for Defs of Defining_Atoms loop
         Defs.Clear;
      end loop;

      return Sorted_Atoms (1 .. Last_Atom_Index);
   end Topo_Sort;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Sort_Ctx : in out Sort_Context) is
   begin
      for Atoms of Sort_Ctx.Defining_Atoms.all loop
         Atoms.Destroy;
      end loop;
      Free (Sort_Ctx.Defining_Atoms);
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

   --------------------
   -- Evaluate_Atoms --
   --------------------

   function Evaluate_Atoms
     (Ctx          : in out Solving_Context;
      Sorted_Atoms : Atomic_Relation_Vectors.Elements_Array) return Boolean is
   begin
      --  If we have a timeout, apply it

      if Ctx.Timeout > 0 then
         if Sorted_Atoms'Length > Ctx.Timeout then
            raise Timeout_Error;
         end if;
         Ctx.Timeout := Ctx.Timeout - Sorted_Atoms'Length;
      end if;

      --  Evaluate each individual atom

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

   -----------------------
   -- Has_Contradiction --
   -----------------------

   function Has_Contradiction
     (Atoms, Unifies : Atomic_Relation_Vector;
      Vars           : Logic_Var_Array;
      Ctx            : in out Solving_Context) return Boolean
   is
      Had_Exception : Boolean := False;
      Exc           : Exception_Occurrence;

      Result : Boolean;
   begin
      Ctx.Sort_Ctx.Has_Contradiction_Counter :=
        Ctx.Sort_Ctx.Has_Contradiction_Counter + 1;

      if Solv_Trace.Is_Active then
         Solv_Trace.Increase_Indent
           ("Looking for a contradiction (number"
            & Ctx.Sort_Ctx.Has_Contradiction_Counter'Image & ")");
         Solv_Trace.Trace (Image (Atoms));
      end if;

      declare
         use Atomic_Relation_Vectors;
         Dummy        : Boolean;
         Sorted_Atoms : constant Elements_Array :=
           Topo_Sort (Atoms, Unifies, Vars, Ctx.Sort_Ctx, Dummy);
      begin
         if Solv_Trace.Is_Active then
            Solv_Trace.Trace ("After partial topo sort");
            Solv_Trace.Trace (Image (Sorted_Atoms));
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
         --
         --  Do not catch the Timeout_Error exception, as it is not supposed to
         --  be raised by predicates: it's the solver that aborts solving.

         begin
            Result := not Evaluate_Atoms (Ctx, Sorted_Atoms);
         exception
            when Timeout_Error =>
               raise;

            when E : others =>
               Save_Occurrence (Exc, E);
               Had_Exception := True;
               Result := False;
         end;

         if Solv_Trace.Is_Active then
            if Had_Exception then
               Solv_Trace.Trace
                 (Exc,
                  "Got an exception, considering no contradiction was found:"
                  & ASCII.LF);
            elsif Result then
               Solv_Trace.Trace ("Contradiction found");
            else
               Solv_Trace.Trace ("No contradiction found");
            end if;
         end if;

         if Solv_Trace.Is_Active then
            Solv_Trace.Decrease_Indent;
         end if;

         Cleanup_Aliases (Vars);
         return Result;
      end;
   end Has_Contradiction;

   --------------
   -- Used_Var --
   --------------

   function Used_Var (Self : Atomic_Relation_Type) return Logic_Var
   is
      --  We handle Unify here, even though it is not strictly treated in the
      --  dependency graph, so that the Unify_From variable is registered in
      --  the list of variables of the equation.
      --
      --  We also pretend that N_Propagate has no dependency here because it
      --  depends on multiple variables. Callers should handle it separately.
      --
      --  TODO??? Might be cleaner to have a separate function to return all
      --  variables a relation uses?
     (case Self.Kind is
         when Assign | N_Propagate | True | False | N_Predicate => null,
         when Propagate => Self.From,
         when Predicate => Self.Target,
         when Unify     => Self.Unify_From);

   -----------------
   -- Defined_Var --
   -----------------

   function Defined_Var (Self : Atomic_Relation_Type) return Logic_Var
   is
      --  We handle Unify here, even though it is not strictly treated in the
      --  dependency graph, so that the Target variable is registered in
      --  the list of variables of the equation. TODO??? Might be cleaner to
      --  have a separate function to return all variables a relation defines?
     (case Self.Kind is
         when Assign | Propagate | N_Propagate | Unify => Self.Target,
         when Predicate | True | False | N_Predicate   => null);

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
            if not Evaluate_Atoms (Ctx, Sorted_Atoms) then
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

            --  The relation we are trying to solve here is the equivalent of::
            --
            --     Ctx.Atoms & All (Ctx.Anys)
            --
            --  Exploring solutions for this complex relation is not linear: we
            --  need recursion. Start with the head of ``Ctx.Anys``::
            --
            --     Ctx.Atoms & Head (Ctx.Anys)
            --
            --  And leave the rest for later:::
            --
            --     Ctx.Atoms & Tail (Ctx.Anys)

            Head := Ctx.Anys.Pop;
            begin
               Result := Solve_Compound (Head, Ctx);

               --  Just like the above call to ``Solve_Compound`` is
               --  supposed to leave upon return ``Ctx.Anys`` as it was at
               --  the beginning of the call, we should restore it here
               --  before returning to the state it was when ``Recurse`` was
               --  called.

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
         --  vectors (``Anys`` and ``Ctx.Atoms``).

         for Sub_Rel of Comp.Rels loop
            case Sub_Rel.Kind is
            when Compound =>
               --  The ``Create_All`` inlines the sub-relations of ``All``
               --  relations passed to it in the relation it returns. For
               --  instance::
               --
               --     Create_All ((Create_All ((A, B)), C))
               --
               --  is equivalent to::
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
              (Ctx.Atoms, Ctx.Unifies, Ctx.Vars.all, Ctx)
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

   ------------------
   -- Trace_Timing --
   ------------------

   procedure Trace_Timing (Label : String; Start : Time) is
   begin
      if Timing_Trace.Is_Active then
         Timing_Trace.Trace (Label & ":" & Duration'Image (Clock - Start));
      end if;
   end Trace_Timing;

   -----------
   -- Solve --
   -----------

   procedure Solve
     (Self              : Relation;
      Solution_Callback : access function
        (Vars : Logic_Var_Array) return Boolean;
      Solve_Options     : Solve_Options_Type := Default_Options;
      Timeout           : Natural := 0)
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

      PRel := Prepare_Relation (Self);
      Ctx := Create (Solution_Callback'Unrestricted_Access.all, PRel.Vars);
      Ctx.Timeout := Timeout;
      Ctx.Cut_Dead_Branches := Solve_Options.Cut_Dead_Branches;

      declare
         Start : constant Time := Clock;
      begin
         case Rel.Kind is
            when Compound =>
               Ignore := Solve_Compound (Rel, Ctx);

            when Atomic =>
               --  We want to use a single entry point for the solver:
               --  ``Solve_Compound``. Create a trivial compound relation to
               --  wrap the given atom.

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

         Trace_Timing ("Solver", Start);
      end;

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
      Solve_Options : Solve_Options_Type := Default_Options;
      Timeout       : Natural := 0) return Boolean
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
      Solve (Self, Callback'Access, Solve_Options, Timeout);
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
      Debug_String : String_Access := null) return Relation
   is
      Conv_Ptr : Converter_Access := null;
   begin
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

   ------------------------
   -- Create_N_Propagate --
   ------------------------

   function Create_N_Propagate
     (To           : Logic_Var;
      Comb         : Combiner_Type'Class;
      Logic_Vars   : Logic_Var_Array;
      Debug_String : String_Access := null) return Relation
   is
      Vars_Vec : Logic_Var_Vector := Logic_Var_Vectors.Empty_Vector;
   begin
      Vars_Vec.Concat (Logic_Var_Vectors.Elements_Array (Logic_Vars));
      return To_Relation
        (Atomic_Relation_Type'(Kind      => N_Propagate,
                               Comb_Vars => Vars_Vec,
                               Comb      => new Combiner_Type'Class'(Comb),
                               Target    => To),
         Debug_String => Debug_String);
   end Create_N_Propagate;

   ----------------------
   -- Create_Propagate --
   ----------------------

   function Create_Propagate
     (From, To     : Logic_Var;
      Conv         : Converter_Type'Class := No_Converter;
      Debug_String : String_Access := null) return Relation
   is
      Conv_Ptr : Converter_Access := null;
   begin
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

         when N_Propagate =>
            Self.Comb_Vars.Destroy;
            Destroy (Self.Comb.all);
            Free (Self.Comb);

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

      function Converted_Val (Val : Value_Type) return Value_Type
      is
        (if Atom.Conv /= null
         then Atom.Conv.Convert_Wrapper (Val)
         else Val);
      --  Assuming ``Atom`` is an Assign or Propagate atom, return ``Val``
      --  transformed by its converter.

      function Assign_Val (Val : Value_Type) return Boolean;
      --  Tries to assign ``Val`` to ``Atom.Target`` and return True either if
      --  ``Atom.Target`` already has a value compatible with ``Val``, or if
      --  it had no value and the assignment succeeded.
      --
      --  This assumes that ``Self`` is either an ``Assign`` or a `Propagate``
      --  relation.

      procedure Get_Values (Vars : Logic_Var_Vector; Vals : out Value_Array);
      --  Assign to ``Vals`` the value of the variables in ``Vars``.
      --
      --  This assumes that ``Vars`` and ``Vals`` have the same bounds. Note
      --  that we could turn this into a function that returns the array, but
      --  this would require secondary stack support and its overhead, whereas
      --  this is performance critical code.

      ----------------
      -- Assign_Val --
      ----------------

      function Assign_Val (Val : Value_Type) return Boolean is
      begin
         if Is_Defined (Atom.Target) then
            return Val = Get_Value (Atom.Target);
         else
            Set_Value (Atom.Target, Val);
            return True;
         end if;
      end Assign_Val;

      ----------------
      -- Get_Values --
      ----------------

      procedure Get_Values (Vars : Logic_Var_Vector; Vals : out Value_Array) is
      begin
         for I in Vals'Range loop
            Vals (I) := Get_Value (Vars.Get (I));
         end loop;
      end Get_Values;

      Ret : Boolean;
   begin
      case Atom.Kind is
         when Assign =>
            Ret := Assign_Val (Converted_Val (Atom.Val));

         when Propagate =>
            pragma Assert (Is_Defined (Atom.From));
            Ret := Assign_Val (Converted_Val (Get_Value (Atom.From)));

         when N_Propagate =>
            declare
               Vals : Value_Array (1 .. Atom.Comb_Vars.Length);
            begin
               Get_Values (Atom.Comb_Vars, Vals);
               Ret := Assign_Val (Atom.Comb.Combine_Wrapper (Vals));
            end;

         when Predicate =>
            pragma Assert (Is_Defined (Atom.Target));
            Ret := Atom.Pred.Call_Wrapper (Get_Value (Atom.Target));

         when N_Predicate =>
            declare
               Vals : Value_Array (1 .. Atom.Vars.Length);
            begin
               Get_Values (Atom.Vars, Vals);
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

      function Var_Args_Image (Vars : Logic_Var_Vector) return String;

      --------------------
      -- Var_Args_Image --
      --------------------

      function Var_Args_Image (Vars : Logic_Var_Vector) return String is
         Vars_Image : XString_Array (1 .. Vars.Length);
      begin
         for I in Vars_Image'Range loop
            Vars_Image (I) := To_XString (Image (Vars.Get (I)));
         end loop;
         return "(" & To_XString (", ").Join (Vars_Image).To_String & ")";
      end Var_Args_Image;

   begin
      case Self.Kind is
         when Propagate =>
            return Prop_Image (Image (Self.Target), Image (Self.From));

         when Assign =>
            return Prop_Image
              (Image (Self.Target), Logic_Vars.Value_Image (Self.Val));

         when N_Propagate =>
            return Image (Self.Target) & " <- " & Self.Comb.Image
                   & Var_Args_Image (Self.Comb_Vars);

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
            begin
               return
                 (if Full_Img /= ""
                  then Full_Img
                  else Self.N_Pred.Image & "?" & Var_Args_Image (Self.Vars));
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
      if Self = null then
         return "None";
      end if;
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

end Langkit_Support.Adalog.Solver;
