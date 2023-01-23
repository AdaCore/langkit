--
--  Copyright (C) 2019-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Assertions; use Ada.Assertions;
with Ada.Calendar;   use Ada.Calendar;

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;

with GNATCOLL.Strings; use GNATCOLL.Strings;

with Langkit_Support.Images;

with AdaSAT.Builders;
with AdaSAT.DPLL;
with AdaSAT.Formulas;
with AdaSAT.Theory;

--  This package implements a solver for Adalog equations. Recall that An
--  Adalog equation looks like:
--
--  .. code::
--
--      All:
--       - x <- 1
--       - Any:
--          - y <- 1
--          - All:
--             - foo?(x)
--             - y <- 2
--       - bar?(y)
--
--  So we have conjunctions, disjunctions, binds, predicates, etc. The goal
--  is to find an assignment for each variable so that the equation is
--  satisfied. One inuitive way to do that is by expanding the equation into
--  a disjunctive-normal form, so that each "possibility" can be tested
--  independently:
--
--  .. code::
--
--      Any:
--       - All:
--          - x <- 1
--          - y <- 1
--          - bar?(y)
--       - All:
--          - x <- 1
--          - foo?(x)
--          - y <- 2
--          - bar?(y)
--
--  However, if we have n Anys one after the other, with each one having 2
--  branches, we end up with 2^n options to evaluate. Fortunately, various
--  optimizations can be applied to prune the search space. For example, if we
--  notice while expanding each possibility that the currently built sub-
--  equation cannot be satisfied, we can abort early and avoid expanding the
--  rest of the equation. This is how the previous implementation worked, and
--  it prevented exponential blowup in most cases. However, this optimization
--  is obviously order-dependent: if the failing sub-equation appears later in
--  the expansion, the amount of branches of the search space that we can prune
--  will be reduced. For this reason, Libadalang (the main user of this
--  framework) was still facing a few resolution timeouts in various Ada
--  codebases.
--
--  So, this Adalog solver works by first encoding Adalog equations into SAT
--  problems. This encoding is obviously not equisat, meaning a solution to the
--  SAT problem is not necessarily a solution to the Adalog problem. That's why
--  we need to instantiate a DPLL-T solver with our own Adalog theory (of which
--  the ``Check`` subprogram defined below is the main component) in order to
--  (in-)validate the models produced by the SAT solver.
--
--  To encode a ``Compound_Relation`` we first introduce the notion of
--  "basic block". A basic block is a sequence of ``Atomic_Relation``s that
--  always come together. For example in the Adalog relation given above, there
--  are 3 basic blocks:
--
--  1. ``[x <- 1, bar?(y)]`` representing the top-level ``All`` relation.
--  2. ``[y <- 1`]``         representing the first branch of the ``Any``.
--  3. ``[foo?(x), y <- 2]`` representing the second branch of the ``Any``.
--
--  We encode the presence or absence of a basic block in the SAT problem
--  using a boolean variable. So if the SAT solver produces a model, say
--  ``[True, False, True]``, the theory will try to check if the concatenation
--  of all atomic relations from the basic blocks which are flagged "present"
--  is a valid solution. In this case, the concatenation produces
--  ``[`x <- 1`, `bar?(y)`, `foo?(x)`, `y <- 2`]``.
--
--  Ideally, we would like the solver to not produce nonsensical models such as
--  ``[True, True, True]`` (i.e. where both branches of the Any are present).
--
--  To see how that's done, let's first call the basic block variables b_1, b_2
--  and b_3 for our example. We now need to encode the fact that b_2 and b_3
--  cannot be true at the same time! So, we simply generate the constraint
--  ``!b_2 | !b_3``.
--
--  However, with this sole constraint, the solver may produce the model
--  ``[True, False, False]``, where none of the branches are taken! So, we must
--  also add a constraint that at least one of them is chosen as soon as the
--  parent relation of the ``Any`` is present. Thus we add the constraint
--  ``b_1 => (b_2 | b_3)``, or as its CNF equivalent, ``!b_1 | b_2 | b_3``.
--
--  We now have another problem, nothing prevents the solver from producing
--  `[False, True, False]`, where one of the branches is taken even though
--  the parent relation is absent. Indeed, the constraint above is vacuously
--  True if b_1 is False. So, we must also include the constraints
--  ``!b_1 => (!b_2 & !b_3)``, or in CNF, ``(b_1 | !b_2) & (b_1 | !b_3)``.
--
--  In the end we also explicitly set the top-level basic block to True,
--  otherwise "all variables set to False" would be a valid solution to the SAT
--  formula.
--
--  So after all this, running the DPLL-T solver on our example above will
--  first call back the theory with model ``[True, True, False]``, and if the
--  theory rejects it, call it back with ``[True, False, True]``, correctly
--  testing the two branches of the Any.
--
--  However at this stage this approach might look strictly inferior to a
--  simple recursive descent approach, since we will also end up checking all
--  possible combinations of branches from the original problem, but with the
--  overhead of a SAT solver.
--
--  In fact, the power of this approach only shows once we start generating
--  contradictions for the SAT-produced models. Consider the following
--  relation:
--
--  .. code::
--
--      All:
--       - x <- 1
--       - Any:
--          - y <- 1
--          - y <- 2
--          - ...
--          - y <- 1000
--       - Any:
--          - (y == 1000)?
--          - x <- 2
--
--
--  A recursive descent approach would explore the solution space by recursing
--  on the branches of the ``Any`` relations, populating its current model with
--  the atoms of each basic block it traverses. In this case, there are 2000
--  combinations to try in total. The recursive solver will attempt each one of
--  them and fail on all paths that include `x <- 1` and `x <- 2` thus wasting
--  time on 1000 combinations.
--  That's because when encountering such a path for the first time, even
--  though we can easily extract the fact that these two atoms are in
--  contradiction, the solver has no way to use the information to adapt it's
--  subsequent traversals (*).
--  The SAT solver on the other hand can be updated with a simple clause that
--  excludes the basic blocks of these atoms from being set at the same time
--  (for example ``!b_1 | !b_1003``). Internally, a watch is placed on b_1 and
--  b_1003 so that as soon as one variable becomes True, the other is set to
--  False. The search space is therefore cut in half at virtually no cost.
--
--  Imagine that the first branch of the last ``Any`` is now `x <- 3`. For us
--  it's easy to see that there is no solution anymore, because any path will
--  pass through `x <- 1` and either `x <- 2` or `x <- 3`. The recursive solver
--  will therefore waste time trying 2000 combinations.
--  The SAT solver will learn the clause ``!b_1 | !b_1002`` on its first try,
--  and ``!b_1 | !b_1003`` on its second try.
--  After that, if b_1 is set to true, then that first clause implies that
--  b_1002 is False, and the second that b_1003 is False. Now remember that in
--  a previous paragraph we introduced the constraint that if the parent block
--  of an Any is set, then one of its branches must be set.
--  In our case, this translates to the clause ``b_1 => (b_1002 | b_1003)``, or
--  ``!b_1 | b_1002 | b1003``. But it's easy to see now that this clause cannot
--  be satisfied, because we have b_1 is True, b_1002 is False and b_1003 is
--  False. Therefore b_1 must be False and that's exactly what the SAT solver
--  learns internally, thus not wasting any more time on this sub-tree anymore.
--
--  We therefore reached the same conclusion with 2 attempts instead of 2000.
--  Besides, note that if this relation is actually part of a bigger relation,
--  the recursive solver would possibly need to make those 2000 traversals
--  several times, whereas the SAT solver has internally learned the fact that
--  b_1 cannot be true and will never take any path that goes through this
--  sub-tree.
--
--  (*) Actually an attempt was made (but never committed to the project) to
--  adapt the traversal order by using those same contradictions we are feeding
--  the SAT solver with. For example, after having seen that the two atoms
--  `x <- 1` and `x <- 2` are incompatible in the example above, it would have
--  tried to swap the order of the two ``Any``s so that the path going through
--  `x <- 1` and `x <- 2` is cut early during the traversal. Unfortunately,
--  these reordering were based on heuristics and made the whole thing pretty
--  fragile, improving runtime in some cases but degrading it in others.

package body Langkit_Support.Adalog.Solver is
   ----------------------
   -- Supporting types --
   ----------------------

   package Atomic_Relation_Vectors is new Langkit_Support.Vectors
     (Atomic_Relation);
   subtype Atomic_Relation_Vector is Atomic_Relation_Vectors.Vector;
   --  Vectors of atomic relations

   --------------------------
   -- Supporting functions --
   --------------------------

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

   type Atom_Mapping is array (Positive range <>)
     of AdaSAT.Variable_Or_Null;
   --  Array type used to map basic block ids to AdaSAT formula variables

   type Atom_Mapping_Access is access Atom_Mapping;

   procedure Free is new Ada.Unchecked_Deallocation
     (Atom_Mapping, Atom_Mapping_Access);

   package BB_Vectors is new Langkit_Support.Vectors
     (Atomic_Relation_Vector);

   type Unified_Vars is record
      First  : Positive;
      Second : Positive;
   end record;
   --  Used to store IDs of pairs of unified variables.
   --  See ``Explain_Contradiction``.

   package Unified_Vars_Vectors is new Langkit_Support.Vectors
     (Unified_Vars);

   type Sort_Context is record
      Defining_Atoms : Positive_Vector_Array_Access;
      --  For each logic variable, list of atoms indexes for atoms that define
      --  this variable.

      Has_Contradiction_Counter : Natural;
      --  Number of times ``Has_Contradiction`` was called. Used for
      --  logging/debugging purposes.

      Unset_Vars : Logic_Var_Vector;
      --  After a call to ``Topo_Sort``, holds the variables which were used
      --  but never defined. Used to build contradictions for the solver.
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
     (Self : Relation; Max_Id : out Natural) return Prepared_Relation;
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

   type Index_Set is array (Positive range <>) of Boolean;

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

      Remaining_Time : Natural;
      --  Number of times left we allow ourselves to evaluate an atom before
      --  aborting the solver. If 0, no timeout applies.

      Sort_Ctx : Sort_Context;
      --  Context used for the topological sort, when reaching a complete
      --  potential solution. Stored once in the context to save ourselves
      --  from reallocating data structures everytime.

      Tried_Solutions : Natural;
      --  Number of tried solutions. Stored for analytics purpose, and
      --  potentially for timeout.

      Max_Id : Natural;
      --  The highest Id that a relation is assigned. This allows allocating
      --  arrays with indices ranging over all relations, in particular
      --  the Atom_Map below.

      Atom_Map : Atom_Mapping_Access;
      --  Maps each relation (uniquely determined by its Id) to the SAT
      --  variable representing the basic block in which it belongs.

      Blocks : BB_Vectors.Vector;
      --  Holds the list of all relations that compose each basic block
   end record;
   --  Context for the solving of a compound relation

   function Create (Vars : Logic_Var_Array) return Sort_Context;
   --  Create a new sorting context. Use ``Destroy`` to free allocated
   --  resources.

   procedure Destroy (Sort_Ctx : in out Sort_Context);
   --  Free resources for the sorting context

   function Create
     (Cb     : Callback_Type;
      Vars   : Logic_Var_Array_Access;
      Max_Id : Natural) return Solving_Context;
   --  Create a new instance of a solving context. The data will be cleaned up
   --  and deallocated by a call to ``Destroy``.

   procedure Destroy (Ctx : in out Solving_Context);
   --  Destroy a solving context, and associated data

   type Unification_Graph is array (Positive range <>)
      of Atomic_Relation_Vector;
   --  Array type used to map a variable (via its Id) to a list of ``Unify``
   --  atoms which it is referenced from. This represents the edges of the
   --  unification graph (see ``Compute_Unification_Graph``).

   type Unification_Graph_Access is access Unification_Graph;

   procedure Free is new Ada.Unchecked_Deallocation
     (Unification_Graph, Unification_Graph_Access);

   function Uses_Var
     (Self : Atomic_Relation_Type; V : Logic_Var) return Boolean;
   --  Return whether the given variable is in a "use" position inside this
   --  atom. For example in ``x <- foo(y)``, ``y`` is "used" whereas ``x`` is
   --  "defined".

   procedure Decrease_Remaining_Time
     (Ctx    : in out Solving_Context;
      Amount : Natural);
   --  If the timeout mechanism is enabled, decrease ``Ctx.Remaining_Time`` by
   --  the given amount. If that operation makes it go below 0, raise a
   --  ``Timeout_Error`` instead.

   function Compute_Unification_Graph
     (Ctx : Solving_Context) return Unification_Graph_Access;
   --  Allocate and populate the array representing the edges of the
   --  unification graph according to the "Unifies" atoms held in the given
   --  context. If ``Ctx.Unifies`` is:
   --
   --  - ``a <-> b``
   --  - ``c <-> d``
   --  - ``d <-> b``.
   --
   --  Then the resulting array will be:
   --
   --  - ``a -> [b]``
   --  - ``b -> [a, d]``
   --  - ``c -> [d]``.
   --
   --  This is used by the ``Mark_Unifying_Path`` algorithm inside the
   --  ``Explain_Contradiction`` subprogram to find out how two given variables
   --  became unified with each other, i.e. what is the set of Unify relations
   --  that make them unified.

   procedure Destroy_Unification_Graph
     (Graph : in out Unification_Graph_Access);
   --  Deallocate the unification graph

   function Evaluate_Atoms
     (Ctx          : in out Solving_Context;
      Sorted_Atoms : Atomic_Relation_Vectors.Elements_Array;
      Explanation  : in out AdaSAT.Builders.Formula_Builder) return Boolean;
   --  Evaluate the given sequence of sorted atoms (see ``Topo_Sort``) and
   --  return whether they are all satisfied: if they are, the logic variables
   --  are assigned values, so it is possible to invoke the user callback for
   --  solutions. If not, the ``Explanation`` formula builder is populated with
   --  one or several clauses explaining the failure.

   function Explain_Contradiction
     (Ctx          : Solving_Context;
      Sorted_Atoms : Atomic_Relation_Vectors.Elements_Array;
      Failed_Atom  : Atomic_Relation;
      Unify_Graph  : in out Unification_Graph_Access;
      Invalid_Vars : in out Index_Set) return AdaSAT.Clause;
   --  Given a sequence of sorted atoms (see ``Topo_Sort``) and the atom on
   --  which evaluation failed (see ``Evaluate_Atoms``), compute the smallest
   --  set of atoms among that sequence that explains why evaluation failed and
   --  return it as a new clause for the SAT solver to learn in order not to
   --  propose this solution again. As an example, consider the following call
   --  parameters:
   --
   --  .. code::
   --
   --      Sorted_Atoms:
   --       1:   x <- 1
   --       2:   y <- 2
   --       3:   is_even(x)
   --
   --      Failed_Atom: is_even(x)
   --
   --
   --  By analyzing the sorted atoms we can see the reason for which
   --  ``is_even(x)`` (atom 3) failed is due to ``x <- 1`` (atom 1), therefore
   --  we can rather teach it ``!1 | !3``. If we had instead teached it the
   --  simple but naive clause ``!1 | !2 | !3`` it might guess that atom 2 was
   --  the problem and call back the theory with other candidate solutions
   --  containing atoms 1 and 3.
   --
   --  Note that we must also include all relevant Unify clauses. For example,
   --  if we instead had this list of atoms:
   --
   --  .. code::
   --
   --      1:   a <- 1
   --      2:   y <- 2
   --      3:   is_even(x)
   --      4:   a <-> b
   --      5:   b <-> x
   --
   --
   --  The resulting explanation should contain atom 1 and 3 as before, but
   --  also 4 and 5, which are needed to explain why ``x`` is assigned ``1``.
   --  There are cases where there are multiple ways to explain why a given
   --  variable is assigned a given value. For example if we add the atom
   --  ``a <-> x`` in the list above, we can now explain the assignment to
   --  ``x`` via that new atom or via the unification atoms 5 and 6. In those
   --  cases, ``Explain_Contradiction`` will not try to generate all the
   --  possible explanations but only one of them. This is generally enough to
   --  get a completely different solution attempt in the next iteration, and
   --  if it's not, we will simply end up contradicting each way of explaining
   --  the assignment until we can't anymore, which is bound to happen because
   --  the number of possibilities is finite.
   --
   --  In order to compute these unification paths, we first compute an
   --  unification graph (see  ``Compute_Unification_Graph``). This graph has
   --  an edge between two variables iff there exists an Unify atom that links
   --  them. Once we have this graph, finding a path between two variables
   --  is only a matter of executing a search algorithm (the current
   --  implementation uses a DFS in ``Mark_Unifying_Path``).
   --
   --  For performance considerations we do not compute this graph before it's
   --  necessary. As such, we take it as an initially null "in out" reference.
   --  ``Explain_Contradiction`` takes care of computing it once it's needed,
   --  and subsequent calls will be able to reuse it if needed again.
   --
   --  The last parameter we have not talked about yet is ``Invalid_Vars``.
   --  Since ``Explain_Contradiction`` can be called multiple times in a single
   --  round (i.e. for one given SAT model) to explain several atom failures,
   --  we want to avoid recomputing the same explanations as much as possible
   --  as we would not only waste time here but also possibly bloat the SAT
   --  solver with useless redundant clauses.
   --
   --  So, this array is used accross multiple calls and maintains the set of
   --  logic variables that are part (directly or indirectly) of an
   --  explanation of why evaluation of an atom failed. After our last example,
   --  this array would contain "a", "b" and "x". This array is then used
   --  to avoid computing the explanation for a second failed atom if that
   --  atoms uses a variable that has already been part of a failure, since
   --  the original explanation will probably change the outcome for that
   --  second failure as well. Although that last part is not guaranteed, the
   --  gains achieved in practice by avoiding cases where it does largely
   --  compensate the cases where it does not.

   procedure Explain_Topo_Sort_Failure
     (Ctx          : in out Solving_Context;
      Model        : AdaSAT.Model;
      Explanation  : in out AdaSAT.Builders.Formula_Builder);
   --  Populate ``Explanation`` by generating clauses that prevent
   --  subsequent models from containing the variables that were found unset
   --  during the topological sort (see ``Ctx.Unset_Var``).

   function Check
     (Ctx            : in out Solving_Context;
      Model          : AdaSAT.Model;
      Contradictions : in out AdaSAT.Formulas.Formula) return Boolean;
   --  Check whether the model produced by the SAT solver validates our theory.
   --  If it does not, populate ``Contradictions`` with clauses that invalidate
   --  the model and explain in the simplest way possible (with fewest clauses
   --  containing fewest atoms possible) why the model is not valid. The SAT
   --  solver will then take these new clauses into account and produce another
   --  model for the theory to validate, etc.

   function Encode_Relation
     (Self           : Compound_Relation;
      Ctx            : in out Solving_Context;
      Variable_Count : out AdaSAT.Variable_Or_Null)
      return AdaSAT.Formulas.Formula;
   --  Encode the given Adalog relation into a SAT problem. See top-level unit
   --  for a complete description. ``Variable_Count`` will be set to the number
   --  of variables that must be allocated in a model.

   function Solve_DPLL
     (Self : Compound_Relation; Ctx : in out Solving_Context) return Boolean;
   --  Solve the given Adalog relation by encoding it as a SAT problem,
   --  feeding it to the SAT solver and iteratively invalidating the produced
   --  model using the ``Check`` subprogram until we find a valid model or
   --  the SAT solver cannot produce any new model.

   package Adalog_Theory is new AdaSAT.Theory (Solving_Context, Check);
   package DPLL_Adalog is new AdaSAT.DPLL (Adalog_Theory);

   procedure Trace_Timing (Label : String; Start : Time);
   --  Log ``Start .. Clock`` as the time it took to run ``Label``

   ------------
   -- Create --
   ------------

   function Create (Vars : Logic_Var_Array) return Sort_Context is
   begin
      return
        (Defining_Atoms            => new Positive_Vector_Array'
           (Vars'Range => Positive_Vectors.Empty_Vector),
         Has_Contradiction_Counter => 0,
         Unset_Vars                => Logic_Var_Vectors.Empty_Vector);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Cb     : Callback_Type;
      Vars   : Logic_Var_Array_Access;
      Max_Id : Natural) return Solving_Context is
   begin
      return Ret : Solving_Context do
         Ret.Cb := Cb;
         Ret.Vars := Vars;
         Ret.Atoms := Atomic_Relation_Vectors.Empty_Vector;
         Ret.Unifies := Atomic_Relation_Vectors.Empty_Vector;
         Ret.Sort_Ctx := Create (Vars.all);
         Ret.Tried_Solutions := 0;
         Ret.Max_Id := Max_Id;
         Ret.Atom_Map := new Atom_Mapping (1 .. Max_Id);
      end return;
   end Create;

   ----------------------
   -- Prepare_Relation --
   ----------------------

   function Prepare_Relation
     (Self : Relation; Max_Id : out Natural) return Prepared_Relation
   is
      --  For determinism, collect variables in the order in which they appear
      --  in the equation.
      Vec : Logic_Var_Vectors.Vector;

      Next_Id : Positive := 1;
      --  Id to assign to the next processed relation

      procedure Add (Var : Logic_Var);
      --  Add ``Var`` to ``Vec``/``Set``

      procedure Process_Atom (Self : Atomic_Relation_Type);
      --  Collect variables from ``Self``

      procedure Track_Vars (Self : Relation);
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

      ----------------
      -- Track_Vars --
      ----------------

      procedure Track_Vars (Self : Relation) is
      begin
         --  For atomic relations, just add the vars it contains. For compound
         --  relations, just recurse over sub-relations.

         case Self.Kind is
         when Atomic =>
            Process_Atom (Self.Atomic_Rel);
            Self.Id := Next_Id;
            Next_Id := Next_Id + 1;

         when Compound =>
            for R of Self.Compound_Rel.Rels loop
               Track_Vars (R);
            end loop;
         end case;
      end Track_Vars;

      --  Fold True/False atoms in the input relation and add all variables to
      --  ``Vars`` in the same pass.

      Result : Prepared_Relation;
   begin
      Track_Vars (Self);

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

      Result.Rel := Self;
      Max_Id := Next_Id - 1;
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

      Seen_Unset_Vars : Index_Set := (Vars'Range => False);
      --  Mask used to not include multiple times the same unset variable
      --  in the ``Sort_Ctx.Unset_Vars`` vector.

      function Append_Definition (Var : Logic_Var) return Boolean;
      --  Try to append an atom that defines ``Var`` instead. This returns
      --  False if there is no atom that defines ``Var`` or if dependency
      --  cycles prevent us from appending a sequence of atoms to achieve
      --  that. Otherwise (on success), return True.

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
         Var_Id : constant Natural := Id (Var);
      begin
         for Definition of Defining_Atoms (Var_Id) loop
            if Append (Definition) then
               return True;
            end if;
         end loop;

         --  ``Var`` is used but does not have any definition, so add it
         --  to the ``Unset_Vars`` vector if not already present.
         if not Seen_Unset_Vars (Var_Id) then
            Seen_Unset_Vars (Var_Id) := True;
            Sort_Ctx.Unset_Vars.Append (Var);
         end if;
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
      Sort_Ctx.Unset_Vars.Clear;
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
      Destroy (Ctx.Sort_Ctx);

      --  Cleanup logic vars for future solver runs using them. Note that no
      --  aliasing information is supposed to be left at this stage.

      for V of Ctx.Vars.all loop
         Reset (V);
         Set_Id (V, 0);
      end loop;

      Free (Ctx.Vars);
      Free (Ctx.Atom_Map);

      for I in 1 .. Ctx.Blocks.Length loop
         Ctx.Blocks.Get_Access (I).Destroy;
      end loop;

      Ctx.Blocks.Destroy;
      Ctx.Sort_Ctx.Unset_Vars.Destroy;
   end Destroy;

   -----------------------------
   -- Decrease_Remaining_Time --
   -----------------------------

   procedure Decrease_Remaining_Time
     (Ctx    : in out Solving_Context;
      Amount : Natural)
   is
   begin
      if Ctx.Remaining_Time > 0 then
         if Amount > Ctx.Remaining_Time then
            raise Timeout_Error;
         end if;
         Ctx.Remaining_Time := Ctx.Remaining_Time - Amount;
      end if;
   end Decrease_Remaining_Time;

   -------------------------------
   -- Compute_Unification_Graph --
   -------------------------------

   function Compute_Unification_Graph
     (Ctx : Solving_Context) return Unification_Graph_Access
   is
      Graph : constant Unification_Graph_Access :=
         new Unification_Graph (Ctx.Vars'Range);
   begin
      for U of Ctx.Unifies loop
         declare
            A : constant Logic_Var := U.Atomic_Rel.Target;
            B : constant Logic_Var := U.Atomic_Rel.Unify_From;
         begin
            Graph.all (A.Id).Append (U);
            Graph.all (B.Id).Append (U);
         end;
      end loop;
      return Graph;
   end Compute_Unification_Graph;

   -------------------------------
   -- Destroy_Unification_Graph --
   -------------------------------

   procedure Destroy_Unification_Graph
     (Graph : in out Unification_Graph_Access) is
   begin
      for E of Graph.all loop
         E.Destroy;
      end loop;
      Free (Graph);
   end Destroy_Unification_Graph;

   ---------------------------
   -- Explain_Contradiction --
   ---------------------------

   function Explain_Contradiction
     (Ctx          : Solving_Context;
      Sorted_Atoms : Atomic_Relation_Vectors.Elements_Array;
      Failed_Atom  : Atomic_Relation;
      Unify_Graph  : in out Unification_Graph_Access;
      Invalid_Vars : in out Index_Set) return AdaSAT.Clause
   is
      use AdaSAT;

      Reason   : AdaSAT.Builders.Clause_Builder;
      --  The final resulting clause that will be fed back to the SAT solver
      --  to contradict the current proposed model.

      Conflict : array (1 .. Variable (Ctx.Blocks.Length)) of Boolean :=
        (1 .. Variable (Ctx.Blocks.Length) => False);
      --  Mask used to not include the same atom multiple times in the
      --  resulting clause.

      Unified  : Unified_Vars_Vectors.Vector;
      --  Contains pairs of variables for which we have already emitted
      --  unifying paths in the resulting clause (see ``Mark_Unifying_Path``).
      --  Used by  ``Mark_Assignment`` to avoid duplicating work.

      To_Visit : Logic_Var_Vector;
      --  Vector that is holds the stack of variables to visit in the
      --  DFS implementation of ``Mark_Unifying_Path``. This lies here
      --  instead of directly inside that function so as to not waste cycles
      --  allocating and freeing its internal array in case we need to call
      --  ``Mark_Unifying_Path`` multiple times.

      procedure Add_Conflict (Atom : Atomic_Relation);
      --  Include the given atom in the explanation, if not already present

      procedure Mark_Assignment (Target : Logic_Var);
      --  Find the atom that assigns a value to the given variable and add
      --  it to the resulting explanation.

      procedure Mark_Unifying_Path (From, To : Logic_Var);
      --  Find all the atoms that are used to unify variables ``From`` and
      --  ``To`` and add them to the resulting explanation. There might be
      --  multiple possibilities to unify those two, so this computes one
      --  of the possible paths arbitrarily. For example consider:
      --
      --  .. code::
      --
      --      All:
      --         a  <-> b1
      --         a  <-> b2
      --         b1 <-> c
      --         b2 <-> d
      --         c  <-> d
      --
      --
      --  There are two ways to explain why ``a`` and ``d`` are unified:
      --
      --  - ``a <-> b1, b1 <-> c, c <-> d``
      --  - ``a <-> b2, b2 <-> d``.
      --
      --  The algorithm implemented here is a simple DFS and so will return
      --  the first one. It theory it would be better to return multiple
      --  clauses for each possible paths, but this would severely complexify
      --  the implementation and this is not a problem in practice: it only
      --  means that we might need multiple round-trips between the theory
      --  and the SAT solver to explain a given failure.

      ------------------
      -- Add_Conflict --
      ------------------

      procedure Add_Conflict (Atom : Atomic_Relation) is
         Index : constant Variable := Ctx.Atom_Map (Atom.Id);
      begin
         if Conflict (Index) then
            return;
         end if;
         Conflict (Index) := True;
         if Index /= 1 then
            --  Micro-optimization: ``-1`` is always False so no need to add
            --  it to the resulting clause.
            Reason.Add (-Index);
         end if;
         if Solv_Trace.Is_Active then
            Solv_Trace.Trace (Image (Atom));
         end if;
      end Add_Conflict;

      ---------------------
      -- Mark_Assignment --
      ---------------------

      procedure Mark_Assignment (Target : Logic_Var) is
         Target_Id : constant Natural := Id (Target);
         Dummy     : Boolean;

         function Check_Assignment (Atom : Atomic_Relation) return Boolean;
         --  Check that the given atom actually assigns a value to the given
         --  target variable. If it's the case, add it to the resulting clause
         --  and return True, otherwise return False.

         ----------------------
         -- Check_Assignment --
         ----------------------

         function Check_Assignment (Atom : Atomic_Relation) return Boolean is
         begin
            if Id (Atom.Atomic_Rel.Target) = Target_Id then
               Add_Conflict (Atom);
               if Atom.Atomic_Rel.Target.Id /= Target.Id then
                  --  This atom does not directly assign ``Target` a value,
                  --  but does so because ``Target`` and its own target are
                  --  unified. So, we must also include in the explanation the
                  --  set of atoms that make the two variables unified.
                  --  First, check if we have not already included in the
                  --  explanation the reason why these two are unified.
                  for U of Unified loop
                     if (U.First = Target.Id and then
                         U.Second = Atom.Atomic_Rel.Target.Id) or else
                        (U.First = Atom.Atomic_Rel.Target.Id and then
                         U.Second = Target.Id)
                     then
                        return True;
                     end if;
                  end loop;

                  --  We haven't included the reason in the explanation yet,
                  --  so do it now.
                  Unified.Append ((Target.Id, Atom.Atomic_Rel.Target.Id));
                  Mark_Unifying_Path
                    (Atom.Atomic_Rel.Target, Target);
               end if;
               return True;
            end if;
            return False;
         end Check_Assignment;
      begin
         Invalid_Vars (Target_Id) := True;

         --  Traverse the atoms in order to find the one that gave a value
         --  to ``Target``. For the ``Propagate`` or ``N_Propagate`` kinds,
         --  we recursively find the assignment of the "arguments". The
         --  rationale is that failure might not necessarily come from the
         --  propagate atom itself but the value it propagates from.
         for Atom of Sorted_Atoms loop
            case Atom.Atomic_Rel.Kind is
               when Assign =>
                  exit when Check_Assignment (Atom);

               when Propagate =>
                  if Check_Assignment (Atom) then
                     Mark_Assignment (Atom.Atomic_Rel.From);
                     exit;
                  end if;

               when N_Propagate =>
                  if Check_Assignment (Atom) then
                     for Var of Atom.Atomic_Rel.Comb_Vars loop
                        Mark_Assignment (Var);
                     end loop;
                     exit;
                  end if;

               when others =>
                  null;
            end case;
         end loop;
      end Mark_Assignment;

      ------------------------
      -- Mark_Unifying_Path --
      ------------------------

      procedure Mark_Unifying_Path (From, To : Logic_Var) is
         Antecedents : array (Ctx.Vars.all'Range) of Atomic_Relation :=
           (others => null);
         --  For each variable that is unified with ``From`` (and therefore
         --  ``To`` as well), this stores the relation that was used to
         --  propagate the unification.

         procedure Retrace_Path;
         --  Builds the final path using the ``Antecedents`` array. So this
         --  recursively looks up the antecedent of each variable starting from
         --  ``To``, adding all the ``Unify`` relations along the way into
         --  the resulting clause.

         procedure Register_Visit
           (Var : Logic_Var; Origin : Atomic_Relation);
         --  Sets the antecedent of ``Var`` to be ``Origin`` and add it to
         --  the list of variables to visit, unless it was already processed.

         ------------------
         -- Retrace_Path --
         ------------------

         procedure Retrace_Path is
            V : Positive := To.Id;
         begin
            while V /= From.Id loop
               declare
                  R : constant Atomic_Relation := Antecedents (V);
               begin
                  Add_Conflict (R);
                  --  Variable ``V`` is on the path to the unification of
                  --  ``From`` and ``To``, and ``R`` is the Unify atom that
                  --  unifies it with another variable on the way. So mark
                  --  ``R`` as part of the explanation (it is necessary to
                  --  explain unification of ``From`` and ``To``) and continue
                  --  the traversal with the other variable, until we reach
                  --  ``From``.
                  if R.Atomic_Rel.Target.Id = V then
                     V := R.Atomic_Rel.Unify_From.Id;
                  else
                     V := R.Atomic_Rel.Target.Id;
                  end if;
               end;
            end loop;
         end Retrace_Path;

         --------------------
         -- Register_Visit --
         --------------------

         procedure Register_Visit
           (Var : Logic_Var; Origin : Atomic_Relation)
         is
         begin
            if Antecedents (Var.Id) = null then
               Antecedents (Var.Id) := Origin;
               To_Visit.Append (Var);
            end if;
         end Register_Visit;
      begin
         --  If not already done, construct the unification graph
         if Unify_Graph = null then
            Unify_Graph := Compute_Unification_Graph (Ctx);
         end if;

         --  Now implement a simple DFS traversal: traverse the unification
         --  graph computed above and starting from ``From`` until we stumble
         --  upon the ``To`` variable. At this point, we can retrace the path
         --  unifying ``From`` and ``To`` using the ``Antecedents`` array.

         To_Visit.Append (From);
         while not To_Visit.Is_Empty loop
            declare
               V : constant Logic_Var := To_Visit.Pop;
            begin
               exit when V = To;
               for U of Unify_Graph.all (V.Id) loop
                  declare
                     A  : constant Logic_Var := U.Atomic_Rel.Target;
                     B  : constant Logic_Var := U.Atomic_Rel.Unify_From;
                  begin
                     if V = A then
                        Register_Visit (B, U);
                     elsif V = B  then
                        Register_Visit (A, U);
                     end if;
                  end;
               end loop;
            end;
         end loop;
         To_Visit.Clear;
         Retrace_Path;
      end Mark_Unifying_Path;
   begin
      if Solv_Trace.Is_Active then
         Solv_Trace.Trace ("Because of");
         Solv_Trace.Increase_Indent;
      end if;

      --  Find out why the given ``Failed_Atom`` failed depending on its kind
      case Failed_Atom.Atomic_Rel.Kind is
         when Assign =>
            --  An ``Assign``atom must have failed because there was already a
            --  previous assignment to the same variable. So, include that
            --  previous assignment in the resulting clause using the
            --  ``Mark_Assignment`` helper.
            Mark_Assignment (Failed_Atom.Atomic_Rel.Target);

         when Propagate =>
            --  An ``Propagate`` failed either because the target variable
            --  was already assigned an incompatible value, or because the
            --  variable we are propagating from doesn't have the right value.
            --  Mark the assignments of these two to account for both options.
            Mark_Assignment (Failed_Atom.Atomic_Rel.From);
            Mark_Assignment (Failed_Atom.Atomic_Rel.Target);

         when N_Propagate =>
            --  Same logic as for the ``Propagate`` case, but since we cannot
            --  know which variable is problematic we must conservatively mark
            --  all of them.
            for Var of Failed_Atom.Atomic_Rel.Comb_Vars loop
               Mark_Assignment (Var);
            end loop;
            Mark_Assignment (Failed_Atom.Atomic_Rel.Target);

         when Predicate =>
            --  A ``Predicate`` most probably failed because the variable
            --  did not hold the expected value. So, we must prevent the atoms
            --  that led to this assignment from appearing again in the model.
            Mark_Assignment (Failed_Atom.Atomic_Rel.Target);

         when N_Predicate =>
            --  Same logic as for the ``Predicate`` case, but since we cannot
            --  know which variable is problematic we must conservatively mark
            --  all of them.
            for Var of Failed_Atom.Atomic_Rel.Vars loop
               Mark_Assignment (Var);
            end loop;

         when others =>
            --  Other atom kinds cannot happen here
            raise Program_Error;
      end case;
      Add_Conflict (Failed_Atom);

      To_Visit.Destroy;
      Unified.Destroy;

      if Solv_Trace.Is_Active then
         Solv_Trace.Decrease_Indent;
      end if;

      return Reason.Build;
   end Explain_Contradiction;

   --------------------
   -- Evaluate_Atoms --
   --------------------

   function Evaluate_Atoms
     (Ctx          : in out Solving_Context;
      Sorted_Atoms : Atomic_Relation_Vectors.Elements_Array;
      Explanation  : in out AdaSAT.Builders.Formula_Builder) return Boolean
   is
      use AdaSAT;

      Max_Index    : Positive := 1;
      Unify_Graph  : Unification_Graph_Access := null;
      Success      : Boolean := True;
      Invalid_Vars : Index_Set := (Ctx.Vars'Range => False);
   begin
      --  If we have a timeout, apply it
      Decrease_Remaining_Time (Ctx, Sorted_Atoms'Length);

      --  Evaluate each individual atom. Note that we don't stop as soon as
      --  one failing atom has been found. Ideally, we want to find several
      --  independent contradictions in a single round to make the most out of
      --  each SAT model. However, by doing that, we must be careful not to
      --  do unnecessary computations (e.g. evaluating predicates although we
      --  know its basic block is already in a contradiction, etc.) and not
      --  derive the same explanation (or a subset of another explanation)
      --  multiple times, hence the important use of ``Invalid_Vars``,
      --  ``Explanation.Is_Feasible`` and ``Add_Simplify`` instead of ``Add``.
      for Atom of Sorted_Atoms loop
         if Atom.Atomic_Rel.Kind in Predicate and then
            (Invalid_Vars (Id (Atom.Atomic_Rel.Target))
             or else not Explanation.Is_Feasible (+Ctx.Atom_Map (Atom.Id)))
         then
            null;
         elsif
            Atom.Atomic_Rel.Kind in N_Predicate and then
            ((for some V of Atom.Atomic_Rel.Vars => Invalid_Vars (Id (V)))
             or else not Explanation.Is_Feasible (+Ctx.Atom_Map (Atom.Id)))
         then
            null;
         elsif
            Atom.Atomic_Rel.Kind in Propagate and then
            (Invalid_Vars (Id (Atom.Atomic_Rel.From))
             or else not Explanation.Is_Feasible (+Ctx.Atom_Map (Atom.Id)))
         then
            Invalid_Vars (Id (Atom.Atomic_Rel.Target)) := True;
         elsif
            Atom.Atomic_Rel.Kind in N_Propagate and then
            ((for some V of Atom.Atomic_Rel.Comb_Vars => Invalid_Vars (Id (V)))
             or else not Explanation.Is_Feasible (+Ctx.Atom_Map (Atom.Id)))
         then
            Invalid_Vars (Id (Atom.Atomic_Rel.Target)) := True;
         elsif not Solve_Atomic (Atom) then
            if Solv_Trace.Is_Active then
               Solv_Trace.Trace ("Failed on " & Image (Atom));
            end if;

            Success := False;
            Explanation.Add_Simplify (Explain_Contradiction
              (Ctx, Sorted_Atoms (1 .. Max_Index - 1), Atom,
               Unify_Graph, Invalid_Vars));
         end if;
         Max_Index := Max_Index + 1;
      end loop;

      if Unify_Graph /= null then
         Destroy_Unification_Graph (Unify_Graph);
      end if;

      return Success;
   end Evaluate_Atoms;

   --------------
   -- Uses_Var --
   --------------

   function Uses_Var
     (Self : Atomic_Relation_Type; V : Logic_Var) return Boolean
   is
      V_Id : constant Natural := Id (V);
   begin
      case Self.Kind is
         when Assign | True | False =>
            return False;
         when Predicate =>
            return Id (Self.Target) = V_Id;
         when Propagate =>
            return Id (Self.From) = V_Id;
         when N_Predicate =>
            return (for some W of Self.Vars => Id (W) = V_Id);
         when N_Propagate =>
            return (for some W of Self.Comb_Vars => Id (W) = V_Id);
         when Unify =>
            return Id (Self.Target) = V_Id or else Id (Self.Unify_From) = V_Id;
      end case;
   end Uses_Var;

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

   -------------------
   -- Image_With_Id --
   -------------------

   function Image_With_Id (V : Logic_Var) return String is
     (Image (V) & " (ID:" & Natural'Image (Id (V)) & ")");

   -------------------------------
   -- Explain_Topo_Sort_Failure --
   -------------------------------

   procedure Explain_Topo_Sort_Failure
     (Ctx          : in out Solving_Context;
      Model        : AdaSAT.Model;
      Explanation  : in out AdaSAT.Builders.Formula_Builder)
   is
      --  For a given variable that was used but unset, causing topological
      --  sort to fail, we want to construct a contradiction roughly saying
      --  if we want to use this variable, then we need to have at least
      --  one atom that defines it! So in the simplest case, we simply collect
      --  all the atoms of the current solution that use that variable, then
      --  we collect all the atoms *not* in the solution that define it, and
      --  we emit a clause to force that each "using" atom implies at least one
      --  "defining" atom. This is exactly what ``Contradict_Unset_Var`` does.
      --
      --  But now assume that that two variable ``x`` and ``y`` are found to
      --  be unset during toposort, and that the equations that involve them
      --  in the current solution are:
      --
      --  .. code::
      --
      --     bar?(y)
      --     x <- foo(y)
      --
      --  Here, we can see that indeed ``y`` is never set and thefore we can
      --  call ``Contradict_Unset_Var`` on it. However, ``x`` is unset only
      --  because ``y`` is unset: if ``y`` was set, the atom ``x <- foo(y)``
      --  would assign a value to ``x``. In such cases, it would be incorrect
      --  to call ``Contradict_Unset_Var`` on ``x``, because it would generate
      --  a clause that basically says that the atoms of the current solution
      --  cannot ever give a value to ``x``, which is wrong because in another
      --  context the atom ``x <- foo(y)`` could do it.
      --
      --  Instead, we can simply ignore those kinds of unset variables: it
      --  suffices to contradict the root cause of the problem, that is, the
      --  ultimate variable which really has no defining atom in the current
      --  solution: in the next round, that variable will be set and therefore
      --  all variables that depended on it will be set as well.
      --  So in order to detect and ignore those variables, we generate a
      --  dependency graph, which has an edge from ``x`` to ``y`` for each
      --  variable ``x`` that would be set if variable ``y`` was set. This
      --  graph is built through successive calls to ``Populate_Dependencies``
      --  on each variable found unset during toposort.
      --  Then, we populate the ``Atomic_Unset_Vars`` vector with actual unset
      --  variables by only adding those that have no edge in the dependency
      --  graph.
      --
      --  There is one last problem that we haven't mentionned yet: cyclic
      --  dependencies. Assume the following Adalog equation:
      --
      --  .. code::
      --
      --     x <- foo(y)
      --     y <- bar(z)
      --     z <- baz(x)
      --
      --  If we execute the algorithm as we have presented it so far on this
      --  example, we will not be able to generate an explanation, since there
      --  is no "root" variable: all of them have a dependency.
      --
      --  For such a cycle, we would like to emit a clause that says "any atom
      --  in the current solution that uses any of those variables needs at
      --  least one atom *not* in the current solution that defines any of
      --  those variable" (since defining any of those will automatically
      --  define the rest of them). The insight here is that if all variables
      --  of the cycle were unified, then we could generate this clause using
      --  ``Contradict_Unset_Var`` on a single one of those variable directly,
      --  because then, fetching atoms that use or define that variable will
      --  contain all the atoms that use or define any of the variables of the
      --  cycle.
      --
      --  So, this is exactly what we implement in ``Alias_Cycle``: we detect
      --  cycles in the dependency graph and unify all variables that are part
      --  of a cycle. Once a cycle is found, we populate ``Atomic_Unset_Vars``
      --  with a single member of that cycle, which allows the subsequent call
      --  to ``Contradict_Unset_Var`` to contradict the whole cycle.

      use AdaSAT;

      type Dependency_Graph is array (Positive range <>)
         of Logic_Var_Vector;

      Dependencies      : Dependency_Graph := (Ctx.Vars'Range => <>);
      Atomic_Unset_Vars : Logic_Var_Vector;

      procedure Populate_Dependencies (V : Logic_Var);
      --  Analyze the atoms of the current solution and add as dependency of
      --  ``V`` any variable ``W`` such that if ``W`` was set, then an atom of
      --  the current solution would define ``V``.

      procedure Alias_Cycle (V : Logic_Var);
      --  Find cycles in the dependency graph and unify all variables that are
      --  part of a cycle together. Also, popupate the ``Atomic_Unset_Vars``
      --  vector with ``V`` if ``V`` has no dependency or if ``V`` is part
      --  of a cycle and a representent of that cycle is not already present in
      --  the vector.

      procedure Contradict_Unset_Var (V : Logic_Var);
      --  Assuming ``V`` is used but undefined in the current solution, build
      --  a clause that contradicts the current solution by ensuring that
      --  ``V`` must be defined if we want to use it.

      ---------------------------
      -- Populate_Dependencies --
      ---------------------------

      procedure Populate_Dependencies (V : Logic_Var) is
         V_Id : constant Natural := Id (V);
      begin
         if Solv_Trace.Is_Active then
            Solv_Trace.Trace
              ("Dependencies of unset var " & Image_With_Id (V));
         end if;

         for R of Ctx.Atoms loop
            declare
               Atom : Atomic_Relation_Type renames R.Atomic_Rel;
            begin
               --  Only propagation atoms can create dependencies between
               --  variables.
               case Atom.Kind is
                  when Propagate =>
                     --  Create the dependency only if the variable we are
                     --  propagating from is not ourself.

                     if Id (Atom.Target) = V_Id and then
                        Id (Atom.From) /= V_Id
                     then
                        Dependencies (V_Id).Append (Atom.From);
                        if Solv_Trace.Is_Active then
                           Solv_Trace.Trace
                             (" - " & Image_With_Id (Atom.From));
                        end if;
                     end if;

                  when N_Propagate =>
                     --  Right now, the meaning of ``X`` depends on ``A, B, C``
                     --  is: ``X`` would be defined if any of ``A, B, C`` is
                     --  defined. So, what we do here is not optimal: we are
                     --  basically saying that ``V`` would be set if any of the
                     --  variables of the ``N_Propagate`` is defined, but we
                     --  should rather say when *all* of them are.
                     --  We cannot express this right now but it is okay: it
                     --  simply means that we might need multiple rounds of
                     --  of toposort contradictions to converge. Since this is
                     --  not an issue for now, we let it be handled that way.

                     if Id (Atom.Target) = V_Id then
                        for W of Atom.Comb_Vars loop
                           if Id (W) /= V_Id then
                              Dependencies (V_Id).Append (W);
                              if Solv_Trace.Is_Active then
                                 Solv_Trace.Trace
                                   (" - " & Image_With_Id (W));
                              end if;
                           end if;
                        end loop;
                     end if;

                  when others =>
                     null;
               end case;
            end;
         end loop;
      end Populate_Dependencies;

      -------------------
      -- Is_Atomic_Var --
      -------------------

      function Is_Atomic_Var (V : Logic_Var) return Boolean is
        (for some W of Atomic_Unset_Vars => Id (V) = Id (W));
      --  Return whether the given variable is contained in the
      --  ``Atomic_Unset_Vars`` vector.

      -----------------
      -- Alias_Cycle --
      -----------------

      procedure Alias_Cycle (V : Logic_Var) is
         V_Id    : constant Natural := Id (V);
         Visited : Index_Set := (Ctx.Vars'Range => False);

         function DFS (W : Logic_Var) return Boolean;
         --  Implement a basic depth-first search in the dependency graph
         --  in order to find out whether there is a cycle that involves
         --  variable ``V``. If it's the case, unify all variables that
         --  are part of that cycle.

         ---------
         -- DFS --
         ---------

         function DFS (W : Logic_Var) return Boolean is
            W_Id : constant Natural := Id (W);
         begin
            if Visited (W_Id) then
               return False;
            end if;

            Visited (W_Id) := True;

            for Dep of Dependencies (W_Id) loop
               if Id (Dep) = V_Id or else DFS (Dep) then
                  if Solv_Trace.Is_Active then
                     Solv_Trace.Trace (" - New alias " & Image_With_Id (W));
                  end if;
                  Alias (W, V);
                  return True;
               end if;
            end loop;

            return False;
         end DFS;
      begin
         if Solv_Trace.Is_Active then
            Solv_Trace.Trace ("Aliasing var " & Image_With_Id (V));
         end if;

         --  Avoid adding the same variable twice in ``Atomic_Unset_Vars``
         if Is_Atomic_Var (V) then
            return;
         end if;

         --  If this variable has no dependency, it is atomic
         if Dependencies (V_Id).Is_Empty then
            Atomic_Unset_Vars.Append (V);
            return;
         end if;

         --  Otherwise, check if it is part of a cycle. Note that we check
         --  again if ``Atomic_Unset_Vars`` contains it after the DFS run,
         --  as it may have been aliased to another variable during the
         --  search.
         if DFS (V) and then not Is_Atomic_Var (V) then
            Atomic_Unset_Vars.Append (V);
         end if;
      end Alias_Cycle;

      --------------------------
      -- Contradict_Unset_Var --
      --------------------------

      procedure Contradict_Unset_Var (V : Logic_Var) is
         V_Id   : constant Natural := Id (V);
         Result : AdaSAT.Builders.Clause_Builder;
      begin
         if Solv_Trace.Is_Active then
            Solv_Trace.Trace ("Orphan rels for unset var " & Image (V) & ":");
         end if;

         --  First gather all equations that use V. First include all unifying
         --  atoms.
         for U of Ctx.Unifies loop
            if Id (U.Atomic_Rel.Target) = V_Id then
               if Solv_Trace.Is_Active then
                  Solv_Trace.Trace (Image (U));
               end if;

               Result.Add_Simplify (-Ctx.Atom_Map (U.Id));
            end if;
         end loop;

         --  And then also include the rest of the atoms
         for R of Ctx.Atoms loop
            if Uses_Var (R.Atomic_Rel, V) then
               if Solv_Trace.Is_Active then
                  Solv_Trace.Trace (Image (R));
               end if;
               Result.Add_Simplify (-Ctx.Atom_Map (R.Id));
            end if;
         end loop;

         Solv_Trace.Trace ("Candidate defining rels:");

         for Block_Id in 1 .. Ctx.Blocks.Length loop
            if Model (Variable (Block_Id)) in False then
               for R of Ctx.Blocks.Get (Block_Id) loop
                  declare
                     W : constant Logic_Var := Defined_Var (R.Atomic_Rel);
                  begin
                     if W /= null and then Id (W) = V_Id then
                        if Solv_Trace.Is_Active then
                           Solv_Trace.Trace (Image (R));
                        end if;
                        Result.Add_Simplify (+Variable (Block_Id));
                        exit;
                     end if;
                  end;
               end loop;
            end if;
         end loop;

         Explanation.Add (Result.Build);
      end Contradict_Unset_Var;
   begin
      --  Take into account the amount of work that we need to do here in our
      --  timeout estimation.
      Decrease_Remaining_Time (Ctx, Ctx.Atoms.Length);

      --  Stage 1: build the dependency graph
      for V of Ctx.Sort_Ctx.Unset_Vars loop
         Populate_Dependencies (V);
      end loop;

      --  Stage 2: extract unset variables that are "atomic" (i.e. that have
      --  no dependencies), and detect cycles in the graph.
      for V of Ctx.Sort_Ctx.Unset_Vars loop
         Alias_Cycle (V);
      end loop;

      pragma Assert (not Atomic_Unset_Vars.Is_Empty);

      --  Stage 3: contradict atomic variables
      for V of Atomic_Unset_Vars loop
         Contradict_Unset_Var (V);
      end loop;

      --  Stage 4: free everything
      for Deps of Dependencies loop
         Deps.Destroy;
      end loop;
      Atomic_Unset_Vars.Destroy;
   end Explain_Topo_Sort_Failure;

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
   -- Check --
   -----------

   function Check
     (Ctx            : in out Solving_Context;
      Model          : AdaSAT.Model;
      Contradictions : in out AdaSAT.Formulas.Formula) return Boolean
   is
      use AdaSAT;
      use AdaSAT.Formulas;

      function Cleanup (Result : Boolean) return Boolean;
      --  Helper used to deallocate memory or reset relevant data structures
      --  before returning. Returns the given boolean.

      procedure Fail;
      --  Populate ``Contradictions`` so as to invalidate this exact model
      --  only.

      -------------
      -- Cleanup --
      -------------

      function Cleanup (Result : Boolean) return Boolean is
      begin
         Cleanup_Aliases (Ctx.Vars.all);
         if Solv_Trace.Is_Active then
            if Contradictions.Length > 0 then
               Solv_Trace.Trace ("Learning the following clauses:");
               Solv_Trace.Trace (Image (Contradictions));
            end if;
         end if;
         return Result;
      end Cleanup;

      ----------
      -- Fail --
      ----------

      procedure Fail is
         New_Clause : constant Clause :=
            new Literal_Array (1 .. Ctx.Blocks.Length);
      begin
         for I in New_Clause'Range loop
            declare
               V : constant Variable := Variable (I);
            begin
               New_Clause (I) := (if Model (V) in True then -V else +V);
            end;
         end loop;
         Contradictions.Append (New_Clause);
      end Fail;
   begin
      Ctx.Unifies.Clear;
      Ctx.Atoms.Clear;

      if Solv_Trace.Is_Active then
         Solv_Trace.Trace ("Trying with: " & Image (Model));
      end if;
      for Block_Id in 1 .. Ctx.Blocks.Length loop
         if Model (Variable (Block_Id)) in True then
            for R of Ctx.Blocks.Get (Block_Id) loop
               declare
                  Atom : Atomic_Relation_Type renames R.Atomic_Rel;
               begin
                  if Atom.Kind = Unify then
                     if Atom.Unify_From /= Atom.Target then
                        Ctx.Unifies.Append (R);
                     end if;
                  else
                     Ctx.Atoms.Append (R);
                  end if;
               end;
            end loop;
         end if;
      end loop;
      if Solv_Trace.Is_Active then
         for R of Ctx.Atoms loop
            Solv_Trace.Trace (Image (R));
         end loop;
      end if;

      declare
         use Atomic_Relation_Vectors;
         Sorting_Error : Boolean;
         Explanation   : Builders.Formula_Builder;
         Sorted_Atoms  : constant Elements_Array :=
           Topo_Sort (Ctx.Atoms,
                      Ctx.Unifies,
                      Ctx.Vars.all,
                      Ctx.Sort_Ctx,
                      Sorting_Error);
      begin
         --  There was an error in the topo sort: continue to next potential
         --  solution.
         if Sorting_Error then
            if Solv_Trace.Is_Active then
               Solv_Trace.Trace ("Topo fail!");
            end if;
            begin
               --  First try to revoke this partial solution by finding
               --  contradictions in the sorted subset of atoms.
               if Evaluate_Atoms (Ctx, Sorted_Atoms, Explanation) then
                  --  If evaluation was successful, we need to revoke this
                  --  partial solution in another manner: we explain that
                  --  this solution is not feasible because some atoms are
                  --  orphans.
                  pragma Assert (not Ctx.Sort_Ctx.Unset_Vars.Is_Empty);
                  Explain_Topo_Sort_Failure (Ctx, Model, Explanation);
               end if;

               --  Explanation must be filled at this stage, either by
               --  ``Evaluate_Atoms`` or by ``Explain_Topo_Sort_Failure``.
               Contradictions := Explanation.Build;
               pragma Assert (not Contradictions.Is_Empty);
               return Cleanup (False);
            exception
               when Timeout_Error =>
                  raise;
               when others =>
                  null;
            end;
            Fail;
            return Cleanup (False);
         end if;

         --  Once the topological sort has been done, we just have to solve
         --  every relation in order. Abort if one doesn't solve.
         if not Evaluate_Atoms (Ctx, Sorted_Atoms, Explanation) then
            Contradictions := Explanation.Build;
            return Cleanup (False);
         end if;

         --  All atoms have correctly solved: we have found a solution: let
         --  the user defined callback know and decide if we should continue
         --  exploring the solution space.
         if Ctx.Cb (Ctx.Vars.all) then
            Fail;
            return Cleanup (False);
         end if;
      end;
      return Cleanup (True);
   end Check;

   ---------------------
   -- Encode_Relation --
   ---------------------

   function Encode_Relation
     (Self           : Compound_Relation;
      Ctx            : in out Solving_Context;
      Variable_Count : out AdaSAT.Variable_Or_Null)
      return AdaSAT.Formulas.Formula
   is
      use AdaSAT;

      Var_Id   : Variable := 1;
      --  The counter used to assign a unique variable to each basic block

      Problem  : AdaSAT.Builders.Formula_Builder;
      --  The builder for the resulting formula

      procedure Process_Atom
        (R : Atomic_Relation; From_Block_Id : Variable);
      --  Update the necessary data structures to include the given atomic
      --  relation inside the basic block represented by ``Block_Id``.

      procedure Process_All
        (R : Compound_Relation; From_Block_Id : Variable);
      --  Process the given relation (assuming its a compound one) as if it
      --  was an ``All``. That is, include all its inner atomic relations in
      --  the given basic block id.

      procedure Process_Any
        (R : Compound_Relation; From_Block_Id : Variable);
      --  Process the given relation (assuming its a compound one) as if it
      --  was an ``Any``. That is allocate new basic blocks for its inner
      --  branches, populate the SAT formula with constraints implementing
      --  the semantics of Adalog ``Any``relations, and recursively call
      --  ``Create_Problem`` for the branches.

      procedure Create_Problem
        (R : Relation; From_Block_Id : Variable);
      --  Encode the given relation by populate the ``Problem`` formula builder
      --  with constraints implementing the semantics of ``All`` and ``Any``
      --  relations.

      ------------------
      -- Process_Atom --
      ------------------

      procedure Process_Atom
        (R : Atomic_Relation; From_Block_Id : Variable)
      is
         Index : constant Natural := Natural (From_Block_Id);
      begin
         --  Make sure there is enough room in ``Ctx.Blocks`` to access the
         --  blocks at the given Id.
         while Ctx.Blocks.Length < Index loop
            Ctx.Blocks.Append (Atomic_Relation_Vectors.Empty_Vector);
         end loop;

         if R.Atomic_Rel.Kind in False then
            --  If this is a ``False`` relation, simply add the constraint that
            --  this basic block cannot be part of the solution.
            Problem.Add (new Literal_Array'(1 => -From_Block_Id));
         else
            --  Populate the data structure to account for the fact that the
            --  given relation belongs to the basic block given by its Id.
            Ctx.Blocks.Get_Access (Index).Append (R);
            Ctx.Atom_Map (R.Id) := From_Block_Id;
         end if;
      end Process_Atom;

      -----------------
      -- Process_All --
      -----------------

      procedure Process_All
        (R : Compound_Relation; From_Block_Id : Variable)
      is
      begin
         for I in 1 .. R.Compound_Rel.Rels.Length loop
            Create_Problem (R.Compound_Rel.Rels.Get (I), From_Block_Id);
         end loop;
      end Process_All;

      -----------------
      -- Process_Any --
      -----------------

      procedure Process_Any
        (R : Relation; From_Block_Id : Variable)
      is
         Rels : Relation_Vectors.Vector renames R.Compound_Rel.Rels;
      begin
         if Rels.Length = 0 then
            --  An ``Any`` with 0 relations is a ``False`` relation
            Problem.Add (new Literal_Array'(1 => -From_Block_Id));
         elsif Rels.Length = 1 then
            --  An ``Any`` with 1 relation is the same as its inner relation
            --  appearing by itself.
            Create_Problem (Rels.Get (1), From_Block_Id);
         else
            declare
               Cur_Branch_Id : Variable := Var_Id + 1;
               --  Holds the Id of the branch we are going to handle next

               CB : AdaSAT.Builders.Clause_Builder;
               --  This builder is used to generate the constraint that if
               --  ``From_Block_Id`` is True, then at least one of the branches
               --  of the ``Any`` must be taken.
            begin
               CB.Reserve (Rels.Length);
               Var_Id := Var_Id + Variable (Rels.Length);

               --  Generate the constraint that only one branch can be taken
               Problem.Add_At_Most_One (Cur_Branch_Id, Var_Id);

               if From_Block_Id /= 1 then
                  CB.Add (-From_Block_Id);
               end if;

               for I in 1 .. Rels.Length loop
                  --  If ``From_Block_Id`` is True, then this branch must be
                  --  taken as well.
                  CB.Add (+Cur_Branch_Id);

                  --  Recursively encode the problem of the branch
                  Create_Problem (Rels.Get (I), Cur_Branch_Id);

                  --  Generate the constraints that no branch should be
                  --  taken if the ``From_Block_Id`` is not True. There is
                  --  no need to generate this constraint for the ``Anys``
                  --  that appear in the top-level relation, since that one
                  --  is necessarily True.
                  if From_Block_Id /= 1 then
                     Problem.Add (new Literal_Array'
                       ((+From_Block_Id, -Cur_Branch_Id)));
                  end if;

                  Cur_Branch_Id := Cur_Branch_Id + 1;
               end loop;

               Problem.Add (CB.Build);
            end;
         end if;
      end Process_Any;

      --------------------
      -- Create_Problem --
      --------------------

      procedure Create_Problem
        (R : Relation; From_Block_Id : Variable)
      is
      begin
         case R.Kind is
            when Atomic =>
               Process_Atom (R, From_Block_Id);
            when Compound =>
               case R.Compound_Rel.Kind is
                  when Kind_All =>
                     Process_All (R, From_Block_Id);
                  when Kind_Any =>
                     Process_Any (R, From_Block_Id);
               end case;
         end case;
      end Create_Problem;
   begin
      Create_Problem (Self, Var_Id);

      --  Make sure all block ids can be used to access the ``Ctx.Blocks``
      --  vector.
      while Ctx.Blocks.Length < Natural (Var_Id) loop
         Ctx.Blocks.Append (Atomic_Relation_Vectors.Empty_Vector);
      end loop;

      Variable_Count := Variable_Or_Null (Ctx.Blocks.Length);

      --  Force the top-level relation to be set, if it exists
      if Variable_Count > 0 then
         Problem.Add (new Literal_Array'(1 => +1));
      end if;

      return Problem.Build;
   end Encode_Relation;

   ----------------
   -- Solve_DPLL --
   ----------------

   function Solve_DPLL
     (Self : Compound_Relation; Ctx : in out Solving_Context) return Boolean
   is
      use AdaSAT;
      use AdaSAT.Formulas;

      Var_Count : Variable_Or_Null;
      Problem   : constant Formula := Encode_Relation (Self, Ctx, Var_Count);
      Solution  : Model := (1 .. Var_Count => Unset);
   begin
      if Solv_Trace.Is_Active then
         for I in 1 .. Ctx.Blocks.Length loop
            Solv_Trace.Trace ("Block" & I'Image);
            Solv_Trace.Increase_Indent;
            for R of Ctx.Blocks.Get (I) loop
               Solv_Trace.Trace (Image (R));
            end loop;
            Solv_Trace.Decrease_Indent;
         end loop;
         Solv_Trace.Trace (Var_Count'Image & " vs " & Ctx.Blocks.Length'Image);
         Solv_Trace.Trace ("clauses:" & Problem.Length'Image);
         Solv_Trace.Trace (Image (Problem));
      end if;

      return DPLL_Adalog.Solve
        (Problem,
         Ctx,
         Solution,
         Variable_Or_Null (Ctx.Blocks.Length));
   end Solve_DPLL;

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
      pragma Unreferenced (Solve_Options);

      PRel   : Prepared_Relation;
      Rel    : Relation renames PRel.Rel;
      Ctx    : Solving_Context;
      Max_Id : Natural;
      Ignore : Boolean;

      procedure Cleanup;
      --  Cleanup helper to call before exiting Solve

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup is
      begin
         Destroy (Ctx);
      end Cleanup;

   begin
      PRel := Prepare_Relation (Self, Max_Id);
      if Solver_Trace.Is_Active then
         Solver_Trace.Trace ("Solving equation:");
         Solver_Trace.Trace (Image (Rel));
      end if;
      Ctx := Create
        (Solution_Callback'Unrestricted_Access.all, PRel.Vars, Max_Id);
      Ctx.Remaining_Time := Timeout;

      declare
         Start : constant Time := Clock;
      begin
         Ignore := Solve_DPLL (Rel, Ctx);
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
      --  This assumes that ``Self`` is either an ``Assign`` or a
      --  ``Propagate`` relation.

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
