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

package body Langkit_Support.Adalog.Symbolic_Solver is

   -----------------------
   --  Supporting types --
   -----------------------

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
   --  Helper subtype. Allows us to check that we only have any relations in
   --  Any_Relation_Vectors.Vector.

   package Any_Relation_Lists is new Langkit_Support.Functional_Lists
     (Any_Rel);
   subtype Any_Relation_List is Any_Relation_Lists.List;
   --  Vectors of Anys

   function Image (Self : Any_Relation_Lists.List) return String;

   package Atomic_Relation_Lists is new Langkit_Support.Functional_Lists
     (Atomic_Relation);

   package Var_Ids_To_Atoms_Vectors is new Langkit_Support.Vectors
     (Atomic_Relation_Lists.List);
   subtype Var_Ids_To_Atoms is Var_Ids_To_Atoms_Vectors.Vector;
   --  Vector mapping logic var ids to atomic relations

   --------------------------
   -- Supporting functions --
   --------------------------

   procedure Reserve (V : in out Var_Ids_To_Atoms; Size : Positive);
   --  Reserve ``N`` elements in ``V``. TODO??? Add that to vectors.

   function Create_Propagate
     (From, To     : Var;
      Conv         : Converter_Access := null;
      Eq           : Comparer_Access := null;
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

   type Callback_Type is access function (Vars : Var_Array) return Boolean;
   --  Type used to store the callback. TODO??? This should make more data
   --  accessible, like the numbers of solutions tried so far.

   type Atom_And_Index is record
      Atom       : Atomic_Relation;
      Atom_Index : Positive;
   end record;
   --  Simple record storing an atom along with its index. Used to construct
   --  the dependency graph during topo sort.

   package Atom_Lists is new Langkit_Support.Functional_Lists (Atom_And_Index);
   package Atom_Lists_Vectors is new Langkit_Support.Vectors (Atom_Lists.List);
   --  Vectors of lists of ``Atom_And_Index``. Used to construct the
   --  dependency graph during topo sort.

   type Sort_Context_Type is record
      Using_Atoms : Atom_Lists_Vectors.Vector;
      --  Maps var ids to lists of atoms. TODO??? we could use a vector rather
      --  than a list as the inner storing type. Would be more efficient.

      Working_Set : Atom_Lists.List;
      --  Working set of atoms. Used as a temporary list to store atoms in the
      --  graph that need to be subsequently added.

      N_Preds : Atom_Lists.List;
      --  List of N_Predicates, to be applied at the end of solving. TODO??? we
      --  could apply this policy for all predicates, which would simplify the
      --  code a bit.
   end record;
   --  Type storing data used when doing a topological sort, when we reach a
   --  complete potential solution. The data is stored in a shared variable
   --  stored in the Solving_Context, so as to spare allocations.
   --
   --  TODO??? Fix Functional_List.Clear, because we're leaking heaps of memory
   --  at the moment.

   type Sort_Context is access all Sort_Context_Type;

   type Nat_Access is access all Natural;

   type Solving_Context is record
      Cb : Callback_Type;
      --  User callback, to be called when a solution is found

      Atoms : Atoms_Vector_Access;
      --  Current flat list of atoms that will at the end of the traversal of a
      --  branch constitute a potential solution.

      Aliases : Atoms_Vector_Access;
      --  List of alias relations. TODO??? not clear why this is stored in the
      --  context, and not as a local variable in Solve_Compound.

      Anys : Any_Relation_List := Any_Relation_Lists.No_List;
      --  Remaining list of Any relations to traverse

      Vars : Logic_Var_Vector_Access;
      --  Set of all variables. TODO??? Store an array rt. a vector by
      --  traversing the equation first.

      Vars_To_Atoms : Var_Ids_To_Atoms;
      --  Stores a mapping of variables to atoms, used for exponential
      --  resolution optimization.
      --
      --  TODO???
      --
      --  1. Store an array rt. a vector (traversing the equation first to find
      --  out all variables).
      --
      --  2. Store vectors rt. than lists, to have a more bounded memory
      --  behavior.
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

   function Get_Id (Ctx : Solving_Context; Logic_Var : Var) return Positive;
   --  Get the id of variable ``Logic_Var`` in ``Ctx``.
   --
   --  TODO??? Due to Aliasing, a logic variable can have several ids.
   --  Consequently, things like exponential resolution optimization are
   --  not complete.

   procedure Assign_Ids (Ctx : Solving_Context; Atom : Atomic_Relation);
   --  Assign ids to variables that Atom uses or defines

   procedure Reset_Vars (Ctx : Solving_Context; Reset_Ids : Boolean := False);
   --  Reset all logic variables. If Reset_Ids is true, only reset ids.
   --
   --  TODO??? Should really be two separate procedures.

   ----------------------------
   --  Comparer pred wrapper --
   ----------------------------

   type Comparer_N_Pred is new N_Predicate_Type with record
      Conv : Converter_Access;
      Eq   : Comparer_Access;
   end record;
   --  This is a wrapper type, constructed when we have a propagate
   --  with an ``Eq`` predicate.

   overriding procedure Destroy (Self : in out Comparer_N_Pred);

   overriding function Call
     (Self : Comparer_N_Pred; Vals : Value_Array) return Boolean
   is
     (Self.Eq.Compare ((if Self.Conv /= null
                        then Self.Conv.Convert (Vals (1))
                        else Vals (1)), Vals (2)));

   overriding function Full_Image
     (Self : Comparer_N_Pred; Vars : Var_Array) return String
   is (Self.Eq.Image & "?("
       & (if Self.Conv /= null
          then Self.Conv.Image & "(" & Image (Vars (1)) & ")"
          else Image (Vars (1)))
       & ", " & Image (Vars (2)) & ")");

   type Comparer_Pred is new Predicate_Type with record
      Eq   : Comparer_Access;
      Conv : Converter_Access;
      Val  : Value_Type;
   end record;
   --  This is a wrapper type, constructed when we have an assign
   --  with an ``Eq`` predicate.

   overriding procedure Destroy (Self : in out Comparer_Pred);

   overriding function Call
     (Self : Comparer_Pred; Val : Value_Type) return Boolean
   is
     (Self.Eq.Compare (Val, (if Self.Conv /= null
                             then Self.Conv.Convert (Self.Val)
                             else Self.Val)));

   overriding function Full_Image
     (Self : Comparer_Pred; Logic_Var : Var) return String
   is (Self.Eq.Image & "?("
       & (if Self.Conv /= null
          then Self.Conv.Image & "(" & Element_Image (Self.Val) & ")"
          else Element_Image (Self.Val)) & ", " & Image (Logic_Var) & ")");

   function Solve_Compound
     (Self : Compound_Relation; Ctx : Solving_Context) return Boolean;
   --  TODO???

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Self : in out Comparer_N_Pred) is
   begin
      Free (Self.Eq);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Self : in out Comparer_Pred) is
   begin
      Free (Self.Eq);
   end Destroy;

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

   ----------------
   -- Reset_Vars --
   ----------------

   procedure Reset_Vars (Ctx : Solving_Context; Reset_Ids : Boolean := False)
   is
   begin
      for V of Ctx.Vars.all loop
         if Reset_Ids then
            Set_Id (V, 0);
         else
            Reset (V);
         end if;
      end loop;
   end Reset_Vars;

   ------------
   -- Create --
   ------------

   function Create return Solving_Context is
   begin
      return Ret : Solving_Context do
         Ret.Vars := new Logic_Var_Vector;
         Ret.Sort_Ctx := new Sort_Context_Type;
         Ret.Tried_Solutions := new Natural'(0);
      end return;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Ctx : in out Solving_Context) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Atomic_Relation_Vector, Atoms_Vector_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Logic_Var_Vector, Logic_Var_Vector_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Sort_Context_Type, Sort_Context);
      procedure Free is new Ada.Unchecked_Deallocation
        (Natural, Nat_Access);
   begin
      Ctx.Aliases.Destroy;
      Ctx.Atoms.Destroy;
      Any_Relation_Lists.Destroy (Ctx.Anys);
      Ctx.Vars.Destroy;
      Free (Ctx.Vars);
      Ctx.Vars_To_Atoms.Destroy;
      Free (Ctx.Atoms);
      Free (Ctx.Aliases);
      Free (Ctx.Tried_Solutions);

      Atom_Lists.Destroy (Ctx.Sort_Ctx.N_Preds);
      Atom_Lists.Destroy (Ctx.Sort_Ctx.Working_Set);
      for I in  Ctx.Sort_Ctx.Using_Atoms.First_Index ..
        Ctx.Sort_Ctx.Using_Atoms.Last_Index
      loop
         Atom_Lists.Destroy (Ctx.Sort_Ctx.Using_Atoms.Get_Access (I).all);
      end loop;
      Ctx.Sort_Ctx.Using_Atoms.Destroy;
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

   ------------
   -- Get_Id --
   ------------

   function Get_Id (Ctx : Solving_Context; Logic_Var : Var) return Positive is
   begin
      if Id (Logic_Var) = 0 then
         if Verbose_Trace.Is_Active then
            Verbose_Trace.Trace ("No id for logic var " & Image (Logic_Var));
         end if;

         Ctx.Vars.Append (Logic_Var);
         Ctx.Sort_Ctx.Using_Atoms.Append (Atom_Lists.Create);
         Set_Id (Logic_Var, Ctx.Vars.Last_Index);
      end if;
      return Id (Logic_Var);
   end Get_Id;

   ----------------
   -- Assign_Ids --
   ----------------

   procedure Assign_Ids (Ctx : Solving_Context; Atom : Atomic_Relation) is
      procedure Assign_Id (Var : Var_Or_Null);
      --  Helper to assign the Ids on the given variable

      ---------------
      -- Assign_Id --
      ---------------

      procedure Assign_Id (Var : Var_Or_Null) is
         Id : Positive;
      begin
         if Var.Exists then
            Id := Get_Id (Ctx, Var.Logic_Var);

            if Verbose_Trace.Is_Active then
               Verbose_Trace.Trace ("Assigning Id " & Id'Image
                                    & " to var " & Image (Var.Logic_Var));
            end if;
         end if;
      end Assign_Id;

   begin
      Assign_Id (Defined_Var (Atom));
      Assign_Id (Used_Var (Atom));
   end Assign_Ids;

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
      --  dependency graph, so that the Unify_From variable is registered in
      --  the list of variables of the equation. TODO??? Might be cleaner to
      --  have a separate function to return all variables a relation uses?
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
      function Topo_Sort
        (Atoms : Atomic_Relation_Vector; Error : out Boolean)
         return Atomic_Relation_Vectors.Elements_Array;
      --  Do a topological sort of the atomic relations in ``Atoms``. Atoms
      --  with no dependencies will come first. Then, atoms will be sorted
      --  according to their dependencies. Finally, ``N_Predicate``s will come
      --  last, because they have multiple dependencies but nothing can depend
      --  on them.
      --
      --  If an atom is an "orphan", that is to say it is not part of the
      --  resulting sorted collection, then ``Topo_Sort`` returns ``False``.

      function Try_Solution (Atoms : Atomic_Relation_Vector) return Boolean;
      --  Try to solve the current solution

      function Process_Atom (Atom : Atomic_Relation) return Boolean;
      --  Process one atom, whether we are in an All or Any branch. Returns
      --  whether we should abort current path or not, in the case of an All
      --  relation.

      function Cleanup (Val : Boolean) return Boolean;
      --  Cleanup helper to call before exitting Solve_Compound

      procedure Branch_Cleanup;
      --  TODO???

      use Any_Relation_Lists;
      use Atomic_Relation_Lists;

      ---------------
      -- Topo_Sort --
      ---------------

      function Topo_Sort
        (Atoms : Atomic_Relation_Vector; Error : out Boolean)
         return Atomic_Relation_Vectors.Elements_Array
      is
         Sorted_Atoms    : Atomic_Relation_Vectors.Elements_Array
           (Atoms.First_Index .. Atoms.Last_Index);
         Last_Atom_Index : Natural := 0;

         procedure Append (Atom : Atomic_Relation);
         --  Append Atom to Sorted_Atoms

         Appended : array (Sorted_Atoms'Range) of Boolean := (others => False);
         --  TODO???

         use Atom_Lists;

         function Id (S : Var_Or_Null) return Natural
         is (if S.Exists then Get_Id (Ctx, S.Logic_Var) else 0);
         --  TODO???

         function Defined (S : Atomic_Relation) return Natural
         is (Id (Defined_Var (S)));
         --  TODO???

         ------------
         -- Append --
         ------------

         procedure Append (Atom : Atomic_Relation) is
         begin
            Last_Atom_Index := Last_Atom_Index + 1;
            Sorted_Atoms (Last_Atom_Index) := Atom;
         end Append;

         Using_Atoms : constant Atom_Lists_Vectors.Vector :=
           Ctx.Sort_Ctx.Using_Atoms;
         N_Preds     : Atom_Lists.List := Ctx.Sort_Ctx.N_Preds;
         Working_Set : Atom_Lists.List := Ctx.Sort_Ctx.Working_Set;
      begin
         Error := False;

         --  Step 1: create:
         --
         --    1. A map of vars to all atoms that use them.
         --
         --    2. The base working set for the topo sort, constituted of all
         --       atoms with no dependencies.

         for I in reverse Atoms.First_Index .. Atoms.Last_Index loop
            declare
               Current_Atom : constant Atomic_Relation := Atoms.Get (I);

               --  Resolve the Id of the var used. If the var aliases to
               --  another var, resolve to the aliased var's Id.
               Used_Logic_Var : constant Var_Or_Null :=
                 Used_Var (Current_Atom);
               Used_Id        : constant Natural :=
                 (if Used_Logic_Var.Exists
                  then (if Get_Alias (Used_Logic_Var.Logic_Var) /= No_Var
                        then Get_Id (Ctx, Get_Alias (Used_Logic_Var.Logic_Var))
                        else Get_Id (Ctx, Used_Logic_Var.Logic_Var))
                  else 0);
            begin
               if Current_Atom.Kind = Unify then
                  --  Unifys are handled by aliasing before. We don't need to
                  --  ever explicitly solve them. Mark them as appended.

                  Appended (I) := True;

               elsif Current_Atom.Kind = N_Predicate then
                  --  N_Predicates are appended at the end separately

                  N_Preds := (Current_Atom, I) & N_Preds;

               elsif Used_Id = 0 then
                  --  Put atoms with no dependency in the working set

                  Working_Set := (Current_Atom, I) & Working_Set;

               else
                  --  For other atoms, put them in the using atoms map, which
                  --  represent the edges of the dependency graph.

                  declare
                     L : Atom_Lists.List renames
                       Using_Atoms.Get_Access (Used_Id).all;
                  begin
                     L := (Current_Atom, I) & L;
                  end;
               end if;
            end;
         end loop;

         --  Step 2: Do the topo sort

         while Has_Element (Working_Set) loop
            --  Take items from the working set in order. In the beginning, it
            --  will only contain atoms with no dependencies.
            declare
               Atom    : constant Atom_And_Index := Head (Working_Set);
               Defd_Id : constant Natural := Defined (Atom.Atom);
            begin
               Working_Set := Tail (Working_Set);

               Append (Atom.Atom);
               Appended (Atom.Atom_Index) := True;

               --  If the atom defines a variable that is used by other atoms,
               --  put those other atoms in the working set.
               if Defd_Id /= 0 then
                  for El of Using_Atoms.Get (Defd_Id) loop
                     Working_Set := El & Working_Set;
                  end loop;

                  --  Remove items from Using_Atoms, so that they're not
                  --  appended again to the working set.
                  Clear (Using_Atoms.Get_Access (Defd_Id).all);
               end if;
            end;
         end loop;

         --  Append N_Predicates at the end
         for N_Pred of N_Preds loop
            Append (N_Pred.Atom);
            Appended (N_Pred.Atom_Index) := True;
         end loop;

         --  Verify that every atom has been appended. TODO??? This feels like
         --  an inefficient way to handle this problem, but I haven't found a
         --  better way yet.
         for I in Appended'Range loop
            if not Appended (I) then
               Error := True;

               if Solv_Trace.Is_Active then
                  Solv_Trace.Trace
                    ("Orphan relation: " & Image (Atoms.Get (I)));
               end if;

               return Atomic_Relation_Vectors.Empty_Array;
            end if;
         end loop;

         Clear (Working_Set);
         Clear (N_Preds);

         return Sorted_Atoms (1 .. Last_Atom_Index);
      end Topo_Sort;

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
         Reset_Vars (Ctx);

         Ctx.Tried_Solutions.all := Ctx.Tried_Solutions.all + 1;

         Sol_Trace.Trace ("Tried solutions: " & Ctx.Tried_Solutions.all'Image);

         declare
            use Atomic_Relation_Vectors;
            Sorting_Error : Boolean;
            Sorted_Atoms  : constant Elements_Array :=
              Topo_Sort (Atoms, Sorting_Error);
         begin
            --  There was an error in the topo sort. Continue to next potential
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
            for Atom of Sorted_Atoms loop
               if not Solve_Atomic (Atom) then
                  if Solv_Trace.Is_Active then
                     Solv_Trace.Trace ("Failed on " & Image (Atom));
                  end if;

                  return Cleanup (True);
               end if;
            end loop;

            if Sol_Trace.Is_Active then
               Sol_Trace.Trace ("Valid solution");
               Sol_Trace.Trace (Image (Sorted_Atoms));
            end if;

            --  Call the user defined callback and return
            return Cleanup (Ctx.Cb (Var_Array (Ctx.Vars.To_Array)));
         end;
      end Try_Solution;

      Vars_To_Atoms          : Var_Ids_To_Atoms := Ctx.Vars_To_Atoms.Copy;
      Anys                   : Any_Relation_List := Ctx.Anys;
      Initial_Atoms_Length   : Natural renames Ctx.Atoms.Last_Index;
      Initial_Aliases_Length : Natural renames Ctx.Aliases.Last_Index;

      --------------------
      -- Branch_Cleanup --
      --------------------

      procedure Branch_Cleanup is
      begin
         Ctx.Atoms.Cut (Initial_Atoms_Length);

         --  Unalias every var that was aliased
         for I in Initial_Aliases_Length + 1 .. Ctx.Aliases.Last_Index loop
            if Verbose_Trace.Is_Active then
               Verbose_Trace.Trace
                 ("Unaliasing "
                  & Image (Ctx.Aliases.Get_Access (I).Unify_From));
            end if;
            Unalias (Ctx.Aliases.Get_Access (I).Unify_From);
         end loop;

         Ctx.Aliases.Cut (Initial_Aliases_Length);
         Vars_To_Atoms.Destroy;
         Vars_To_Atoms := Ctx.Vars_To_Atoms.Copy;
      end Branch_Cleanup;

      -------------
      -- Cleanup --
      -------------

      function Cleanup (Val : Boolean) return Boolean is
      begin
         Branch_Cleanup;
         Vars_To_Atoms.Destroy;
         Trav_Trace.Decrease_Indent;
         return Val;
      end Cleanup;

      ------------------
      -- Process_Atom --
      ------------------

      function Process_Atom (Atom : Atomic_Relation) return Boolean is
         Id : Positive;
      begin
         Assign_Ids (Ctx, Atom);

         if Atom.Kind = Unify and then Atom.Unify_From /= Atom.Target then
            if Verbose_Trace.Is_Active then
               Verbose_Trace.Trace
                 ("Aliasing var " & Image (Atom.Unify_From)
                  & " to " & Image (Atom.Target));
            end if;
            Alias (Atom.Unify_From, Atom.Target);
            Id := Get_Id (Ctx, Atom.Unify_From);
            Reserve (Vars_To_Atoms, Id);
            Ctx.Aliases.Append (Atom);
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
               V : constant Var_Or_Null := (if Atom.Kind = Predicate
                                            then Used_Var (Atom)
                                            else Defined_Var (Atom));
            begin
               Id := Get_Id (Ctx, V.Logic_Var);
            end;
            Reserve (Vars_To_Atoms, Id);
            Vars_To_Atoms.Get_Access (Id).all := Atom & Vars_To_Atoms.Get (Id);
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
      --  list of atoms.
      --
      --  2. Add disjunctions from this relation to our list of disjunctions
      --  that we need to explore.
      --
      --  Explore every possible alternative created by disjunctions, by
      --  recursing on them.

      when Kind_All =>
         --  First step: gather anys and atoms in their own vectors

         for Sub_Rel of Self.Rels loop
            case Sub_Rel.Kind is
            when Compound =>
               --  Implicit assertion: an all can only contain an Any
               Anys := Sub_Rel.Compound_Rel & Anys;
            when Atomic =>
               if not Process_Atom (Sub_Rel.Atomic_Rel) then
                  return Cleanup (True);
               end if;
            end case;
         end loop;

         if Ctx.Cut_Dead_Branches then
            --  Exponential resolution optimization: Check if any atoms
            --  *defines* the value of a var that is *used* by another atom
            --  in that solution branch.
            --
            --  PROBLEM: While this avoids exponential resolutions, it also
            --  makes the default algorithm quadratic (?), since we re-iterate
            --  on all atoms at every depth of the recursion. What we could do
            --  is:
            --
            --  1. Either not activate this opt for certain trees.
            --
            --  2. Either try to check only for new atoms. This seems
            --  hard/impossible since new constraints are added at every
            --  recursion, so old atoms need to be checked again for
            --  completeness. But maybe there is a way. Investigate later.

            for Atom of Ctx.Atoms.all loop
               if Atom.Kind = Assign then
                  declare
                     V : constant Var_Or_Null := Defined_Var (Atom);

                     --  TODO??? with aliasing, a variable can have several ids
                     Id : constant Positive := Get_Id (Ctx, V.Logic_Var);
                  begin
                     Reset (V.Logic_Var);

                     pragma Assert (Vars_To_Atoms.Length >= Id);

                     if Length (Vars_To_Atoms.Get (Id)) > 0 then

                        --  We have some relations that apply on this variable.
                        --  Call the assign atom, then see if the relations
                        --  solve.

                        declare
                           Dummy : constant Boolean := Solve_Atomic (Atom);
                        begin
                           null;
                        end;
                        for User of Vars_To_Atoms.Get (Id) loop

                           --  If applying a predicate fails, then we exit the
                           --  solving of this branch early.

                           if not Solve_Atomic (User) then
                              if Solv_Trace.Active then
                                 Solv_Trace.Trace
                                   ("Aborting due to exp res optim");
                                 Solv_Trace.Trace
                                   ("Current atoms: " & Image (Ctx.Atoms.all));
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
         end if;

         if Length (Anys) = 0 then
            --  We don't have any Any relation left: We have a complete
            --  potential solution. Try to solve it.
            return Cleanup (Try_Solution (Ctx.Atoms.all));

         else
            if Trav_Trace.Is_Active then
               Trav_Trace.Trace ("Before recursing in solve All");
               Trav_Trace.Trace (Image (Ctx.Atoms.all));
               Trav_Trace.Trace (Image (Anys));
            end if;

            return Cleanup (Solve_Compound
                              (Head (Anys),
                               Ctx'Update (Anys          => Tail (Anys),
                                           Vars_To_Atoms => Vars_To_Atoms)));
         end if;

      when Kind_Any =>
         for Sub_Rel of Self.Rels loop
            case Sub_Rel.Kind is
               when Atomic =>
                  pragma Assert (Sub_Rel.Atomic_Rel.Kind /= False);

                  declare
                     Dummy : Boolean := Process_Atom (Sub_Rel.Atomic_Rel);
                  begin
                     null;
                  end;

                  if Length (Ctx.Anys) > 0 then
                     if not Solve_Compound
                       (Head (Anys),
                        Ctx'Update (Anys          => Tail (Anys),
                                    Vars_To_Atoms => Vars_To_Atoms))
                     then
                        return Cleanup (False);
                     end if;
                  else
                     if not Try_Solution (Ctx.Atoms.all) then
                        return Cleanup (False);
                     end if;
                  end if;

               when Compound =>
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
      Ctx    : Solving_Context := Create;
      Ignore : Boolean;

      procedure Cleanup;
      --  Cleanup helper to call before exitting Solve

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup is
      begin
         Reset_Vars (Ctx, Reset_Ids => True);
         Destroy (Ctx);
      end Cleanup;

   begin
      Ctx.Cut_Dead_Branches := Solve_Options.Cut_Dead_Branches;

      if Solver_Trace.Is_Active then
         Solver_Trace.Trace ("Solving equation:");
         Solver_Trace.Trace (Image (Self));
      end if;

      Ctx.Cb := Solution_Callback'Unrestricted_Access.all;
      Ctx.Atoms := new Atomic_Relation_Vector;
      Ctx.Aliases := new Atomic_Relation_Vector;

      case Self.Kind is
         when Compound =>
            Ignore := Solve_Compound (Self.Compound_Rel, Ctx);

         when Atomic =>
            --  If we're trying to eval a singleton relation that doesn't
            --  define anything, then it's an early binding error.
            if Used_Var (Self.Atomic_Rel).Exists then
               --  TODO??? This is incomplete or N_Preds, since they depend on
               --  more than one var. Not very important.
               raise Early_Binding_Error with
                 "Invalid equation " & Image (Self.Atomic_Rel)
                 & ": depends on undefined var "
                 & Image (Used_Var (Self.Atomic_Rel).Logic_Var);
            end if;

            if Defined_Var (Self.Atomic_Rel).Exists then
               --  Reset defined var if it exists, and solve
               Reset (Defined_Var (Self.Atomic_Rel).Logic_Var);
               if Solve_Atomic (Self.Atomic_Rel) then
                  Ignore := Solution_Callback
                    ((1 => Defined_Var (Self.Atomic_Rel).Logic_Var));
               end if;
            elsif Solve_Atomic (Self.Atomic_Rel) then
               --  Solve with empty vars array. TODO??? Maybe try to factor
               --  that code?
               Ignore := Solution_Callback ((1 .. 0 => <>));
            end if;

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

      type Var_Array_Access is access all Var_Array;
      type Val_Array_Access is access all Val_Array;

      Last_Vars : Var_Array_Access := null;
      Last_Vals : Val_Array_Access := null;

      procedure Free is new Ada.Unchecked_Deallocation
        (Var_Array, Var_Array_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Val_Array, Val_Array_Access);

      --------------
      -- Callback --
      --------------

      function Callback (Vars : Logic_Var_Array) return Boolean is
      begin
         Ret := True;
         Last_Vals := new Val_Array (Vars'Range);
         Last_Vars := new Var_Array'(Vars);
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
     (Logic_Var    : Var;
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
     (Logic_Vars   : Variable_Array;
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
     (Logic_Var    : Var;
      Value        : Value_Type;
      Conv         : Converter_Type'Class := No_Converter;
      Eq           : Comparer_Type'Class := No_Comparer;
      Debug_String : String_Access := null) return Relation
   is
      Conv_Ptr : Converter_Access := null;
      Eq_Ptr   : Comparer_Access := null;
      Ass      : Relation;
   begin
      if Conv /= No_Converter then
         Conv_Ptr := new Converter_Type'Class'(Conv);
      end if;

      if Eq /= No_Comparer then
         Eq_Ptr := new Comparer_Type'Class'(Eq);
      end if;

      Ass := To_Relation
        (Atomic_Relation'
           (Kind     => Assign,
            Can_Fail => False,
            Conv     => Conv_Ptr,
            Val      => Value,
            Target   => Logic_Var),
         Debug_String => Debug_String);

      if Eq_Ptr /= null then
         --  Here, the ownership shares we have for Conv_Ptr and Eq_Ptr are
         --  transferred to N_Pred, so there is nothing else to do ref-counting
         --  wise.

         declare
            N_Pred : Relation :=
              Create_Predicate (Logic_Var,
                                Comparer_Pred'(Ref_Count => 1,
                                               Eq        => Eq_Ptr,
                                               Conv      => Conv_Ptr,
                                               Val       => Value),
                                Debug_String);
            Ret    : constant Relation :=
              Create_All ((Ass, N_Pred), Debug_String => Debug_String);
         begin
            Ass.Debug_Info := Debug_String;
            Ass.Atomic_Rel.Can_Fail := True;
            Dec_Ref (N_Pred);
            Dec_Ref (Ass);
            return Ret;
         end;
      else
         return Ass;
      end if;
   end Create_Assign;

   ------------------
   -- Create_Unify --
   ------------------

   function Create_Unify
     (From, To     : Var;
      Debug_String : String_Access := null) return Relation is
   begin
      return To_Relation
        (Atomic_Relation'(Kind => Unify, Target => To, Unify_From => From),
         Debug_String => Debug_String);
   end Create_Unify;

   ----------------------
   -- Create_Propagate --
   ----------------------

   function Create_Propagate
     (From, To     : Var;
      Conv         : Converter_Access := null;
      Eq           : Comparer_Access := null;
      Debug_String : String_Access := null) return Relation
   is
      Propag : Relation :=
        To_Relation
          (Atomic_Relation'
             (Kind     => Propagate,
              Can_Fail => False,
              Conv     => Conv,
              From     => From,
              Target   => To),
         Debug_String => Debug_String);
   begin
      if Eq /= null then
         declare
            N_Pred : Relation :=
              Create_N_Predicate ((From, To),
                                  Comparer_N_Pred'(Ref_Count => 1,
                                                   Eq        => Eq,
                                                   Conv      => Conv),
                                  Debug_String);
            Ret    : constant Relation := Create_All
              ((Propag, N_Pred), Debug_String => Debug_String);
         begin
            Propag.Debug_Info := Debug_String;
            Propag.Atomic_Rel.Can_Fail := True;
            Dec_Ref (N_Pred);
            Dec_Ref (Propag);
            return Ret;
         end;
      else
         return Propag;
      end if;
   end Create_Propagate;

   ----------------------
   -- Create_Propagate --
   ----------------------

   function Create_Propagate
     (From, To     : Var;
      Conv         : Converter_Type'Class := No_Converter;
      Eq           : Comparer_Type'Class := No_Comparer;
      Debug_String : String_Access := null) return Relation
   is
      Conv_Ptr : Converter_Access := null;
      Eq_Ptr   : Comparer_Access := null;
   begin
      if Conv /= No_Converter then
         Conv_Ptr := new Converter_Type'Class'(Conv);
      end if;
      if Eq /= No_Comparer then
         Eq_Ptr := new Comparer_Type'Class'(Eq);
      end if;

      return Create_Propagate
        (From, To, Conv_Ptr, Eq_Ptr, Debug_String => Debug_String);
   end Create_Propagate;

   -------------------
   -- Create_Domain --
   -------------------

   function Create_Domain
     (Logic_Var    : Var;
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

         elsif Cmp_Kind = Kind_All
               and then R.Kind = Atomic
               and then R.Atomic_Rel.Kind = True
         then
            --  Remove True relations from Alls
            null;

         elsif Cmp_Kind = Kind_Any
               and then R.Kind = Atomic
               and then R.Atomic_Rel.Kind = False
         then
            --  Remove False relations from Anys
            null;

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

      --  If this is a compound relation with only one sub-relation, then it's
      --  equivalent to the sub-relation. Inline the sub-relation.
      if Rels.Length = 1 then
         return Ret : constant Relation := Rels.Get (1) do
            Rels.Destroy;
         end return;
      end if;

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
      --  Tries to assign ``Val`` to ``Self.Target`` return True either if
      --  ``Self.Target`` already has a value compatible with ``Val``, or if
      --  it had no value and the assignment succeeded.

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
      --  If the value for self is not defined, raise an error.
      --  TODO??? This is a bit strange because it will never happen when
      --  called from ``Solve_Compound``, which will do a topological sort
      --  first and will stop if there is an orphan relation, eg a relation
      --  that uses something that is never defined, so maybe we should unify
      --  those two mechanisms, and most importantly make them fail in the same
      --  way.
      if not Is_Defined_Or_Null (Used_Var (Self)) then
         raise Early_Binding_Error with
           "Relation " & Image (Self)
           & " needs var " & Image (Used_Var (Self).Logic_Var)
           & " to be defined";
      end if;

      case Self.Kind is
         when Assign =>
            Ret := Assign_Val (Self.Val);
            Ret := Ret or else Self.Can_Fail;

         when Propagate =>
            pragma Assert (Is_Defined (Self.From));
            Ret := Assign_Val (Get_Value (Self.From));
            Ret := Ret or else Self.Can_Fail;

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
                  Solv_Trace.Trace ("Var = " & Element_Image (Get_Value (V)));
               end if;
            end loop;

            declare
               Vals : Val_Array (1 .. Self.Vars.Length);
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

      function Left_Image (Left : String) return String
      is
        (if Self.Conv /= null
         then Self.Conv.Image & "(" & Left & ")"
         else Left);

      function Prop_Image
        (Left, Right : String; Can_Fail : Boolean := False) return String
      is
        (Left_Image (Left) & (if Can_Fail then " -?> " else " -> ") & Right);
   begin
      case Self.Kind is
         when Propagate =>
            return Prop_Image
              (Image (Self.From), Image (Self.Target), Self.Can_Fail);

         when Assign =>
            return Prop_Image
              (Logic_Vars.Element_Image (Self.Val), Image (Self.Target),
               Self.Can_Fail);

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
                 Self.N_Pred.Full_Image (Var_Array (Self.Vars.To_Array));
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

   function Image (Self : Any_Relation_Lists.List) return String is

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
