with Langkit_Support.Functional_Lists;
with Langkit_Support.Images;
with GNATCOLL.Strings; use GNATCOLL.Strings;
with Ada.Assertions; use Ada.Assertions;

pragma Style_Checks ("-s");

package body Langkit_Support.Adalog.Solver is

   -----------------------
   --  Supporting types --
   -----------------------

   package Atomic_Relation_Vectors
   is new Langkit_Support.Vectors (Atomic_Relation);
   subtype Atomic_Relation_Vector is Atomic_Relation_Vectors.Vector;
   --  Vectors of atomic relations

   function Image
   is new Langkit_Support.Images.Array_Image
     (Atomic_Relation,
      Positive,
      Atomic_Relation_Vectors.Elements_Array);
   function Image (Self : Atomic_Relation_Vector) return String;

   subtype Any_Rel is Compound_Relation
     with Predicate => Any_Rel.Kind = Kind_Any;
   --  Helper subtype. Allows us to check that we only have any relations in
   --  Any_Relation_Vectors.Vector.

   package Any_Relation_Lists
   is new Langkit_Support.Functional_Lists (Any_Rel);
   subtype Any_Relation_List is Any_Relation_Lists.List;
   --  Vectors of Anys
   function Image (Self : Any_Relation_Lists.List) return String;

   package Var_Ids_To_Atoms_Vectors
   is new Langkit_Support.Vectors (Atomic_Relation_Vector);
   subtype Var_Ids_To_Atoms is Var_Ids_To_Atoms_Vectors.Vector;
   --  Vector mapping logic var ids to atomic relations.

   --------------------------
   -- Supporting functions --
   --------------------------

   procedure Reserve (V : in out Var_Ids_To_Atoms; Size : Positive);
   --  Reserve ``N`` elements in ``V``. TODO ??? Add that to vectors.

   function Create_Propagate
     (From, To : Var;
      Conv      : Converter_Access := null;
      Eq        : Comparer_Access := null) return Relation;
   --  Helper function to create a Propagate relation

   function Create_Compound
     (Relations : Relation_Array; Cmp_Kind : Compound_Kind) return Relation;
   --  Helper to create a compound relationship.

   type Callback_Type is access function (Vars : Var_Array) return Boolean;

   type Solving_Context is record
      Cb                : Callback_Type;
      Atoms             : Atomic_Relation_Vector;
      Anys              : Any_Relation_List := Any_Relation_Lists.No_List;
      Vars              : Logic_Var_Vector_Access;
      Vars_To_Atoms     : Var_Ids_To_Atoms;
      Cut_Dead_Branches : Boolean := False;
   end record;
   --  Context for the solving of a compound relation.

   function Create return Solving_Context;
   procedure Destroy (Ctx : in out Solving_Context);
   function Get_Id
     (Ctx : Solving_Context; Logic_Var : Var) return Positive;
   --  Get the id of variable ``Logic_Var`` in ``Ctx``.

   procedure Assign_Ids (Ctx : Solving_Context; Atom : Atomic_Relation);
   procedure Reset_Vars (Ctx : Solving_Context; Reset_Ids : Boolean := False);

   ----------------------------------
   --  Stateless functors wrappers --
   ----------------------------------

   type Predicate_Fn is access function (V : Value_Type) return Boolean;
   type Converter_Fn is access function (V : Value_Type) return Value_Type;
   type N_Predicate_Fn is access function (Vs : Value_Array) return Boolean;
   type Comparer_Fn is access function (L, R : Value_Type) return Boolean;

   type Predicate_Fn_Wrapper is new Predicate_Type with record
      Callback : Predicate_Fn;
      Name     : XString;
   end record;

   function Call (Self : Predicate_Fn_Wrapper; Val : Value_Type) return Boolean
   is (Self.Callback (Val));

   function Image (Self : Predicate_Fn_Wrapper) return String
   is (Self.Name.To_String);

   type N_Predicate_Fn_Wrapper is new N_Predicate_Type with record
      Callback : N_Predicate_Fn;
      Name     : XString;
   end record;

   function Call
     (Self : N_Predicate_Fn_Wrapper; Vs : Value_Array) return Boolean
   is (Self.Callback (Vs));

   function Image (Self : N_Predicate_Fn_Wrapper) return String
   is (Self.Name.To_String);

   type Converter_Wrapper is new Converter_Type with record
      Callback : Converter_Fn;
      Name     : XString;
   end record;

   function Convert
     (Self : Converter_Wrapper; Val : Value_Type) return Value_Type is
     (Self.Callback (Val));

   function Image (Self : Converter_Wrapper) return String
   is (Self.Name.To_String);

   type Comparer_Wrapper is new Comparer_Type with record
      Callback : Comparer_Fn;
      Name     : XString;
   end record;

   function Compare
     (Self : Comparer_Wrapper; L, R : Value_Type) return Boolean is
     (Self.Callback (L, R));

   function Image (Self : Comparer_Wrapper) return String
   is (Self.Name.To_String);

   ----------------
   -- Reset_Vars --
   ----------------

   procedure Reset_Vars (Ctx : Solving_Context; Reset_Ids : Boolean := False)
   is
   begin
      for V of Ctx.Vars.all loop
         Reset (V);
         if Reset_Ids then
            Set_Id (V, 0);
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
      end return;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Ctx : in out Solving_Context) is
   begin
      Ctx.Atoms.Destroy;
      Any_Relation_Lists.Destroy (Ctx.Anys);
      Ctx.Vars.Destroy;
      Ctx.Vars_To_Atoms.Destroy;
   end Destroy;

   function Solve_Compound
     (Self : Compound_Relation;
      Ctx  : Solving_Context) return Boolean;

   -------------
   -- Reserve --
   -------------

   procedure Reserve (V : in out Var_Ids_To_Atoms; Size : Positive) is
   begin
      while V.Length < Size loop
         V.Append (Atomic_Relation_Vectors.Empty_Vector);
      end loop;
   end Reserve;

   ------------
   -- Get_Id --
   ------------

   function Get_Id
     (Ctx : Solving_Context; Logic_Var : Var) return Positive
   is
   begin
      if Id (Logic_Var) = 0 then
         Ctx.Vars.Append (Logic_Var);
         Set_Id (Logic_Var, Ctx.Vars.Last_Index);
      end if;
      return Id (Logic_Var);
   end Get_Id;

   ----------------
   -- Assign_Ids --
   ----------------

   procedure Assign_Ids (Ctx : Solving_Context; Atom : Atomic_Relation) is
      procedure Assign_Id (Ctx : Solving_Context; Var : Var_Or_Null);
      procedure Assign_Id (Ctx : Solving_Context; Var : Var_Or_Null) is
         Dummy : Positive;
      begin
         if Var.Exists then
            Dummy := Get_Id (Ctx, Var.Logic_Var);
         end if;
      end Assign_Id;
   begin
      Assign_Id (Ctx, Defined_Var (Atom));
      Assign_Id (Ctx, Used_Var (Atom));
   end Assign_Ids;

   --------------
   -- Used_Var --
   --------------

   function Used_Var (Self : Atomic_Relation) return Var_Or_Null
   is
      --  We handle Unify here, even though it is not strictly treated in the
      --  dependency graph, so that the Unify_From variable is registered in
      --  the list of variables of the equation. TODO ??? Might be cleaner to
      --  have a separate function to return all variables a relation uses ?
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
      --  the list of variables of the equation. TODO ??? Might be cleaner to
      --  have a separate function to return all variables a relation uses ?
     (case Self.Kind is
         when Assign | Propagate | Unify => (True, Self.Target),
         when Predicate | True | False | N_Predicate => Null_Var);

   -----------------
   -- To_Relation --
   -----------------

   function To_Relation
     (Inner : Atomic_Relation) return Relation
   is
      (new Relation_Type'(Atomic, Atomic_Rel => Inner, Ref_Count => <>));

   -----------------
   -- To_Relation --
   -----------------

   function To_Relation
     (Inner : Compound_Relation) return Relation
   is
    (new Relation_Type'(Compound, Compound_Rel => Inner, Ref_Count => <>));

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : Relation) is
   begin
      Self.Ref_Count := Self.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Relation) is
      procedure Unchecked_Free
      is new Ada.Unchecked_Deallocation (Relation_Type, Relation);
   begin
      if Self = null then
         return;
      end if;

      Self.Ref_Count := Self.Ref_Count - 1;
      if Self.Ref_Count = 0 then
         Destroy (Self);
         Unchecked_Free (Self);
      end if;
      Self := null;
   end Dec_Ref;

   --------------------
   -- Solve_Compound --
   --------------------

   function Solve_Compound
     (Self : Compound_Relation;
      Ctx  : Solving_Context) return Boolean
   is
      function Topo_Sort
        (Atoms : in out Atomic_Relation_Vector) return Boolean;
      function Try_Solution
        (Atoms : in out Atomic_Relation_Vector) return Boolean;
      --  Try to solve the current solution.

      use Any_Relation_Lists;

      ---------------
      -- Topo_Sort --
      ---------------

      function Topo_Sort
        (Atoms : in out Atomic_Relation_Vector) return Boolean
      is

         type Atom_And_Index is record
            Atom       : Atomic_Relation;
            Atom_Index : Positive;
         end record;

         package Atom_Lists
         is new Langkit_Support.Functional_Lists (Atom_And_Index);

         Old_Atoms      : constant Atomic_Relation_Vectors.Elements_Array
           := Atoms.To_Array;
         Appended       : array (Old_Atoms'Range) of Boolean
           := (others => False);

         Using_Atoms : array (1 .. Ctx.Vars.Length) of Atom_Lists.List
           := (others => Atom_Lists.Create);

         Used_Id        : Natural;
         Working_Set    : Atom_Lists.List := Atom_Lists.Create;
         Current_Atom   : Atomic_Relation;
         use Atom_Lists;

         function Id (S : Var_Or_Null) return Natural is
           (if S.Exists then Get_Id (Ctx, S.Logic_Var) else 0);

         function Defined (S : Atomic_Relation) return Natural
         is (Id (Defined_Var (S)));
      begin
         Atoms.Clear;

         --  TODO ??? This pass is probably inefficient. We should alias unify
         --  variables when we are first appending them to the atoms queue.
         for Atom of Old_Atoms loop
            if Atom.Kind = Unify then
               Solver_Trace.Trace ("Aliasing var " & Image (Atom.Unify_From)
                                   & " to " & Image (Atom.Target));
               Alias (Atom.Unify_From, Atom.Target);
            end if;
         end loop;

         --  Step 1: create a map of vars to all atoms that defines them.
         for I in Old_Atoms'Range loop
            Current_Atom := Old_Atoms (I);
            declare
               Used_Logic_Var : constant Var_Or_Null :=
                 Used_Var (Current_Atom);
            begin
               if Used_Logic_Var.Exists then

                  --  Resolve the Id of the var used. If the var aliases to
                  --  another var, resolve to the aliased var's Id.
                  Used_Id :=
                    (if Get_Alias (Used_Logic_Var.Logic_Var) /= No_Var
                     then Get_Id (Ctx, Get_Alias (Used_Logic_Var.Logic_Var))
                     else Get_Id (Ctx, Used_Logic_Var.Logic_Var));
                  Solver_Trace.Trace (Image (Used_Logic_Var.Logic_Var)
                                      & " Id = " & Used_Id'Image);
               else
                  Used_Id := 0;
               end if;

               if Current_Atom.Kind = Unify then
                  Appended (I) := True;
               elsif Used_Id = 0 then
                  Working_Set := (Current_Atom, I) & Working_Set;
               else
                  Using_Atoms (Used_Id) :=
                    (Current_Atom, I) & Using_Atoms (Used_Id);
               end if;
            end;
         end loop;

         --  Step 2: Do the topo sort
         while Has_Element (Working_Set) loop
            declare
               Atom : constant Atom_And_Index := Head (Working_Set);
            begin
               Working_Set := Tail (Working_Set);
               Atoms.Append (Atom.Atom);
               Appended (Atom.Atom_Index) := True;
               if Defined (Atom.Atom) /= 0 then
                  for El of Using_Atoms (Defined (Atom.Atom)) loop
                     Working_Set := El & Working_Set;
                  end loop;
               end if;
            end;
         end loop;

         for I in Appended'Range loop
            if not Appended (I) then

               Solver_Trace.Trace
                 ("Orphan relation: " & Image (Old_Atoms (I)));
               return False;
            end if;
         end loop;

         return True;

      end Topo_Sort;

      ------------------
      -- Try_Solution --
      ------------------

      function Try_Solution
        (Atoms : in out Atomic_Relation_Vector) return Boolean
      is
         function Cleanup (Val : Boolean) return Boolean with Inline_Always
         is begin
            Solver_Trace.Decrease_Indent;
            Reset_Vars (Ctx);
            return Val;
         end Cleanup;
      begin
         Solver_Trace.Increase_Indent ("In try solution");
         Solver_Trace.Trace (Image (Atoms));

         if not Topo_Sort (Atoms) then
            return Cleanup (True);
         end if;

         Solver_Trace.Trace ("After topo sort");
         Solver_Trace.Trace (Image (Atoms));

         for Atom of Atoms loop
            if not Solve (Atom) then
               return Cleanup (True);
            end if;
         end loop;

         return Cleanup (Ctx.Cb (Var_Array (Ctx.Vars.To_Array)));
      end Try_Solution;

      V             : Var_Or_Null;
      Id            : Positive;
      Ignore        : Boolean;
      Vars_To_Atoms : Var_Ids_To_Atoms := Ctx.Vars_To_Atoms.Copy;
      Atoms         : Atomic_Relation_Vector := Ctx.Atoms.Copy;
      Anys          : Any_Relation_List := Ctx.Anys;

      function Cleanup (Val : Boolean) return Boolean with Inline_Always
      is begin
         Vars_To_Atoms.Destroy;
         Atoms.Destroy;
         Solver_Trace.Decrease_Indent;
         return Val;
      end Cleanup;

   begin
      Solver_Trace.Increase_Indent ("In Solve_Compound " & Self.Kind'Image);
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
         --  First step: gather anys and atoms in their own vectors.

         for Sub_Rel of Self.Rels loop
            case Sub_Rel.Kind is
            when Compound =>
               --  Implicit assertion: an all can only contain an Any
               Anys := Sub_Rel.Compound_Rel & Anys;
            when Atomic =>
               Assign_Ids (Ctx, Sub_Rel.Atomic_Rel);
               Atoms.Append (Sub_Rel.Atomic_Rel);

               if Ctx.Cut_Dead_Branches then
                  --  Exponential resolution optimization: If relevant, add
                  --  atomic relation to the mappings of vars to atoms.
                  case Sub_Rel.Atomic_Rel.Kind is
                  when Predicate | Assign =>
                     V := (if Sub_Rel.Atomic_Rel.Kind = Predicate
                           then Used_Var (Sub_Rel.Atomic_Rel)
                           else Defined_Var (Sub_Rel.Atomic_Rel));
                     Id := Get_Id (Ctx, V.Logic_Var);
                     Reserve (Vars_To_Atoms, Id);
                     Vars_To_Atoms.Get_Access (Id)
                       .Append (Sub_Rel.Atomic_Rel);
                  when others => null;
                  end case;
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
            --  1. Either not activate this opt for certain trees
            --
            --  2. Either try to check only for new atoms. This seems
            --  hard/impossible since new constraints are added at every
            --  recursion, so old atoms need to be checked again for
            --  completeness. But maybe there is a way. Investigate later.

            for Atom of Atoms loop
               if Atom.Kind = Assign then
                  V := Defined_Var (Atom);
                  Id := Get_Id (Ctx, V.Logic_Var);
                  if Vars_To_Atoms.Get (Id).Length > 0 then
                     --  We have some relations that apply on this variable.
                     --  Call the assign atom, then see if the relations solve.
                     Ignore := Solve (Atom);
                     for User of Vars_To_Atoms.Get (Id) loop

                        --  If applying a predicate fails, then we exit the
                        --  solving of this branch early.

                        if not Solve (User) then
                           Reset (V.Logic_Var);
                           return Cleanup (True);
                        end if;
                     end loop;

                     --  Else, reset the value of var for further solving.
                     Reset (V.Logic_Var);
                  end if;
               end if;
            end loop;
         end if;

         if Length (Anys) = 0 then
            --  We don't have any Any relation left: We have a complete
            --  potential solution. Try to solve it.
            return Cleanup (Try_Solution (Atoms));
         else
            Solver_Trace.Trace ("Before recursing in solve All");
            Solver_Trace.Trace (Image (Atoms));
            Solver_Trace.Trace (Image (Anys));

            return Cleanup
              (Solve_Compound
                 (Head (Anys),
                  Ctx'Update
                    (Vars_To_Atoms => Vars_To_Atoms,
                     Anys          => Tail (Anys),
                     Atoms         => Atoms)));
         end if;

      when Kind_Any =>
         for Sub_Rel of Self.Rels loop
            case Sub_Rel.Kind is
               when Atomic =>
                  declare
                     Atoms : Atomic_Relation_Vector := Ctx.Atoms.Copy;
                  begin
                     Atoms.Append (Sub_Rel.Atomic_Rel);
                     Assign_Ids (Ctx, Sub_Rel.Atomic_Rel);
                     if Length (Ctx.Anys) > 0 then
                        if not
                          Solve_Compound
                            (Head (Anys),
                             Ctx'Update
                               (Anys => Tail (Anys), Atoms => Atoms))
                        then
                           Atoms.Destroy;
                           return Cleanup (False);
                        end if;
                     else
                        if not Try_Solution (Atoms) then
                           Atoms.Destroy;
                           return Cleanup (False);
                        end if;
                     end if;
                     Atoms.Destroy;
                  end;
               when Compound =>
                  pragma Assert (Sub_Rel.Compound_Rel.Kind = Kind_All);
                  if not Solve_Compound (Sub_Rel.Compound_Rel, Ctx) then
                     return Cleanup (False);
                  end if;
            end case;
         end loop;

         return Cleanup (True);
      end case;

   end Solve_Compound;

   -----------
   -- Solve --
   -----------

   procedure Solve
     (Self              : Relation;
      Solution_Callback : access function
        (Vars : Logic_Var_Array) return Boolean)
   is
      Ctx    : Solving_Context := Create;
      Ignore : Boolean;
   begin
      Solver_Trace.Trace ("Solving equation:");
      Solver_Trace.Trace (Image (Self));

      Ctx.Cb := Solution_Callback'Unrestricted_Access.all;
      case Self.Kind is
         when Compound =>
            Ignore := Solve_Compound (Self.Compound_Rel, Ctx);
            Reset_Vars (Ctx, Reset_Ids => True);
         when Atomic =>
            if Solve (Self.Atomic_Rel) then
               Ignore := Solution_Callback
                 ((1 => Defined_Var (Self.Atomic_Rel).Logic_Var));
            end if;
      end case;

      Destroy (Ctx);
   end Solve;

   -----------
   -- Solve --
   -----------

   procedure Solve
     (Self              : Relation;
      Solution_Callback : access function return Boolean)
   is
      function Internal_Callback (Dummy : Var_Array) return Boolean is
      begin
         return Solution_Callback.all;
      end Internal_Callback;
   begin
      Solve (Self, Internal_Callback'Unrestricted_Access);
   end Solve;

   -----------------
   -- Solve_First --
   -----------------

   function Solve_First (Self : Relation) return Boolean is

      Ret : Boolean := False;

      function Callback return Boolean;
      --  Simple callback that will stop on first solution.

      --------------
      -- Callback --
      --------------

      function Callback return Boolean is
      begin
         Ret := True;
         return False;
      end Callback;
   begin
      Solve (Self, Callback'Access);
      return Ret;
   end Solve_First;

   -----------------
   -- Create_True --
   -----------------

   function Create_True return Relation is
     (To_Relation (Atomic_Relation'(Kind => True, Target => <>)));

   ------------------
   -- Create_False --
   ------------------

   function Create_False return Relation is
     (To_Relation (Atomic_Relation'(Kind => False, Target => <>)));

   ----------------------
   -- Create_Predicate --
   ----------------------

   function Create_Predicate
     (Logic_Var : Var; Pred : Predicate_Type'Class) return Relation
   is
   begin
      return To_Relation
        (Atomic_Relation'
           (Kind   => Predicate,
            Target => Logic_Var,
            Pred   => new Predicate_Type'Class'(Pred)));
   end Create_Predicate;

   ----------------------
   -- Create_Predicate --
   ----------------------

   function Create_N_Predicate
     (Logic_Vars : Variable_Array;
      Pred       : N_Predicate_Type'Class) return Relation
   is
      Vars_Vec : Logic_Var_Vector;
   begin
      Vars_Vec.Concat (Logic_Var_Vectors.Elements_Array (Logic_Vars));
      return To_Relation
        (Atomic_Relation'
           (Kind   => N_Predicate,
            N_Pred => new N_Predicate_Type'Class'(Pred),
            Vars   => Vars_Vec,
            Target => <>));
   end Create_N_Predicate;

   ---------------
   -- Predicate --
   ---------------

   function Predicate
     (Pred      : access function (V : Value_Type) return Boolean;
      Pred_Name : String := "Predicate") return Predicate_Type'Class
   is
   begin
      return Predicate_Fn_Wrapper'(Pred'Unrestricted_Access.all,
                                   To_XString (Pred_Name));
   end Predicate;

   -----------------
   -- N_Predicate --
   -----------------

   function N_Predicate
     (Pred       : access function (V : Value_Array) return Boolean;
      Pred_Name  : String := "N_Predicate")
      return N_Predicate_Type'Class
   is
   begin
      return N_Predicate_Fn_Wrapper'(Pred'Unrestricted_Access.all,
                                     To_XString (Pred_Name));
   end N_Predicate;

   --------------
   -- Comparer --
   --------------

   function Comparer
     (Pred : access function (L, R : Value_Type) return Boolean;
      Pred_Name : String := "Comparer") return Comparer_Type'Class
   is
   begin
      return Comparer_Wrapper'(Pred'Unrestricted_Access.all,
                               To_XString (Pred_Name));
   end Comparer;

   ---------------
   -- Converter --
   ---------------

   function Converter
     (Pred : access function (V : Value_Type) return Value_Type;
      Pred_Name : String := "Converter") return Converter_Type'Class
   is
   begin
      return Converter_Wrapper'(Pred'Unrestricted_Access.all,
                               To_XString (Pred_Name));
   end Converter;

   -------------------
   -- Create_Assign --
   -------------------

   function Create_Assign
     (Logic_Var : Var;
      Value     : Value_Type;
      Conv      : Converter_Type'Class := No_Converter;
      Eq        : Comparer_Type'Class := No_Comparer) return Relation
   is
   begin
      return Rel : constant Relation := To_Relation
        (Atomic_Relation'
           (Kind   => Assign,
            Conv   => null,
            Eq     => null,
            Val    => Value,
            Target => Logic_Var))
      do
         if Conv /= No_Converter then
            Rel.Atomic_Rel.Conv := new Converter_Type'Class'(Conv);
         end if;
         if Eq /= No_Comparer then
            Rel.Atomic_Rel.Eq := new Comparer_Type'Class'(Eq);
         end if;
      end return;
   end Create_Assign;

   ------------------
   -- Create_Unify --
   ------------------

   function Create_Unify (From, To : Var) return Relation is
   begin
      return To_Relation
        (Atomic_Relation'
           (Kind => Unify, Target => To, Unify_From => From));
   end Create_Unify;

   ----------------------
   -- Create_Propagate --
   ----------------------

   function Create_Propagate
     (From, To  : Var;
      Conv      : Converter_Access := null;
      Eq        : Comparer_Access := null) return Relation
   is
   begin
      return To_Relation
        (Atomic_Relation'
           (Kind   => Propagate,
            Conv   => Conv,
            Eq     => Eq,
            From   => From,
            Target => To));
   end Create_Propagate;

   ----------------------
   -- Create_Propagate --
   ----------------------

   function Create_Propagate
     (From, To  : Var;
      Conv      : Converter_Type'Class := No_Converter;
      Eq        : Comparer_Type'Class := No_Comparer) return Relation
   is
   begin
      return Rel : constant Relation := Create_Propagate
        (From, To, null, null)
      do
         if Conv /= No_Converter then
            Rel.Atomic_Rel.Conv :=  new Converter_Type'Class'(Conv);
         end if;
         if Eq /= No_Comparer then
            Rel.Atomic_Rel.Eq := new Comparer_Type'Class'(Eq);
         end if;
      end return;
   end Create_Propagate;

   -------------------
   -- Create_Domain --
   -------------------

   function Create_Domain
     (Logic_Var : Var; Domain : Value_Array) return Relation
   is
      Rels : Relation_Array (Domain'Range);
   begin
      for I in Domain'Range loop
         Rels (I) := Create_Assign (Logic_Var, Domain (I));
      end loop;

      return Create_Any (Rels);
   end Create_Domain;

   ---------------------
   -- Create_Compound --
   ---------------------

   function Create_Compound
     (Relations : Relation_Array; Cmp_Kind : Compound_Kind) return Relation
   is
      Rels : Relation_Vectors.Vector;

      procedure Append (R : Relation);
      --  If ``R`` is an All, inline its relations inside ``Rels``. Else, just
      --  append ``R`` to Rels``.

      procedure Append (R : Relation) is
      begin
         if R.Kind = Compound and then R.Compound_Rel.Kind = Cmp_Kind then
            for El of R.Compound_Rel.Rels loop
               Append (El);
            end loop;
         else
            Inc_Ref (R);
            Rels.Append (R);
         end if;
      end Append;
   begin
      for El of Relations loop
         Append (El);
      end loop;

      return To_Relation (Compound_Relation'(Cmp_Kind, Rels));
   end Create_Compound;

   ----------------
   -- Create_Any --
   ----------------

   function Create_Any (Relations : Relation_Array) return Relation is
     (Create_Compound (Relations, Kind_Any));

   ----------------
   -- Create_All --
   ----------------

   function Create_All (Relations : Relation_Array) return Relation is
     (Create_Compound (Relations, Kind_All));

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Atomic_Relation) is
   begin
      case Self.Kind is
         when Assign | Propagate =>
            Free (Self.Conv);
            Free (Self.Eq);
         when Predicate =>
            Free (Self.Pred);
         when N_Predicate =>
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
         when Atomic => Destroy (Self.Atomic_Rel);
         when Compound => Destroy (Self.Compound_Rel);
      end case;
   end Destroy;

   -----------
   -- Solve --
   -----------

   function Solve (Self : Atomic_Relation) return Boolean is
      function Assign_Val (Val : Value_Type) return Boolean;
      --  Tries to assign ``Val`` to ``Self.Target`` return True either if
      --  ``Self.Target`` already has a value compatible with ``Val``, or if
      --  it had no value and the assignment succeeded.

      ----------------
      -- Assign_Val --
      ----------------

      function Assign_Val (Val : Value_Type) return Boolean is
         Conv_Val : constant Value_Type :=
           (if Self.Conv /= null then Self.Conv.Convert (Val)
            else Val);
      begin
         if Is_Defined (Self.Target) then
            declare
               Var_Val : Value_Type renames Get_Value (Self.Target);
            begin
               return (Self.Eq /= null
                       and then Self.Eq.Compare (Conv_Val, Var_Val))
                 or else Conv_Val = Var_Val;
            end;
         else
            Set_Value (Self.Target, Conv_Val);
            return True;
         end if;
      end Assign_Val;

   begin
      case Self.Kind is
         when Assign =>
            return Assign_Val (Self.Val);

            --  TODO ??? Maybe the Is_Defined checks in the two branches below
            --  are not necessary, and the topo sort is enough.
         when Propagate =>
            pragma Assert (Is_Defined (Self.From));
            return Assign_Val (Get_Value (Self.From));
         when Predicate =>
            pragma Assert (Is_Defined (Self.From));
            return Self.Pred.Call (Get_Value (Self.Target));
         when N_Predicate =>
            pragma Assert (for all V of Self.Vars => Is_Defined (V));
            declare
               Vals : Val_Array (1 .. Self.Vars.Length);
            begin
               for I in Self.Vars.First_Index .. Self.Vars.Last_Index loop
                  Vals (I) := Get_Value (Self.Vars.Get (I));
               end loop;
               return Self.N_Pred.Call (Vals);
            end;
         when True => return True;
         when False => return False;
         when Unify => raise Assertion_Error with "Should never happen";
      end case;
   end Solve;

   -----------
   -- Image --
   -----------

   function Image (Self : Atomic_Relation) return String
   is
      function Var_Image (V : Var) return String is ("%" & Image (V));
      function Left_Image (Left : String) return String is
        (if Self.Conv /= null
            then Self.Conv.Image & "(" & Left & ")"
         else Left);
      function Prop_Image (Left, Right : String) return String is
        (if Self.Eq /= null
         then Self.Eq.Image & "->(" & Left_Image (Left) & ", " & Right & ")"
         else Left_Image (Left) & " -> " & Right);
   begin
      case Self.Kind is
         when Propagate =>
            return Prop_Image (Var_Image (Self.From), Var_Image (Self.Target));
         when Assign =>
            return Prop_Image
              (Logic_Vars.Element_Image (Self.Val), Var_Image (Self.Target));
         when Predicate =>
            return Self.Pred.Image & "?(" & Var_Image (Self.Target) & ")";
         when N_Predicate =>
            declare
               Vars_Image : XString_Array (1 .. Self.Vars.Length);
            begin
               for I in Vars_Image'Range loop
                  Vars_Image (I) := To_XString (Var_Image (Self.Vars.Get (I)));
               end loop;
               return Self.N_Pred.Image
                 & "?(" & To_XString (", ").Join (Vars_Image).To_String & ")";
            end;
         when True => return "True";
         when False => return "False";
         when Unify =>
            return Var_Image (Self.Unify_From)
              & " <-> " & Var_Image (Self.Target);
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
      function Anys_Array_Image
      is new Langkit_Support.Images.Array_Image
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
     (Self : Compound_Relation; Level : Natural := 0) return String
   is
      Ret : XString;
   begin
      case Self.Kind is
         when Kind_All =>
            Ret.Append ("All:" & ASCII.LF);
         when Kind_Any =>
            Ret.Append ("Any:" & ASCII.LF);
      end case;

      for Rel of Self.Rels loop
         Ret.Append ((1 .. Level + 4 => ' ')
                     & Image (Rel, Level + 4) & ASCII.LF);
      end loop;

      return Ret.To_String;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Self : Relation; Level : Natural := 0) return String is
   begin
      case Self.Kind is
         when Compound => return Image (Self.Compound_Rel, Level);
         when Atomic => return Image (Self.Atomic_Rel);
      end case;
   end Image;

   ------------------
   -- Stub implems --
   ------------------

   type No_Comparer_Type is new Comparer_Type with null record;
   function Compare
     (Dummy            : No_Comparer_Type;
      Dummy_L, Dummy_R : Value_Type) return Boolean
   is (False);

   type No_Converter_Type is new Converter_Type with null record;
   function Convert
     (Dummy : No_Converter_Type; Dummy_From : Value_Type) return Value_Type
   is (raise Program_Error);

   function No_Comparer return Comparer_Type'Class
   is (No_Comparer_Type'(null record));

   function No_Converter return Converter_Type'Class
   is (No_Converter_Type'(null record));

end Langkit_Support.Adalog.Solver;
