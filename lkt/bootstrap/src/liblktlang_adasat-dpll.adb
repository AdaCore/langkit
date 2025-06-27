--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Unchecked_Deallocation;

with Liblktlang_AdaSAT.Vectors;
with Liblktlang_AdaSAT.Internals; use Liblktlang_AdaSAT.Internals;

package body Liblktlang_AdaSAT.DPLL is
   type Decision_Array is array (Variable_Or_Null range <>) of Natural;
   type Decision_Array_Access is access Decision_Array;

   type Antecedent_Array is array (Variable_Or_Null range <>) of Clause;
   type Antecedent_Array_Access is access Antecedent_Array;

   type Literal_Mask is array (Literal range <>) of Boolean;
   type Literal_Mask_Access is access Literal_Mask;

   type Watcher is record
      Blit     : Literal;
      Other    : Literal;
      Literals : Clause;
   end record;
   --  A wrapper around a clause, used as value type in an internal formula's
   --  ``Occurs_List``. The ``Blit`` and ``Other`` literals serve different
   --  purposes, depending on whether they have a non-0 value or not:
   --  1. If both ``Blit`` and ``Other`` are non-0, this is a binary clause
   --     (so, we have that ``Literals'Length = 2``), and these two literals
   --     are the two literals of the underlying clause. This means that we
   --     never need to dereference the clause when doing unit propagation on
   --     binary clauses.
   --
   --  2. If both ``Blit`` and ``Other`` are 0, this is an "At-Most-One"
   --     constraint. In that case, the second and third literals of the
   --     underlying clause give the range of the constraint.
   --
   --  3. If ``Blit`` is non-0 but ``Other`` is 0, this is a "normal" clause,
   --     i.e. neither a binary clause nor an AMO clause. In that case,
   --     ``Blit`` is used as a blocking literal. A blocking literal is an
   --     arbitrary literal extracted from the underlying clause which
   --     we use to skip unit propagation of a clause early - before we need
   --     to deference the actual clause - in case it evaluates to True.
   --     See ``Unit_Propagate`` for more information.

   type Watcher_Array is array (Positive range <>) of Watcher;

   package Watcher_Vectors is new Vectors (Watcher, Watcher_Array);

   type Literal_To_Watcher_Map is array (Literal range <>) of
      aliased Watcher_Vectors.Vector;

   type Internal_Formula (First, Last : Literal) is record
      Clauses     : Clause_Vectors.Vector;
      Occurs_List : Literal_To_Watcher_Map (First .. Last);
   end record;
   --  An internal formula is the internal representation of a formula,
   --  which uses a vector for clauses so as to easily append new ones.
   --  It also maintaints some data structures around the clauses such as
   --  the occurrence list, to optimize the different routines.

   type Internal_Formula_Access is access Internal_Formula;

   procedure Free is new Ada.Unchecked_Deallocation
     (Decision_Array, Decision_Array_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Antecedent_Array, Antecedent_Array_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Literal_Mask, Literal_Mask_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Formula, Internal_Formula_Access);

   procedure Destroy (F : in out Internal_Formula);
   --  Free the memory allocated for the formula's internal structures.
   --  Does not free the inner clauses.

   procedure Append_Watcher
     (F : in out Internal_Formula; C : Clause);
   --  Create a watcher for the given non-empty clause and install it
   --  in the internal formula.

   procedure Append_Clause
     (F : in out Internal_Formula; C : Clause);
   --  Append the given clause to the internal formula, updating its
   --  data structures.

   procedure Append_Formula
     (F : in out Internal_Formula; Other : Formula);
   --  Append the given formula to the internal formula, updating its
   --  data structures.

   function Unassigned_Count (M : Model) return Natural;
   --  Return the number of Unset variables in the given model

   function Solve_Internal
     (F        : in out Internal_Formula;
      Ctx      : in out User_Theory.User_Context;
      M        : in out Model;
      Min_Vars : Variable_Or_Null) return Boolean;
   --  Solve the given formula with the given partial model.
   --  This is where the DPLL/CDCL algorithm is implemented.

   -------------
   -- Destroy --
   -------------

   procedure Destroy (F : in out Internal_Formula) is
   begin
      Free_All (F.Clauses);
      for V of F.Occurs_List loop
         V.Destroy;
      end loop;
   end Destroy;

   --------------------
   -- Create_Watcher --
   --------------------

   procedure Append_Watcher
     (F : in out Internal_Formula; C : Clause)
   is
      K : constant Natural := C'First;
   begin
      if C (K) = 0 then
         --  This is a special "At-Most-One" constraint: the two following
         --  literals give the range. For example if the clause is
         --  ``(0, 2, 4)``, the ``0`` indicates that this is an AMO constraint,
         --  while ``2`` and ``4`` indicate that variables from 2 to 4 are part
         --  of the constraint. That is, 0 or 1 of these 3 variables can be
         --  True at the same time.
         --  This is equivalent to adding clauses ``(¬2 | ¬3)``,
         --  ``(¬2 | ¬4)`` and ``(¬3 | ¬4)`` but uses a single clause
         --  instead of O(n²) where n represents the number of variables in
         --  the constraint.
         declare
            W : constant Watcher := (0, 0, C);
         begin
            --  Since all variables in the range are impacted by this
            --  constraint, we need to place a watch on each of them.
            --  We still don't want to materialize the n² clauses, so we
            --  will need special handling of AMO constraint in
            --  ``Unit_Propagate`` as well.
            for Lit in C (K + 1) .. C (C'Last) loop
               F.Occurs_List (-Lit).Append (W);
            end loop;
         end;
      elsif C'Length = 2 then
         F.Occurs_List (C (K)).Append ((C (K + 1), C (K), C));
         F.Occurs_List (C (K + 1)).Append ((C (K), C (K + 1), C));
      else
         pragma Assert (C'Length > 2);
         F.Occurs_List (C (K)).Append ((C (K + 1), 0, C));
         F.Occurs_List (C (K + 1)).Append ((C (K), 0, C));
      end if;
   end Append_Watcher;

   -------------------
   -- Append_Clause --
   -------------------

   procedure Append_Clause
     (F : in out Internal_Formula; C : Clause)
   is
   begin
      if C'Length >= 2 then
         Append_Watcher (F, C);
      end if;
      F.Clauses.Append (C);
   end Append_Clause;

   --------------------
   -- Append_Formula --
   --------------------

   procedure Append_Formula
     (F : in out Internal_Formula; Other : Formula)
   is
   begin
      for C of Other loop
         Append_Clause (F, C);
      end loop;
   end Append_Formula;

   ----------------------
   -- Unassigned_Count --
   ----------------------

   function Unassigned_Count (M : Model) return Natural
   is
      Count : Natural := 0;
   begin
      for I in M'Range loop
         if M (I) = Unset then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Unassigned_Count;

   --------------------
   -- Solve_Internal --
   --------------------

   function Solve_Internal
     (F        : in out Internal_Formula;
      Ctx      : in out User_Theory.User_Context;
      M        : in out Model;
      Min_Vars : Variable_Or_Null) return Boolean
   is
      Unassigned_Left : Natural := Unassigned_Count (M);
      --  Track the number of variables that are not yet set

      First_Unset : Variable := M'First;
      --  Track the closest variable which can be decided next

      Decision_Level : Natural := 0;
      --  The current decision level

      Lit_Decisions : Decision_Array_Access :=
        new Decision_Array'(1 .. Variable_Or_Null (Unassigned_Left) => 0);
      --  Maps each variable to the decision level in which a value was
      --  set for it.

      Lit_Antecedents : Antecedent_Array_Access :=
        new Antecedent_Array'(1 .. Variable_Or_Null (Unassigned_Left) => null);
      --  Maps each variable to its antecedent clause: if the variable was
      --  assigned a value through unit propagation, this is the clause that
      --  was unit. If the variable was assigned a value through a decision,
      --  its antecedent is null.

      To_Propagate : Literal_Vectors.Vector;
      --  The list of literals that need to be propagated during the next
      --  call to ``Unit_Propagate``.

      Learnt_Clause : Literal_Vectors.Vector;
      --  The vector we use to generate the learnt clause during a backjump

      Learnt_Mask : Literal_Mask_Access :=
        new Literal_Mask (-M'Last .. +M'Last);
      --  The mask used during backjump to indicate which literals are
      --  already present (or were present at some point) in the learnt clause.

      procedure Assign
        (Var : Variable; Value : Boolean; Antecedent : Clause);
      --  Assigns a value to the given variable, updating the appropriate
      --  data structures. It is assumed that the variable does not yet
      --  have a value.

      function Check_Assign
        (Var : Variable; Value : Boolean; Antecedent : Clause) return Boolean;
      --  Assigns a value to the given variable, updating the appropriate data
      --  structures. Returns ``False`` if the variable already has a value
      --  and it does not match to the given value. Otherwise returns
      --  ``True``.

      procedure Unassign (Var : Variable);
      --  Unassigns a variable, updating the appropriate data structures

      procedure Unassign_All (Level : Natural);
      --  Unassigns all variables that have a decision level higher than
      --  the given level.

      procedure Add_To_Propagate (L : Literal);
      --  Include the given variable in the list of variables that must be
      --  propagated during the next round of Unit_Propagate.

      procedure Clear_Propagation;
      --  Clears the propagation related data structure, in particular the
      --  propagation stack as well as the propagation mask.

      function Val (X : Literal) return Variable_Value with Inline_Always;
      --  Return evaluation of the given literal

      function Unit_Propagate return Boolean;
      --  Implements the boolean constraint propagation routine. This is the
      --  most crucial part of the solver, where most of the time is spent.

      function Backtrack return Boolean with Unreferenced;
      --  Implements chronological backtracking. This is not used ATM but
      --  is kept here to allow experiments.

      procedure Setup_Backjump (Conflicting_Clause : Clause);
      --  Prepare data structures for an incoming backjump. This overload
      --  takes the conflicting clause and initializes the learnt clause
      --  with it.

      procedure Setup_Backjump (First_Lit, Second_Lit : Literal);
      --  Prepare data structures for an incoming backjump. This overload
      --  takes two literals of a virtual clause and initializes the learnt
      --  clause with it. This is used to avoid allocating an actual clause
      --  when backjumping from a conflict that arose during an At-Most-One
      --  constraint propagation.

      function Backjump return Boolean;
      --  Implements non-chronological backtracking through conflict analysis

      procedure Resolve (Right : Clause; Pivot_Index : Natural);
      --  Implements the resolution rule between ``Learnt_Clause`` and
      --  ``Right``: Assuming that both clauses contain an occurrence of Pivot,
      --  update ``Learnt_Clause`` to be the resolvent of the rule, that is the
      --  concatenation of itself and the ``Right`` clause without occurrences
      --  of the pivot. Mask should be updated so that the new literals that
      --  appear in ``Learnt_Clause`` clause are set to True.

      procedure Decide;
      --  Performs the decision to set a yet unset variable

      function Cleanup (Result : Boolean) return Boolean;
      --  Cleanup allocated resources and return the given boolean result

      function Reorder_Clause (C : Clause) return Natural;
      --  Reorder the given clause to place appropriate watched literals first.
      --  Return the number of non-False literals that we were able to watch,
      --  So either 0, 1 or 2.

      ------------
      -- Assign --
      ------------

      procedure Assign
        (Var : Variable; Value : Boolean; Antecedent : Clause)
      is
      begin
         pragma Assert (M (Var) = Unset);
         M (Var) := (if Value then True else False);
         Lit_Decisions (Var) := Decision_Level;
         Lit_Antecedents (Var) := Antecedent;
         Unassigned_Left := Unassigned_Left - 1;
         Add_To_Propagate ((if Value then -Var else +Var));
      end Assign;

      ------------------
      -- Check_Assign --
      ------------------

      function Check_Assign
        (Var : Variable; Value : Boolean; Antecedent : Clause) return Boolean
      is
         Expected : constant Variable_Value := (if Value then True else False);
         Actual   : constant Variable_Value := M (Var);
      begin
         if Actual = Unset then
            Assign (Var, Value, Antecedent);
            return Unit_Propagate;
         else
            return Actual = Expected;
         end if;
      end Check_Assign;

      --------------
      -- Unassign --
      --------------

      procedure Unassign (Var : Variable) is
      begin
         M (Var) := Unset;
         Lit_Decisions (Var) := 0;
         Unassigned_Left := Unassigned_Left + 1;
      end Unassign;

      ------------------
      -- Unassign_All --
      ------------------

      procedure Unassign_All (Level : Natural) is
         Found_First : Boolean := False;
      begin
         for Var in Lit_Decisions'Range loop
            if Lit_Decisions (Var) > Level then
               Unassign (Var);
               if not Found_First then
                  Found_First := True;
                  First_Unset := Var;
               end if;
            end if;
         end loop;
      end Unassign_All;

      ----------------------
      -- Add_To_Propagate --
      ----------------------

      procedure Add_To_Propagate (L : Literal) is
      begin
         pragma Assert (Val (L) = False);
         To_Propagate.Append (L);
      end Add_To_Propagate;

      -----------------------
      -- Clear_Propagation --
      -----------------------

      procedure Clear_Propagation is
      begin
         To_Propagate.Clear;
      end Clear_Propagation;

      ---------
      -- Val --
      ---------

      function Val (X : Literal) return Variable_Value is
         Var : constant Variable := Get_Var (X);
      begin
         if X > 0 then
            return M (Var);
         else
            return (case M (Var) is
               when True => False,
               when False => True,
               when Unset => Unset);
         end if;
      end Val;

      --------------------
      -- Unit_Propagate --
      --------------------

      function Unit_Propagate return Boolean is
         Watchers         : Watcher_Vectors.Vector;
         Watch_Count      : Natural;
         Being_Propagated : Literal;
         J                : Natural;
      begin
         pragma Assert (To_Propagate.Length >= 1);
         while not To_Propagate.Is_Empty loop
            Being_Propagated := To_Propagate.Pop;
            Watchers := F.Occurs_List (Being_Propagated);
            Watch_Count := Watchers.Length;
            J := 1;

            while J <= Watch_Count loop
               declare
                  W : constant Watcher_Vectors.Element_Access :=
                     Watchers.Get_Access (J);

                  Is_Sat      : Boolean;
                  Lits        : Clause;
                  Other_Lit   : Literal;
               begin
                  if W.Blit = 0 then
                     --  This is an AMO constraint. We know that the variable
                     --  being propagated was set to True, so we must assign
                     --  all the other variables to False.
                     pragma Assert (M (Get_Var (Being_Propagated)) = True);
                     Lits := W.Literals;
                     declare
                        From : constant Variable :=
                           Get_Var (Lits (Lits'First + 1));

                        To   : constant Variable :=
                           Get_Var (Lits (Lits'Last));
                     begin
                        for Var in From .. To loop
                           case M (Var) is
                              when True =>
                                 --  If another variable is already true, we
                                 --  have a conflict.
                                 if Var /= Get_Var (Being_Propagated) then
                                    Setup_Backjump (-Var, Being_Propagated);
                                    Clear_Propagation;
                                    return False;
                                 end if;
                              when False =>
                                 null;
                              when Unset =>
                                 Assign (Var, False, Lits);
                           end case;
                        end loop;
                     end;
                  elsif W.Other /= 0 then
                     --  This is a binary clause, we know that W.Other is the
                     --  literal being propagated (therefore it is False), and
                     --  ``W.Blit`` always holds the other literal. There we
                     --  can determine what to do with a single lookup on
                     --  ``W.Blit``.
                     pragma Assert (W.Other = Being_Propagated);
                     case Val (W.Blit) is
                        when True =>
                           --  Clause is satisfied
                           null;
                        when False =>
                           --  Clause is conflicting
                           Setup_Backjump (W.Literals);
                           Clear_Propagation;
                           return False;
                        when Unset =>
                           --  Clause is unit
                           Assign (Get_Var (W.Blit), W.Blit > 0, W.Literals);
                     end case;
                  elsif Val (W.Blit) /= True then
                     --  This is a "normal" clause and the blocking literal did
                     --  not allow us to skip it. So we now have no choice
                     --  but to dereference ``W.Literals`` now and inspect each
                     --  one of its literals.
                     pragma Assert (W.Blit /= Being_Propagated);

                     Lits := W.Literals;

                     pragma Assert (Lits'Length > 2);

                     --  Make sure the false literal is in second position
                     --  to simplify lookup expressions in the next lines.
                     if Being_Propagated = Lits (Lits'First) then
                        Lits (Lits'First) := Lits (Lits'First + 1);
                        Lits (Lits'First + 1) := Being_Propagated;
                     end if;

                     pragma Assert (Lits (Lits'First + 1) = Being_Propagated);

                     --  Now retrieve the other watched literal
                     Other_Lit := Lits (Lits'First);

                     --  and let's check what's inside
                     if Other_Lit /= W.Blit and then
                        Val (Other_Lit) = True
                     then
                        --  The other watched literal is true meaning the
                        --  clause is satisfied, simply update the watcher's
                        --  blocking literal and we're done.
                        W.Blit := Other_Lit;
                     else
                        W.Blit := Other_Lit;
                        Is_Sat := False;

                        --  The other watched literal is not true, so let's
                        --  see if we can find a non-False literal among the
                        --  other literals of the clause.
                        --  Note that at this stage, we still don't know if the
                        --  other watched literal is False or Unset.
                        for K in Lits'First + 2 .. Lits'Last loop
                           if Val (Lits (K)) /= False then
                              --  Found a non-false literal! swap its place
                              --  inside the clause to consider it as the other
                              --  watched literal.
                              Lits (Lits'First + 1) := Lits (K);
                              Lits (K) := Being_Propagated;

                              --  Update the occurrence list of the new watched
                              --  literal.
                              F.Occurs_List
                                (Lits (Lits'First + 1)).Append (W.all);

                              --  Update the occurrence list of the old watched
                              --  literal. Note that there must be no code path
                              --  using `W` after this line, since `W` now
                              --  points on another watcher (we have moved the
                              --  data inside the vector).
                              F.Occurs_List
                                (Being_Propagated).Swap_And_Remove (J);

                              Watch_Count := Watch_Count - 1;
                              J := J - 1;
                              Is_Sat := True;
                              exit;
                           end if;
                        end loop;

                        if not Is_Sat then
                           --  We couldn't find another non-False literal. We
                           --  can finally check the value of the other watched
                           --  literal to determine if we have a unit clause or
                           --  a conflict.
                           if Val (Other_Lit) = False then
                              Setup_Backjump (Lits);
                              Clear_Propagation;
                              return False;
                           else
                              Assign
                                (Get_Var (Other_Lit), Other_Lit > 0, Lits);
                           end if;
                        end if;
                     end if;
                  end if;
               end;
               J := J + 1;
            end loop;
         end loop;
         return True;
      end Unit_Propagate;

      ---------------
      -- Backtrack --
      ---------------

      function Backtrack return Boolean is
         First : Variable := M'Last;
         Value : Variable_Value;
      begin
         if Decision_Level <= 0 then
            return False;
         end if;

         Decision_Level := Decision_Level - 1;

         for Index in M'Range loop
            if Lit_Decisions (Index) > Decision_Level then
               if Index < First then
                  First := Index;
                  Value := M (Index);
               end if;
               Unassign (Index);
            end if;
         end loop;
         Assign (First, Value = False, null);
         First_Unset := First + 1;
         return True;
      end Backtrack;

      --------------------
      -- Setup_Backjump --
      --------------------

      procedure Setup_Backjump (Conflicting_Clause : Clause) is
      begin
         Learnt_Clause := Literal_Vectors.Empty_Vector;
         Learnt_Clause.Reserve (Conflicting_Clause.all'Length);
         Learnt_Mask.all := (others => False);

         for Lit of Conflicting_Clause.all loop
            if not Learnt_Mask (Lit) then
               Learnt_Clause.Append (Lit);
               Learnt_Mask (Lit) := True;
            end if;
         end loop;
      end Setup_Backjump;

      --------------------
      -- Setup_Backjump --
      --------------------

      procedure Setup_Backjump (First_Lit, Second_Lit : Literal) is
      begin
         Learnt_Clause := Literal_Vectors.Empty_Vector;
         Learnt_Mask.all := (others => False);

         Learnt_Clause.Append (First_Lit);
         Learnt_Clause.Append (Second_Lit);
         Learnt_Mask (First_Lit) := True;
         Learnt_Mask (Second_Lit) := True;
      end Setup_Backjump;

      --------------
      -- Backjump --
      --------------

      function Backjump return Boolean is
         Found         : Natural := 0;
         Pivot         : Variable_Or_Null := 0;
         Pivot_Index   : Natural;
      begin
         if Decision_Level <= 0 then
            Learnt_Clause.Destroy;
            return False;
         end if;

         --  We now want to build an asserting clause out of the conflicting
         --  clause. For that, we will replace each of its literal that was set
         --  at the current decision level with their antecedent clause by
         --  applying the resolution rule, until there is only one literal left
         --  at the current decision level. That way, we know that this literal
         --  will be the only one unset after backtracking, and therefore our
         --  clause will be unit and allow unit propagation straightaway.
         while True loop
            Found := 0;
            Pivot := 0;

            --  Find all the variables that were set at this decision level
            --  and choose a pivot among those, that it, one literal which
            --  assignment we will "explain" using literals at older decision
            --  levels.
            for Lit_Index in 1 .. Learnt_Clause.Length loop
               declare
                  Var : constant Variable :=
                     Get_Var (Learnt_Clause.Get (Lit_Index));
               begin
                  if Lit_Decisions (Var) = Decision_Level then
                     Found := Found + 1;
                     if Pivot = 0 then
                        if Lit_Antecedents (Var) /= null then
                           Pivot       := Var;
                           Pivot_Index := Lit_Index;
                        end if;
                     elsif Found > 1 then
                        --  We know there are at least two pivots and we
                        --  already selected one (since ``Pivot /= 0``), so we
                        --  can exit early from this internal loop to save some
                        --  cycles. This is OK because we don't need to know
                        --  how many potential pivots there were, only that
                        --  there were more than one.
                        exit;
                     end if;
                  end if;
               end;
            end loop;

            --  Either we found a single literal set at the current decision,
            --  in which case we have our asserting clause and we can exit,
            --  or we found several of them, in which case we will try to
            --  remove one (the pivot) by replacing it with its antecedents.
            if Found = 1 then
               exit;
            end if;

            --  Update the learnt clause by resolving it with the antecedent
            --  of the pivot.
            Resolve (Lit_Antecedents (Pivot), Pivot_Index);
         end loop;

         --  Find the decision level to which we should backjump by taking
         --  the maximum decision level among the literals of the learnt
         --  clause which is not the current decision level.
         declare
            Backjump_Decision_Level : Natural := 0;
            Lit_Decision_Level      : Natural := 0;
            Asserting_Lit           : Literal;

            Learnt : constant Clause :=
               Get_Literal_Vector_Array (Learnt_Clause);
         begin
            for I in Learnt'Range loop
               Lit_Decision_Level := Lit_Decisions (Get_Var (Learnt (I)));

               if Lit_Decision_Level = Decision_Level then
                  --  Since we are building asserting clauses, only one literal
                  --  is at the current decision level. We put it in front of
                  --  the clause to set it up as a watched literal, so it will
                  --  become unset after the backtracking.
                  Asserting_Lit := Learnt (I);
                  Learnt (I) := Learnt (1);
                  Learnt (1) := Asserting_Lit;
               elsif Lit_Decision_Level > Backjump_Decision_Level then
                  Backjump_Decision_Level := Lit_Decision_Level;
               end if;
            end loop;

            --  Unset all the variables that were set at a decision level
            --  higher than the one we are backjumping to.
            Decision_Level := Backjump_Decision_Level;
            Unassign_All (Decision_Level);

            --  Add the learnt clause to the internal formula
            Append_Clause (F, Learnt);

            --  Since we are building asserting clauses, the `Asserting_Lit`
            --  extracted from the above loop will be the only literal which
            --  is unset, with all the others being False. Since this is now
            --  a unit clause, we can directly assign the literal so as to
            --  satisfy the clause.
            Assign (Get_Var (Asserting_Lit), Asserting_Lit > 0, Learnt);
            return True;
         end;
      end Backjump;

      -------------
      -- Resolve --
      -------------

      procedure Resolve (Right : Clause; Pivot_Index : Natural) is
      begin
         Learnt_Clause.Swap_And_Remove (Pivot_Index);

         if Right (Right'First) = 0 then
            --  AMO constraint
            for Var in
               Variable (Right (Right'First + 1))
               .. Variable (Right (Right'Last))
            loop
               if M (Var) = True then
                  if not Learnt_Mask (-Var) then
                     Learnt_Mask (-Var) := True;
                     Learnt_Clause.Append (-Var);
                  end if;
                  exit;
               end if;
            end loop;
         else
            for Lit of Right.all loop
               if not Learnt_Mask (Lit) then
                  Learnt_Mask (Lit) := True;
                  Learnt_Clause.Append (Lit);
               end if;
            end loop;
         end if;
      end Resolve;

      ------------
      -- Decide --
      ------------

      procedure Decide is
         Var : constant Variable := Next_Decision (M, First_Unset);
      begin
         Decision_Level := Decision_Level + 1;
         Assign (Var, True, null);
      end Decide;

      -------------
      -- Cleanup --
      -------------

      function Cleanup (Result : Boolean) return Boolean is
      begin
         Free (Lit_Decisions);
         Free (Lit_Antecedents);
         Free (Learnt_Mask);
         To_Propagate.Destroy;
         Destroy (F);
         return Result;
      end Cleanup;

      --------------------
      -- Reorder_Clause --
      --------------------

      function Reorder_Clause (C : Clause) return Natural is
         I : Natural := C'First;
         T : Literal := 0;
      begin
         --  Find first literal to watch
         while I <= C'Last loop
            if Val (C (I)) /= False then
               T := C (I);
               C (I) := C (C'First);
               C (C'First) := T;
               I := I + 1;
               exit;
            end if;
            I := I + 1;
         end loop;

         if T = 0 then
            --  We could not even find one non-False literal in the clause, the
            --  clause is trivially False.
            return 0;
         end if;

         --  Find second literal to watch
         while I <= C'Last loop
            if Val (C (I)) /= False then
               T := C (I);
               C (I) := C (C'First + 1);
               C (C'First + 1) := T;
               return 2;
            end if;
            I := I + 1;
         end loop;

         --  We could only find one non-False literal, the clause is either
         --  satisfied or unit, depending on whether that literal is True or
         --  Unset, respectively.
         return 1;
      end Reorder_Clause;

   begin
      --  Perform initial BCP: the formula might be resolvable without
      --  making any decision.
      for C of F.Clauses loop
         --  Check that it has not already been set by a duplicate clause
         if C'Length = 1 and then
            not Check_Assign (Get_Var (C (C'First)), C (C'First) > 0, C)
         then
            return Cleanup (False);
         end if;
      end loop;
      pragma Assert (To_Propagate.Is_Empty);

      while True loop
         --  While there are still variables that have not been set,
         --  make a decision and propagate as much as possible, or backjump if
         --  necessary.
         while Unassigned_Left > 0 and then First_Unset <= Min_Vars loop
            Decide;
            while True loop
               if Unit_Propagate then
                  exit;
               elsif not Backjump then
                  return Cleanup (False);
               end if;
            end loop;
         end loop;

         --  The pure SAT problem was solved. Now, check if the theory accepts
         --  the model found. If not, restart but update the formula with the
         --  theory-provided reason for conflict.
         declare
            Explanation : Formula;
         begin
            declare
               Dummy : Boolean;
            begin
               if User_Theory.Check (Ctx, M, Explanation) then
                  return Cleanup (True);
               end if;
            exception
               when others =>
                  --  Make sure we clean up everything in case there was an
                  --  exception during theory checking before propagating.
                  Free_All (Explanation);
                  Dummy := Cleanup (False);
                  raise;
            end;

            if Explanation.Length = 0 then
               return Cleanup (False);
            end if;

            Decision_Level := 0;
            Unassign_All (Decision_Level);

            --  Process the explanations so that two unset literals appear
            --  first in the clauses to setup the two watched literals.
            for C of Explanation loop
               declare
                  Non_False : constant Natural := Reorder_Clause (C);
               begin
                  if Non_False = 0 then
                     Free_All (Explanation);
                     return Cleanup (False);
                  elsif Non_False = 1 then
                     if not Check_Assign
                       (Get_Var (C (C'First)), C (C'First) > 0, C)
                     then
                        Free_All (Explanation);
                        return Cleanup (False);
                     end if;
                  end if;
               end;
            end loop;

            Append_Formula (F, Explanation);

            Explanation.Destroy;

            pragma Assert (To_Propagate.Is_Empty);
         end;
      end loop;
      return Cleanup (True);
   end Solve_Internal;

   ------------
   --  Solve --
   ------------

   function Solve
     (F        : Formula;
      Ctx      : in out User_Theory.User_Context;
      M        : in out Model;
      Min_Vars : Variable_Or_Null := 0) return Boolean
   is
      procedure Cleanup;
      --  Clean up locally allocated memory before returning or propagating
      --  an exception.

      Is_Empty : constant Boolean := M'Length = 0;
      First    : constant Literal := (if Is_Empty then 1 else -M'Last);
      Last     : constant Literal := (if Is_Empty then 0 else +M'Last);
      Internal : Internal_Formula_Access := new Internal_Formula'
        (First => First, Last => Last, Clauses => F, Occurs_List => <>);

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup is
      begin
         Free (Internal);
      end Cleanup;
   begin
      for C of F loop
         if C'Length >= 2 then
            Append_Watcher (Internal.all, C);
         end if;
      end loop;
      return R : constant Boolean := Solve_Internal
        (Internal.all, Ctx, M, (if Min_Vars = 0 then M'Last else Min_Vars))
      do
         Cleanup;
      end return;
   exception
      when others =>
         Cleanup;
         raise;
   end Solve;
end Liblktlang_AdaSAT.DPLL;
