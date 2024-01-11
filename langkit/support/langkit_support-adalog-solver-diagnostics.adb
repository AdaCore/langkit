package body Langkit_Support.Adalog.Solver.Diagnostics is

   function Equivalent (Left, Right : Internal_Diagnostic) return Boolean;
   --  Return whether two diagnostics are equal modulo the round during
   --  which they were emitted.

   procedure Compare_Context_Sets
     (Left, Right            : Logic_Context_Vectors.Vector;
      Is_Subset, Is_Superset : out Boolean);
   --  Compare the two given sets of contexts and returns whether the left
   --  one is subset of the right one, a superset of it, both (i.e. they are
   --  equal), or neither of those.

   procedure Trace_Diagnostic (Diag : Internal_Diagnostic);
   --  Print a human-readable representation of the internal diagnostic to
   --  the ``LANGKIT.SOLVER.DIAGNOSTICS`` trace.

   procedure Destroy (Diag : in out Internal_Diagnostic);
   --  Free all memory by this internal diagnostic

   procedure Remove_Duplicate_Rounds
     (Accumulator : in out Diagnostic_Accumulator);
   --  Scans all the diagnostics emitted so far and remove rounds which can be
   --  explained in terms of the last round registered. This should only be
   --  called if the last round is "complete" (i.e. it will not receive
   --  diagnostics anymore), otherwise we might remove rounds which were in
   --  fact not explainable by that last round after seeing all its
   --  diagnostics.

   ----------------------
   -- Register_Failure --
   ----------------------

   procedure Register_Failure
     (Accumulator : in out Diagnostic_Accumulator;
      Failed_Atom : Atomic_Relation_Type;
      Contexts    : Logic_Context_Vectors.Vector;
      Round       : Natural)
   is
   begin
      --  If that's not an atom for which we're able to generate a diagnostic,
      --  ignore it.
      if Failed_Atom.Kind not in Predicate | N_Predicate then
         return;
      end if;

      if Round > Accumulator.Last_Round then
         --  This atom was emitted in a new round than the last diagnostic we
         --  registered: before processing it, we consider the last round as
         --  "complete", and check if that round can remove a previous round
         --  or be removed by a previous round, if its diagnostics are a
         --  subset or a superset of that of a previous round.
         Diags_Trace.Trace ("New round, removing subsets!");
         Remove_Duplicate_Rounds (Accumulator);
         Accumulator.Last_Round := Round;
         Accumulator.Last_Diag_Index_Of_Previous_Round :=
           Accumulator.Diagnostics.Last_Index;
      else
         --  This atom was emitted in the same round as that of the last
         --  diagnostic we registered: beofre processing it, check if we can
         --  explain its failure in terms of a diagnostic we already saw in
         --  this round, or converesely if it can be used to explain the
         --  failure of other diagnostics seens during this round.
         declare
            I : Natural := Accumulator.Last_Diag_Index_Of_Previous_Round + 1;
            Is_Subset, Is_Superset : Boolean;
         begin
            while I <= Accumulator.Diagnostics.Last_Index loop
               Compare_Context_Sets
                 (Contexts,
                  Accumulator.Diagnostics.Get (I).Contexts,
                  Is_Subset,
                  Is_Superset);

               if Is_Subset and not Is_Superset then
                  --  The new diagnostic triggers a failure using a subset of
                  --  the contexts of the diagnostic at index ``I``, therefore
                  --  we can remove that one.
                  Diags_Trace.Trace ("Found diag with subset contexts!");
                  Destroy (Accumulator.Diagnostics.Get_Access (I).all);
                  Accumulator.Diagnostics.Pop (I);
               elsif Is_Superset and not Is_Subset then
                  --  The new diagnostic triggers a failure using a superset
                  --  of the contexts of the diagnostic at index ``I``, so we
                  --  don't need to register it in the end.
                  Diags_Trace.Trace ("Is superset contexts!");
                  return;
               else
                  I := I + 1;
               end if;
            end loop;
         end;
      end if;

      case Failed_Atom.Kind is
         when Predicate =>
            Accumulator.Diagnostics.Append
              ((Atom     => Failed_Atom,
                Values   => new Value_Array'
                  (1 => Get_Value (Failed_Atom.Target)),
                Contexts => Contexts.Copy,
                Round    => Round));
         when N_Predicate =>
            declare
               Vals : constant Value_Array_Access :=
                 new Value_Array (1 .. Failed_Atom.Vars.Length);
            begin
               Get_Values (Failed_Atom.Vars, Vals.all);
               Accumulator.Diagnostics.Append
                 ((Atom     => Failed_Atom,
                   Values   => Vals,
                   Contexts => Contexts.Copy,
                   Round    => Round));
            end;
         when others =>
            return;
      end case;

      if Diags_Trace.Is_Active then
         Diags_Trace.Trace ("Added new diagnostic:");
         Trace_Diagnostic (Accumulator.Diagnostics.Last_Element);
      end if;
   end Register_Failure;

   ----------------------------
   -- Report_All_And_Cleanup --
   ----------------------------

   procedure Emit_All_And_Destroy
     (Accumulator : in out Diagnostic_Accumulator;
      Emitter     : Diagnostic_Emitter)
   is
   begin
      --  Since we're about to emit and clean everything, consider the current
      --  round as "complete" (i.e. we will not receive diagnostics anymore)
      --  and so try to remove duplicate rounds one last time.
      Remove_Duplicate_Rounds (Accumulator);

      for Diag of Accumulator.Diagnostics loop
         case Diag.Atom.Kind is
            when Predicate =>
               Diag.Atom.Pred.Failed
                 (Diag.Values (1),
                  Diag.Contexts.To_Array,
                  Diag.Round,
                  Emitter);
            when N_Predicate =>
               Diag.Atom.N_Pred.Failed
                 (Diag.Values.all,
                  Diag.Contexts.To_Array,
                  Diag.Round,
                  Emitter);
            when others =>
               raise Program_Error with "Cannot happen";
         end case;
      end loop;

      Destroy (Accumulator);
   end Emit_All_And_Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Accumulator : in out Diagnostic_Accumulator) is
   begin
      for I in 1 .. Accumulator.Diagnostics.Last_Index loop
         Destroy (Accumulator.Diagnostics.Get_Access (I).all);
      end loop;
      Accumulator.Diagnostics.Destroy;
   end Destroy;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (Left, Right : Internal_Diagnostic) return Boolean is
   begin
      if Left.Atom /= Right.Atom then
         return False;
      end if;
      if Left.Values.all /= Right.Values.all then
         return False;
      end if;
      if Left.Contexts.Length /= Right.Contexts.Length then
         return False;
      end if;
      for I in 1 .. Left.Contexts.Length loop
         if not Same_Contexts
           (Left.Contexts.Get (I), Right.Contexts.Get (I))
         then
            return False;
         end if;
      end loop;
      return True;
   end Equivalent;

   --------------------------
   -- Compare_Context_Sets --
   --------------------------

   procedure Compare_Context_Sets
     (Left, Right            : Logic_Context_Vectors.Vector;
      Is_Subset, Is_Superset : out Boolean)
   is
      Right_Hit_Count : Natural := 0;
      --  The number of times we found an element in the ``Right`` set of
      --  contexts.
   begin
      Is_Subset := True;
      Is_Superset := False;
      for CL of Left loop
         declare
            Found : Boolean := False;
         begin
            for CR of Right loop
               if Same_Contexts (CL, CR) then
                  Found := True;
                  Right_Hit_Count := Right_Hit_Count + 1;
                  exit;
               end if;
            end loop;
            if not Found then
               Is_Subset := False;
            end if;
         end;
      end loop;

      --  Since contexts should appear only once in each set, we can conclude
      --  that ``Left`` is a superset of ``Right`` if the number of times we
      --  found a matching element in ``Right`` corresponds to the size of
      --  the ``Right`` set.
      Is_Superset := Right_Hit_Count = Right.Length;
   end Compare_Context_Sets;

   ----------------------
   -- Trace_Diagnostic --
   ----------------------

   procedure Trace_Diagnostic (Diag : Internal_Diagnostic) is
   begin
      Diags_Trace.Trace ("Atom" & Image (Diag.Atom));
      Diags_Trace.Trace ("At round" & Diag.Round'Image);
      Diags_Trace.Trace ("With values:");
      Diags_Trace.Increase_Indent;
      for V of Diag.Values.all loop
         Diags_Trace.Trace (Solver_Ifc.Logic_Vars.Value_Image (V));
      end loop;
      Diags_Trace.Decrease_Indent;
      Diags_Trace.Trace ("With contexts");
      Diags_Trace.Increase_Indent;
      for Ctx of Diag.Contexts loop
         Diags_Trace.Trace (Image (Ctx));
      end loop;
      Diags_Trace.Decrease_Indent;
   end Trace_Diagnostic;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Diag : in out Internal_Diagnostic) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Value_Array, Value_Array_Access);
   begin
      Free (Diag.Values);
      Diag.Contexts.Destroy;
   end Destroy;

   -----------------------------
   -- Remove_Duplicate_Rounds --
   -----------------------------

   procedure Remove_Duplicate_Rounds
     (Accumulator : in out Diagnostic_Accumulator)
   is
      Last_Round_Index : Natural renames
        Accumulator.Last_Diag_Index_Of_Previous_Round;

      Diags : Internal_Diagnostic_Vectors.Vector renames
        Accumulator.Diagnostics;

      procedure Destroy_And_Remove_Diags (F, T : Natural);
      --  Destroy all diagnostics of ``Diags`` that lie between the given
      --  indices, and remove the corresponding entries.

      function Process_Round (F, T : Natural) return Boolean;
      --  Check if the round made of the set of diagnostics between indices
      --  ``F`` and ``T`` can be explained in terms of the diagnostics
      --  reported last round. If so, all diagnostics of that round are
      --  removed from ``Diags``, and the function returns ``True``.
      --  Otherwise ``False`` is returned.

      function Remove_One_Round return Boolean;
      --  Calls ``Process_Round`` on all rounds registered so far, except
      --  the last one as it is the reference one using which we try to
      --  explain other rounds. Return ``True`` as soon as one round was
      --  removed. Return ``False`` if no round were removed.

      ------------------------------
      -- Destroy_And_Remove_Diags --
      ------------------------------

      procedure Destroy_And_Remove_Diags (F, T : Natural) is
      begin
         for I in F .. T loop
            Destroy (Diags.Get_Access (I).all);
         end loop;
         Diags.Remove_At (F, T - F + 1);
      end Destroy_And_Remove_Diags;

      -------------------
      -- Process_Round --
      -------------------

      function Process_Round (F, T : Natural) return Boolean is
         Count : constant Natural := T - F + 1;
         Found_Count : Natural := 0;
         Is_Subset   : Boolean := True;
      begin
         Diags_Trace.Trace
           ("Trying" & F'Image & " .." & T'Image);

         for I in Last_Round_Index + 1 .. Diags.Last_Index loop
            declare
               Diag  : constant Internal_Diagnostic := Diags.Get (I);
               Found : Boolean := False;
            begin
               for J in F .. T loop
                  if Equivalent (Diag, Diags.Get (J)) then
                     Found := True;
                     Found_Count := Found_Count + 1;
                     exit;
                  end if;
               end loop;
               if not Found then
                  Is_Subset := False;
               end if;
            end;
         end loop;

         if Is_Subset then
            Destroy_And_Remove_Diags (F, T);
            Last_Round_Index := Last_Round_Index - Count;
            Diags_Trace.Trace ("Is superset of last round, remove it");
            return True;
         elsif Found_Count = Count then
            Destroy_And_Remove_Diags (Last_Round_Index + 1, Diags.Last_Index);
            Last_Round_Index := Diags.Last_Index;
            Diags_Trace.Trace ("Is subset of last round, remove last round");
            return True;
         end if;

         return False;
      end Process_Round;

      ----------------------
      -- Remove_One_Round --
      ----------------------

      function Remove_One_Round return Boolean is
         Current_Round : Integer := -1;
         Prev_Round_First_Index : Natural := Diags.First_Index;
      begin
         for R in Diags.First_Index .. Last_Round_Index loop
            if Current_Round = -1 then
               Current_Round := Diags.Get (R).Round;
            elsif Diags.Get (R).Round > Current_Round then
               if Process_Round (Prev_Round_First_Index, R - 1) then
                  return True;
               end if;
               Prev_Round_First_Index := R;
               Current_Round := Diags.Get (R).Round;
            end if;
         end loop;
         if Current_Round >= 0 then
            return Process_Round (Prev_Round_First_Index, Last_Round_Index);
         end if;
         return False;
      end Remove_One_Round;
   begin
      --  Try to remove rounds as long as the last iteration successfully
      --  did so, unless we found in the last iteration that last round
      --  could be explained in terms of a previous round. We can stop early
      --  in this case since the remaining rounds already went through this
      --  processing.
      while Remove_One_Round loop
         exit when Last_Round_Index = Diags.Last_Index;
      end loop;
   end Remove_Duplicate_Rounds;

end Langkit_Support.Adalog.Solver.Diagnostics;
