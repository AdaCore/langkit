with Adalog.Debug; use Adalog.Debug;

package body Adalog.Predicates is

   ---------------
   -- Predicate --
   ---------------

   package body Predicate is

      ----------
      -- Free --
      ----------

      procedure Free (Inst : in out Predicate_Logic) is
      begin
         Remove_Predicate (Inst.Ref, Inst'Unrestricted_Access);
         Free (Inst.Pred);
      end Free;

      -----------
      -- Apply --
      -----------

      overriding function Apply
        (Inst : in out Predicate_Logic) return Boolean
      is
      begin
         if Is_Defined (Inst.Ref) then
            Trace ("In predicate apply, calling predicate");
            return A : Boolean do
               A := Call (Inst.Pred, GetL (Inst.Ref));
               Trace (A'Img);
            end return;
         else
            Trace ("In predicate apply, var " & Image (Inst.Ref)
                   & " not defined, deferring application");

            --  If the variable is not set, then predicate will return True all
            --  the time, and we register the predicate to be called at a later
            --  time.
            Add_Predicate (Inst.Ref, Inst'Unchecked_Access);
            return True;
         end if;
      end Apply;

      ------------
      -- Revert --
      ------------

      procedure Revert (Inst : in out Predicate_Logic) is
      begin
         Remove_Predicate (Inst.Ref, Inst'Unchecked_Access);
      end Revert;

   end Predicate;

   -----------------
   -- N_Predicate --
   -----------------

   package body N_Predicate is

      ----------
      -- Free --
      ----------

      procedure Free (Inst : in out Predicate_Logic) is
      begin
         for Ref of Inst.Refs loop
            Remove_Predicate (Ref, Inst'Unrestricted_Access);
         end loop;
         Free (Inst.Pred);
      end Free;

      -----------
      -- Apply --
      -----------

      overriding function Apply
        (Inst : in out Predicate_Logic) return Boolean
      is
      begin
         if (for all Ref of Inst.Refs => Is_Defined (Ref)) then
            declare
               Vals : Val_Array (1 .. Arity);
            begin
               for I in Inst.Refs'Range loop
                  Vals (I) := GetL (Inst.Refs (I));
               end loop;

               return Call (Inst.Pred, Vals);
            end;
         else
            for Ref of Inst.Refs loop
               Add_Predicate (Ref, Inst'Unchecked_Access);
            end loop;

            return True;
         end if;
      end Apply;

      ------------
      -- Revert --
      ------------

      procedure Revert (Inst : in out Predicate_Logic) is
      begin
         for Ref of Inst.Refs loop
            Remove_Predicate (Ref, Inst'Unchecked_Access);
         end loop;
      end Revert;

   end N_Predicate;

   package body Predicate_2 is

      ----------
      -- Free --
      ----------

      procedure Free (Self : in out Predicate_Wrapper) is
      begin
         Free (Self.T);
      end Free;

      ------------
      -- Create --
      ------------

      function Create
        (L, R : Var.Var; Pred : Predicate_Type) return Relation
      is
      begin
         return Predicate_2_Internal.Create
           ((L, R), Predicate_Wrapper'(Pred, L, R));
      end Create;

   end Predicate_2;

end Adalog.Predicates;
