with Langkit_Support.Adalog.Debug; use Langkit_Support.Adalog.Debug;

package body Langkit_Support.Adalog.Predicates is

   ---------------
   -- Predicate --
   ---------------

   package body Predicate is

      ----------
      -- Free --
      ----------

      procedure Free (Self : in out Predicate_Logic) is
      begin
         Remove_Predicate (Self.Ref, Self'Unrestricted_Access);
         Free (Self.Pred);
      end Free;

      -----------
      -- Apply --
      -----------

      overriding function Apply
        (Self : in out Predicate_Logic) return Boolean
      is
      begin
         if Is_Defined (Self.Ref) then
            Trace ("In predicate apply, calling predicate");
            return A : Boolean do
               A := Call (Self.Pred, GetL (Self.Ref));
               Trace (A'Img);
            end return;
         else
            Trace ("In predicate apply, var " & Image (Self.Ref)
                   & " not defined, deferring application");

            --  If the variable is not set, then predicate will return True all
            --  the time, and we register the predicate to be called at a later
            --  time.
            Add_Predicate (Self.Ref, Self'Unchecked_Access);
            return True;
         end if;
      end Apply;

      ------------
      -- Revert --
      ------------

      procedure Revert (Self : in out Predicate_Logic) is
      begin
         Remove_Predicate (Self.Ref, Self'Unchecked_Access);
      end Revert;

   end Predicate;

   -----------------
   -- N_Predicate --
   -----------------

   package body N_Predicate is

      ----------
      -- Free --
      ----------

      procedure Free (Self : in out Predicate_Logic) is
      begin
         for Ref of Self.Refs loop
            Remove_Predicate (Ref, Self'Unrestricted_Access);
         end loop;
         Free (Self.Pred);
      end Free;

      -----------
      -- Apply --
      -----------

      overriding function Apply
        (Self : in out Predicate_Logic) return Boolean
      is
      begin
         if (for all Ref of Self.Refs => Is_Defined (Ref)) then
            declare
               Vals : Val_Array (1 .. Arity);
            begin
               for I in Self.Refs'Range loop
                  Vals (I) := GetL (Self.Refs (I));
               end loop;

               return Call (Self.Pred, Vals);
            end;
         else
            for Ref of Self.Refs loop
               Add_Predicate (Ref, Self'Unchecked_Access);
            end loop;

            return True;
         end if;
      end Apply;

      ------------
      -- Revert --
      ------------

      procedure Revert (Self : in out Predicate_Logic) is
      begin
         for Ref of Self.Refs loop
            Remove_Predicate (Ref, Self'Unchecked_Access);
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

end Langkit_Support.Adalog.Predicates;
