package body Langkit_Support.Adalog.Relations is

   -------------------
   -- Pure_Relation --
   -------------------

   package body Pure_Relation is

      ----------
      -- Call --
      ----------

      function Solve_Impl (Inst : in out Rel) return Boolean is
      begin
         if Inst.Done then
            return False;
         end if;
         Inst.Done := True;
         return Apply (Inst.Rel);
      end Solve_Impl;

      overriding procedure Reset (Inst : in out Rel) is
      begin
         Inst.Done := False;
      end Reset;

      ----------
      -- Free --
      ----------

      procedure Cleanup (Inst : in out Rel) is
      begin
         Free (Inst.Rel);
      end Cleanup;

   end Pure_Relation;

   -----------------------
   -- Stateful_Relation --
   -----------------------

   package body Stateful_Relation is

      ----------
      -- Call --
      ----------

      function Solve_Impl (Inst : in out Rel) return Boolean is
      begin
         case Inst.State is
            when Start =>
               if Apply (Inst.Rel) then
                  Inst.State := Success;
                  return True;
               else
                  Inst.State := Finish;
                  return False;
               end if;
            when Success =>
               Revert (Inst.Rel);
               Inst.State := Finish;
               return False;
            when Finish =>
               return False;
         end case;
      end Solve_Impl;

      -----------
      -- Reset --
      -----------

      procedure Reset (Inst : in out Rel) is
      begin
         Inst.State := Start;
         Revert (Inst.Rel);
      end Reset;

      ----------
      -- Free --
      ----------

      procedure Cleanup (Inst : in out Rel) is
      begin
         Free (Inst.Rel);
      end Cleanup;

   end Stateful_Relation;

end Langkit_Support.Adalog.Relations;
