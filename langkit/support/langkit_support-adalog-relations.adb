with Langkit_Support.Adalog.Debug; use Langkit_Support.Adalog.Debug;

package body Langkit_Support.Adalog.Relations is

   -------------------
   -- Pure_Relation --
   -------------------

   package body Pure_Relation is

      ----------------
      -- Solve_Impl --
      ----------------

      function Solve_Impl
        (Self    : in out Rel;
         Context : in out Solving_Context) return Solving_State
      is
         pragma Unreferenced (Context);
      begin
         if Self.Done then
            Trace ("In Pure_Relation: already done, returning UNSATISFIED");
            return Unsatisfied;
         end if;
         Self.Done := True;
         Trace ("In Pure_Relation: not done yet, evaluating...");
         return Apply (Self.Rel);
      end Solve_Impl;

      -----------
      -- Reset --
      -----------

      overriding procedure Reset (Self : in out Rel) is
      begin
         Self.Done := False;
      end Reset;

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup (Self : in out Rel) is
      begin
         Free (Self.Rel);
      end Cleanup;

      ------------------
      -- Custom_Image --
      ------------------

      overriding function Custom_Image (Self : Rel) return String
      is
      begin
         return Custom_Image (Self.Rel);
      end Custom_Image;

   end Pure_Relation;

   -----------------------
   -- Stateful_Relation --
   -----------------------

   package body Stateful_Relation is

      ----------------
      -- Solve_Impl --
      ----------------

      function Solve_Impl
        (Self    : in out Rel;
         Context : in out Solving_Context) return Solving_State
      is
         pragma Unreferenced (Context);
      begin
         case Self.State is
            when Start =>
               Trace ("In Stateful_Relation: Start state, evaluating...");
               case Apply (Self.Rel) is
                  when No_Progress => return No_Progress;
                  when Progress    => return Progress;

                  when Satisfied =>
                     Trace ("In Stateful_Relation: moving to Success state");
                     Self.State := Success;
                     return Satisfied;

                  when Unsatisfied =>
                     Trace ("In Stateful_Relation: moving to Finish state");
                     Self.State := Finish;
                     return Unsatisfied;
               end case;

            when Success =>
               Trace ("In Stateful_Relation: Success state, reverting, moving"
                      & " to Finish state and returning UNSATISFIED");
               Revert (Self.Rel);
               Self.State := Finish;
               return Unsatisfied;

            when Finish =>
               Trace ("In Stateful_Relation: Finish state, returning"
                      & " UNSATISFIED");
               return Unsatisfied;
         end case;
      end Solve_Impl;

      -----------
      -- Reset --
      -----------

      procedure Reset (Self : in out Rel) is
      begin
         Self.State := Start;
         Revert (Self.Rel);
      end Reset;

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup (Self : in out Rel) is
      begin
         Free (Self.Rel);
      end Cleanup;

   end Stateful_Relation;

end Langkit_Support.Adalog.Relations;
