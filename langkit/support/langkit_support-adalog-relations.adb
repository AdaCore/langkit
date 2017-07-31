package body Langkit_Support.Adalog.Relations is

   -------------------
   -- Pure_Relation --
   -------------------

   package body Pure_Relation is

      ----------------
      -- Solve_Impl --
      ----------------

      function Solve_Impl (Self : in out Rel) return Solving_State is
      begin
         if Self.Done then
            return Unsatisfied;
         end if;
         Self.Done := True;
         return Apply (Self.Rel);
      end Solve_Impl;

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

      function Solve_Impl (Self : in out Rel) return Solving_State is
      begin
         case Self.State is
            when Start =>
               case Apply (Self.Rel) is
                  when Progress | No_Progress =>
                     raise Program_Error with "not implemented yet";

                  when Satisfied =>
                     Self.State := Success;
                     return Satisfied;

                  when Unsatisfied =>
                     Self.State := Finish;
                     return Unsatisfied;
               end case;

            when Success =>
               Revert (Self.Rel);
               Self.State := Finish;
               return Unsatisfied;

            when Finish =>
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
