package body Langkit_Support.Adalog.Relations is

   -------------------
   -- Pure_Relation --
   -------------------

   package body Pure_Relation is

      ----------
      -- Call --
      ----------

      function Solve_Impl (Self : in out Rel) return Boolean is
      begin
         if Self.Done then
            return False;
         end if;
         Self.Done := True;
         return Apply (Self.Rel);
      end Solve_Impl;

      overriding procedure Reset (Self : in out Rel) is
      begin
         Self.Done := False;
      end Reset;

      ----------
      -- Free --
      ----------

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

      ----------
      -- Call --
      ----------

      function Solve_Impl (Self : in out Rel) return Boolean is
      begin
         case Self.State is
            when Start =>
               if Apply (Self.Rel) then
                  Self.State := Success;
                  return True;
               else
                  Self.State := Finish;
                  return False;
               end if;
            when Success =>
               Revert (Self.Rel);
               Self.State := Finish;
               return False;
            when Finish =>
               return False;
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

      ----------
      -- Free --
      ----------

      procedure Cleanup (Self : in out Rel) is
      begin
         Free (Self.Rel);
      end Cleanup;

   end Stateful_Relation;

end Langkit_Support.Adalog.Relations;
