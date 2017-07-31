with Langkit_Support.Array_Utils;

with Langkit_Support.Adalog.Debug; use Langkit_Support.Adalog.Debug;
with Langkit_Support.Adalog.Pure_Relations;
use Langkit_Support.Adalog.Pure_Relations;

package body Langkit_Support.Adalog.Operations is

   package Rel_Arrays_Utils is new Langkit_Support.Array_Utils
     (Relation, Positive, Relation_Array);

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Self : in out Base_Aggregate_Rel) is
   begin
      for R of Self.Sub_Rels loop
         R.Reset;
      end loop;
      Self.State := 1;
   end Reset;

   -------------
   -- Cleanup --
   -------------

   overriding procedure Cleanup (Self : in out Base_Aggregate_Rel) is
   begin
      for R of Self.Sub_Rels loop
         Dec_Ref (R);
      end loop;
   end Cleanup;

   ------------------
   -- Custom_Image --
   ------------------

   overriding function Custom_Image (Self : Any_Rel) return String
   is ("<Any>");

   ------------------
   -- Custom_Image --
   ------------------

   overriding function Custom_Image (Self : All_Rel) return String
   is ("<All>");

   ----------------
   -- Solve_Impl --
   ----------------

   overriding function Solve_Impl (Self : in out Any_Rel) return Solving_State
   is
   begin
      while Self.State <= Self.Count loop
         case Self.Sub_Rels (Self.State).Solve is
            when Progress .. No_Progress =>
               raise Program_Error with "not implemented yet";

            when Satisfied =>
               return Satisfied;

            when Unsatisfied =>
               Self.State := Self.State + 1;
         end case;
      end loop;
      return Unsatisfied;
   end Solve_Impl;

   ----------------
   -- Solve_Impl --
   ----------------

   overriding function Solve_Impl (Self : in out All_Rel) return Solving_State
   is
   begin
      if Self.State = Self.Count + 1 then
         Self.State := Self.Count;
      end if;

      while Self.State <= Self.Count loop
         case Self.Sub_Rels (Self.State).Solve is
            when Progress | No_Progress =>
               raise Program_Error with "not implemented yet";

            when Satisfied =>
               Trace ("Solving rel " & Self.State'Image
                      & " succeeded, moving on to next rel");
               Self.State := Self.State + 1;

            when Unsatisfied =>
               if Self.State = 1 then
                  return Unsatisfied;
               else
                  Trace ("Solving rel " & Self.State'Image
                         & " failed, let's reset and try previous rel again");
                  Self.Sub_Rels (Self.State).Reset;
                  Self.State := Self.State - 1;
               end if;
         end case;
      end loop;
      return Satisfied;
   end Solve_Impl;

   --------------
   -- Logic_Or --
   --------------

   function Logic_Or (L, R : Relation) return Relation is
   begin
      return Logic_Any ((L, R));
   end Logic_Or;

   ---------------
   -- Logic_And --
   ---------------

   function Logic_And (L, R : Relation) return Relation is
   begin
      return Logic_All ((L, R));
   end Logic_And;

   ---------------
   -- Logic_Any --
   ---------------

   function Logic_Any (Rels : Relation_Array) return Relation is
      function Process (Rel : Relation) return Relation_Array
      is
        (if Rel.all in False_Relation.Rel'Class then Empty_Array
         elsif Rel.all in Any_Rel'Class then Any_Rel (Rel.all).Sub_Rels
         else (1 => Rel));

      function Process_Rels is new Rel_Arrays_Utils.Id_Flat_Map_Gen (Process);

      Keep_Rels : constant Relation_Array := Process_Rels (Rels);
   begin

      if Keep_Rels'Length = 0 then
         return False_Rel;
      end if;

      for Rel of Keep_Rels loop
         Inc_Ref (Rel);
      end loop;

      if Keep_Rels'Length = 1 then
         return Keep_Rels (1);
      end if;

      return new Any_Rel'(Ref_Count => 1,
                          Count     => Keep_Rels'Length,
                          Sub_Rels  => Keep_Rels,
                          State     => <>);
   end Logic_Any;

   ---------------
   -- Logic_All --
   ---------------

   function Logic_All (Rels : Relation_Array) return Relation is

      function Process (Rel : Relation) return Relation_Array
      is
        (if Rel.all in True_Relation.Rel'Class then Empty_Array
         elsif Rel.all in All_Rel'Class then All_Rel (Rel.all).Sub_Rels
         else (1 => Rel));

      function Process_Rels is new Rel_Arrays_Utils.Id_Flat_Map_Gen (Process);

      Keep_Rels : constant Relation_Array := Process_Rels (Rels);

   begin
      if Keep_Rels'Length = 0 then
         return True_Rel;
      end if;

      for Rel of Keep_Rels loop
         Inc_Ref (Rel);
      end loop;

      if Keep_Rels'Length = 1 then
         return Keep_Rels (1);
      end if;

      return new All_Rel'(Ref_Count => 1,
                          Count     => Keep_Rels'Length,
                          Sub_Rels  => Keep_Rels,
                          State     => <>);
   end Logic_All;

end Langkit_Support.Adalog.Operations;
