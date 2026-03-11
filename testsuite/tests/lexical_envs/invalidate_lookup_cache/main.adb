with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Common;    use Libfoolang.Common;
with Libfoolang.Iterators; use Libfoolang.Iterators;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U_A : constant Analysis_Unit := Ctx.Get_From_Buffer
     ("a.txt", Buffer => "def a { +dummy }");
   U_B : constant Analysis_Unit := Ctx.Get_From_Buffer
     ("b.txt", Buffer => "def b : a { }");
   U_C : constant Analysis_Unit := Ctx.Get_From_Buffer
     ("c.txt", Buffer => "def c : b { +d }");
   U_D : constant Analysis_Unit := Ctx.Get_From_Buffer
     ("d.txt", Buffer => "def d : a { }");

   Dummy_Ref : constant Ref_Clause :=
     Find_First (U_A.Root, Kind_Is (Foo_Ref_Clause)).As_Ref_Clause;

   D_Ref : constant Ref_Clause :=
     Find_First (U_C.Root, Kind_Is (Foo_Ref_Clause)).As_Ref_Clause;

   Units : constant array (1 .. 4) of Analysis_Unit :=
     (U_A, U_B, U_C, U_D);
begin
   for U of Units loop
      if U.Has_Diagnostics then
         for D of U.Diagnostics loop
            Put_Line (U.Format_GNU_Diagnostic (D));
         end loop;
         raise Program_Error;
      end if;

      U.Populate_Lexical_Env;
   end loop;

   Put_Line ("=== Initial Resolution ===");

   --  The following lookup will populate the lookup cache for key d in the
   --  whole hierarchy of envs of which c is a leaf, so: in c, b, a,
   --  and __root__.
   Put_Line ("Lookup d from c : " & D_Ref.P_Lookup.Image);

   Put_Line ("=== Clearing d.txt ===");

   Ctx.Get_From_Buffer ("d.txt", Buffer => "").Populate_Lexical_Env;

   --  Since an unit was reparsed in the context, env caches of any unit will
   --  be invalidated as soon as a property in that unit is called. So, for
   --  the following lookup, we don't care about the result: we simply want to
   --  call a property that will cause the invalidation of the caches in a.
   Put_Line ("Lookup dummy from a : " & Dummy_Ref.P_Lookup.Image);

   --  Note that the above doesn't actively invalidate caches of its children
   --  envs. However, if we were to lookup d again here (from c), we would
   --  correctly avoid reusing the cached value because we first check that all
   --  parent environments have a valid lookup cache, and it wouldn't be the
   --  case here (thus they are "passively" invalidated).
   --
   --  But for this test case, the problem we want to reproduce happens when we
   --  actually *do not* trigger another lookup, because the latter would reset
   --  all the envs down the hierarchy.

   Put_Line ("=== Repairing d.txt ===");

   Ctx.Get_From_Buffer
     ("d.txt", Buffer => "def d : a { }").Populate_Lexical_Env;

   --  Now, we reset unit d to its original content. Then, we trigger a lookup
   --  from a, which will cause a reset of the caches of a (since they were
   --  previously invalidated). Note that the lookup cache of a is completely
   --  valid at this point: in particular, it doesn't contain any stale value
   --  for key d.
   Put_Line ("Lookup dummy from a : " & Dummy_Ref.P_Lookup.Image);

   --  Now, we perform another lookup of d from c. The lookup cache of c itself
   --  will be reset by the property call happening on c, however, remember
   --  that envs in the middle of the hierarchy (in particular that of b) were
   --  not actively invalidated by the reparsings. Thus, when the lookup from c
   --  recurses on the env of b, it will ask whether that env is valid: before
   --  fixing the underlying issue, the answer would be yes, because it is
   --  itself valid, as well as all of its parents (in particular a, which was
   --  reset to a valid state due to the previous dummy lookup above). Now, it
   --  will correctly consider it invalid, by comparing the stored lookup cache
   --  version of its parent to the actual one.
   Put_Line ("Lookup d from c : " & D_Ref.P_Lookup.Image);

   Put_Line ("main.adb: Done.");
end Main;
