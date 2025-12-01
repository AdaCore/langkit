--  Check that diagnostics emitted during parsing are correctly discarded when
--  the "branch" responsible for them is discarded itself.

with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;

   procedure Check (Label, Buffer : String);

   -----------
   -- Check --
   -----------

   procedure Check (Label, Buffer : String) is
      U : constant Analysis_Unit :=
        Ctx.Get_From_Buffer (Filename => "buffer", Buffer => Buffer);
   begin
      Put_Line ("== " & Label & " ==");
      New_Line;
      if U.Has_Diagnostics then
         for D of U.Diagnostics loop
            Put_Line (U.Format_GNU_Diagnostic (D));
         end loop;
         New_Line;
      end if;
      U.Root.Print;
      New_Line;
   end Check;

begin
   Check ("or_rule", "or alt b;");
   Check ("or_all_fail_rule", "or_all_fail alt = 0");
   Check ("opt_rule", "opt id;");
   Check ("list_rule", "list item id=; item end");
   Check ("last_fail_packrat", "last_fail_packrat def var ;");
end Main;
