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
   --  The names f(x,y) and g(y,x) are properly parsed, no diagnostics emitted

   Check ("complete case", "f(x,y);g(y,x);");

   --  The incomplete code below lead to the parsing of the incomplete name
   --  f(x,g(y,x), diagnostics are emitted the first time that name is parsed,
   --  i.e. during the parsing of the AssignStmt rule. This name isn't part of
   --  such a statement, it is memoized along with its associated diagnostics.
   --  This test ensure that those diagnostics are properly kept during the
   --  parsing of the next matching rule (CallStmt).

   Check ("simple incomplete case", "f(x,g(y,x);");

   --   Likewise, but with additional diagnostic deeper in the parsing
   --   recursion.

   Check ("deeper incomplete case", "f(x.)(y.).(;");
end Main;
