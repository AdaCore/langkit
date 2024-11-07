with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;

   procedure Check (Label, Buffer : String; Dump : Boolean := False);
   procedure Unit_Test (Key : String);

   -----------
   -- Check --
   -----------

   procedure Check (Label, Buffer : String; Dump : Boolean := False) is
      U : constant Analysis_Unit :=
        Ctx.Get_From_Buffer (Filename => "buffer", Buffer => Buffer);
   begin
      Put_Line ("== " & Label & " ==");
      New_Line;
      for D of U.Diagnostics loop
         Put_Line (U.Format_GNU_Diagnostic (D));
      end loop;
      if Dump then
         U.Root.Print;
      end if;
      New_Line;
   end Check;

   ---------------
   -- Unit_Test --
   ---------------

   procedure Unit_Test (Key : String) is
   begin
      Check (Key, Key & " .");
   end Unit_Test;
begin
   Check
     ("after cut",
      "var a = A" & ASCII.LF & "var b = B;" & ASCII.LF & "var c = C",
      Dump => True);
   Unit_Test ("token_name");
   Unit_Test ("token_lit");
   Unit_Test ("skip");
   Unit_Test ("dont_skip");
   Unit_Test ("or");
   Unit_Test ("list");
   Unit_Test ("opt");
   Unit_Test ("pick");
   Unit_Test ("discard");
   Unit_Test ("defer");
   Unit_Test ("transform");
   Unit_Test ("null_tok");
   Unit_Test ("predicate");
   Unit_Test ("stop_cut");
   Unit_Test ("cut");
   Unit_Test ("recursion_test");
end Main;
