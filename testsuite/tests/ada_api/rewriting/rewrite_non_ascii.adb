with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text;     use Langkit_Support.Text;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Common;    use Libfoolang.Common;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;

with Process_Apply;

procedure Rewrite_Non_ASCII is
   Eacute : constant Character := Character'Val (233);
   Buffer : constant String := "def a = 1 # H" & Eacute & "llo" & ASCII.LF;

   procedure Check_Diagnostics (U : Analysis_Unit);

   -----------------------
   -- Check_Diagnostics --
   -----------------------

   procedure Check_Diagnostics (U : Analysis_Unit) is
   begin
      if Has_Diagnostics (U) then
         Put_Line ("Errors in " & Get_Filename (U) & ":");
         for D of Diagnostics (U) loop
            Put_Line (Format_GNU_Diagnostic (U, D));
         end loop;
         return;
      end if;
   end Check_Diagnostics;

   Ctx : constant Analysis_Context := Create_Context (Charset => "iso-8859-1");
   U   : constant Analysis_Unit := Get_From_Buffer
     (Ctx, "main.txt", Buffer => Buffer);
   RH  : Rewriting_Handle;

begin
   Check_Diagnostics (U);

   RH := Start_Rewriting (Ctx);

   Put_Line ("Appending a def node...");
   declare
      Decl_List : constant Node_Rewriting_Handle :=
         Handle (Root (U));
      B_Def     : constant Node_Rewriting_Handle := Create_Def
        (Handle => RH,
         F_Name => Create_Token_Node (RH, Foo_Name, "b"),
         F_Args => No_Node_Rewriting_Handle,
         F_Expr => Create_Token_Node (RH, Foo_Literal, "1"));
   begin
      Insert_Last (Decl_List, B_Def);
   end;

   New_Line;
   Put_Line ("Applying the diff...");
   Process_Apply (RH);

   New_Line;
   Put_Line ("main.txt:");
   Put_Line (Image (U.Text));

   Put_Line ("rewrite_non_ascii.adb: Done.");
end Rewrite_Non_ASCII;
