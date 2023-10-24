with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis;      use Libfoolang.Analysis;
with Libfoolang.Common;        use Libfoolang.Common;
with Libfoolang.Rewriting;     use Libfoolang.Rewriting;

with Process_Apply;

procedure Clone_Synthetic is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Get_From_Buffer
     (Ctx, "u.txt", Buffer => "");
   RH  : Rewriting_Handle;
   N   : Node_Rewriting_Handle;

   A_Id, B_Id : Node_Rewriting_Handle;
begin
   if Has_Diagnostics (U) then
      Put_Line ("Errors in " & Get_Filename (U) & ":");
      for D of Diagnostics (U) loop
         Put_Line (Format_GNU_Diagnostic (U, D));
      end loop;
      return;
   end if;

   RH := Start_Rewriting (Ctx);
   N := Handle (Root (U));
   A_Id := Create_Token_Node (RH, Foo_Name, "A");
   B_Id := Create_Token_Node (RH, Foo_Name, "B");

   declare
      Ref : constant Node_Rewriting_Handle :=
         Create_Ref (RH, Clone (A_Id));
      Def : constant Node_Rewriting_Handle :=
         Create_Def (RH, Clone (B_Id), No_Node_Rewriting_Handle, Ref);
   begin
      Insert_Last (N, Def);
   end;

   Put_Line ("Applying the diff...");
   Process_Apply (RH);

   New_Line;
   Put_Line ("u.txt:");
   Root (U).Print (Show_Slocs => False);

   Put_Line ("clone_synthetic.adb: Done.");
end Clone_Synthetic;
