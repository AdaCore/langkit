with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Common;    use Libfoolang.Common;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;

procedure Check_Rotate is
   Buffer : constant String :=
     ("def a = 1" & ASCII.LF
      & "def b = 2" & ASCII.LF
      & "def c = 3" & ASCII.LF);

   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit :=
     Ctx.Get_From_Buffer ("main.txt", Buffer => Buffer);

   N_A : constant Foo_Node := U.Root.Child (1);
   N_B : constant Foo_Node := U.Root.Child (2);
   N_C : constant Foo_Node := U.Root.Child (3);

   procedure Dump (Handle : Node_Rewriting_Handle; Prefix : String);
   procedure Check (Label : String; Nodes : Foo_Node_Array);
   procedure Try (Label : String; Handles : Node_Rewriting_Handle_Array);

   ----------
   -- Dump --
   ----------

   procedure Dump (Handle : Node_Rewriting_Handle; Prefix : String) is
   begin
      Put_Line (Prefix & Image (Handle));
      if Handle /= No_Node_Rewriting_Handle then
         for C of Children (Handle) loop
            Dump (C, Prefix & "  ");
         end loop;
      end if;
   end Dump;

   -----------
   -- Check --
   -----------

   procedure Check (Label : String; Nodes : Foo_Node_Array) is
      RH      : Rewriting_Handle := Start_Rewriting (Ctx);
      Handles : Node_Rewriting_Handle_Array (Nodes'Range);
   begin
      Put_Line ("== " & Label & " ==");
      New_Line;
      for I in Nodes'Range loop
         declare
            N : Foo_Node renames Nodes (I);
         begin
            Handles (I) := (if N.Is_Null
                            then No_Node_Rewriting_Handle
                            else Handle (N));
         end;
      end loop;
      Rotate (Handles);
      Put_Line ("Done with no precondition failure:");
      Dump (Root (Handle (U)), "");
      Abort_Rewriting (RH);
      New_Line;
   end Check;

   ---------
   -- Try --
   ---------

   procedure Try (Label : String; Handles : Node_Rewriting_Handle_Array) is
   begin
      Put_Line ("== " & Label & " ==");
      New_Line;
      begin
         Rotate (Handles);
      exception
         when Exc : Libfoolang.Common.Precondition_Failure =>
            Put_Line
              ("Precondition failure: " & Exception_Message (Exc));
            New_Line;
            return;
      end;
      Put_Line ("No error:");
      Dump (Root (Handle (U)), "");
      New_Line;
   end Try;

begin
   Check
     ("[field] Swap",
      (1 => N_A.As_Def.F_Name.As_Foo_Node,
       2 => N_B.As_Def.F_Name.As_Foo_Node));
   Check
     ("[field] Rotate with null",
      (1 => N_A.As_Def.F_Expr.As_Foo_Node,
       2 => No_Foo_Node));

   Check ("[list] Swap", (N_A, N_B));
   Check ("[list] Tri-rotate", (N_A, N_B, N_C));
   Check ("[list] Swap with null", (N_A, No_Foo_Node));
   Check
     ("[list] Tri-rotate with nulls",
      (N_A, No_Foo_Node, N_B, No_Foo_Node, N_C));

   declare
      RH : Rewriting_Handle := Start_Rewriting (Ctx);
   begin
      Try ("[nop] No handle", (1 .. 0 => <>));
      Try ("[nop] One handle", (1 => Handle (N_A)));
      Try ("[nop] Two null handles", (1 .. 2 => No_Node_Rewriting_Handle));
      Try
        ("[nop] Three untied handles",
         (Create_Token_Node (RH, Foo_Name, "x"),
          Create_Token_Node (RH, Foo_Name, "y"),
          Create_Token_Node (RH, Foo_Name, "z")));
      Try ("[error] Duplicate handles", (Handle (N_A), Handle (N_A)));
      Abort_Rewriting (RH);
   end;

   Put_Line ("check_rotate.adb: Done.");
end Check_Rotate;
