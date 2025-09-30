with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Common;    use Libfoolang.Common;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;

with Process_Apply;

procedure Apply_Error is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit :=
     Ctx.Get_From_Buffer ("main.txt", Buffer => "");
   RH  : Rewriting_Handle;
   N   : Node_Rewriting_Handle;

   No_NRH : Node_Rewriting_Handle renames No_Node_Rewriting_Handle;

   function Create_Def
     (Name, Args, Expr : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is (Create_Def
         (RH,
          Name,
          (if Args = No_NRH then Create_Node (RH, Foo_Name_List) else Args),
          Expr));

   function Create_Literal (Text : Text_Type) return Node_Rewriting_Handle
   is (Create_Token_Node (RH, Foo_Literal, Text));

   function Create_Name (Text : Text_Type) return Node_Rewriting_Handle
   is (Create_Token_Node (RH, Foo_Name, Text));

   procedure Start (Label : String);
   procedure Run;

   -----------
   -- Start --
   -----------

   procedure Start (Label : String) is
   begin
      Put_Line ("# " & Label);
      RH := Start_Rewriting (Ctx);
      N := Handle (Root (U));
   end Start;

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      Put_Line ("Rewritten tree:");
      Print (Root (Handle (U)), Line_Prefix => "  ");
      Process_Apply (RH, Abort_On_Error => False);
      New_Line;
      Abort_Rewriting (RH);
   end Run;
begin
   Start ("Put an integer literal instead of an indentifier");
   Insert_Last
     (N, Create_Def (RH, Create_Literal ("1"), No_NRH, Create_Name ("a")));
   Run;

   Start ("Different shapes: null/non-null");
   Insert_Last
     (N, Create_Def (RH, Create_Name ("a"), No_NRH, Create_Literal ("1")));
   Run;

   Start ("Different shapes: different types");
   Insert_Last
     (N, Create_Def (Create_Name ("a"), No_NRH, Create_Literal ("b")));
   Run;

   Start ("Different shapes: different token text");
   Insert_Last
     (N, Create_Def (Create_Name ("a"), No_NRH, Create_Literal ("1 # cmt")));
   Run;

   Start ("Different shapes: shorter list");
   declare
      Nums : constant Node_Rewriting_Handle :=
        Create_Node (RH, Foo_Id_Or_Num_List);
      Names : constant Node_Rewriting_Handle :=
        Create_Node (RH, Foo_Id_Or_Num_List);
   begin
      Insert_Last (Nums, Create_Literal ("1"));
      Insert_Last (Nums, Create_Literal ("2"));
      Insert_Last (Nums, Create_Name ("a"));
      Insert_Last (Names, Create_Name ("b"));
      Insert_Last (Names, Create_Name ("c"));
      Insert_Last (N, Create_Id_Num_List (RH, Nums, Names));
   end;
   Run;

   Start ("Different shapes: longer list");
   declare
      Nums : constant Node_Rewriting_Handle :=
        Create_Node (RH, Foo_Id_Or_Num_List);
      Names : constant Node_Rewriting_Handle :=
        Create_Node (RH, Foo_Id_Or_Num_List);
   begin
      Insert_Last (Nums, Create_Literal ("1"));
      Insert_Last (Nums, Create_Literal ("2"));
      Insert_Last (Names, Create_Literal ("3"));
      Insert_Last (Names, Create_Name ("a"));
      Insert_Last (Names, Create_Name ("b"));
      Insert_Last (N, Create_Id_Num_List (RH, Nums, Names));
   end;
   Run;

   Start ("Different shapes: associativity");
   Insert_Last
     (N,
      Create_Def
        (Create_Name ("a"),
         No_NRH,
         Create_Plus
           (RH,
            Create_Plus
              (RH, Create_Literal ("1"), Create_Literal ("2")),
            Create_Plus
              (RH, Create_Literal ("3"), Create_Literal ("4")))));
   Run;

   Put_Line ("apply_error.adb: Done.");
end Apply_Error;
