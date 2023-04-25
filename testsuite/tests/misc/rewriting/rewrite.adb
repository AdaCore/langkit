with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Common;    use Libfoolang.Common;
with Libfoolang.Generic_API.Introspection;
use Libfoolang.Generic_API.Introspection;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;

with Process_Apply;

procedure Rewrite is
   Buffer : constant String :=
     ("def a = 1" & ASCII.LF
      & "def b = (2 + a) + 3" & ASCII.LF
      & "def c = a + b" & ASCII.LF
      & "def d = 4" & ASCII.LF
      & "def e = 5" & ASCII.LF);

   procedure Try (Label : String; Proc : access procedure);

   function Create_Def
     (Name : Text_Type;
      Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle;

   procedure Check_Diagnostics (U : Analysis_Unit);

   ---------
   -- Try --
   ---------

   procedure Try (Label : String; Proc : access procedure) is
   begin
      Put_Line (Label & "...");
      Proc.all;
      Put_Line ("   Done with no precondition failure");
   exception
      when Libfoolang.Common.Precondition_Failure =>
         Put_Line ("   Got a precondition failure");
   end Try;

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

   Ctx : constant Analysis_Context := Create_Context;
   U1  : constant Analysis_Unit := Get_From_Buffer
     (Ctx, "u1.txt", Buffer => Buffer);
   U2  : constant Analysis_Unit := Get_From_Buffer
     (Ctx, "u2.txt", Buffer => "def z = 100");
   RH  : Rewriting_Handle;
   N   : Node_Rewriting_Handle;

   ----------------
   -- Create_Def --
   ----------------

   function Create_Def
     (Name : Text_Type;
      Expr : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
      Name_Node : constant Node_Rewriting_Handle :=
         Create_Token_Node (RH, Foo_Name, Name);
   begin
      return Create_Def (RH, Name_Node, No_Node_Rewriting_Handle, Expr);
   end Create_Def;

begin
   Check_Diagnostics (U1);
   Check_Diagnostics (U2);

   RH := Start_Rewriting (Ctx);
   N := Handle (Root (U1));

   Put ("Node type for the root: ");
   Put_Line (Debug_Name (Type_Of (N)));

   declare
      procedure Set_Tied_Child;
      procedure Create_Error_Node;
      procedure Create_Regular_Error_Node;

      ----------
      -- Proc --
      ----------

      procedure Set_Tied_Child is
      begin
         Set_Child (N, 2, Child (N, 3));
      end Set_Tied_Child;

      -----------------------
      -- Create_Error_Node --
      -----------------------

      procedure Create_Error_Node is
      begin
         N := Create_Node (RH, Foo_Error_Def);
      end Create_Error_Node;

      -------------------------------
      -- Create_Regular_Error_Node --
      -------------------------------

      procedure Create_Regular_Error_Node is
      begin
         N := Create_Regular_Node (RH, Foo_Error_Def, (1 .. 0 => <>));
      end Create_Regular_Error_Node;

   begin
      Try ("Try assigning a child that is already tied to a tree",
           Set_Tied_Child'Access);
      Try ("Try creating an error node (Create_Node)",
           Create_Error_Node'Access);
      Try ("Try creating an error node (Create_Regular_Node)",
           Create_Regular_Error_Node'Access);

      New_Line;
      Put_Line ("Replace the middle definition (b) with a clone of the last"
                & " definition (c)");
      Set_Child (N, 2, Clone (Child (N, 3)));
   end;

   New_Line;
   Put_Line ("Creating a tree from a template:");
   declare
      N : constant Node_Rewriting_Handle :=
        Create_From_Template
          (Handle    => RH,
           Template  => "def foo = 1 + 2",
           Arguments => (1 .. 0 => <>),
           Rule      => Def_Rule_Rule);

      function Img (N : Node_Rewriting_Handle) return String
      is (Image (Unparse (N)));
   begin
      Put_Line ("  Tree: " & Img (N));
      Put_Line ("  F_Name child: " & Img (Child (N, Member_Refs.Def_F_Name)));
      Put_Line ("  F_Expr/F_LHS child: "
                & Img (Child (N, (Member_Refs.Def_F_Expr,
                                  Member_Refs.Plus_F_Lhs))));
   end;

   New_Line;
   Put_Line ("Swap first and fourth defs");
   declare
      N1 : constant Node_Rewriting_Handle := Child (N, 1);
      N4 : constant Node_Rewriting_Handle := Child (N, 4);
   begin
      if not Tied (N1) then
         raise Program_Error;
      end if;
      if not Tied (N4) then
         raise Program_Error;
      end if;

      Set_Child (N, 1, No_Node_Rewriting_Handle);
      Set_Child (N, 4, No_Node_Rewriting_Handle);

      if Tied (N1) then
         raise Program_Error;
      end if;
      if Tied (N4) then
         raise Program_Error;
      end if;

      Set_Child (N, 1, N4);
      Set_Child (N, 4, N1);

      if not Tied (N1) then
         raise Program_Error;
      end if;
      if not Tied (N4) then
         raise Program_Error;
      end if;
   end;

   New_Line;
   Put_Line ("Replace the expression of the fifth definition");
   declare
      Nested_Expr : constant Node_Rewriting_Handle := Create_Paren_Expr
        (RH, Create_Plus
               (RH,
                Create_Token_Node (RH, Foo_Literal, "5"),
                Create_Ref (RH, Create_Token_Node (RH, Foo_Name, "c"))));

      Top_Expr : constant Node_Rewriting_Handle := Create_Paren_Expr
        (RH, Create_Plus
               (RH,
                Create_Ref (RH, Create_Token_Node (RH, Foo_Name, "d")),
                Nested_Expr));

      Fifth_Child : constant Node_Rewriting_Handle := Child (N, 5);
   begin
      Set_Child (Fifth_Child, Member_Refs.Def_F_Expr, Top_Expr);
   end;

   New_Line;
   Put_Line ("Replace the root of unit 2");
   declare
      New_Root : constant Node_Rewriting_Handle :=
         Create_Node (RH, Foo_Foo_Node_List);
      Expr_1   : constant Node_Rewriting_Handle :=
         Create_Token_Node (RH, Foo_Literal, "111");
      Expr_2   : constant Node_Rewriting_Handle :=
         Create_Token_Node (RH, Foo_Literal, "222");
   begin
      Append_Child (New_Root, Create_Def ("zz", Expr_1));
      Append_Child (New_Root, Create_Def ("yy", Expr_2));
      Replace (Expr_2, Create_Token_Node (RH, Foo_Literal, "333"));
      Replace (Handle (Root (U2)), New_Root);
   end;

   New_Line;
   Put_Line ("Applying the diff...");
   Process_Apply (RH);

   New_Line;
   Put_Line ("u1.txt:");
   Root (U1).Print (Show_Slocs => False);

   New_Line;
   Put_Line ("u2.txt:");
   Root (U2).Print (Show_Slocs => False);

   Put_Line ("rewrite.adb: Done.");
end Rewrite;
