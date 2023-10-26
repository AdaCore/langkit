with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis;      use Libfoolang.Analysis;
with Libfoolang.Common;        use Libfoolang.Common;
with Libfoolang.Generic_API.Introspection;
use Libfoolang.Generic_API.Introspection;
with Libfoolang.Rewriting;     use Libfoolang.Rewriting;

with Process_Apply;

procedure Templates is
   Buffer : constant String :=
     ("def a = 1" & ASCII.LF
      & "def b = (2 + a) + 3" & ASCII.LF);

   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Get_From_Buffer
     (Ctx, "main.txt", Buffer => Buffer);
   RH  : Rewriting_Handle := Start_Rewriting (Ctx);
   N   : constant Node_Rewriting_Handle := Handle (Root (U));
begin

   Put_Line ("Test various template instantiation errors...");
   declare
      type Arguments_Access is access all Node_Rewriting_Handle_Array;
      type Test_Case is record
         Label     : Unbounded_String;
         Template  : Text_Access;
         Arguments : Arguments_Access;
      end record;
      function Create
        (Label     : String;
         Template  : Text_Type;
         Arguments : Node_Rewriting_Handle_Array := (1 .. 0 => <>))
         return Test_Case;
      procedure Free is new Ada.Unchecked_Deallocation
        (Node_Rewriting_Handle_Array, Arguments_Access);

      ------------
      -- Create --
      ------------

      function Create
        (Label     : String;
         Template  : Text_Type;
         Arguments : Node_Rewriting_Handle_Array := (1 .. 0 => <>))
         return Test_Case is
      begin
         return (To_Unbounded_String (Label),
                 new Text_Type'(Template),
                 new Node_Rewriting_Handle_Array'(Arguments));
      end Create;

      Test_Vector : array (Positive range <>) of Test_Case :=
        (Create ("standalone {", "{a"),
         Create ("trailing standalone {", "a{"),
         Create ("standalone }", "}a"),
         Create ("too many args (1)", "1", (1 => N)),
         Create ("too many args (2)", "{}", (N, N)),
         Create ("missing args (0)", "{}"),
         Create ("missing args (1)", "{} {}", (1 => N)),
         Create ("cannot reparse", "def"));
   begin
      for Test of Test_Vector loop
         declare
            Dummy : Node_Rewriting_Handle;
         begin
            Put_Line ("* " & To_String (Test.Label));
            Dummy := Create_From_Template
              (RH, Test.Template.all, Test.Arguments.all, Expr_Rule);
            Put_Line ("  Done with no exception");
         exception
            when Exc : others =>
               Put_Line ("  Got a " & Exception_Name (Exc));
         end;
         Free (Test.Template);
         Free (Test.Arguments);
      end loop;
   end;

   New_Line;
   Put_Line ("Use templates to create trees of node rewriting handles...");
   declare
      Nested_Expr  : constant Node_Rewriting_Handle :=
         Create_From_Template (RH, "(a + 3)", (1 .. 0 => <>), Expr_Rule);
      Full_Expr    : constant Node_Rewriting_Handle :=
         Create_From_Template (RH, "(b + {})", (1 => Nested_Expr), Expr_Rule);
      Second_Child : constant Node_Rewriting_Handle := Last_Child (N);
   begin
      Set_Child (Second_Child, Member_Refs.Decl_F_Expr, Full_Expr);
   end;

   New_Line;
   Put_Line ("Applying the diff...");
   Process_Apply (RH);

   Root (U).Print (Show_Slocs => False);

   Put_Line ("templates.adb: Done.");
end Templates;
