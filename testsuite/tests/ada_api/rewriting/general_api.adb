with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Common;    use Libfoolang.Common;
with Libfoolang.Generic_API.Introspection;
use Libfoolang.Generic_API.Introspection;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;

with Process_Apply;
with Support; use Support;

procedure General_API is
   Buffer : constant String :=
     ("def a = 1" & ASCII.LF
      & "def b = (2 + a) + 3" & ASCII.LF
      & "def c = a + b" & ASCII.LF);

   procedure Print_Exc (Exc : Exception_Occurrence);
   --  Helper to print exception information on the standard output

   ---------------
   -- Print_Exc --
   ---------------

   procedure Print_Exc (Exc : Exception_Occurrence) is
   begin
      Put_Line ("  " & Exception_Name (Exc) & ": " & Exception_Message (Exc));
   end Print_Exc;

   Ctx       : Analysis_Context := Create_Context;
   Dummy_Ctx : Analysis_Context;
   U         : Analysis_Unit :=
     Ctx.Get_From_Buffer ("main.txt", Buffer => Buffer);

   Other_Ctx : constant Analysis_Context := Create_Context;
   Other_U   : constant Analysis_Unit :=
     Other_Ctx.Get_From_Buffer ("main.txt", Buffer => "def z = 0");
   Other_RH  : Rewriting_Handle := Start_Rewriting (Other_Ctx);
   Other_NH  : constant Node_Rewriting_Handle :=
     Create_Token_Node (Other_RH, Foo_Name, "foo");

   Dummy_U      : Analysis_Unit;
   Dummy_N      : Foo_Node;
   RH, Dummy_RH : Rewriting_Handle;
   Dummy_UH     : Unit_Rewriting_Handle;
   NH           : Node_Rewriting_Handle;
   Dummy_Result : Apply_Result;
   Dummy_Text   : Unbounded_Text_Type;
   Dummy_Kind   : Foo_Node_Kind_Type;
   Dummy_Int    : Integer;
begin
   if Has_Diagnostics (U) then
      Put_Line ("Errors:");
      for D of Diagnostics (U) loop
         Put_Line (Format_GNU_Diagnostic (U, D));
      end loop;
      return;
   end if;

   ---------------------------------------------------------
   -- Getting rewriting handles out of rewriting sessions --
   ---------------------------------------------------------

   if Handle (Ctx) /= No_Rewriting_Handle then
      raise Program_Error;
   end if;

   Put_Line ("Get a unit rewriting handle out of a rewriting session");
   begin
      Dummy_UH := Handle (U);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Get a node rewriting handle out of a rewriting session");
   begin
      NH := Handle (U.Root);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   ------------------------------------------------
   -- Creating two concurrent rewriting sessions --
   ------------------------------------------------

   Put_Line ("Create a rewriting handle");
   RH := Start_Rewriting (Ctx);
   begin
      Put_Line ("Create a second rewriting handle");
      Dummy_RH := Start_Rewriting (Ctx);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   if Handle (Ctx) /= RH then
      raise Program_Error;
   elsif Context (RH) /= Ctx then
      raise Program_Error;
   end if;

   -----------------------------
   -- Rewriting and reparsing --
   -----------------------------

   --  Test that analysis context getters that return units are properly
   --  protected against invalid uses when there is an active rewriting
   --  context.

   begin
      Put_Line ("Call Get_From_File (Reparse => False)");
      begin
         Dummy_U := Ctx.Get_From_File ("helper.txt");
      exception
         when Exc : Precondition_Failure =>
            Print_Exc (Exc);
      end;

      Put_Line ("Call Get_From_File (Reparse => True)");
      begin
         Dummy_U := Ctx.Get_From_File ("helper.txt", Reparse => True);
      exception
         when Exc : Precondition_Failure =>
            Print_Exc (Exc);
      end;

      Put_Line ("Call Get_From_Buffer_Str");
      begin
         Dummy_U := Ctx.Get_From_Buffer ("helper.txt", Buffer => "");
      exception
         when Exc : Precondition_Failure =>
            Print_Exc (Exc);
      end;

      Put_Line ("Call Get_From_Buffer_Unb_Str");
      begin
         Dummy_U := Ctx.Get_From_Buffer
           ("helper.txt",
            Buffer => Ada.Strings.Unbounded.Null_Unbounded_String);
      exception
         when Exc : Precondition_Failure =>
            Print_Exc (Exc);
      end;

      Put_Line ("Call Get_With_Error");
      begin
         Dummy_U := Ctx.Get_With_Error ("helper.txt", "error message");
      exception
         when Exc : Precondition_Failure =>
            Print_Exc (Exc);
      end;
   end;

   -------------------------------
   -- Example rewriting session --
   -------------------------------

   Put_Line ("Get a rewriting handle for the analysis unit");
   Dummy_UH := Handle (U);

   Put_Line ("Apply the rewriting");
   Process_Apply (RH);

   if Handle (Ctx) /= No_Rewriting_Handle then
      raise Program_Error;
   end if;

   --  Test that we can do a new rewriting session once the previous one is
   --  done.

   Put_Line ("Create a second rewriting handler");
   RH := Start_Rewriting (Ctx);
   Put_Line ("Apply the rewriting");
   Process_Apply (RH);

   New_Line;
   Put_Line ("# Rewriting with a file reader");
   New_Line;
   Ctx := Create_Context (File_Reader => Create_File_Reader);
   U := Ctx.Get_From_File ("to_preprocess.txt");

   Put_Line ("Original source:");
   Put_Line (Image (U.Text, With_Quotes => True));
   New_Line;

   RH := Start_Rewriting (Ctx);
   Remove_Child (Handle (U.Root.Child (1)));
   Process_Apply (RH);
   Put_Line ("Rewritten source:");
   Put_Line (Image (U.Text, With_Quotes => True));
   New_Line;

   -----------------------------------------------------------
   -- Remaining error cases for Rewriting_Handle primitives --
   -----------------------------------------------------------

   RH := Start_Rewriting (Ctx);
   NH := Create_Token_Node (RH, Foo_Name, "foo");

   Put_Line ("Handle (No_Analysis_Context)");
   begin
      Dummy_RH := Handle (No_Analysis_Context);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Context (No_Rewriting_Handle)");
   begin
      Dummy_Ctx := Context (No_Rewriting_Handle);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Start_Rewriting (No_Analysis_Context)");
   begin
      Dummy_RH := Start_Rewriting (No_Analysis_Context);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Abort_Rewriting (No_Rewriting_Handle)");
   Dummy_RH := No_Rewriting_Handle;
   begin
      Abort_Rewriting (Dummy_RH);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Apply (No_Rewriting_Handle)");
   Dummy_RH := No_Rewriting_Handle;
   begin
      Dummy_Result := Apply (Dummy_RH);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Unit_Handles (No_Rewriting_Handle)");
   begin
      declare
         Dummy : constant Unit_Rewriting_Handle_Array :=
           Unit_Handles (Dummy_RH);
      begin
         null;
      end;
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   New_Line;

   Put_Line ("Handle (No_Analysis_Unit)");
   begin
      Dummy_UH := Handle (No_Analysis_Unit);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Unit (No_Unit_Rewriting_Handle)");
   begin
      Dummy_U := Unit (No_Unit_Rewriting_Handle);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Root (No_Unit_Rewriting_Handle)");
   begin
      NH := Root (No_Unit_Rewriting_Handle);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Set_Root (No_Unit_Rewriting_Handle, ...)");
   begin
      Set_Root (No_Unit_Rewriting_Handle, NH);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Unparse (No_Unit_Rewriting_Handle)");
   begin
      Dummy_Text := Unparse (No_Unit_Rewriting_Handle);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   New_Line;

   Put_Line ("Handle (No_Foo_Node)");
   begin
      NH := Handle (No_Foo_Node);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Node (No_Node_Rewriting_Handle)");
   begin
      Dummy_N := Node (No_Node_Rewriting_Handle);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Context (No_Node_Rewriting_Handle)");
   begin
      Dummy_RH := Context (No_Node_Rewriting_Handle);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Unparse (No_Node_Rewriting_Handle)");
   begin
      declare
         Dummy : constant Text_Type := Unparse (No_Node_Rewriting_Handle);
      begin
         null;
      end;
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Kind (No_Node_Rewriting_Handle)");
   begin
      Dummy_Kind := Kind (No_Node_Rewriting_Handle);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Image (No_Node_Rewriting_Handle)");
   begin
      Put_Line ("  " & Image (No_Node_Rewriting_Handle));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Tied (No_Node_Rewriting_Handle)");
   begin
      if Tied (No_Node_Rewriting_Handle) then
         Put_Line ("  Tied");
      else
         Put_Line ("  Not tied");
      end if;
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Parent (No_Node_Rewriting_Handle)");
   begin
      NH := Parent (No_Node_Rewriting_Handle);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Children_Count (No_Node_Rewriting_Handle)");
   begin
      Dummy_Int := Children_Count (No_Node_Rewriting_Handle);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Child (No_Node_Rewriting_Handle, ...)");
   begin
      NH := Child (No_Node_Rewriting_Handle, Member_Refs.Decl_F_Name);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Child (..., No_Struct_Member_Ref)");
   begin
      NH := Child (NH, No_Struct_Member_Ref);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Children (No_Node_Rewriting_Handle)");
   begin
      declare
         Dummy : constant Node_Rewriting_Handle_Array :=
           Children (No_Node_Rewriting_Handle);
      begin
         null;
      end;
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Set_Child (No_Node_Rewriting_Handle, ...)");
   begin
      Set_Child
        (No_Node_Rewriting_Handle, Member_Refs.Decl_F_Name, Handle (U.Root));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Set_Child (..., No_Struct_Member_Ref, ...)");
   begin
      Set_Child (Handle (U.Root.Child (1)), No_Struct_Member_Ref, NH);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Set_Child (..., Tied_Node)");
   begin
      Set_Child
        (Handle (U.Root.Child (1)),
         Member_Refs.Decl_F_Name,
         Handle (U.Root.Child (1).As_Decl.F_Args));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Set_Child (..., From_Other_Context)");
   begin
      Set_Child (Handle (U.Root.Child (1)), Member_Refs.Decl_F_Name, Other_NH);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Text (No_Node_Rewriting_Handle)");
   begin
      declare
         Dummy : constant Text_Type := Text (No_Node_Rewriting_Handle);
      begin
         null;
      end;
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Text (Non_Token_Node)");
   begin
      declare
         Dummy : constant Text_Type := Text (Handle (U.Root));
      begin
         null;
      end;
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Set_Text (No_Node_Rewriting_Handle, ...)");
   begin
      Set_Text (No_Node_Rewriting_Handle, "foo");
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Set_Text (Non_Token_Node, ...)");
   begin
      Set_Text (Handle (U.Root), "foo");
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Replace (..., Node_From_Other_Ctx)");
   begin
      Replace (Handle (U.Root), Other_NH);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Rotate (Node present twice)");
   declare
      N1 : constant Node_Rewriting_Handle := Handle (U.Root);
      N2 : constant Node_Rewriting_Handle := Handle (U.Root.Child (1));
   begin
      Rotate ((N1, N2, No_Node_Rewriting_Handle, N1));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Rotate ((..., Node_From_Other_Ctx))");
   begin
      Rotate ((Handle (U.Root), Handle (Other_U.Root)));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Is_List_Node (No_Node_Rewriting_Handle)");
   begin
      if Is_List_Node (No_Node_Rewriting_Handle) then
         Put_Line ("  List node");
      else
         Put_Line ("  Non-list node");
      end if;
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   New_Line;

   Put_Line ("First_Child (No_Node_Rewriting_Handle)");
   begin
      NH := First_Child (No_Node_Rewriting_Handle);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("First_Child (Non_List_Node)");
   begin
      NH := First_Child (Handle (U.Root.Child (1)));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Last_Child (No_Node_Rewriting_Handle)");
   begin
      NH := Last_Child (No_Node_Rewriting_Handle);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Last_Child (Non_List_Node)");
   begin
      NH := Last_Child (Handle (U.Root.Child (1)));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Next_Child (No_Node_Rewriting_Handle)");
   begin
      NH := Next_Child (No_Node_Rewriting_Handle);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Next_Child (Null_Parent)");
   begin
      NH := Next_Child (Handle (U.Root));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Next_Child (Non_List_Children)");
   begin
      NH := Next_Child (Handle (U.Root.Child (1).As_Decl.F_Name));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Previous_Child (No_Node_Rewriting_Handle)");
   begin
      NH := Previous_Child (No_Node_Rewriting_Handle);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Previous_Child (Null_Parent)");
   begin
      NH := Previous_Child (Handle (U.Root));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Previous_Child (Non_List_Children)");
   begin
      NH := Previous_Child (Handle (U.Root.Child (1).As_Decl.F_Name));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Insert_Before (No_Node_Rewriting_Handle, ...)");
   begin
      Insert_Before (No_Node_Rewriting_Handle, NH);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Insert_Before (Non_List_Children, ...)");
   begin
      Insert_Before (Handle (U.Root.Child (1).As_Decl.F_Name), NH);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Insert_Before (..., Tied_Node)");
   begin
      Insert_Before
        (Handle (U.Root.Child (1)), Handle (U.Root.Child (1).As_Decl.F_Name));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Insert_Before (..., Node_From_Other_Ctx)");
   begin
      Insert_Before (Handle (U.Root.Child (1)), Other_NH);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Insert_After (No_Node_Rewriting_Handle, ...)");
   begin
      Insert_After (No_Node_Rewriting_Handle, NH);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Insert_After (Non_List_Children, ...)");
   begin
      Insert_After (Handle (U.Root.Child (1).As_Decl.F_Name), NH);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Insert_After (..., Tied_Node)");
   begin
      Insert_After
        (Handle (U.Root.Child (1)), Handle (U.Root.Child (1).As_Decl.F_Name));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Insert_After (..., Node_From_Other_Ctx)");
   begin
      Insert_After (Handle (U.Root.Child (1)), Other_NH);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Insert_First (No_Node_Rewriting_Handle, ...)");
   begin
      Insert_First
        (No_Node_Rewriting_Handle, Handle (U.Root.Child (1).As_Decl.F_Name));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Insert_First (Non_List_Node, ...)");
   begin
      Insert_First
        (Handle (U.Root.Child (1)), Handle (U.Root.Child (1).As_Decl.F_Name));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Insert_First (..., Tied_Node)");
   begin
      Insert_First
        (Handle (U.Root), Handle (U.Root.Child (1).As_Decl.F_Name));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Insert_First (..., Node_From_Other_Ctx)");
   begin
      Insert_First (Handle (U.Root), Other_NH);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Insert_Last (No_Node_Rewriting_Handle, ...)");
   begin
      Insert_Last
        (No_Node_Rewriting_Handle, Handle (U.Root.Child (1).As_Decl.F_Name));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Insert_Last (Non_List_Node, ...)");
   begin
      Insert_Last
        (Handle (U.Root.Child (1)), Handle (U.Root.Child (1).As_Decl.F_Name));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Insert_Last (..., Tied_Node)");
   begin
      Insert_Last
        (Handle (U.Root), Handle (U.Root.Child (1).As_Decl.F_Name));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Insert_Last (..., Node_From_Other_Ctx)");
   begin
      Insert_Last (Handle (U.Root), Other_NH);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   New_Line;

   Put_Line ("Create_Node (No_Rewriting_Handle, ...)");
   begin
      NH := Create_Node (No_Rewriting_Handle, Foo_Def);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Create_Node (..., Error_Node_Kind)");
   begin
      NH := Create_Node (RH, Foo_Error_Decl);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Create_Token_Node (No_Rewriting_Handle, ...)");
   begin
      NH := Create_Token_Node (No_Rewriting_Handle, Foo_Name, "foo");
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Create_Token_Node (..., Non_Token_Node, ...)");
   begin
      NH := Create_Token_Node (RH, Foo_Plus, "foo");
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Create_Regular_Node (No_Rewriting_Handle, ...)");
   begin
      NH := Create_Regular_Node
        (No_Rewriting_Handle, Foo_Paren_Expr, (1 => No_Node_Rewriting_Handle));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Create_Regular_Node (..., Non_Regular_Node, ...)");
   begin
      NH := Create_Regular_Node
        (RH, Foo_Name, (1 .. 0 => No_Node_Rewriting_Handle));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Create_Regular_Node (..., Wrong_Children_Count)");
   begin
      NH := Create_Regular_Node
        (RH, Foo_Paren_Expr, (1 .. 2 => No_Node_Rewriting_Handle));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Create_Regular_Node (..., Tied_Node)");
   begin
      NH := Create_Regular_Node
        (RH, Foo_Paren_Expr, (2 => Handle (U.Root.Child (1).As_Decl.F_Name)));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Create_Regular_Node (..., Node_From_Other_Context)");
   begin
      NH := Create_Regular_Node (RH, Foo_Paren_Expr, (1 => Other_NH));
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   New_Line;

   Put_Line ("Create_From_Template (No_Rewriting_Handle, ...)");
   begin
      NH := Create_From_Template
        (No_Rewriting_Handle, "({})", (1 => NH), Expr_Rule);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Put_Line ("Create_From_Template (..., Standalone_Open, ...)");
   begin
      NH := Create_From_Template (RH, "{", (1 => NH), Expr_Rule);
   exception
      when Exc : Template_Format_Error =>
         Print_Exc (Exc);
   end;

   Put_Line ("Create_From_Template (..., Standalone_Close, ...)");
   begin
      NH := Create_From_Template (RH, "}", (1 => NH), Expr_Rule);
   exception
      when Exc : Template_Format_Error =>
         Print_Exc (Exc);
   end;

   Put_Line ("Create_From_Template (..., Invalid_Syntax, ...)");
   begin
      NH := Create_From_Template
        (RH, "|@-", (1 .. 0 => No_Node_Rewriting_Handle), Expr_Rule);
   exception
      when Exc : Template_Instantiation_Error =>
         Print_Exc (Exc);
   end;

   Put_Line ("Create_From_Template (..., Too_Few_Args ...)");
   begin
      NH := Create_From_Template (RH, "{} + {}", (1 => NH), Expr_Rule);
   exception
      when Exc : Template_Args_Error =>
         Print_Exc (Exc);
   end;

   Put_Line ("Create_From_Template (..., Too_Many_Args ...)");
   begin
      NH := Create_From_Template (RH, "({})", (1 .. 2 => NH), Expr_Rule);
   exception
      when Exc : Template_Args_Error =>
         Print_Exc (Exc);
   end;

   Put_Line ("Create_From_Template (..., Node_From_Other_Context)");
   begin
      NH := Create_From_Template (RH, "({})", (1 .. 2 => Other_NH), Expr_Rule);
   exception
      when Exc : Precondition_Failure =>
         Print_Exc (Exc);
   end;

   Abort_Rewriting (RH);
   Abort_Rewriting (Other_RH);
   New_Line;
   Put_Line ("main.adb: Done.");
end General_API;
