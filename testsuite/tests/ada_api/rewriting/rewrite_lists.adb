with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;
with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Common;    use Libfoolang.Common;
with Libfoolang.Generic_API.Introspection;
use Libfoolang.Generic_API.Introspection;
with Libfoolang.Rewriting; use Libfoolang.Rewriting;

with Process_Apply;

procedure Rewrite_Lists is
   Ctx  : constant Analysis_Context := Create_Context;
   U    : Analysis_Unit;
   RH   : Rewriting_Handle;
   N, C : Node_Rewriting_Handle;
begin
   --  Test basic list rewriting operations

   declare
      procedure Check (Label : String; List : Node_Rewriting_Handle);

      -----------
      -- Check --
      -----------

      procedure Check (Label : String; List : Node_Rewriting_Handle) is
         Expected_Count : Natural;
         Actual_Count   : Natural := 0;
         Prev, Current  : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
      begin
         Put_Line (Label);
         Expected_Count := Children_Count (List);
         Put_Line ("   ... list of" & Expected_Count'Image & " nodes:");
         Current := First_Child (List);
         while Current /= No_Node_Rewriting_Handle loop
            Actual_Count := Actual_Count + 1;
            if not Tied (Current) then
               raise Program_Error with "children must be tied";
            elsif Parent (Current) /= List then
               raise Program_Error with "unexpected parent";
            elsif Previous_Child (Current) /= Prev then
               raise Program_Error with "unexpected previous";
            elsif Prev /= No_Node_Rewriting_Handle
                  and then Next_Child (Prev) /= Current
            then
               raise Program_Error with "unexpected next";
            end if;
            Put_Line ("  " & Image (Current));
            Prev := Current;
            Current := Next_Child (Current);
         end loop;
         if Prev /= Last_Child (List) then
            raise Program_Error with "unexpected last";
         end if;
         if Actual_Count = 0 then
            Put_Line ("  <empty>");
         end if;
         if Actual_Count /= Expected_Count then
            raise Program_Error with "unexpected count";
         end if;
         New_Line;
      end Check;
   begin
      RH := Start_Rewriting (Ctx);

      N := Create_Node (RH, Foo_Name_List);
      Check ("fresh empty list", N);

      N := Create_Regular_Node
        (RH,
         Foo_Name_List,
         (1 => Create_Token_Node (RH, Foo_Name, "n1")));
      Check ("fresh 1-item list", N);

      N := Create_Regular_Node
        (RH,
         Foo_Name_List,
         (1 => Create_Token_Node (RH, Foo_Name, "n1"),
          2 => Create_Token_Node (RH, Foo_Name, "n2")));
      Check ("fresh 2-items list", N);

      N := Create_Node (RH, Foo_Name_List);
      Insert_First (N, Create_Token_Node (RH, Foo_Name, "n1"));
      Check ("empty + insert_first", N);

      N := Create_Node (RH, Foo_Name_List);
      Insert_First (N, Create_Token_Node (RH, Foo_Name, "n2"));
      Insert_First (N, Create_Token_Node (RH, Foo_Name, "n1"));
      Check ("empty + 2 insert_first", N);

      N := Create_Node (RH, Foo_Name_List);
      Insert_Last (N, Create_Token_Node (RH, Foo_Name, "n1"));
      Check ("empty + insert_last", N);

      N := Create_Node (RH, Foo_Name_List);
      Insert_Last (N, Create_Token_Node (RH, Foo_Name, "n1"));
      Insert_Last (N, Create_Token_Node (RH, Foo_Name, "n2"));
      Check ("empty + 2 insert_last", N);

      C := Create_Token_Node (RH, Foo_Name, "n2");
      N := Create_Regular_Node (RH, Foo_Name_List, (1 => C));
      Insert_Before (C, Create_Token_Node (RH, Foo_Name, "n1"));
      Check ("insert_before on the first", N);

      C := Create_Token_Node (RH, Foo_Name, "n3");
      N := Create_Regular_Node
        (RH,
         Foo_Name_List,
         (1 => Create_Token_Node (RH, Foo_Name, "n1"),
          2 => C));
      Insert_Before (C, Create_Token_Node (RH, Foo_Name, "n2"));
      Check ("insert_before on the second", N);

      C := Create_Token_Node (RH, Foo_Name, "n1");
      N := Create_Regular_Node (RH, Foo_Name_List, (1 => C));
      Insert_After (C, Create_Token_Node (RH, Foo_Name, "n2"));
      Check ("insert_after on the first", N);

      C := Create_Token_Node (RH, Foo_Name, "n2");
      N := Create_Regular_Node
        (RH,
         Foo_Name_List,
         (1 => Create_Token_Node (RH, Foo_Name, "n1"),
          2 => C));
      Insert_After (C, Create_Token_Node (RH, Foo_Name, "n3"));
      Check ("insert_after on the second", N);

      C := Create_Token_Node (RH, Foo_Name, "before");
      N := Create_Regular_Node (RH, Foo_Name_List, (1 => C));
      Replace (C, Create_Token_Node (RH, Foo_Name, "after"));
      Check ("replace single element", N);

      C := Create_Token_Node (RH, Foo_Name, "before");
      N := Create_Regular_Node
        (RH,
         Foo_Name_List,
         (1 => C,
          2 => Create_Token_Node (RH, Foo_Name, "sibling")));
      Replace (C, Create_Token_Node (RH, Foo_Name, "after"));
      Check ("replace first element", N);

      C := Create_Token_Node (RH, Foo_Name, "before");
      N := Create_Regular_Node
        (RH,
         Foo_Name_List,
         (1 => Create_Token_Node (RH, Foo_Name, "first"),
          2 => C,
          3 => Create_Token_Node (RH, Foo_Name, "last")));
      Replace (C, Create_Token_Node (RH, Foo_Name, "after"));
      Check ("replace middle element", N);

      C := Create_Token_Node (RH, Foo_Name, "before");
      N := Create_Regular_Node
        (RH,
         Foo_Name_List,
         (1 => Create_Token_Node (RH, Foo_Name, "sibling"),
          2 => C));
      Replace (C, Create_Token_Node (RH, Foo_Name, "after"));
      Check ("replace last element", N);

      for I in 1 .. 3 loop
         declare
            Children : constant Node_Rewriting_Handle_Array (1 .. 3) :=
              (1 => Create_Token_Node (RH, Foo_Name, "n1"),
               2 => Create_Token_Node (RH, Foo_Name, "n2"),
               3 => Create_Token_Node (RH, Foo_Name, "n3"));
         begin
            N := Create_Regular_Node (RH, Foo_Name_List, Children);
            Remove_Child (Children (I));
            Check ("remove_child on child" & I'Image, N);
         end;
      end loop;

      C := Create_Token_Node (RH, Foo_Name, "n1");
      N := Create_Regular_Node (RH, Foo_Name_List, (1 => C));
      Remove_Child (C);
      Check ("remove_child on the only child", N);

      N := Create_From_Template
        (RH, "def d = 0", (1 .. 0 => <>), Def_Rule_Rule);
      N := Child (N, Member_Refs.Decl_F_Args);
      Check ("create_from_template (empty list)", N);

      N := Clone (N);
      Check ("clone (empty list)", N);

      N := Create_From_Template
        (RH, "def d (n1, n2, n3) = 0", (1 .. 0 => <>), Def_Rule_Rule);
      N := Child (N, Member_Refs.Decl_F_Args);
      Check ("create_from_template (non-empty list)", N);

      N := Clone (N);
      Check ("clone (non-empty list)", N);

      Abort_Rewriting (RH);

      U := Ctx.Get_From_Buffer ("main.txt", Buffer => "def d = 0");
      RH := Start_Rewriting (Ctx);
      Check ("expanded (empty list)", Handle (U.Root.Child (1).As_Def.F_Args));
      Abort_Rewriting (RH);

      U := Ctx.Get_From_Buffer
        ("main.txt", Buffer => "def d (n1, n2, n3) = 0");
      RH := Start_Rewriting (Ctx);
      Check
        ("expanded (non-empty list)", Handle (U.Root.Child (1).As_Def.F_Args));
      Abort_Rewriting (RH);
   end;

   U := Ctx.Get_From_Buffer ("main.txt", Buffer => "");
   RH := Start_Rewriting (Ctx);
   N := Handle (U.Root);
   declare
      function Create_Def (Name, Lit : Text_Type) return Node_Rewriting_Handle
      is (Create_Def
            (RH,
             Create_Token_Node (RH, Foo_Name, Name),
             No_Node_Rewriting_Handle,
             Create_Token_Node (RH, Foo_Literal, Lit)));
   begin
      Insert_Last (N, Create_Def ("a", "1"));
      C := Create_Def ("b", "2");
      Insert_Last (N, C);
      Insert_Last (N, Create_Def ("c", "3"));
      Remove_Child (C);

      Put_Line ("Applying the diff...");
      Process_Apply (RH);

      Root (U).Print (Show_Slocs => False);
   end;

   Put_Line ("rewrite_lists.adb: Done.");
end Rewrite_Lists;
