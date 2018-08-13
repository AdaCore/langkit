with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   Ctx    : constant Analysis_Context := Create_Context;
   Unit_1 : constant Analysis_Unit := Get_From_File (Ctx, "source1.txt");
   Unit_2 : constant Analysis_Unit := Get_From_File (Ctx, "source2.txt");
   Units  : constant array (Positive range <>) of Analysis_Unit :=
     (Unit_1, Unit_2);

   function Visit (Node : Foo_Node'Class) return Visit_Status;
   function Node_Image (Node : Foo_Node'Class) return String;
   procedure Resolve;

   -----------
   -- Visit --
   -----------

   function Visit (Node : Foo_Node'Class) return Visit_Status is
   begin
      if not Node.Is_Null and then Node.Kind = Foo_Var then
         declare
            V    : constant Var := Node.As_Var;
            Decl : constant Foo_Node := V.F_Value.P_Resolve;
         begin
            Put_Line (Node_Image (V) & " -> " & Node_Image (Decl));
         end;
      end if;
      return Into;
   end Visit;

   ----------------
   -- Node_Image --
   ----------------

   function Node_Image (Node : Foo_Node'Class) return String is
   begin
      if Node.Is_Null then
         return "None";
      end if;

      case Node.Kind is
         when Foo_Prefix | Foo_Id =>
            return Image (Node.Text);

         when Foo_Scope =>
            return "Scope " & Node_Image (Node.As_Scope.F_Name);

         when Foo_Var =>
            return "Var " & Node_Image (Node.As_Var.F_Name)
                   & " in " & Node_Image (Node.Parent.Parent);

         when others =>
            declare
               Fullname : constant String := Get_Filename (Node.Unit);
               Basename : constant String := +Create (+Fullname).Base_Name;
            begin
               return ("<" & Node.Kind_Name & " " & Basename & ":"
                       & Image (Node.Sloc_Range) & ">");
            end;
      end case;
   end Node_Image;

   -------------
   -- Resolve --
   -------------

   procedure Resolve is
      Dummy : Visit_Status;
   begin
      for U of Units loop
         Dummy := Root (U).Traverse (Visit'Access);
      end loop;
      New_Line;
   end Resolve;

begin
   Discard_Errors_In_Populate_Lexical_Env (Ctx, False);

   for U of Units loop
      Populate_Lexical_Env (U);
      for D of Diagnostics (U) loop
         Put_Line (Format_GNU_Diagnostic (U, D));
      end loop;
   end loop;

   Put_Line ("From scratch:");
   Resolve;

   Put_Line ("Reloading source1.txt:");
   Reparse (Unit_1);
   Resolve;

   Put_Line ("Reloading source2.txt:");
   Reparse (Unit_2);
   Resolve;

   Put_Line ("Done.");
end Main;
