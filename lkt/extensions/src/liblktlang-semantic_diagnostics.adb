with Ada.Directories;                 use Ada.Directories;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
use Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;           use Ada.Wide_Wide_Text_IO;

with Liblktlang.Implementation.Extensions;
use Liblktlang.Implementation.Extensions;
with Liblktlang_Support.Slocs; use Liblktlang_Support.Slocs;

package body Liblktlang.Semantic_Diagnostics is

   ---------------------------
   -- Set_Solver_Debug_Mode --
   ---------------------------

   procedure Set_Solver_Debug_Mode (Enable : Boolean) is
      Unused : Boolean;
   begin
      Unused := Lkt_Node_P_Set_Solver_Debug_Mode (null, Enable);
   end Set_Solver_Debug_Mode;

   --------------------------------------
   -- Print_Solver_Diagnostics_In_Unit --
   --------------------------------------

   procedure Print_Solver_Diagnostics_In_Unit
     (Unit : Analysis.Analysis_Unit) is
   begin
      Print_Solver_Diagnostics_In_Node (Unit.Root);
   end Print_Solver_Diagnostics_In_Unit;

   --------------------------------------
   -- Print_Solver_Diagnostics_In_Node --
   --------------------------------------

   procedure Print_Solver_Diagnostics_In_Node (Node : Lkt_Node) is
   begin
      if Node.P_Xref_Entry_Point then
         for Diagnostic of Node.P_Nameres_Diagnostics loop
            Print_Solver_Diagnostic (Diagnostic);
         end loop;
      end if;
      for Child of Node.Children loop
         if not Child.Is_Null then
            Print_Solver_Diagnostics_In_Node (Child);
         end if;
      end loop;
   end Print_Solver_Diagnostics_In_Node;

   -----------------------------
   -- Print_Solver_Diagnostic --
   -----------------------------

   procedure Print_Solver_Diagnostic (Diag : Solver_Diagnostic)
   is
      Node     : constant Lkt_Node := Location (Diag).As_Lkt_Node;
      Filename : constant String := Node.Unit.Get_Filename;
   begin
      Ada.Text_IO.Put (Simple_Name (Filename) & ":");
      Put (Node.Sloc_Range.Image & ": error: ");
      Put_Line (Render_Solver_Diagnostic (Diag));

      Ada.Text_IO.Put (Node.Sloc_Range.Start_Line'Image & " | ");
      Put_Line (Node.Unit.Get_Line (Integer (Node.Sloc_Range.Start_Line)));

      declare
         S : String (1 .. Integer (Node.Sloc_Range.End_Column)) :=
            (others => ' ');
      begin
         Put ((Integer (Node.Sloc_Range.Start_Line'Image'Length)) * " ");
         Ada.Text_IO.Put (" | ");
         S (Integer (Node.Sloc_Range.Start_Column) ..
            Integer (Node.Sloc_Range.End_Column - 1)) :=
            (others => '^');
         Ada.Text_IO.Put_Line (S);
      end;
   end Print_Solver_Diagnostic;

   ------------------------------
   -- Render_Solver_Diagnostic --
   ------------------------------

   function Render_Solver_Diagnostic
     (Diag : Solver_Diagnostic) return Unbounded_Text_Type
   is
      --  Index to the position of the previous "{}" pattern found in the
      --  diagnostic message pattern while rendering the diagnostic.
      Last_Index : Natural := 1;
      Result     : Unbounded_Text_Type :=
        To_Unbounded_Wide_Wide_String (Message_Template (Diag));
   begin
      for Arg of Args (Diag) loop
         Last_Index := Index (Result, "{}", Last_Index);
         Replace_Slice
            (Result, Last_Index, Last_Index + 1, Arg.As_Decl.P_Full_Name);
      end loop;
      return Result;
   end Render_Solver_Diagnostic;

end Liblktlang.Semantic_Diagnostics;
