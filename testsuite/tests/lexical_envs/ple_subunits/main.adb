with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   Ctx  : constant Analysis_Context := Create_Context;
   Unit : constant Analysis_Unit := Get_From_File (Ctx, "main.txt");

   function Visit (Node : Foo_Node'Class) return Visit_Status;
   function Node_Image (Node : Foo_Node'Class) return String;

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

      declare
         Fullname : constant String := Get_Filename (Node.Unit);
         Basename : constant String := +Create (+Fullname).Base_Name;
      begin
         return ("<" & Node.Kind_Name & " " & Basename & ":"
                 & Image (Node.Sloc_Range) & ">");
      end;
   end Node_Image;

   Dummy : Visit_Status := Root (Unit).Traverse (Visit'Access);
begin
   for D of Diagnostics (Unit) loop
      Put_Line (Format_GNU_Diagnostic (Unit, D));
   end loop;
end Main;
