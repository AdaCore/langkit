with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;

   procedure Process_Unit (Label, Buffer : String);
   function Visit (Node : Foo_Node'Class) return Visit_Status;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Label, Buffer : String) is
      Unit : constant Analysis_Unit :=
        Ctx.Get_From_Buffer ("main.txt", Buffer => Buffer);
   begin
      Put_Line ("== " & Label & " ==");
      New_Line;
      for D of Diagnostics (Unit) loop
         Put_Line (Format_GNU_Diagnostic (Unit, D));
      end loop;
      Unit.Root.Traverse (Visit'Access);
      New_Line;
   end Process_Unit;

   -----------
   -- Visit --
   -----------

   function Visit (Node : Foo_Node'Class) return Visit_Status is
   begin
      Put_Line
        ("PLE root for " & Node.Image & ": " & Node.P_Get_Ple_Root.Image);
      return Into;
   end Visit;

begin
   Process_Unit ("No PLE root", "a = b");
   Process_Unit ("Root is PLE root", "{a}");
   Process_Unit
     ("Several roots",
      "{a}"
      & ASCII.LF & " {b c}"
      & ASCII.LF & " {d}");
end Main;
