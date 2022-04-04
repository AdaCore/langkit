with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Get_From_Buffer
     (Ctx, "foo.txt",
      Buffer =>
        ("scope_1 {" & ASCII.LF
         & "    a = scope_1" & ASCII.LF
         & "    b {" & ASCII.LF
         & "        c {" & ASCII.LF
         & "            unreachable = scope_1" & ASCII.LF
         & "        }" & ASCII.LF
         & "    }" & ASCII.LF
         & "    d = scope_1" & ASCII.LF
         & "}" & ASCII.LF
         & ASCII.LF
         & "scope_2 {" & ASCII.LF
         & "    e = scope_1.a" & ASCII.LF
         & "    f = scope_1.b" & ASCII.LF
         & "    g = scope_1.c" & ASCII.LF
         & "    h = scope_1.d" & ASCII.LF
         & "}" & ASCII.LF));

   function Visit (Node : Foo_Node'Class) return Visit_Status;

   -----------
   -- Visit --
   -----------

   function Visit (Node : Foo_Node'Class) return Visit_Status is
   begin
      if not Node.Is_Null and then Node.Kind = Foo_Var then
         declare
            Name : constant Id := Node.As_Var.F_Name;
            Decl : constant Foo_Node := Name.P_Resolve;
         begin
            Put_Line (Node.Image & " -> " & Decl.Image);
         end;
      end if;
      return Into;
   end Visit;

begin
   GNATCOLL.Traces.Parse_Config ("LIBFOOLANG.MAIN_TRACE=yes >&1");

   declare
      Dummy : constant Visit_Status := Root (U).Traverse (Visit'Access);
   begin
      null;
   end;

   Put_Line ("main.adb: Done.");
end Main;
