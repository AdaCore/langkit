with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : Analysis_Context := Create;
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
            Put_Line (Node.Short_Image & " -> " & Decl.Short_Image);
         end;
      end if;
      return Into;
   end Visit;

begin
   GNATCOLL.Traces.Parse_Config
     ("Main_Trace=yes >&1");
   GNATCOLL.Traces.Set_Active (Main_Trace, True);

   declare
      Dummy : constant Visit_Status := Root (U).Traverse (Visit'Access);
   begin
      null;
   end;

   Destroy (Ctx);
   Put_Line ("main.adb: Done.");
end Main;
