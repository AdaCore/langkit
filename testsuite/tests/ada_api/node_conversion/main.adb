with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   U     : constant Analysis_Unit := Create_Context.Get_From_File ("foo.txt");
   Decls : Var_Decl_List;
   R     : Ref;
begin
   --  Downcast work only when the destination type is correct

   --  This one works
   Decls := U.Root.As_Var_Decl_List;
   Put_Line ("Decls: " & Decls.Image);

   --  This one does not
   begin
      R := U.Root.As_Ref;
      Put_Line ("UNREACHABLE" & R.Image);
   exception
      when Exc : Constraint_Error =>
         Put_Line ("R: Constraint_Error: " & Exception_Message (Exc));
   end;

   --  Upcasts always work

   declare
      F : constant Foo_Node := Decls.As_Foo_Node;
   begin
      Put_Line ("F: " & F.Image);
   end;

   Put_Line ("Done.");
end Main;
