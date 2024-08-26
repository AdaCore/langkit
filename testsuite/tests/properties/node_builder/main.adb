with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   U : Analysis_Unit;
   N : Literal_Sequence;
begin
   Put_Line ("main.adb: Running...");
   New_Line;

   U := Create_Context.Get_From_Buffer
     (Filename => "main.txt", Buffer => "(main 1, 2, 3)");
   if U.Has_Diagnostics then
      for D of U.Diagnostics loop
         Put_Line (U.Format_GNU_Diagnostic (D));
      end loop;
      raise Program_Error;
   end if;

   N := U.Root.As_Literal_Sequence;

   Put_Line ("== Copy_Non_Null ==");
   declare
      R : constant Literal_Sequence := N.F_Lf_Copy_Non_Null;
   begin
      Put_Line (R.Image);
      if R /= N then
         raise Program_Error;
      end if;
   end;
   New_Line;

   Put_Line ("== Copy_Null ==");
   declare
      R : constant Literal_Sequence := N.F_Lf_Copy_Null;
   begin
      Put_Line (R.Image);
      if not R.Is_Null then
         raise Program_Error;
      end if;
   end;
   New_Line;

   Put_Line ("== User_Field ==");
   declare
      R : constant Synth_User_Field := N.F_Lf_User_Field;
   begin
      Put_Line (R.Image);
      Put_Line ("Number: " & R.P_Get_Number.Image);
   end;
   New_Line;

   Put_Line ("== Non_Nullable_Null ==");
   declare
      R : Synth_Non_Nullable;
   begin
      R := N.F_Lf_Non_Nullable_Null;
      Put_Line (R.Image);
   exception
      when Exc : Property_Error =>
         Put_Line ("Property_Error: " & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("== Non_Nullable_Not_Null ==");
   declare
      R : Synth_Non_Nullable;
   begin
      R := N.F_Lf_Non_Nullable_Not_Null;
      Put_Line (R.Image);
   end;
   New_Line;

   Put_Line ("== Nullable_Null ==");
   declare
      R : Synth_Nullable;
   begin
      R := N.F_Lf_Nullable_Null;
      Put_Line (R.Image);
   exception
      when Exc : Property_Error =>
         Put_Line ("Property_Error: " & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("== Nullable_Not_Null ==");
   declare
      R : Synth_Nullable;
   begin
      R := N.F_Lf_Nullable_Not_Null;
      Put_Line (R.Image);
   end;
   New_Line;

   Put_Line ("== Parent_Root ==");
   declare
      R  : Synth_Parent;
      C1 : Synth_Child1;
      C2 : Synth_Child2;
      C3 : Foo_Node;
   begin
      R := N.F_Lf_Parent_Root;
      Put_Line ("R: " & R.Image);
      Put_Line ("R.Parent: " & R.Parent.Image);
      New_Line;

      C1 := R.F_F.As_Synth_Child1;
      Put_Line ("R.F_F: " & C1.Image);
      Put_Line ("R.F_F.Parent: " & C1.Parent.Image);
      New_Line;

      C2 := C1.F_F.As_Synth_Child2;
      Put_Line ("R.F_F.F_F: " & C2.Image);
      Put_Line ("R.F_F.F_F.Parent: " & C2.Parent.Image);
      New_Line;

      C3 := C2.F_F;
      Put_Line ("R.F_F.F_F.F_F: " & C3.Image);
      Put_Line ("R.F_F.F_F.F_F.Parent: " & C3.Parent.Image);
   end;
   New_Line;

   Put_Line ("== Parent_Self ==");
   declare
      R  : Synth_Parent;
      C1 : Synth_Child1;
      C2 : Foo_Node;
   begin
      R := N.F_Lf_Parent_Child;
      Put_Line ("R: " & R.Image);
      Put_Line ("R.Parent: " & R.Parent.Image);
      New_Line;

      C1 := R.F_F.As_Synth_Child1;
      Put_Line ("R.F_F: " & C1.Image);
      Put_Line ("R.F_F.Parent: " & C1.Parent.Image);
      New_Line;

      C2 := C1.F_F;
      Put_Line ("R.F_F.F_F: " & C2.Image);
      Put_Line ("R.F_F.F_F.Parent: " & C2.Parent.Image);
   end;
   New_Line;

   Put_Line ("== Parent_Null ==");
   declare
      R : Synth_Parent;
   begin
      R := N.F_Lf_Parent_Null;
      Put_Line ("R: " & R.Image);
      Put_Line ("R.Parent: " & R.Parent.Image);
      Put_Line ("R.F_F: " & R.F_F.Image);
   end;
   New_Line;

   Put_Line ("== Parent_Foreign ==");
   declare
      R : Synth_Parent;
   begin
      R := N.F_Lf_Parent_Foreign;
      Put_Line (R.Image);
   exception
      when Exc : Property_Error =>
         Put_Line ("Property_Error: " & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("== Factored_Node_Builder ==");
   declare
      R : Synth_Non_Nullable;
   begin
      R := N.F_Lf_Factored_Node_Builder;
      Put_Line (R.Image);
   exception
      when Exc : Property_Error =>
         Put_Line ("Property_Error: " & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("== Staged_Node_Builder ==");
   declare
      R : Synth_Non_Nullable;
   begin
      R := N.F_Lf_Staged_Node_Builder;
      Put_Line (R.Image);
   exception
      when Exc : Property_Error =>
         Put_Line ("Property_Error: " & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("main.adb: Done.");
end Main;
