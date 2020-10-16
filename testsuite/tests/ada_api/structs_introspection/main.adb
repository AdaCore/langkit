with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;

with Libfoolang.Analysis;      use Libfoolang.Analysis;
with Libfoolang.Common;        use Libfoolang.Common;
with Libfoolang.Introspection; use Libfoolang.Introspection;

procedure Main is

   procedure Put_Line (Exc : Exception_Occurrence);
   --  Helper to print exception information

   function Build_Big_Integer (Value : Integer) return Value_Type;
   --  Helper to workaround a GNAT crash

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Exc : Exception_Occurrence) is
   begin
      Put_Line
        ("Error: " & Exception_Name (Exc) & " : " & Exception_Message (Exc));
   end Put_Line;

   -----------------------
   -- Build_Big_Integer --
   -----------------------

   function Build_Big_Integer (Value : Integer) return Value_Type is
      BI : GNATCOLL.GMP.Integers.Big_Integer;
   begin
      BI.Set (GNATCOLL.GMP.Long (Value));
      return Create_Big_Integer (BI);
   end Build_Big_Integer;

begin
   Put_Line ("All struct types:");
   for Kind in Struct_Value_Kind loop
      declare
         Dummy : Type_Constraint (Kind);
      begin
         Put_Line ("  " & DSL_Name (Dummy));
      end;
   end loop;
   New_Line;

   Put_Line ("Fields for Point:");
   for F of Struct_Fields (Point_Value) loop
      Put_Line ("* " & Member_Name (F) & " : " & DSL_Name (Member_Type (F)));
   end loop;
   New_Line;

   Put_Line ("Struct creation (normal):");
   declare
      X_Value      : constant Value_Type := Build_Big_Integer (1);
      Y_Value      : constant Value_Type := Build_Big_Integer (2);
      Polym_Value  : constant Value_Type :=
         Create_Struct (Point_Value, (X_Value, Y_Value));
      Actual_Value : constant Point := As_Point (Polym_Value);
   begin
      for F of Struct_Fields (Point_Value) loop
         declare
            F_Value : constant Value_Type := Eval_Member (Polym_Value, F);
         begin
            Put_Line ("Polymorphic: " & Member_Name (F) & " = "
                      & As_Big_Integer (F_Value).Image);
         end;
      end loop;
      Put_Line ("Actual: X = " & X (Actual_Value).Image);
      Put_Line ("Actual: Y = " & Y (Actual_Value).Image);
   end;
   New_Line;

   Put_Line ("Struct creation (too few fields):");
   declare
      X_Value     : constant Value_Type := Build_Big_Integer (1);
      Polym_Value : Any_Value_Type;
   begin
      Polym_Value := Create_Struct (Point_Value, (1 => X_Value));
      Put_Line ("not supposed to be printed");
   exception
      when Exc : Bad_Type_Error =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Struct creation (too many fields):");
   declare
      X_Value     : constant Value_Type := Build_Big_Integer (1);
      Polym_Value : Any_Value_Type;
   begin
      Polym_Value := Create_Struct (Point_Value, (X_Value, X_Value, X_Value));
      Put_Line ("not supposed to be printed");
   exception
      when Exc : Bad_Type_Error =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Struct creation (type mismatch):");
   declare
      X_Value     : constant Value_Type := Build_Big_Integer (1);
      Polym_Value : Any_Value_Type;
   begin
      Polym_Value :=
         Create_Struct (Point_Value, (X_Value, Create_Boolean (True)));
      Put_Line ("not supposed to be printed");
   exception
      when Exc : Bad_Type_Error =>
         Put_Line (Exc);
   end;
   New_Line;

   declare
      X_Value     : constant Value_Type := Build_Big_Integer (1);
      Y_Value     : constant Value_Type := Build_Big_Integer (2);
      Polym_Value : constant Value_Type :=
         Create_Struct (Point_Value, (X_Value, Y_Value));
      No_Arg      : constant Value_Array := (1 .. 0 => <>);
      Result      : Any_Value_Type;
   begin
      Put_Line ("Member eval for struct (property):");
      begin
         Result := Eval_Member (Polym_Value, Foo_Node_Parent, No_Arg);
         Put_Line ("not supposed to be printed");
      exception
         when Exc : Bad_Type_Error =>
            Put_Line (Exc);
      end;
      New_Line;

      Put_Line ("Member eval for struct (wrong struct field):");
      begin
         Result := Eval_Member (Polym_Value, Node_Result_F_N, No_Arg);
         Put_Line ("not supposed to be printed");
      exception
         when Exc : Bad_Type_Error =>
            Put_Line (Exc);
      end;
      New_Line;

      Put_Line ("Member eval for struct (args):");
      begin
         Result := Eval_Member (Polym_Value, Point_F_X, (1 => X_Value));
         Put_Line ("not supposed to be printed");
      exception
         when Exc : Bad_Type_Error =>
            Put_Line (Exc);
      end;
      New_Line;
   end;

   Put_Line ("Done.");
end Main;
