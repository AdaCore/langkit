with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Common;        use Libfoolang.Common;
with Libfoolang.Introspection; use Libfoolang.Introspection;

procedure Main is

   procedure Put_Line (Exc : Exception_Occurrence);
   --  Helper to print exception information

   procedure Check (ET : Enum_Value_Kind);
   --  Check the introspection of enum types for ET for regular cases

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Exc : Exception_Occurrence) is
   begin
      Put_Line
        ("Error: " & Exception_Name (Exc) & " : " & Exception_Message (Exc));
   end Put_Line;

   -----------
   -- Check --
   -----------

   procedure Check (ET : Enum_Value_Kind) is
      Dummy : Type_Constraint (ET);
   begin
      Put_Line ("== " & Image (DSL_Name (Dummy)) & " ==");
      New_Line;

      Put_Line ("Default value index:" & Enum_Default_Value (ET)'Image);

      for I in 1 .. Enum_Last_Value (ET) loop
         Put_Line (I'Image & ": " & Image (Enum_Value_Name (ET, I)));
         if Enum_Index (Create_Enum (ET, I)) /= I then
            raise Program_Error;
         end if;
         if Lookup_Enum_Value (ET, Enum_Value_Name (ET, I)) /= I then
            raise Program_Error;
         end if;
      end loop;

      New_Line;
   end Check;

begin
   --  Check the API in regular cases

   Check (E1_Value);
   Check (E2_Value);

   --  Now check error cases

   declare
      subtype Values is Any_Enum_Value_Index
      with Static_Predicate => Values in 0 | 4;
   begin
      for I in Values loop
         Put_Line ("Enum_Value_Name: out of bounds (" & I'Image & ")");
         begin
            Put_Line (Image (Enum_Value_Name (E1_Value, I)));
         exception
            when Exc : Out_Of_Bounds_Error =>
               Put_Line (Exc);
         end;
         New_Line;
      end loop;
   end;

   Put_Line ("Lookup_Enum_Value: no such value");
   Put_Line ("->" & Lookup_Enum_Value (E1_Value, "foo")'Image);
   New_Line;

   Put_Line ("Create_Enum: out of bounds");
   declare
      V : Any_Value_Type;
   begin
      V := Create_Enum (E1_Value, 4);
   exception
      when Exc : Out_Of_Bounds_Error =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Enum_Index: bad type");
   begin
      Put_Line (Enum_Index (Create_Integer (0))'Image);
   exception
      when Exc : Bad_Type_Error =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Done.");
end Main;
