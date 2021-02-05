with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis;      use Libfoolang.Analysis;
with Libfoolang.Common;        use Libfoolang.Common;
with Libfoolang.Introspection; use Libfoolang.Introspection;

procedure Main is

   procedure Put_Line (Exc : Exception_Occurrence);
   --  Helper to print exception information

   function Build_Big_Integer_Array return Big_Integer_Array;
   --  Helper to workaround a GNAT crash

   function Image (Value : Value_Type; Indent : String := "") return String;
   --  Helper to print a value

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Exc : Exception_Occurrence) is
   begin
      Put_Line
        ("Error: " & Exception_Name (Exc) & " : " & Exception_Message (Exc));
   end Put_Line;

   -----------------------------
   -- Build_Big_Integer_Array --
   -----------------------------

   function Build_Big_Integer_Array return Big_Integer_Array is
   begin
      return Result : Big_Integer_Array (1 .. 3) do
         Result (1).Set (10);
         Result (2).Set (20);
         Result (3).Set (300);
      end return;
   end Build_Big_Integer_Array;

   -----------
   -- Image --
   -----------

   function Image (Value : Value_Type; Indent : String := "") return String is
      Result : Unbounded_String := To_Unbounded_String (Indent);
   begin
      case Kind (Value) is
         when Integer_Value =>
            Append (Result, As_Integer (Value)'Image);
         when Big_Integer_Value =>
            Append (Result, As_Big_Integer (Value).Image);

         when Array_Value_Kind =>
            Append
              (Result,
               "Array of "
               & Image (DSL_Name (Array_Element_Constraint (Kind (Value))))
               & ":" & ASCII.LF);
            declare
               N : constant Natural := Array_Length (Value);
            begin
               if N = 0 then
                  Append (Result, Indent & "<none>");
               else
                  for I in 1 .. N loop
                     if I > 1 then
                        Append (Result, ASCII.LF);
                     end if;
                     Append
                       (Result,
                        Indent & "* "
                        & Image (Array_Element (Value, N), Indent & "  "));
                  end loop;
               end if;
            end;

         when others =>
            raise Program_Error;
      end case;
      return To_String (Result);
   end Image;

   A : constant Value_Type :=
      Create_Big_Integer_Array (Build_Big_Integer_Array);
   V : constant Value_Type := Create_Boolean (True);
begin
   Put_Line ("Base array:");
   Put_Line (Image (A));
   New_Line;

   --  Get the "native" array of big integers so that we can then test
   --  "Create_Array".
   declare
      Values : Value_Array (1 .. 3);
      New_A  : Any_Value_Type;
   begin
      for I in Values'Range loop
         Values (I) := Array_Element (A, I);
      end loop;
      New_A := Create_Array (Big_Integer_Array_Value, Values);

      Put_Line ("After back-and-forth conversions:");
      Put_Line (Image (New_A));
   end;
   New_Line;

   --  Test error cases...

   Put_Line ("Calling Array_Length on a boolean...");
   declare
      Dummy : Natural;
   begin
      Dummy := Array_Length (V);
   exception
      when Exc : Bad_Type_Error =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Calling Array_Item on a boolean...");
   declare
      Dummy : Any_Value_Type;
   begin
      Dummy := Array_Element (V, 1);
   exception
      when Exc : Bad_Type_Error =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Calling Array_Item for out-of-bounds...");
   declare
      Dummy : Any_Value_Type;
   begin
      Dummy := Array_Element (A, 4);
   exception
      when Exc : Out_Of_Bounds_Error =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Calling Create_Array with heterogeneous inputs...");
   declare
      Dummy : Any_Value_Type;
   begin
      Dummy := Create_Array
        (Integer_Array_Value,
         (1 => Create_Boolean (False),
          2 => Create_Integer (3)));
   exception
      when Exc : Bad_Type_Error =>
         Put_Line (Exc);
   end;
   New_Line;

   Put_Line ("Done.");
end Main;
