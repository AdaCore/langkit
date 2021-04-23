with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Names; use Langkit_Support.Names;
with Langkit_Support.Text;  use Langkit_Support.Text;

procedure Main is

   function "+" (N : Text_Type) return String
   is (Image (N, With_Quotes => True));

   procedure Check (Name : Text_Type; Casing : Casing_Convention);

   -----------
   -- Check --
   -----------

   procedure Check (Name : Text_Type; Casing : Casing_Convention) is
      N : Name_Type;
   begin
      Put_Line
        ("Checking " & Image (Name, With_Quotes => True)
         & " in " & Casing'Image);
      begin
         N := Create_Name (Name, Casing);
      exception
         when Invalid_Name_Error =>
            Put_Line ("  Invalid name");
            return;
      end;

      for C in Casing_Convention'Range loop
         Put_Line ("  " & C'Image & ": " & Image (Format_Name (N, C)));
      end loop;
   end Check;

begin
   for C in Casing_Convention'Range loop
      Check ("", C);
      Check ("_", C);
   end loop;
   New_Line;

   Check ("a", Camel_With_Underscores);
   Check ("A_", Camel_With_Underscores);
   Check ("_A", Camel_With_Underscores);
   Check ("A", Camel_With_Underscores);
   Check ("ABC", Camel_With_Underscores);
   Check ("A_BC", Camel_With_Underscores);
   Check ("A_Bc", Camel_With_Underscores);
   Check ("A_cB", Camel_With_Underscores);
   New_Line;

   Check ("a", Camel);
   Check ("ABc", Camel);
   Check ("AbcDEf", Camel);
   New_Line;

   Check ("A", Lower);
   Check ("ab_Cde", Lower);
   Check ("a", Lower);
   Check ("a_b_c", Lower);
   Check ("ab_c_de", Lower);
   New_Line;

   Check ("a", Upper);
   Check ("ABc", Upper);
   Check ("A", Upper);
   Check ("A_B_C", Upper);
   Check ("AB_C_DE", Upper);
   New_Line;

   --  Check Format_Name with uninitialized data (we have provisions for this
   --  case).

   Put_Line ("Calling Format_Name with uninitialized name...");
   declare
      pragma Warnings (Off);
      N : Name_Type;
      pragma Warnings (On);
   begin
      Put_Line ("  *** " & Image (Format_Name (N, Camel)));
   exception
      when Invalid_Name_Error =>
         Put_Line ("  Got the expected Invalid_Name_Error exception");
   end;
end Main;
