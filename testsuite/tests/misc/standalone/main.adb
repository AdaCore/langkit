with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text;
with Libbarlang.Analysis;
with Libfoolang.Analysis;
with Libfoolang_Support.Text;

procedure Main is

   procedure Check_Foo;
   procedure Check_Bar;

   ---------------
   -- Check_Foo --
   ---------------

   procedure Check_Foo is
      use Libfoolang.Analysis;
      use Libfoolang_Support.Text;

      U : constant Analysis_Unit := Create_Context.Get_From_Buffer
        (Filename => "main.txt", Buffer => "example");
   begin
      if U.Has_Diagnostics then
         Put_Line ("Errors:");
         for D of U.Diagnostics loop
            Put_Line ("  " & U.Format_GNU_Diagnostic (D));
         end loop;
      else
         Put_Line ("Success: " & Image (U.Text, With_Quotes => True));
      end if;
      New_Line;
   end Check_Foo;

   ---------------
   -- Check_Bar --
   ---------------

   procedure Check_Bar is
      use Langkit_Support.Text;
      use Libbarlang.Analysis;

      U : constant Analysis_Unit := Create_Context.Get_From_Buffer
        (Filename => "main.txt", Buffer => "example");
   begin
      if U.Has_Diagnostics then
         Put_Line ("Errors:");
         for D of U.Diagnostics loop
            Put_Line ("  " & U.Format_GNU_Diagnostic (D));
         end loop;
      else
         Put_Line ("Success: " & Image (U.Text, With_Quotes => True));
      end if;
      New_Line;
   end Check_Bar;

begin
   Put_Line ("main.adb: Starting...");

   Put_Line ("Libfoolang:");
   Check_Foo;

   Put_Line ("Libbarlang:");
   Check_Bar;

   Put_Line ("main.adb: Done.");
end Main;
