with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   procedure Check (Buffer : String);

   -----------
   -- Check --
   -----------

   procedure Check (Buffer : String) is
      U : constant Analysis_Unit :=
        Create_Context.Get_From_Buffer ("main.txt", Buffer => Buffer);
   begin
      Put_Line ("Buffer: " & Buffer);
      if U.Has_Diagnostics then
         Put_Line ("Diagnostics:");
         for D of U.Diagnostics loop
            Put_Line ("  " & U.Format_GNU_Diagnostic (D));
         end loop;
      else
         Put_Line ("No diagnostic");
      end if;
      New_Line;
   end Check;
begin
   Check ("example");
   Check ("example # trigger");
   Put_Line ("main.adb: Done.");
end Main;
