with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   Ctx : Analysis_Context;
   U   : Analysis_Unit;

   function Load (Filename, Buffer : String) return Analysis_Unit;

   ----------
   -- Load --
   ----------

   function Load (Filename, Buffer : String) return Analysis_Unit is
   begin
      return Result : constant Analysis_Unit :=
        Ctx.Get_From_Buffer (Filename, Buffer => Buffer)
      do
         if Result.Has_Diagnostics then
            for D of Result.Diagnostics loop
               Put_Line (Result.Format_GNU_Diagnostic (D));
            end loop;
            raise Program_Error;
         end if;
      end return;
   end Load;

begin
   Put_Line ("main.adb: Running...");

   Ctx := Create_Context;
   U := Load ("root.txt", "def root {}");
   U.Populate_Lexical_Env;

   U := Load ("foo.txt", "root {}");
   Put_Line ("Evaluating .f_synth...");
   begin
      Put_Line ("-> " & U.Root.Child (1).As_Regular_Block.F_Synth.Image);
   exception
      when Exc : Property_Error =>
         Put_Line ("Got a property error: " & Exception_Message (Exc));
   end;

   Put_Line ("main.adb: Done.");
end Main;
