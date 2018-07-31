with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
begin

   declare
      Dummy : constant Analysis_Context := Create;
   begin
      null; --  Ctx is supposed to be destroyed when leaving the scope
   end;

   declare
      U : Analysis_Unit;
   begin
      declare
         C : constant Analysis_Context := Create;
      begin
         U := C.Get_From_Buffer ("main.txt", "ASCII", "example");
      end;
      --  U keeps a reference to C, so C must be still alive at this point

      if U.Context.Get_From_File ("main.txt") /= U then
         raise Program_Error;
      end if;

      --  When leaving this scope, the context reference that U owns dies, so
      --  the context will be destroyed.
   end;

   Put_Line ("Done.");
end Main;
