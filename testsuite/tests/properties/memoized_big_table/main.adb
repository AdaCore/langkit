with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx  : constant Analysis_Context := Create;
   Unit : constant Analysis_Unit :=
      Get_From_Buffer (Ctx, "foo.txt", Buffer => "example");
   Node : constant Example := Root (Unit).As_Example;
begin
   if Has_Diagnostics (Unit) then
      for D of Diagnostics (Unit) loop
         Put_Line (To_Pretty_String (D));
      end loop;
      raise Program_Error;
   end if;

   Discard_Errors_In_Populate_Lexical_Env (Ctx, False);

   --  Fill the memoization map with a lot of entries

   for I in 1 .. 500_000 loop
      declare
         Dummy : constant Integer := Node.P_Compute (I);
      begin
         null;
      end;
   end loop;

   --  ... then let the destructor do its magic. This is where the crash used
   --  to happen.
end Main;
