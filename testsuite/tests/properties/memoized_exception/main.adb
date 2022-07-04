with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   Ctx          : constant Analysis_Context := Create_Context;
   Unit         : constant Analysis_Unit :=
      Get_From_Buffer (Ctx, "foo.txt", Buffer => "example");
   Example_Node : Example;
begin
   if Has_Diagnostics (Unit) then
      for D of Diagnostics (Unit) loop
         Put_Line (To_Pretty_String (D));
      end loop;
      raise Program_Error;
   end if;

   Discard_Errors_In_Populate_Lexical_Env (Ctx, False);

   Example_Node := Root (Unit).As_Example;

   Put_Line ("Calling P_Prop1...");
   for I in 1 .. 3 loop
      declare
         Dummy : Boolean;
      begin
         Dummy := Example_Node.P_Prop1;
      exception
         when Exc : Property_Error =>
            Put_Line ("Got an exception!");
            Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
      end;
   end loop;

   Put_Line ("Calling P_Prop2...");
   for I in 1 .. 3 loop
      declare
         Dummy : Boolean;
      begin
         Dummy := Example_Node.P_Prop2;
      exception
         when Exc : Property_Error =>
            Put_Line ("Got an exception!");
            Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
      end;
   end loop;

   Put_Line ("Calling P_Prop3...");
   for I in 1 .. 3 loop
      declare
         Dummy : Boolean;
      begin
         Dummy := Example_Node.P_Prop3;
      exception
         when Exc : Property_Error | Precondition_Failure =>
            Put_Line ("Got an exception!");
            Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
      end;
   end loop;
end Main;
