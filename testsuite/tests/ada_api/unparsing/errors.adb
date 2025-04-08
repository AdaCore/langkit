with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Prettier_Ada.Documents; use Prettier_Ada.Documents;

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Unparsing;
use Langkit_Support.Generic_API.Unparsing;

with Libfoolang.Generic_API; use Libfoolang.Generic_API;

procedure Errors is
   Config : constant Unparsing_Configuration :=
     Default_Unparsing_Configuration (Self_Id);
   Ctx    : constant Lk_Context := Create_Context (Self_Id);
   U      : constant Lk_Unit :=
     Ctx.Get_From_Buffer (Filename => "main.txt", Buffer => "invalid");
   Dummy  : Document_Type;
begin
   Put_Line ("errors.adb: starting..");
   New_Line;

   Put_Line ("Unparse_To_Prettier on a null node:");
   begin
      Dummy := Unparse_To_Prettier (No_Lk_Node, Config);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a precondition failure: " & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("Unparse_To_Prettier with parsing errors:");
   begin
      Dummy := Unparse_To_Prettier (U.Root, Config);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a precondition failure: " & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("Done.");
end Errors;
