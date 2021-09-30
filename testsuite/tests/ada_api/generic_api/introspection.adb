with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Errors;      use Langkit_Support.Errors;
with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;

with Libfoolang.Generic_API;

procedure Introspection is

   Id : Language_Id renames Libfoolang.Generic_API.Id;

   procedure Put_Exc (Exc : Exception_Occurrence);
   --  Print info about the given exception occurence

   -------------
   -- Put_Exc --
   -------------

   procedure Put_Exc (Exc : Exception_Occurrence) is
   begin
      Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
   end Put_Exc;

begin
   New_Line;

   Put_Line ("All types:");
   New_Line;
   for T in Value_Type'First .. Last_Value_Type (Id) loop
      Put_Line (Debug_Name (Id, T));
   end loop;
   New_Line;

   Put_Line ("Trying to get the debug name of an invalid type...");
   begin
      declare
         Dummy : constant String := Debug_Name (Id, Last_Value_Type (Id) + 1);
      begin
         raise Program_Error;
      end;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;
end Introspection;
