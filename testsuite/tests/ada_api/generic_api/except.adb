with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Errors;      use Langkit_Support.Errors;
with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;

with Libfoolang.Generic_API; use Libfoolang.Generic_API;

with Libfoolang_Support; use Libfoolang_Support;

procedure Except is
   procedure Check (Id : Exception_Id);
   --  Check if the given exception is considered as managed by Libfoolang

   -----------
   -- Check --
   -----------

   procedure Check (Id : Exception_Id) is
   begin
      Put_Line (Exception_Name (Id) & ":");
      Put_Line
        ("  Is managed? " & Is_Managed_Exception (Foo_Lang_Id, Id)'Image);
   end Check;

begin
   Check (Program_Error'Identity);
   Check (Property_Error'Identity);
   Check (My_Exception_1'Identity);
   Check (My_Exception_2'Identity);
end Except;
