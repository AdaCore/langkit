with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Errors;
with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;

with Libfoolang.Generic_API; use Libfoolang.Generic_API;
with Libfoolang.Generic_API.Introspection;
use Libfoolang.Generic_API.Introspection;

procedure Main is

   procedure Put_Error (Exc : Exception_Occurrence);
   --  Print a description on the given exception occurrence

   ---------------
   -- Put_Error --
   ---------------

   procedure Put_Error (Exc : Exception_Occurrence) is
   begin
      Put_Line (Exception_Name (Exc) & ":");
      Put_Line ("  " & Exception_Message (Exc));
   end Put_Error;

   All_Nodes : constant Type_Ref_Array := All_Node_Types (Self_Id);
   Members   : constant Struct_Member_Ref_Array :=
     (Member_Refs.Foo_Node_F_Fld_1,
      Member_Refs.Foo_Node_F_Fld_2,
      Member_Refs.Foo_Node_F_Fld_3);
   Dummy     : Boolean;

begin
   Put_Line ("Is_Null_For truth table:");
   for M of Members loop
      declare
         Empty : Boolean := True;
      begin
         Put (Debug_Name (M) & ":");
         for N of All_Nodes loop
            if Is_Null_For (M, N) then
               Empty := False;
               Put (" " & Debug_Name (N));
            end if;
         end loop;
         if Empty then
            Put_Line (" <none>");
         else
            New_Line;
         end if;
      end;
   end loop;
   New_Line;

   Put_Line ("Calling on a null member:");
   begin
      Dummy := Is_Null_For (No_Struct_Member_Ref, Type_Refs.Foo_Node);
   exception
      when Exc : Langkit_Support.Errors.Precondition_Failure =>
         Put_Error (Exc);
   end;
   New_Line;

   Put_Line ("Calling on a null type:");
   begin
      Dummy := Is_Null_For (Member_Refs.Parent, No_Type_Ref);
   exception
      when Exc : Langkit_Support.Errors.Precondition_Failure =>
         Put_Error (Exc);
   end;
   New_Line;

   Put_Line ("Calling on a non-node type ref:");
   begin
      Dummy := Is_Null_For (Member_Refs.Parent, Type_Refs.Analysis_Unit);
   exception
      when Exc : Langkit_Support.Errors.Precondition_Failure =>
         Put_Error (Exc);
   end;
   New_Line;

   Put_Line ("Calling on a member not owned by the node");
   begin
      Dummy := Is_Null_For (Member_Refs.A1_F_Only_On_A1, Type_Refs.A2);
   exception
      when Exc : Langkit_Support.Errors.Precondition_Failure =>
         Put_Error (Exc);
   end;
   New_Line;

   Put_Line ("main.adb: Done.");
end Main;
