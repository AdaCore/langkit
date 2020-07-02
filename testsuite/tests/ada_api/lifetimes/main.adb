with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is

   procedure Try_Node (N : Foo_Node; Label : String);

   --------------
   -- Try_Node --
   --------------

   procedure Try_Node (N : Foo_Node; Label : String) is
   begin
      Put_Line (Label);
      Put_Line (N.Image);
      Put_Line ("   ... did not get the expected stale reference error");
   exception
      when Stale_Reference_Error =>
         Put_Line ("   ... got the expected stale reference error");
   end Try_Node;

begin

   declare
      Dummy : constant Analysis_Context := Create_Context;
   begin
      null; --  Ctx is supposed to be destroyed when leaving the scope
   end;

   declare
      U : Analysis_Unit;
   begin
      declare
         C : constant Analysis_Context := Create_Context;
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

   declare
      U : constant Analysis_Unit := Create_Context.Get_From_Buffer
        (Filename => "main.txt",
         Buffer   => "example");
      R : constant Foo_Node := U.Root;
   begin
      U.Reparse ("example");
      Try_Node (R, "Using node after unit reparse");
   end;

   declare
      N : Foo_Node;
   begin
      declare
         U : constant Analysis_Unit := Create_Context.Get_From_Buffer
           (Filename => "main.txt", Buffer   => "example");
      begin
         N := U.Root;
      end;

      Try_Node (N, "Using node after context destruction");

      declare
         Dummy_Unit : constant Analysis_Unit := Create_Context.Get_From_Buffer
           (Filename => "main.txt", Buffer   => "example");
      begin
         Try_Node (N, "Using node after context re-use");
      end;
   end;

   Put_Line ("Done.");
end Main;
