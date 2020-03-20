with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_File
     (Filename => "main.txt");

   Count : Integer := 0;
begin
   Put_Line ("main.adb: Running...");
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;

   --  Attempting to iterate using an array:
   --    ```
   --    for Elem of U.Root.P_Get_All_Examples loop
   --       Put_Line (Image (Elem));
   --    end loop;
   --    ```
   --  Crashes with `raised STORAGE_ERROR: s-intman.adb:136 explicit raise`.

   --  It works when iterating using an iterator
   declare
      Iter : constant Foo_Node_Iterator :=
         U.Root.P_Iter_Examples;
      Elem : Foo_Node;
   begin
      while Next (Iter, Elem) loop
         Count := Count + 1;
      end loop;
   end;

   Put_Line ("Iterated over " & Count'Image & " elements.");

   Put_Line ("main.adb: Done.");
end Main;
