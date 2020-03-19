with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

with Langkit_Support.Text; use Langkit_Support.Text;

procedure Main is
   function Create_Buffer return Unbounded_String;

   function Create_Buffer return Unbounded_String is
      Buffer : Unbounded_String;
   begin
      for I in 1 .. 1_500_000 loop
         Append (Buffer, "e ");
      end loop;
      return Buffer;
   end Create_Buffer;

   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "main.txt", Buffer => Create_Buffer);

   Count : Integer := 0;
begin
   Put_Line ("main.adb: Running...");
   if U.Has_Diagnostics then
      for D of U.Diagnostics loop
         Put_Line (Image (To_Text (D.Message)));
      end loop;
      raise Program_Error;
   end if;

   --  Attempting to iterate through an array:
   --     ```
   --     for Elem of U.Root.P_Get_As_Array loop
   --        Count:= Count + 1;
   --     end loop;
   --     ```
   --  Crashes with `raised STORAGE_ERROR: s-intman.adb:136 explicit raise`.

   --  It works when iterating using an iterator
   declare
      Iter : constant Foo_Node_Iterator := U.Root.P_Get_As_Iterator;
      Elem : Foo_Node;
   begin
      while Next (Iter, Elem) loop
         Count := Count + 1;
      end loop;
   end;

   Put_Line ("main.adb: Iterated over" & Count'Image & " elements.");
   Put_Line ("main.adb: Done.");
end Main;
