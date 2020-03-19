with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Errors; use Langkit_Support.Errors;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "example");
   Ex  : Example;
begin
   Put_Line ("main.adb: Running...");
   if U.Has_Diagnostics then
      raise Program_Error;
   end if;
   Ex := U.Root.As_Example;

   Put_Line ("Iterating through array...");
   for Elem of U.Root.As_Example.P_Values_Array loop
      Put_Line (Elem'Image);
   end loop;
   New_Line;

   Put_Line ("Iterating though iterator...");
   declare
      Iter : Integer_Iterator := Ex.P_Values_Iterator;
      Elem : Integer;
   begin
      while Next (Iter, Elem) loop
         Iter := Ex.P_Iterator_Identity (Iter);
         Put_Line (Elem'Image);
      end loop;
   end;
   New_Line;

   Put_Line ("Identity on an unintialized (null) iterator...");
   declare
      Dummy : Integer_Iterator;
   begin
      Dummy := Ex.P_Iterator_Identity (Dummy);
      Put_Line ("... got no exception");
   exception
      when Precondition_Failure =>
         Put_Line ("... got a precondition failure");
   end;
   New_Line;

   Put_Line ("Next on an unintialized (null) iterator...");
   declare
      Iter       : Integer_Iterator;
      Dummy_Int  : Integer;
      Dummy_Bool : Boolean;
   begin
      Dummy_Bool := Next (Iter, Dummy_Int);
      Put_Line ("... got no exception");
   exception
      when Precondition_Failure =>
         Put_Line ("... got a precondition failure");
   end;
   New_Line;

   Put_Line ("Identity on a stale iterator...");
   declare
      Iter : Integer_Iterator := Ex.P_Values_Iterator;
   begin
      U.Reparse (Buffer => " example ");
      Ex := U.Root.As_Example;
      Iter := Ex.P_Iterator_Identity (Iter);
      Put_Line ("... got no exception");
   exception
      when Stale_Reference_Error =>
         Put_Line ("... got a stale reference error");
   end;
   New_Line;

   Put_Line ("main.adb: Done.");
end Main;
