with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;

with Langkit_Support.Errors; use Langkit_Support.Errors;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is

   procedure Put_Title (Label : String);

   ---------------
   -- Put_Title --
   ---------------

   procedure Put_Title (Label : String) is
   begin
      Put_Line (Label);
      Put_Line ((1 .. Label'Length => '#'));
      New_Line;
   end Put_Title;

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

   Put_Title ("Int iterator");

   Put_Line ("Base array:");
   for Elem of U.Root.As_Example.P_Int_Array loop
      Put_Line ("  " & Elem'Image);
   end loop;

   Put_Line ("Iteration:");
   declare
      Iter : Integer_Iterator := Ex.P_Int_Iterator;
      Elem : Integer;
   begin
      while Next (Iter, Elem) loop
         Iter := Ex.P_Int_Iterator_Identity (Iter);
         Put_Line ("  " & Elem'Image);
      end loop;
   end;
   New_Line;

   Put_Line ("Identity on an unintialized (null) iterator...");
   declare
      Dummy : Integer_Iterator;
   begin
      Dummy := Ex.P_Int_Iterator_Identity (Dummy);
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
      Iter : Integer_Iterator := Ex.P_Int_Iterator;
   begin
      U.Reparse (Buffer => " example ");
      Ex := U.Root.As_Example;
      Iter := Ex.P_Int_Iterator_Identity (Iter);
      Put_Line ("... got no exception");
   exception
      when Stale_Reference_Error =>
         Put_Line ("... got a stale reference error");
   end;
   New_Line;

   Put_Title ("Bigint iterator");

   --  TODO (UB16-045)??? Enable this. Due to a GNAT bug, this currently runs
   --  invalid generated machine code.
   if False then
      Put_Line ("Base array:");
      declare
         Items : constant Big_Integer_Array :=
           U.Root.As_Example.P_Bigint_Array;
      begin
         for Elem of Items loop
            Put_Line ("  " & Image (Elem));
         end loop;
      end;
   end if;

   Put_Line ("Iteration:");
   declare
      Iter : constant Big_Integer_Iterator :=
         U.Root.As_Example.P_Bigint_Iterator;
      Elem : Big_Integer;
   begin
      while Next (Iter, Elem) loop
         Put_Line ("  " & Image (Elem));
      end loop;
   end;
   New_Line;

   Put_Title ("Entity iterator");

   Put_Line ("Base array:");
   for Elem of U.Root.As_Example.P_Entities_Array loop
      Put_Line ("  " & Image (Elem));
   end loop;

   Put_Line ("Iteration:");
   declare
      Iter : constant Example_Iterator :=
         U.Root.As_Example.P_Entities_Iterator;
      Elem : Example;
   begin
      while Next (Iter, Elem) loop
         Put_Line ("  " & Image (Elem));
      end loop;
   end;
   New_Line;

   Put_Line ("main.adb: Done.");
end Main;
