with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Errors;      use Langkit_Support.Errors;
with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Names;       use Langkit_Support.Names;
with Langkit_Support.Text;        use Langkit_Support.Text;

with Libfoolang.Generic_API;

procedure Analysis is
   Id : Language_Id renames Libfoolang.Generic_API.Foo_Lang_Id;

   Ctx : Lk_Context;
   U   : Lk_Unit;
   N   : Lk_Node;
begin
   New_Line;

   Put_Line
     ("Language name: "
      & Image (Format_Name (Language_Name (Id), Camel_With_Underscores)));
   New_Line;

   Put_Line ("Grammar rules:");
   for I in 1 .. Last_Grammar_Rule (Id) loop
      declare
         Rule : constant Grammar_Rule_Ref := From_Index (Id, I);
      begin
         Put ("  " & Image (Format_Name (Grammar_Rule_Name (Rule),
                                         Camel_With_Underscores)));
         if Rule = Default_Grammar_Rule (Id) then
            Put_Line (" (default)");
         else
            New_Line;
         end if;
      end;
   end loop;
   New_Line;

   Put_Line ("Token kinds:");
   for I in 1 .. Last_Token_Kind (Id) loop
      declare
         Kind : constant Token_Kind_Ref := From_Index (Id, I);
      begin
         Put_Line ("  " & Image (Format_Name (Token_Kind_Name (Kind),
                                              Camel_With_Underscores)));
      end;
   end loop;
   New_Line;

   Put_Line ("Use of null context:");
   declare
      Dummy : Boolean;
   begin
      Dummy := Ctx.Has_Unit ("foo.txt");
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a Precondition_Failure exception: "
                   & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("Use of null unit:");
   begin
      --  Disable warnings about reading U before it is initialized: we have
      --  special provision to handle that case in the API, and we want to
      --  check that the behavior is deterministic here.
      pragma Warnings (Off);
      N := U.Root;
      pragma Warnings (On);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a Precondition_Failure exception: "
                   & Exception_Message (Exc));
   end;
   New_Line;

   Put_Line ("Use of null node:");
   begin
      N := N.Parent;
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Line ("Got a Precondition_Failure exception: "
                   & Exception_Message (Exc));
   end;
   New_Line;

   Ctx := Create_Context (Id);

   Put_Line ("Parsing example.txt...");
   U := Ctx.Get_From_File ("example.txt");
   N := U.Root;

   if U.Context /= Ctx then
      raise Program_Error with "wrong unit->context backlink";
   elsif N.Unit /= U then
      raise Program_Error with "wrong node->unit backlink";
   end if;

   Put_Line ("Base filename: " & (+Create (+U.Filename).Base_Name));

   declare
      Has_1 : constant Boolean := Ctx.Has_Unit ("example.txt");
      Has_2 : constant Boolean := Ctx.Has_Unit ("foo.txt");
   begin
      Put_Line ("Has example.txt? -> " & Has_1'Image);
      Put_Line ("Has foo.txt? -> " & Has_2'Image);
   end;

   Put_Line ("Line 2:");
   Put_Line ("  " & Image (U.Get_Line (2), With_Quotes => True));

   Put_Line ("Traversing its parsing tree...");
   declare
      function Visit (N : Lk_Node'Class) return Visit_Status;

      -----------
      -- Visit --
      -----------

      function Visit (N : Lk_Node'Class) return Visit_Status is
      begin
         Put_Line (N.Image);
         return Into;
      end Visit;

   begin
      N.Traverse (Visit'Access);
   end;
   New_Line;

   Put_Line ("Testing various node operations:");
   Put_Line ("Root.Is_Null -> " & N.Is_Null'Image);

   N := N.Next_Sibling;
   Put_Line ("Root.Next_Sibling.Image -> " & N.Image);
   Put_Line ("Root.Next_Sibling.Is_Null -> " & N.Is_Null'Image);

   N := U.Root.Child (2);
   Put_Line ("Root.Child (2).Image -> " & N.Image);

   declare
      Prev : constant Lk_Node := N.Previous_Sibling;
      Equal_1 : constant Boolean := Prev.Next_Sibling = N;
      Equal_2 : constant Boolean := Prev = N;
   begin
      Put_Line ("Root.Child (2).Previous_Sibling.Image -> " & Prev.Image);
      Put_Line
        ("[...].Previous_Sibling = [...] -> " & Equal_1'Image);
      Put_Line
        ("[...].Previous_Sibling = [...].Previous_Sibling.Next_Sibling -> "
         & Equal_2'Image);
   end;
   Put_Line ("Root.Children:");
   for C of U.Root.Children loop
      Put_Line ("  -> " & C.Image);
   end loop;
   New_Line;

   Put_Line ("Use of stale node reference:");
   U := Ctx.Get_From_File ("example.txt", Reparse => True);
   begin
      Put_Line ("--> " & N.Image);
      raise Program_Error;
   exception
      when Exc : Stale_Reference_Error =>
         Put_Line ("Got a Stale_Reference_Error exception: "
                   & Exception_Message (Exc));
   end;
   New_Line;
end Analysis;
