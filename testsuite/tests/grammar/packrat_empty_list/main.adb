with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is

   procedure Check_Parents (Node, Parent : Foo_Node; Prefix : String);

   -------------------
   -- Check_Parents --
   -------------------

   procedure Check_Parents (Node, Parent : Foo_Node; Prefix : String) is
   begin
      if Node = No_Foo_Node then
         return;
      end if;

      Put_Line (Prefix & Node.Image);

      if Node.Parent /= Parent then
         Put_Line (Prefix & "Invalid parent...");
      end if;

      for C of Node.Children loop
         Check_Parents (C, Node, Prefix & "  ");
      end loop;
   end Check_Parents;

   Buffer : constant String :=
     ("def a def b");
   Unit   : constant Analysis_Unit := Create_Context.Get_From_Buffer
     ("main.txt", Buffer => Buffer);

begin
   Check_Parents (Unit.Root, No_Foo_Node, "");
   Put_Line ("main.adb: Done.");
end Main;
