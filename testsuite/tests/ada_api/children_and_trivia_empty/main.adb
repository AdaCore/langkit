with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   U : constant Analysis_Unit := Create_Context.Get_From_File ("main.txt");

   procedure P_Of (N : Foo_Node);
   --  Iterate over `N`'s children with the for .. of loop kind

   procedure P_In (N : Foo_Node);
   --  Iterate over `N`'s children with the for .. in loop kind

   ----------
   -- P_Of --
   ----------

   procedure P_Of (N : Foo_Node) is
   begin
      Put ("Node: " & N.Image);
      Put (" has the following children: [");
      for C of N.Children_And_Trivia loop
         if C.Kind = Child then
            Put (C.Node.Image);
         end if;
      end loop;
      Put_Line ("]");
   end P_Of;

   ----------
   -- P_In --
   ----------

   procedure P_In (N : Foo_Node) is
      A : constant Children_Array := N.Children_And_Trivia;
   begin
      Put ("Node: " & N.Image);
      Put (" has the following children: [");
      for C in A loop
         if Element (A, C).Kind = Child then
            Put (Element (A, C).Node.Image);
         end if;
      end loop;
      Put_Line ("]");
   end P_In;

begin
   P_Of (U.Root.Children (1));
   P_In (U.Root.Children (1));
   Put_Line ("main.adb: Done.");
end Main;
