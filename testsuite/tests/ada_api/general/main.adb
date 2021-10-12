with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   U : constant Analysis_Unit := Create_Context.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "a # foo"
                   & ASCII.LF & "b error # bar");

   U2 : constant Analysis_Unit := Create_Context.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "");

   function Visit (N : Foo_Node'Class) return Visit_Status;
   --  Callback for Libfoolang.Analysis.Traverse

   -----------
   -- Visit --
   -----------

   function Visit (N : Foo_Node'Class) return Visit_Status is
   begin
      Put_Line (N.Image);
      Put_Line ("First child index = " & N.First_Child_Index'Image);
      Put_Line ("Last child index = " & N.Last_Child_Index'Image);
      Put_Line ("First child = " & N.First_Child.Image);
      Put_Line ("Last child = " & N.First_Child.Image);
      New_Line;
      return Into;
   end Visit;

begin
   Put_Line ("Testing first/last child methods");
   Put_Line ("================================");
   New_Line;
   U.Root.Traverse (Visit'Access);
   U2.Root.Traverse (Visit'Access);

   Put_Line ("main.adb: Done.");
end Main;
