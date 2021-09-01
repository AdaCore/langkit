with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Hashed_Sets;

with Libfoolang.Common;   use Libfoolang.Common;
with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   U : constant Analysis_Unit := Create_Context.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "a # foo"
                   & ASCII.LF & "b error # bar");

   U2 : constant Analysis_Unit := Create_Context.Get_From_Buffer
     (Filename => "main.txt",
      Buffer   => "");

   function Visit (N : Foo_Node'Class) return Visit_Status;

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

   Put_Line ("Testing generic container instantiation's node equality");
   Put_Line ("=======================================================");
   New_Line;

   --  Here, we test that metadata is ignored in equality, and that the proper
   --  equality function is used when instantiating containers. We do that by
   --  inserting two entities in a set that differ by their metadata, but
   --  represent the same entity. This should trigger an error.
   declare
      package Node_Sets is new Ada.Containers.Hashed_Sets
       (Element_Type => Decl_List,
        Hash => Hash, "=" => "=", Equivalent_Elements => "=");

      S : Node_Sets.Set;
   begin
      S.Insert (U.Root.P_Set_Dummy.As_Decl_List);

      begin
         S.Insert (U.Root.As_Decl_List);
      exception
         when Constraint_Error =>
            Put_Line ("Element already in set");
      end;
   end;

   Put_Line ("main.adb: Done.");
end Main;
