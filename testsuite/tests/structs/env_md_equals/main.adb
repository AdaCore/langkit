with Ada.Containers.Hashed_Sets;
with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis;  use Libfoolang.Analysis;
with Libfoolang.Common;    use Libfoolang.Common;
with Libfoolang.Iterators; use Libfoolang.Iterators;

procedure Main is
   U : constant Analysis_Unit :=
     Create_Context.Get_From_Buffer ("foo.txt", Buffer => "a b c");

   N : constant Libfoolang.Analysis.Name :=
      Find (U.Root, Kind_Is (Foo_Name)).Consume (1).As_Name;

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Foo_Node,
      Hash                => Hash,
      Equivalent_Elements => Equals,
      "="                 => Equals);

   S  : Node_Sets.Set;
   N1 : constant Foo_Node := N.P_With_Md (True, False);
   N2 : constant Foo_Node := N.P_With_Md (True, True);
   N3 : constant Foo_Node := N.P_With_Md (False, True);

   use Ada.Containers;
begin

   --  N1 and N2 will be considered equivalent, because the second metadata
   --  field is not considered in equality.
   S.Include (N1);
   S.Include (N2);

   Put_Line ("Set length = " & S.Length'Image & " (should be 1)");
   pragma Assert (S.Length = 1);

   --  N3 will *NOT* be considered equivalent to N1 and N2, because the first
   --  metadata field is considered in equality.
   S.Include (N3);

   Put_Line ("Set length = " & S.Length'Image & " (should be 2)");
   pragma Assert (S.Length = 2);

   Put_Line ("Done.");
end Main;
