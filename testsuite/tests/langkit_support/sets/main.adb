with Ada.Text_IO;              use Ada.Text_IO;

with Langkit_Support.Cheap_Sets;

procedure Main is
   package Int_Sets is new Langkit_Support.Cheap_Sets
   (Integer, No_Element => -1);

   Int_Set : Int_Sets.Set;

   use Int_Sets;
begin
   pragma Assert (Add (Int_Set, 2));
   pragma Assert (Remove (Int_Set, 2));

   pragma Assert (Add (Int_Set, 3));
   pragma Assert (Add (Int_Set, 7));
   pragma Assert (Add (Int_Set, 9));
   pragma Assert (Add (Int_Set, 12));

   pragma Assert (Has (Int_Set, 2) = False);

   pragma Assert (Remove (Int_Set, 22) = False);
   pragma Assert (Add (Int_Set, 22));
   pragma Assert (Has (Int_Set, 22));
   pragma Assert (Remove (Int_Set, 22));
end Main;
