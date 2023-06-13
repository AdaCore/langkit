with Langkit_Support.Vectors;

procedure Main is

   package Int_Vectors is new Langkit_Support.Vectors (Integer);

   V : Int_Vectors.Vector;
begin
   V.Reserve (0);
   V.Reserve (0);
   V.Reserve (0);
   V.Reserve (0);
end Main;
