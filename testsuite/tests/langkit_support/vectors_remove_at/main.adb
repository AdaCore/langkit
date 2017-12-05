with Ada.Text_IO;              use Ada.Text_IO;

with Langkit_Support.Vectors;

procedure Main is

   package Int_Vectors is new Langkit_Support.Vectors (Integer);
   procedure Put (V : Int_Vectors.Vector);

   ---------
   -- Put --
   ---------

   procedure Put (V : Int_Vectors.Vector) is
   begin
      Put ('[');
      for I in V.First_Index .. V.Last_Index loop
         if I > V.First_Index then
            Put (", ");
         end if;
         Put (Integer'Image (V.Get (I)));
      end loop;
      Put (']');
      New_Line;
   end Put;

   V : Int_Vectors.Vector;

begin
   Put_Line ("Empty vector");
   Put (V);

   Put_Line ("Add 5 elements");
   for I in 1 .. 5 loop
      V.Append (I);
   end loop;
   Put (V);

   Put_Line ("Remove the middle element");
   V.Remove_At (3);
   Put (V);

   Put_Line ("Remove the first element");
   V.Remove_At (1);
   Put (V);

   Put_Line ("Remove the last element");
   V.Remove_At (3);
   Put (V);

   Put_Line ("Remove remaining elements");
   while V.Length > 0 loop
      V.Remove_At (1);
   end loop;
   Put (V);
end Main;
