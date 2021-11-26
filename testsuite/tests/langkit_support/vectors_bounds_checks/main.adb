with Ada.Text_IO;              use Ada.Text_IO;
with System.Assertions;

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

   declare
      Dummy : Integer;
   begin
      Dummy := V.Get (6);
   exception
      when Constraint_Error =>
         Put_Line ("Out of bound access");
   end;

   declare
      Dummy : Integer;
   begin
      for I in 1 .. 6 loop
         Dummy := V.Pop;
      end loop;
   exception
      when System.Assertions.Assert_Failure =>
         Put_Line ("Out of bound access");
   end;

   V.Destroy;
end Main;
