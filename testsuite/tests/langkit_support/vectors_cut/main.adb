with Ada.Text_IO; use Ada.Text_IO;
with System.Assertions;

with Langkit_Support.Vectors;

procedure Main is

   package Int_Vectors is new Langkit_Support.Vectors (Integer);
   procedure Put (V : Int_Vectors.Vector);
   function Create (N : Natural) return Int_Vectors.Vector;

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

   ------------
   -- Create --
   ------------

   function Create (N : Natural) return Int_Vectors.Vector is
   begin
      return Result : Int_Vectors.Vector do
         for I in 1 .. N loop
            Result.Append (I);
         end loop;
      end return;
   end Create;

   V : Int_Vectors.Vector;

begin
   Put ("Empty vector: ");
   V := Create (0);
   begin
      V.Cut (5);
      raise Program_Error;
   exception
      when System.Assertions.Assert_Failure =>
         Put_Line ("Out of bound access");
   end;
   V.Destroy;

   Put ("Out-of-bounds: ");
   V := Create (3);
   begin
      V.Cut (5);
      raise Program_Error;
   exception
      when System.Assertions.Assert_Failure =>
         Put_Line ("Out of bound access");
   end;
   V.Destroy;

   Put ("In-bounds: ");
   V := Create (5);
   V.Cut (3);
   Put (V);
   V.Destroy;
end Main;
