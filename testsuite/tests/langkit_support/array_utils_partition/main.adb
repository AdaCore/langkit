with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Array_Utils;

procedure Main is

   type Int_Array is array (Positive range <>) of Integer;
   package Int_Arrays is new Langkit_Support.Array_Utils
     (Index_Type   => Positive,
      Element_Type => Integer,
      Array_Type   => Int_Array);

   function Predicate (I : Integer) return Boolean is (I >= 0);

   function Image (I : Integer) return String is
      S : constant String := I'Img;
   begin
      return S (S'First + (if S (S'First) = ' ' then 1 else 0) .. S'Last);
   end Image;

   procedure Put (A : Int_Array) is
      First : Boolean := True;
   begin
      Put ('(');
      if A'Length > 0 then
         Put (Image (A'First) & " => ");
      end if;
      for I of A loop
         if First then
            First := False;
         else
            Put (", ");
         end if;
         Put (Image (I));
      end loop;
      Put (')');
   end Put;

   procedure Run (A : Int_Array) is
      PA : Int_Array := A;
      L  : Integer;
   begin
      Int_Arrays.Partition (PA, Predicate'Access, L);

      Put (A);
      New_Line;
      Put ("  -> ");
      Put ('[' & Image (L) & "] ");
      Put (PA);
      New_Line;
   end Run;

begin
   Run ((1 .. 0 => 0));
   Run ((1 => 1));
   Run ((1 => -1));
   Run ((1, 2));
   Run ((1, -2));
   Run ((1, 2, 3));
   Run ((-1, -2, -3));
   Run ((1, -2, 3, -4, 5));
end Main;
