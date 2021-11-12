with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is

   type Integer_Array is array (Positive range <>) of Integer;

   procedure BIA (Big_Ints : out Big_Integer_Array; Ints : Integer_Array);
   --  Build the array of big integers corresponding to the ``Values`` array of
   --  integers.

   function Image (Values : Big_Integer_Array) return String;
   --  Serialize an array of big integers

   procedure Check (Left, Right : Big_Integer_Array);
   --  Report about the result of ``Left = Right``

   ---------
   -- BIA --
   ---------

   procedure BIA (Big_Ints : out Big_Integer_Array; Ints : Integer_Array) is
   begin
      for I in 0 .. Big_Ints'Length - 1 loop
         Big_Ints (Big_Ints'First + I).Set
           (GNATCOLL.GMP.Long (Ints (Ints'First + I)));
      end loop;
   end BIA;

   -----------
   -- Image --
   -----------

   function Image (Values : Big_Integer_Array) return String is
      Result : Unbounded_String;
   begin
      Append (Result, "(");
      Append (Result, Integer'Image (Values'First));
      Append (Result, " .. ");
      Append (Result, Integer'Image (Values'Last));
      Append (Result, " => ");
      if Values'Length = 0 then
         Append (Result, "<>");
      end if;
      for I in Values'Range loop
         if I > Values'First then
            Append (Result, ", ");
         end if;
         Append (Result, Image (Values (I)));
      end loop;
      Append (Result, ")");
      return To_String (Result);
   end Image;

   -----------
   -- Check --
   -----------

   procedure Check (Left, Right : Big_Integer_Array) is
   begin
      Put_Line (Image (Left) & " = " & Image (Right) & " = "
                & Boolean'Image (Left = Right));
   end Check;

   Empty_1 : constant Big_Integer_Array (1 .. 0) := (1 .. 0 => <>);
   Empty_2 : constant Big_Integer_Array (10 .. 9) := (10 .. 9 => <>);

   Single_10_A : Big_Integer_Array (1 .. 1);
   Single_10_B : Big_Integer_Array (10 .. 10);

   Triple_1 : Big_Integer_Array (1 .. 3);
   Triple_2 : Big_Integer_Array (1 .. 3);
   Triple_3 : Big_Integer_Array (1 .. 3);

begin
   BIA (Single_10_A, (1 => 10));
   BIA (Single_10_B, (1 => 20));

   BIA (Triple_1, (1, 2, 3));
   BIA (Triple_2, (1, 2, 3));
   BIA (Triple_3, (1, 3, 2));

   Check (Empty_1, Empty_2);
   Check (Empty_1, Single_10_A);
   Check (Empty_1, Triple_1);

   Check (Single_10_A, Single_10_B);

   Check (Triple_1, Triple_2);
   Check (Triple_1, Triple_3);

   Put_Line ("Done.");
end Main;
