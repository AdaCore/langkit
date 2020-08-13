with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Relative_Get;

procedure Main is
   type Char_Array is array (Natural range <>) of Character;
   function Length (A : Char_Array) return Natural is (A'Length);
   function Get (A : Char_Array; Index : Integer) return Character is
     (A (Index));

   function Relative_Get is new Langkit_Support.Relative_Get
     (Character, Char_Array, Length, Get);

   A    : constant Char_Array (0 .. 5) := ('A', 'B', 'C', 'D', 'E', 'F');
   Item : Character;
begin
   for I in -7 .. 6 loop
      if Relative_Get (A, I, Item) then
         Put_Line (I'Image & ": " & Item);
      else
         Put_Line (I'Image & ": out of bound");
      end if;
   end loop;
end Main;
