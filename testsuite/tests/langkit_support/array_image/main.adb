with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Images;

procedure Main is
   function Str_Image is new Langkit_Support.Images.Array_Image
     (Character, Positive, String, Character'Image);
begin
   Put_Line (Str_Image (""));
   Put_Line (Str_Image ("Hello"));
   Put_Line (Str_Image ("Hello", Limit => 4));
end Main;
