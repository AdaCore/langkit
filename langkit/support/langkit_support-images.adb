package body Langkit_Support.Images is

   function Stripped_Image (I : Integer) return String is
      Result : constant String := Integer'Image (I);
   begin
      return (if Result (Result'First) = ' '
              then Result (Result'First + 1 .. Result'Last)
              else Result);
   end Stripped_Image;

end Langkit_Support.Images;
