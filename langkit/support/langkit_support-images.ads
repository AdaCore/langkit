--  This package provides string formatting helpers related to 'Image attribute
--  references.

package Langkit_Support.Images is

   function Stripped_Image (I : Integer) return String;
   --  Return the same as Integer'Image (I), but without any leading space

end Langkit_Support.Images;
