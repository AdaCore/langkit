--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides string formatting helpers related to 'Image attribute
--  references.

package Langkit_Support.Images is

   function Stripped_Image (I : Integer) return String;
   --  Return the same as Integer'Image (I), but without any leading space

   generic
      type T is private;
      type Idx is (<>);
      type Array_Type is array (Idx range <>) of T;
      with function Image (Self : T) return String is <>;
   function Array_Image
     (Self : Array_Type; Limit : Positive := 80) return String;
   --  Return an image for the array, given an image function for elements. The
   --  array will be represented enclosed in brackets, and elements will be
   --  separated by colons. If the image is longer than ``Limit``, then some
   --  wrapping will be applied.

end Langkit_Support.Images;
