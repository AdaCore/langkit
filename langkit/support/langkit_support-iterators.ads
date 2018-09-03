------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.   See the  GNU  General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with this software;  see file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

--  This generic package provides for each Element_Type an interface that
--  used to implement iterators.
--
--  Beyond the fact that it's super simple, the difference with standard
--  iterators is that iteration is destructive: once an element has been
--  yielded, it's not possible to re-yield it once more.
--
--  The analogy with standard Ada containers is that there's no way to keep a
--  cursor to preserve an iteration state.

with Ada.Unchecked_Deallocation;

generic
   type Element_Type is private;
   type Element_Array is array (Positive range <>) of Element_Type;
package Langkit_Support.Iterators is

   type Iterator is limited interface;
   type Iterator_Access is access all Iterator'Class;
   --  Iterator interface: iterator consumers do not need to mind about
   --  concrete interator implementations.

   function Next
     (I       : in out Iterator;
      Element : out Element_Type) return Boolean is abstract;
   --  Get the next iteration element. If there was no element to yield
   --  anymore, return False. Otherwise, return True and set Element.

   procedure Iterate
     (I    : in out Iterator'Class;
      Proc : access procedure (Element : Element_Type));
   --  Consume the I iterator completely, calling Proc on all yielded elements

   function Consume (I : Iterator'Class) return Element_Array;
   --  Consume the I iterator completely, putting the results in an array and
   --  returning it.

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Iterator'Class, Iterator_Access);

end Langkit_Support.Iterators;
