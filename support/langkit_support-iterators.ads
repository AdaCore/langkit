------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                         --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
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

generic
   type Element_Type is private;
   --  Type for values produced at each iteration

   type Element_Array is array (Positive range <>) of Element_Type;
   --  Array type to use when consume the iterator into an array of elements

package Langkit_Support.Iterators is

   type Iterator is interface;
   --  Abstraction for iterating over a container

   function Next
     (I       : in out Iterator;
      Element : out Element_Type) return Boolean is abstract;
   --  Get the next iteration element. If there was no element to yield
   --  anymore, return false. Otherwise, return true and set ``Element``.

   procedure Iterate
     (I    : in out Iterator'Class;
      Proc : access procedure (Element : Element_Type));
   --  Consume the ``I`` iterator completely, calling ``Proc`` on all yielded
   --  elements.

   function Consume (I : Iterator'Class) return Element_Array;
   --  Consume the ``I`` iterator completely and return an array that contain
   --  the yielded elements.

end Langkit_Support.Iterators;
