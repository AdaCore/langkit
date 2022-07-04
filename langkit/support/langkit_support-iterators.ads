--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

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
