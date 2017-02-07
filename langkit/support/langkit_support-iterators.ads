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
package Langkit_Support.Iterators is

   type Iterator is limited interface;
   type Iterator_Access is access all Iterator'Class;
   --  Iterator interface: iterator consumers do not need to mind about
   --  concrete interator implementations.

   function Next
     (I        : in out Iterator;
      Element  : out Element_Type) return Boolean is abstract;
   --  Get the next iteration element. If there was no element to yield
   --  anymore, return False. Otherwise, return True and set Element.

   procedure Iterate
     (I    : in out Iterator'Class;
      Proc : access procedure (Element : Element_Type));
   --  Consume the I iterator completely, calling Proc on all yielded elements

   type Elements_Array is array (Positive range <>) of Element_Type;

   function Consume
     (I : Iterator'Class) return Elements_Array;
   --  Consume the I iterator completely, putting the results in an array and
   --  returning it.

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Iterator'Class, Iterator_Access);

end Langkit_Support.Iterators;
