------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

private with Langkit_Support.Generic_Bump_Ptr;

--  This package defines a functional singly-linked list type. You can only
--  prepend elements at the beginning of the list. Iterating on the list is
--  done via the ``Head`` and ``Tail`` operations, that will return the head
--  and tail of the list.
--
--  Ultimately, the collection of functional lists form a tree that all
--  converge to the same 'Root', that is the last element in the list.
--
--  To create a list, you concat elements to the empty list. Here is an example
--  of use:
--
--  .. code:: ada
--
--     with Ada.Text_IO; use Ada.Text_IO;
--     with Langkit_Support.Functional_Lists;
--
--     procedure Main is
--        package Int_Lists is new Langkit_Support.Functional_Lists (Integer);
--        use Int_Lists;
--
--        A : List := Create;
--        --  This list is init with Create, so will contain a pool
--
--        B : List;
--        --  This one is just init to ``No_List``. Careful! Pools will be
--        --  created when concat-ing to that.
--
--        D : List := 1 & (2 & (3 & B));
--        --  This one contains a new pool
--
--        E : List := 1 & (2 & (9 & No_List));
--        --  This one contains a new pool too
--
--        ----------------
--        -- Print_List --
--        ----------------
--
--        procedure Print_List (S : List) is
--        begin
--           if not Has_Element (S) then
--              return;
--           else
--              Put_Line (Head (S)'Image);
--              Print_List (Tail (S));
--           end if;
--        end Print_List;
--
--     begin
--        Print_List (A);
--        Print_List (1 & (2 & (3 & A)));
--        Print_List (D);
--        Print_List (122 & Tail (Tail (D)));
--        Print_List (E);
--
--        Destroy (A);
--        Destroy (D);
--        Destroy (E);
--     end Main;

generic
   type T is private;
package Langkit_Support.Functional_Lists is

   type List is private
     with Iterable => (First       => Iter_First,
                       Next        => Iter_Next,
                       Has_Element => Iter_Has_Element,
                       Element     => Iter_Element);
   --  Reference to a linked list.
   --
   --  The resources management model for these list is based on pools:
   --  the ``Create`` function allocates a new pool, while ``&`` and ``Tail``
   --  return lists that refer to the pool of the given list. This means that
   --  several lists may share the same pool, and that deallocating that pool
   --  (using ``Destroy`` below) will make all other references to lists
   --  dangling.

   function Create return List;
   --  Create a new empty list

   procedure Destroy (Self : in out List);
   --  Free all resources allocated with ``Self``'s pool

   procedure Clear (Self : in out List);
   --  Remove all items from ``Self``.
   --
   --  Note that this does not deallocate them: if no other list shares the
   --  same pool, the corresponding memory allocations will leak.

   function "&" (Head : T; Tail : List) return List;
   --  Return a new list that prepends ``Head`` to ``Tail``. The result shares
   --  ``Tail``'s pool, unless it is ``No_List``, in which case a new pool is
   --  allocated.

   function Head (Self : List) return T;
   --  Return the head of ``Self``. Raise a ``Constraint_Error`` if ``Self`` is
   --  an empty list.

   function Tail (Self : List) return List;
   --  Return the tail of ``Self``. Raise a ``Constraint_Error`` if ``Self`` is
   --  an empty list. The result has the same pool as ``Self``.

   function Has_Element (Self : List) return Boolean;
   --  Return whether ``Self`` is *not* an empty list

   function Length (Self : List) return Natural;
   --  Return the number of elements in ``Self``

   procedure Push (Self : in out List; Head : T);
   --  Shortcut for ``Self := Head & Self``

   function Pop (Self : in out List) return T;
   --  Return the head of ``Self`` and replace it with its tail (i.e. remove
   --  its head). Raise a ``Constraint_Error`` if ``Self`` is an empty list.

   type T_Array is array (Positive range <>) of T;
   function To_Array (Self : List) return T_Array;
   --  Convert ``Self`` in the corresponding array. The head is the first array
   --  element, then comes the tail's own head, etc.

   No_List : constant List;
   --  Special value with no associated pool. Can be used as an empty list.

   --  The functions below are used to implement Iterable aspect. They are not
   --  meant for public consumption.

   function Iter_First (Self : List) return List is (Self);
   function Iter_Next (Dummy, Iter : List) return List is (Tail (Iter));
   function Iter_Has_Element (Dummy, Iter : List) return Boolean
   is (Has_Element (Iter));
   function Iter_Element (Dummy, Iter : List) return T is (Head (Iter));

private

   package Bump is new Langkit_Support.Generic_Bump_Ptr (T'Size * 16);
   --  Bump pointer pools to be able to store 16 elements of a list

   type List_Node;
   type Node_Ptr is access all List_Node;

   type List_Node is record
      El   : T;
      Next : Node_Ptr := null;
   end record;

   type List is record
      Pool   : Bump.Bump_Ptr_Pool;
      First  : Node_Ptr := null;
      Length : Natural := 0;
   end record;

   No_List : constant List := (Bump.No_Pool, null, 0);

end Langkit_Support.Functional_Lists;
