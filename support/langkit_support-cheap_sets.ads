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

with Langkit_Support.Vectors;

--  This package implements generic sets in a fast and cheap way, using a
--  vector underneath. This is done because:
--
--  1. Ada.Containers.Sets are controlled objects, which is not always
--  acceptable.
--
--  2. It is expected that for the size of the data sets that we have, an array
--  will be more efficient.
--
--  3. Formal sets could be an option, but you have to precise their size in
--  advance, which is not convenient in our case.

generic
   type Element_Type is private;
   No_Element : Element_Type;
   with function "=" (L, R : Element_Type) return Boolean is <>;
package Langkit_Support.Cheap_Sets is

   package Elements_Vectors is new Langkit_Support.Vectors (Element_Type);

   type Set is private;
   --  For ease of use in our case, Set is made to take a minimal amount of
   --  space when it is not used (size of an access).

   function Add (Self : in out Set; E : Element_Type) return Boolean;
   --  Add a new element to the vector. Returns True if the element was added,
   --  False if it was already in the set.

   function Remove (Self : Set; E : Element_Type) return Boolean;
   --  Remove an element from the set. Return True if the element was removed,
   --  False if it wasn't in the set.

   function Has (Self : Set; E : Element_Type) return Boolean;
   --  Return whether E is part of the set

   function Elements (Self : Set) return Elements_Vectors.Elements_Array;
   --  Return an array of all the elements in the set

   procedure Destroy (Self : in out Set);
   --  Destroy the set

private
   type Elements_Vector is access all Elements_Vectors.Vector;

   type Set is record
      Elements : Elements_Vector := null;
   end record;

end Langkit_Support.Cheap_Sets;
