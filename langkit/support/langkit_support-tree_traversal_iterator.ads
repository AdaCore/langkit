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

with Ada.Finalization;

with Langkit_Support.Iterators;
with Langkit_Support.Vectors;

--  This package provides iterators on general trees. Children are supposed to
--  have indexes and nodes must have a back-link to their parent.

generic
   type Node_Type is private;
   --  Type for the node of trees to traverse

   No_Node : Node_Type;
   --  Special value to represent the absence of a node

   type Node_Array is array (Positive range <>) of Node_Type;
   --  Type to use for array of nodes

   with function First_Child_Index (N : Node_Type) return Natural is <>;
   --  Return the index of the first child in N

   with function Last_Child_Index (N : Node_Type) return Natural is <>;
   --  Return the index of the last child in N

   with function Get_Child
     (N : Node_Type; Index : Natural) return Node_Type is <>;
   --  Return the Index-th child in N. Trees are allowed to have "holes" in
   --  children, so Get_Child can return No_Node.

   with function Get_Parent (N : Node_Type) return Node_Type is <>;
   --  Return the parent node for N. Returning No_Node means that N is the root
   --  node.

   with package Iterators is new Langkit_Support.Iterators
     (Node_Type, Node_Array);

package Langkit_Support.Tree_Traversal_Iterator is

   type Traverse_Iterator is limited new Ada.Finalization.Limited_Controlled
     and Iterators.Iterator with private;
   --  Iterator type for Traverse (see below)

   overriding function Next
     (It      : in out Traverse_Iterator;
      Element : out Node_Type) return Boolean;

   overriding procedure Finalize (It : in out Traverse_Iterator);

   function Create_Tree_Iterator (Root : Node_Type) return Traverse_Iterator;
   --  Create an iterator that will yield all nodes in Root in prefix depth
   --  first order (DFS).

private

   package Natural_Vectors is new Langkit_Support.Vectors (Natural);

   type Traverse_Iterator is limited new Ada.Finalization.Limited_Controlled
     and Iterators.Iterator with
      record
         Node, Parent : Node_Type := No_Node;
         Stack        : Natural_Vectors.Vector;
         Continue     : Boolean := True;
      end record;

end Langkit_Support.Tree_Traversal_Iterator;
