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

   with package Iterators is new Langkit_Support.Iterators (Node_Type);

package Langkit_Support.Tree_Traversal_Iterator is

   type Traverse_Iterator is limited new Ada.Finalization.Limited_Controlled
     and Iterators.Iterator with private;
   --  Iterator type for Traverse (see below)

   overriding function Next
     (It      : in out Traverse_Iterator;
      Element : out Node_Type) return Boolean;

   overriding procedure Finalize (It : in out Traverse_Iterator);

   function Create (Root : Node_Type) return Traverse_Iterator;
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
