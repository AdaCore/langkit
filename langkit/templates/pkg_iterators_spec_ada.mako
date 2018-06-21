## vim: filetype=makoada

with Ada.Finalization;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Iterators;
with Langkit_Support.Tree_Traversal_Iterator;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Common;   use ${ada_lib_name}.Common;

--  This package provides an interface to work with iterators on AST node
--  sequences.

package ${ada_lib_name}.Iterators is

   package ${root_entity.api_name}_Iterators is new Langkit_Support.Iterators
     (Element_Type  => ${root_entity.api_name},
      Element_Array => ${root_entity.array.api_name});

   type Traverse_Iterator is
      limited new ${root_entity.api_name}_Iterators.Iterator
      with private;

   function Traverse
     (Root : ${root_entity.api_name}'Class) return Traverse_Iterator;
   --  Return an iterator that yields all AST nodes under Root (included) in a
   --  prefix DFS (depth first search) fasion.

   type ${root_entity.api_name}_Predicate_Type is interface;
   type ${root_entity.api_name}_Predicate is
      access all ${root_entity.api_name}_Predicate_Type'Class;
   --  Predicate on AST nodes.
   --
   --  Useful predicates often rely on values from some context, so predicates
   --  that are mere accesses to a function are not powerful enough. Having a
   --  full interface for this makes it possible to package both the predicate
   --  code and some data it needs.

   function Evaluate
     (P : access ${root_entity.api_name}_Predicate_Type;
      N : ${root_entity.api_name}) return Boolean is abstract;
   --  Return the value of the predicate for the N node

   procedure Destroy is new Ada.Unchecked_Deallocation
     (${root_entity.api_name}_Predicate_Type'Class,
      ${root_entity.api_name}_Predicate);

   type Find_Iterator is
      limited new ${root_entity.api_name}_Iterators.Iterator
      with private;
   --  Iterator type for Find (see below)

   overriding function Next
     (It      : in out Find_Iterator;
      Element : out ${root_entity.api_name}) return Boolean;

   type Local_Find_Iterator is
      limited new ${root_entity.api_name}_Iterators.Iterator
      with private;
   --  Iterator type for the Find function that takes an access to function. It
   --  is called Local_Find_Iterator because if you use a locally declared
   --  function, the iterator itself will only be valid in the scope of the
   --  function.

   overriding function Next
     (It      : in out Local_Find_Iterator;
      Element : out ${root_entity.api_name}) return Boolean;

   function Find
     (Root      : ${root_entity.api_name}'Class;
      Predicate :
        access function (N : ${root_entity.api_name}) return Boolean := null)
      return Local_Find_Iterator;
   --  Return an iterator that yields all AST nodes under Root (included) that
   --  satisfy the Predicate predicate.

   function Find_First
     (Root      : ${root_entity.api_name}'Class;
      Predicate :
        access function (N : ${root_entity.api_name}) return Boolean := null)
      return ${root_entity.api_name};
   --  Return the first node found under Root (included) that satisfies the
   --  given Predicate. Return a null node if there is no such node.

   function Find
     (Root      : ${root_entity.api_name}'Class;
      Predicate : ${root_entity.api_name}_Predicate) return Find_Iterator;
   --  Return an iterator that yields all AST nodes under Root (included) that
   --  satisfy the Predicate predicate. Predicate will be destroyed when
   --  Find_Iterator is exhausted.

   function Find_First
     (Root      : ${root_entity.api_name}'Class;
      Predicate : ${root_entity.api_name}_Predicate)
      return ${root_entity.api_name};
   --  Return the first node found under Root (included) that satisfies the
   --  given Predicate. Return a null node if there is no such node.

   type ${root_entity.api_name}_Kind_Filter is
      new ${root_entity.api_name}_Predicate_Type with
   record
      Kind : ${root_node_kind_name};
   end record;
   --  Predicate that returns true for all AST nodes of some kind

   function Evaluate
     (P : access ${root_entity.api_name}_Kind_Filter;
      N : ${root_entity.api_name}) return Boolean;

private

   function Get_Parent
     (N : ${root_entity.api_name}) return ${root_entity.api_name};

   function First_Child_Index_For_Traverse
     (N : ${root_entity.api_name}) return Natural;

   function Last_Child_Index_For_Traverse
     (N : ${root_entity.api_name}) return Natural;

   function Get_Child
     (N : ${root_entity.api_name}; I : Natural) return ${root_entity.api_name};

   package Traversal_Iterators is new Langkit_Support.Tree_Traversal_Iterator
     (Node_Type         => ${root_entity.api_name},
      No_Node           => No_${root_entity.api_name},
      Node_Array        => ${root_entity.array.api_name},
      First_Child_Index => First_Child_Index_For_Traverse,
      Last_Child_Index  => Last_Child_Index_For_Traverse,
      Iterators         => ${root_entity.api_name}_Iterators);

   type Traverse_Iterator is
      limited new Traversal_Iterators.Traverse_Iterator with null record;

   type Find_Iterator is
      limited new Ada.Finalization.Limited_Controlled
      and ${root_entity.api_name}_Iterators.Iterator with
   record
      Traverse_It : Traverse_Iterator;
      --  Traverse iterator to fetch all nodes

      Predicate : ${root_entity.api_name}_Predicate;
      --  Predicate used to filter the nodes Traverse_It yields
   end record;

   overriding procedure Finalize (It : in out Find_Iterator);

   type Local_Find_Iterator is limited
      new Ada.Finalization.Limited_Controlled
      and ${root_entity.api_name}_Iterators.Iterator with
   record
      Traverse_It : Traverse_Iterator;
      --  Traverse iterator to fetch all nodes

      Predicate : access function (N : ${root_entity.api_name}) return Boolean;
      --  Predicate used to filter the nodes Traverse_It yields
   end record;
   --  Iterator type for Find (see above)

end ${ada_lib_name}.Iterators;
