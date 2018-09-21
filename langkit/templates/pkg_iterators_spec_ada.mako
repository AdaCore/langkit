## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

with GNATCOLL.Refcount;

with Langkit_Support.Iterators;
with Langkit_Support.Text; use Langkit_Support.Text;
private with Langkit_Support.Tree_Traversal_Iterator;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Common;   use ${ada_lib_name}.Common;

${exts.with_clauses(with_clauses)}

--  This package provides an interface to work with iterators on nodes

<%
   pred_iface = '{}_Predicate_Interface'.format(root_entity.api_name)
   pred_ref = '{}_Predicate'.format(root_entity.api_name)
%>

package ${ada_lib_name}.Iterators is

   --------------------
   -- Iterators core --
   --------------------

   package ${root_entity.api_name}_Iterators is new Langkit_Support.Iterators
     (Element_Type  => ${root_entity.api_name},
      Element_Array => ${root_entity.array.api_name});

   type Traverse_Iterator is new ${root_entity.api_name}_Iterators.Iterator
      with private;
   --  Iterator that yields nodes from a tree

   function Traverse
     (Root : ${root_entity.api_name}'Class) return Traverse_Iterator'Class;
   --  Return an iterator that yields all nodes under ``Root`` (included) in a
   --  prefix DFS (depth first search) fashion.

   ---------------------
   -- Predicates core --
   ---------------------

   type ${pred_iface} is interface;
   --  Predicate on nodes.
   --
   --  Useful predicates often rely on values from some context, so predicates
   --  that are mere accesses to a function are not powerful enough. Having a
   --  full interface for this makes it possible to package both the predicate
   --  code and some data it needs.

   function Evaluate
     (P : in out ${pred_iface};
      N : ${root_entity.api_name}) return Boolean is abstract;
   --  Return the value of the predicate for the ``N`` node

   package ${root_entity.api_name}_Predicate_References is new
      GNATCOLL.Refcount.Shared_Pointers (${pred_iface}'Class);

   subtype ${pred_ref} is ${root_entity.api_name}_Predicate_References.Ref;
   --  Ref-counted reference to a predicate

   type ${pred_ref}_Array is array (Positive range <>) of ${pred_ref};

   function "and" (Left, Right : ${pred_ref}) return ${pred_ref};
   --  Return a predicate that accepts only nodes that are accepted by both
   --  Left and Right.
   --
   --% belongs-to: ${pred_ref}

   function "or" (Left, Right : ${pred_ref}) return ${pred_ref};
   --  Return a predicate that accepts only nodes that are accepted by Left
   --  or Right.
   --
   --% belongs-to: ${pred_ref}

   function For_All (Predicates : ${pred_ref}_Array) return ${pred_ref};
   --  Return a predicate that accepts only nodes that are accepted by all
   --  given ``Predicates``.
   --
   --% belongs-to: ${pred_ref}

   function For_Some (Predicates : ${pred_ref}_Array) return ${pred_ref};
   --  Return a predicate that accepts only nodes that are accepted by at least
   --  one of the given ``Predicates``.
   --
   --% belongs-to: ${pred_ref}

   ---------------------------
   -- Node search functions --
   ---------------------------

   function Find
     (Root      : ${root_entity.api_name}'Class;
      Predicate :
        access function (N : ${root_entity.api_name}) return Boolean := null)
      return Traverse_Iterator'Class;
   --  Return an iterator that yields all nodes under ``Root`` (included) that
   --  satisfy the ``Predicate`` predicate.

   function Find
     (Root : ${root_entity.api_name}'Class; Predicate : ${pred_ref}'Class)
      return Traverse_Iterator'Class;
   --  Return an iterator that yields all nodes under ``Root`` (included) that
   --  satisfy the ``Predicate`` predicate.

   function Find_First
     (Root      : ${root_entity.api_name}'Class;
      Predicate :
        access function (N : ${root_entity.api_name}) return Boolean := null)
      return ${root_entity.api_name};
   --  Return the first node found under ``Root`` (included) that satisfies the
   --  given ``Predicate``. Return a null node if there is no such node.

   function Find_First
     (Root : ${root_entity.api_name}'Class; Predicate : ${pred_ref}'Class)
      return ${root_entity.api_name};
   --  Return the first node found under ``Root`` (included) that satisfies the
   --  given ``Predicate``. Return a null node if there is no such node.

   ----------------
   -- Predicates --
   ----------------

   function Kind_Is (Kind : ${root_node_kind_name}) return ${pred_ref};
   --  Return a predicate that accepts only nodes of the given ``Kind``

   function Text_Is (Text : Text_Type) return ${pred_ref};
   --  Return a predicate that accepts only nodes that match the given ``Text``

   ${exts.include_extension(ctx.ext('iterators', 'pred_public_decls'))}

private

   ------------------------
   -- Iterator internals --
   ------------------------

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
      new Traversal_Iterators.Traverse_Iterator with null record;

   type Find_Iterator is new Traverse_Iterator with record
      Predicate : ${pred_ref};
      --  Predicate used to filter the nodes Traverse_It yields
   end record;
   --  Iterator type for the ``Find`` function

   overriding function Next
     (It      : in out Find_Iterator;
      Element : out ${root_entity.api_name}) return Boolean;

   type Local_Find_Iterator is new Traverse_Iterator with record
      Predicate : access function (N : ${root_entity.api_name}) return Boolean;
      --  Predicate used to filter the nodes Traverse_It yields
   end record;
   --  Iterator type for the ``Find`` function that takes an access to
   --  function. It is called ``Local_Find_Iterator`` because if you use a
   --  locally declared function, the iterator itself will only be valid in the
   --  scope of the function.

   overriding function Next
     (It      : in out Local_Find_Iterator;
      Element : out ${root_entity.api_name}) return Boolean;

   --------------------------
   -- Predicates internals --
   --------------------------

   type For_All_Predicate (N : Natural) is new ${pred_iface} with record
      Predicates : ${pred_ref}_Array (1 .. N);
   end record;

   overriding function Evaluate
     (P : in out For_All_Predicate;
      N : ${root_entity.api_name}) return Boolean;

   type For_Some_Predicate (N : Natural) is new ${pred_iface} with record
      Predicates : ${pred_ref}_Array (1 .. N);
   end record;

   overriding function Evaluate
     (P : in out For_Some_Predicate;
      N : ${root_entity.api_name}) return Boolean;

   type Kind_Predicate is new ${pred_iface} with record
      Kind : ${root_node_kind_name};
   end record;
   --  Predicate that returns true for all nodes of some kind

   overriding function Evaluate
     (P : in out Kind_Predicate; N : ${root_entity.api_name}) return Boolean;

   type Text_Predicate is new ${pred_iface} with record
      Text : Unbounded_Text_Type;
   end record;
   --  Predicate that returns true for all nodes that match some text

   overriding function Evaluate
     (P : in out Text_Predicate; N : ${root_entity.api_name}) return Boolean;

   ${exts.include_extension(ctx.ext('iterators', 'pred_private_decls'))}

end ${ada_lib_name}.Iterators;
