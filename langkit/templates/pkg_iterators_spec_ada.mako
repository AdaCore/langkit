## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

with GNATCOLL.Refcount;

with Langkit_Support.Iterators;
private with Langkit_Support.Tree_Traversal_Iterator;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Common;   use ${ada_lib_name}.Common;

${exts.with_clauses(with_clauses)}

<%
   node = root_entity.api_name
   pred_iface = '{}_Predicate_Interface'.format(node)
   pred_ref = '{}_Predicate'.format(node)
%>

--  This package provides an interface to iterate on nodes in parse trees and
--  to look for node patterns.
--
--  First, as an alternative to ``${ada_lib_name}.Analysis.Traverse``, you can
--  do:
--
--  .. code-block:: ada
--
--     declare
--        It   : Traverse_Iterator'Class := Traverse (My_Unit.Root);
--        Node : ${node};
--     begin
--        while It.Next (Node) loop
--           --  Process Node
--        end loop;
--     end;
--
--  Now, if you are exclusively looking for nodes whose text is either ``foo``
--  or ``bar``, you can replace the call to ``Traverse`` with the following:
--
--  .. code-block:: ada
--
--        Find (My_Unit.Root, Text_Is ("foo") or Text_Is ("bar"));
--
--  The ``Find``-like functions below take as a second argument a *predicate*,
--  which is an object that can decide if a node should be processed or not.
--  This package provides several built-in predicates (``Kind_Is``,
--  ``Text_Is``, etc.), then you can either define your own, derivating the
--  ``${pred_iface}`` type, or compose them using Ada's boolean operators.

package ${ada_lib_name}.Iterators is

   use Support.Text;

   --------------------
   -- Iterators core --
   --------------------

   package ${node}_Iterators is new Support.Iterators
     (Element_Type  => ${node},
      Element_Array => ${root_entity.array.api_name});

   type Traverse_Iterator is new ${node}_Iterators.Iterator with private;
   --  Iterator that yields nodes from a tree

   function Traverse (Root : ${node}'Class) return Traverse_Iterator'Class;
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
   --
   --  Note that predicates are not thread-safe: make sure you don't use a
   --  predicate from multiple threads, as they can contain caches.

   function Evaluate
     (P : in out ${pred_iface}; N : ${node}) return Boolean is abstract;
   --  Return the value of the predicate for the ``N`` node

   package ${node}_Predicate_References is new
      GNATCOLL.Refcount.Shared_Pointers (${pred_iface}'Class);

   subtype ${pred_ref} is ${node}_Predicate_References.Ref;
   --  Ref-counted reference to a predicate

   type ${pred_ref}_Array is array (Positive range <>) of ${pred_ref};

   function "not" (Predicate : ${pred_ref}) return ${pred_ref};
   --  Return a predicate that accepts only nodes that are *not* accepted by
   --  ``Predicate``.
   --
   --% belongs-to: ${pred_ref}

   function "and" (Left, Right : ${pred_ref}) return ${pred_ref};
   --  Return a predicate that accepts only nodes that are accepted by both
   --  ``Left`` and ``Right``.
   --
   --% belongs-to: ${pred_ref}

   function "or" (Left, Right : ${pred_ref}) return ${pred_ref};
   --  Return a predicate that accepts only nodes that are accepted by ``Left``
   --  or ``Right``.
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

   function For_All_Children
     (Predicate : ${pred_ref}; Skip_Null : Boolean := True) return ${pred_ref};
   --  Return a predicate that accepts only nodes for which ``Predicate``
   --  accepts all children. Unless ``Skip_Null`` is false, this does not
   --  evaluate the predicate on null children.
   --
   --% belongs-to: ${pred_ref}

   function For_Some_Children
     (Predicate : ${pred_ref}; Skip_Null : Boolean := True) return ${pred_ref};
   --  Return a predicate that accepts only nodes for which ``Predicate``
   --  accepts at least one child. Unless ``Skip_Null`` is false, this does not
   --  evaluate the predicate on null children.
   --
   --% belongs-to: ${pred_ref}

   function Child_With
     (Field     : Syntax_Field_Reference;
      Predicate : ${pred_ref}) return ${pred_ref};
   --  Return a predicate that accepts only nodes which have a child
   --  corresponding to the given field reference and for which this child is
   --  accepted by the given predicate.
   --
   --% belongs-to: ${pred_ref}

   ---------------------------
   -- Node search functions --
   ---------------------------

   function Find
     (Root      : ${node}'Class;
      Predicate : access function (N : ${node}) return Boolean := null)
      return Traverse_Iterator'Class;
   --  Return an iterator that yields all nodes under ``Root`` (included) that
   --  satisfy the ``Predicate`` predicate.

   function Find
     (Root : ${node}'Class; Predicate : ${pred_ref}'Class)
      return Traverse_Iterator'Class;
   --  Return an iterator that yields all nodes under ``Root`` (included) that
   --  satisfy the ``Predicate`` predicate.

   function Find_First
     (Root      : ${node}'Class;
      Predicate : access function (N : ${node}) return Boolean := null)
      return ${node};
   --  Return the first node found under ``Root`` (included) that satisfies the
   --  given ``Predicate``. Return a null node if there is no such node.

   function Find_First
     (Root : ${node}'Class; Predicate : ${pred_ref}'Class) return ${node};
   --  Return the first node found under ``Root`` (included) that satisfies the
   --  given ``Predicate``. Return a null node if there is no such node.

   ----------------
   -- Predicates --
   ----------------

   function Kind_Is (Kind : ${T.node_kind}) return ${pred_ref};
   --  Return a predicate that accepts only nodes of the given ``Kind``
   --
   --% belongs-to: ${pred_ref}

   function Kind_In (First, Last : ${T.node_kind}) return ${pred_ref};
   --  Return a predicate that accepts only nodes whose kind is in First ..
   --  Last.
   --
   --% belongs-to: ${pred_ref}

   function Text_Is (Text : Text_Type) return ${pred_ref};
   --  Return a predicate that accepts only nodes that match the given ``Text``
   --
   --% belongs-to: ${pred_ref}

   function Node_Is_Null return ${pred_ref};
   --  Return a predicate that accepts only null nodes
   --
   --% belongs-to: ${pred_ref}

   ${exts.include_extension(ctx.ext('iterators', 'pred_public_decls'))}

private

   ------------------------
   -- Iterator internals --
   ------------------------

   function Get_Parent (N : ${node}) return ${node};
   function First_Child_Index_For_Traverse (N : ${node}) return Natural;
   function Last_Child_Index_For_Traverse (N : ${node}) return Natural;
   function Get_Child (N : ${node}; I : Natural) return ${node};

   package Traversal_Iterators is new Langkit_Support.Tree_Traversal_Iterator
     (Node_Type         => ${node},
      No_Node           => No_${node},
      Node_Array        => ${root_entity.array.api_name},
      First_Child_Index => First_Child_Index_For_Traverse,
      Last_Child_Index  => Last_Child_Index_For_Traverse,
      Iterators         => ${node}_Iterators);

   type Traverse_Iterator is
      new Traversal_Iterators.Traverse_Iterator with null record;

   type Find_Iterator is new Traverse_Iterator with record
      Predicate : ${pred_ref};
      --  Predicate used to filter the nodes Traverse_It yields
   end record;
   --  Iterator type for the ``Find`` function

   overriding function Next
     (It : in out Find_Iterator; Element : out ${node}) return Boolean;

   type Local_Find_Iterator is new Traverse_Iterator with record
      Predicate : access function (N : ${node}) return Boolean;
      --  Predicate used to filter the nodes Traverse_It yields
   end record;
   --  Iterator type for the ``Find`` function that takes an access to
   --  function. It is called ``Local_Find_Iterator`` because if you use a
   --  locally declared function, the iterator itself will only be valid in the
   --  scope of the function.

   overriding function Next
     (It : in out Local_Find_Iterator; Element : out ${node}) return Boolean;

   --------------------------
   -- Predicates internals --
   --------------------------

   type Not_Predicate is new ${pred_iface} with record
      Predicate : ${pred_ref};
   end record;

   overriding function Evaluate
     (P : in out Not_Predicate; N : ${node}) return Boolean;

   type For_All_Predicate (N : Natural) is new ${pred_iface} with record
      Predicates : ${pred_ref}_Array (1 .. N);
   end record;

   overriding function Evaluate
     (P : in out For_All_Predicate; N : ${node}) return Boolean;

   type For_Some_Predicate (N : Natural) is new ${pred_iface} with record
      Predicates : ${pred_ref}_Array (1 .. N);
   end record;

   overriding function Evaluate
     (P : in out For_Some_Predicate; N : ${node}) return Boolean;

   type For_All_Children_Predicate is new ${pred_iface} with record
      Predicate : ${pred_ref};
      Skip_Null : Boolean;
   end record;

   overriding function Evaluate
     (P : in out For_All_Children_Predicate; N : ${node}) return Boolean;

   type For_Some_Children_Predicate is new ${pred_iface} with record
      Predicate : ${pred_ref};
      Skip_Null : Boolean;
   end record;

   overriding function Evaluate
     (P : in out For_Some_Children_Predicate; N : ${node}) return Boolean;

   type Child_With_Predicate is new ${pred_iface} with record
      Field     : Syntax_Field_Reference;
      Predicate : ${pred_ref};
   end record;

   overriding function Evaluate
     (P : in out Child_With_Predicate; N : ${node}) return Boolean;

   type Kind_Predicate is new ${pred_iface} with record
      First, Last : ${T.node_kind};
   end record;
   --  Predicate that returns true for all nodes whose kind is in a given range

   overriding function Evaluate
     (P : in out Kind_Predicate; N : ${node}) return Boolean;

   type Text_Predicate is new ${pred_iface} with record
      Text : Unbounded_Text_Type;
   end record;
   --  Predicate that returns true for all nodes that match some text

   overriding function Evaluate
     (P : in out Text_Predicate; N : ${node}) return Boolean;

   type Node_Is_Null_Predicate is new ${pred_iface} with null record;

   overriding function Evaluate
     (P : in out Node_Is_Null_Predicate; N : ${node}) return Boolean;

   ${exts.include_extension(ctx.ext('iterators', 'pred_private_decls'))}

end ${ada_lib_name}.Iterators;
