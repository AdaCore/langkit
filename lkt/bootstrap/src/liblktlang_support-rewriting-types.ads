--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  .. note:: This unit is internal: only Langkit and Langkit-generated
--  libraries are supposed to use it.
--
--  This package provides data structures used to implement the rewriting API.

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with Liblktlang_Support.Bump_Ptr;    use Liblktlang_Support.Bump_Ptr;
with Liblktlang_Support.Bump_Ptr_Vectors;
with Liblktlang_Support.Generic_API; use Liblktlang_Support.Generic_API;
with Liblktlang_Support.Generic_API.Analysis;
use Liblktlang_Support.Generic_API.Analysis;
with Liblktlang_Support.Generic_API.Introspection;
use Liblktlang_Support.Generic_API.Introspection;
with Liblktlang_Support.Generic_API.Unparsing;
use Liblktlang_Support.Generic_API.Unparsing;
with Liblktlang_Support.Text;        use Liblktlang_Support.Text;

with Prettier_Ada.Documents;

package Liblktlang_Support.Rewriting.Types is

   type Rewriting_Handle_Record;
   type Unit_Rewriting_Handle_Record;
   type Node_Rewriting_Handle_Record;

   type Rewriting_Handle_Access is access all Rewriting_Handle_Record;
   type Unit_Rewriting_Handle_Access is
     access all Unit_Rewriting_Handle_Record;
   type Node_Rewriting_Handle_Access is
     access all Node_Rewriting_Handle_Record;

   --  Mainly due to C bindings, the various rewriting handle access types are
   --  converted to raw pointers (`System.Address`), which makes optimizations
   --  based on strict aliasing invalid: disable strict aliasing for these
   --  access types.

   pragma No_Strict_Aliasing (Rewriting_Handle_Access);
   pragma No_Strict_Aliasing (Unit_Rewriting_Handle_Access);
   pragma No_Strict_Aliasing (Node_Rewriting_Handle_Access);

   function Children_Count
     (Handle : Node_Rewriting_Handle_Access) return Natural;
   --  Return the number of children the node represented by ``Handle`` has

   function Text (Handle : Node_Rewriting_Handle_Access) return Text_Type;
   --  Return the text associated to the given token node

   package Unit_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Unit_Rewriting_Handle_Access,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   package Node_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Lk_Node,
      Element_Type    => Node_Rewriting_Handle_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");

   package Nodes_Pools is new Liblktlang_Support.Bump_Ptr_Vectors
     (Node_Rewriting_Handle_Access);

   type Rewriting_Handle_Record is record
      Language : Language_Id;
      --  Language for this rewriting handle

      Context : Lk_Context;
      --  Analysis context this rewriting handle relates to

      Config : Unparsing_Configuration;
      --  Unparsing configuration used to format rewritten parts of the tree

      Options : Prettier_Ada.Documents.Format_Options_Type;
      --  The options to use when formatting rewritten parts of the tree

      Units : Unit_Maps.Map;
      --  Keep track of rewriting handles we create for the units that Context
      --  owns.

      Pool      : Bump_Ptr_Pool;
      New_Nodes : Nodes_Pools.Vector;
      --  Keep track of all node rewriting handles that don't map to original
      --  nodes, i.e. all nodes that were created during this rewriting
      --  session.

      Stubs : Nodes_Pools.Vector;
      --  Keep track of all allocated stub rewriting nodes. These are used in
      --  ``Rotate`` as stubs for rotated ones, and are re-used each time
      --  ``Rotate`` is called.
   end record;

   type Unit_Rewriting_Handle_Record is record
      Context_Handle : Rewriting_Handle_Access;
      --  Rewriting handle for the analysis context this relates to

      Unit : Lk_Unit;
      --  Analysis unit this relates to

      Root : Node_Rewriting_Handle_Access;
      --  Handle for the node that will become the root node of this analysis
      --  unit.

      Nodes : Node_Maps.Map;
      --  Keep track of rewriting handles we create for base AST nodes that
      --  Unit owns.
   end record;

   package Node_Vectors is new Ada.Containers.Vectors
     (Positive, Node_Rewriting_Handle_Access);

   type Node_Children_Kind is (
      Unexpanded,
      --  Dummy node rewriting handle: children don't have their own handle yet

      Expanded_Regular,
      --  Expanded node rewriting handle: children have their own handle. Note
      --  that this is for all but token nodes.

      Expanded_List,
      --  Expanded node rewriting handle, specific for list nodes: element
      --  nodes are stored as a doubly linked list.

      Expanded_Token_Node
      --  Expanded node rewriting handle, specific for token nodes: there is no
      --  children, only some associated text.
   );

   type Node_Children (Kind : Node_Children_Kind := Unexpanded) is record
      case Kind is
         when Unexpanded =>
            null;

         when Expanded_Regular =>
            Vector : Node_Vectors.Vector;
            --  Vector of children for all non-null syntax fields

         when Expanded_List =>
            First, Last : Node_Rewriting_Handle_Access;
            --  Doubly linked list of children

            Count : Natural;
            --  Number of children

         when Expanded_Token_Node =>
            Text : Unbounded_Text_Type;
            --  Text for this token node
      end case;
   end record;
   --  Lazily evaluated vector of children for a Node_Rewriting_Handle_Record.
   --
   --  In order to avoid constructing the whole tree of
   --  Node_Rewriting_Handle_Record for some analysis unit at once, we build
   --  them in a lazy fashion.

   Unexpanded_Children : constant Node_Children := (Kind => Unexpanded);

   type Any_Rewriting_Tile is new Natural;
   subtype Rewriting_Tile is
     Any_Rewriting_Tile range 1 ..  Any_Rewriting_Tile'Last;
   --  Identifiers for rewriting tiles.
   --
   --  A rewriting tile is a group of nodes in a rewriting trees that must
   --  either all be reformatted together, or not reformatted at all. The tiles
   --  in a rewriting tree form a partition over node rewriting handles: each
   --  rewriting node must belong to exactly one tile (children of unexpanded
   --  node implicitly belong to the same tile as the unexpanded node).
   --
   --  Rewriting tiles are computed during rewriting application, so that
   --  partial reformatting can do its work (i.e. reformat only nodes of tiles
   --  that must be reformatted).

   type Node_Rewriting_Handle_Record is record
      Context_Handle : Rewriting_Handle_Access;
      --  Rewriting handle for the analysis context that owns Node

      Node : Lk_Node;
      --  Bare AST node which this rewriting handle relates to

      Parent : Node_Rewriting_Handle_Access;
      --  Rewriting handle for Node's parent, or null if Node is a root node

      Previous, Next : Node_Rewriting_Handle_Access;
      --  If ``Parent`` is a list node, ``Previous`` is the previous sibling
      --  for this node in that list (``null`` for the first sibling), and
      --  ``Next`` is the next sibling (``null`` for the last sibling).
      --
      --  If ``Parent`` is not a list node, both are set to ``null``).

      Kind : Type_Ref;
      --  Type for the node this handle represents. When ``Node`` is not null
      --  (i.e.  when this represents an already existing node, rather than a
      --  new one), this must be equal to ``Type_Of (Node)``.

      Tied : Boolean;
      --  Whether this node is tied to an analysis unit tree. It can be
      --  assigned as a child to another node iff it is not tied.

      Tile : Any_Rewriting_Tile;
      --  Rewriting tile that owns this node. Computed at the very beginning of
      --  rewriting application.

      Root_Of : Unit_Rewriting_Handle_Access;
      --  If the node this handle represents is the root of a rewritten unit,
      --  this references this unit. ``null`` in all other cases.

      Children : Node_Children;
      --  Lazily evaluated vector of children for the rewritten node
   end record;

end Liblktlang_Support.Rewriting.Types;
