--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides support for tree-based source code rewriting.
--
--  General workflow
--  ~~~~~~~~~~~~~~~~
--
--  The starting point to rewrite a source file is to parse it into a
--  :ada:ref:`Liblktlang_Support.Generic_API.Analysis.Lk_Unit` object, which
--  belongs to a :ada:ref:`Liblktlang_Support.Generic_API.Analysis.Lk_Context`
--  object (see :ada:ref:`Liblktlang_Support.Generic_API.Analysis`).  At this
--  point, a rewriting session must be started for the whole analysis context.
--  This is done calling the :ada:ref:`Start_Rewriting` function, which returns
--  a context-wide rewriting handle. A rewriting session keeps track of all
--  tree modification requests, which are not applied to the analysis
--  context/units/nodes until the :ada:ref:`Apply` function returns
--  successfully.
--
--  Once the context-wide rewriting handle exists:
--
--  * The ``Handle`` functions can be used to get a rewriting handle for each
--    analysis unit (:ada:ref:`Unit_Rewriting_Handle`) and parsing node
--    (:ada:ref:`Node_Rewriting_Handle`). Note that attempting to get the
--    rewriting handle for a unit or a node is valid only for units with no
--    syntax error: tree-based rewriting is supported only for source files
--    that were successfully parsed.
--
--  * Nodes can be modified through their rewriting handles using the
--    ``Set_Child``, ``Set_Text``, ``Insert_*``, ``Remove_Child``, ``Replace``
--    and ``Rotate`` procedures.
--
--  * They can also be created from scratch using the :ada:ref:`Clone`,
--    ``Create_*`` and :ada:ref:`Create_From_Template` functions.
--
--  * The root of an analysis unit can be changed with the :ada:ref:`Set_Root`
--    procedure.
--
--  * Once done with all the desired tree modifications, the :ada:ref:`Apply`
--    function must be called to propagate all the requested changes to the
--    analysis units/nodes
--    (:ada:ref:`Liblktlang_Support.Generic_API.Analysis.Lk_Unit` /
--    :ada:ref:`Liblktlang_Support.Generic_API.Analysis.Lk_Node`). Note that this
--    does not modify the actual source files, but only their in-memory
--    representations. Since all tree transformations may not yield a correct
--    syntax tree, :ada:ref:`Apply` may fail: in that case, no modification is
--    done, and this function returns a failing :ada:ref:`Apply_Result` record
--    that indicates which unit could not be re-parsed after unparsing, and the
--    corresponding parsing errors.
--
--    If :ada:ref:`Apply` succeeds, the rewriting context is destroyed upon
--    return. If it fails, the rewriting context is left unchanged.
--
--  * Once a rewriting session has been started, it is also possible to discard
--    it and all the tree modification it kept track of: calling the
--    :ada:ref:`Abort_Rewriting` procedure destroys the rewriting context.
--
--  Analysis contexts can have at most one rewriting session at a given time:
--  once :ada:ref:`Start_Rewriting` has returned a new rewriting context, a
--  successful :ada:ref:`Apply` function call or a :ada:ref:`Abort_Rewriting`
--  procedure call must be done before attempting to call
--  :ada:ref:`Start_Rewriting` again.
--
--  Comments/formatting preservation
--  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
--  The rewriting engine keeps track of the original node
--  (:ada:ref:`Liblktlang_Support.Generic_API.Analysis.Lk_Node`) for
--  each node rewriting handle: the handles created from parsing nodes (i.e.
--  returned by the ``Handle`` function) have an original node, and so do the
--  handles returned by the :ada:ref:`Clone` function when called on a
--  rewriting handle that itself has an original node.
--
--  When unparsing a node rewriting handle that has an original node, trivias
--  (comments and whitespaces) are recovered from the original node on a best
--  effort basis. Comments and trivias that follow a removed/replaced node are
--  lost.

with Liblktlang_Support.Diagnostics; use Liblktlang_Support.Diagnostics;
with Liblktlang_Support.Generic_API.Analysis;
use Liblktlang_Support.Generic_API.Analysis;
with Liblktlang_Support.Generic_API.Introspection;
use Liblktlang_Support.Generic_API.Introspection;
with Liblktlang_Support.Generic_API.Unparsing;
use Liblktlang_Support.Generic_API.Unparsing;
private with Liblktlang_Support.Internal.Analysis;
private with Liblktlang_Support.Rewriting.Types;
private with Liblktlang_Support.Types;

package Liblktlang_Support.Generic_API.Rewriting is

   type Rewriting_Handle is private;
   --  Handle for an analysis context rewriting session

   type Unit_Rewriting_Handle is private;
   --  Handle for the process of rewriting an analysis unit. Such handles are
   --  owned by a Rewriting_Handle instance.

   type Node_Rewriting_Handle is private;
   --  Handle for the process of rewriting an AST node. Such handles are owned
   --  by a Rewriting_Handle instance.

   No_Rewriting_Handle      : constant Rewriting_Handle;
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle;
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle;

   type Unit_Rewriting_Handle_Array is
      array (Positive range <>) of Unit_Rewriting_Handle;

   type Node_Rewriting_Handle_Array is
      array (Positive range <>) of Node_Rewriting_Handle;

   -----------------------
   -- Context rewriting --
   -----------------------

   function Handle (Context : Lk_Context) return Rewriting_Handle;
   --  Return the rewriting handle associated to ``Context``, or
   --  ``No_Rewriting_Handle`` if ``Context`` is not being rewritten.

   function Context (Handle : Rewriting_Handle) return Lk_Context;
   --  Return the analysis context associated to ``Handle``

   function Language (Handle : Rewriting_Handle) return Language_Id
   is (Context (Handle).Language);
   --  Return the language associated to the given handle

   function Start_Rewriting
     (Context : Lk_Context;
      Config  : Unparsing_Configuration := No_Unparsing_Configuration)
      return Rewriting_Handle;
   --  Start a rewriting session for ``Context``.
   --
   --  This handle will keep track of all changes to do on ``Context``'s
   --  analysis units. Once the set of changes is complete, call the ``Apply``
   --  procedure to actually update ``Context``. This makes it possible to
   --  inspect the "old" ``Context`` state while creating the list of changes.
   --
   --  There can be only one rewriting session per analysis context, so this
   --  will raise an ``Existing_Rewriting_Handle_Error`` exception if
   --  ``Context`` already has a living rewriting session.
   --
   --  ``Config`` is used to format the rewritten parts of the trees. Use the
   --  language's default if not provided.

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle);
   --  Discard all modifications registered in ``Handle`` and close ``Handle``.
   --  This invalidates all related unit/node handles.

   type Apply_Result (Success : Boolean := True) is record
      case Success is
         when False =>
            Unit : Lk_Unit;
            --  Reference to the analysis unit that was being processed when
            --  the error occurred.

            Diagnostics : Diagnostics_Vectors.Vector;
            --  Corresponding list of error messages
         when True => null;
      end case;
   end record;

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result;
   --  Apply all modifications to ``Handle``'s analysis context. If that
   --  worked, close ``Handle`` and return ``(Success => True)``. Otherwise,
   --  reparsing did not work, so keep ``Handle`` and its Context unchanged and
   --  return details about the error that happened.
   --
   --  Note that on success, this invalidates all related unit/node handles.

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array;
   --  Return the list of unit rewriting handles in the given context handle
   --  for units that the ``Apply`` primitive will modify.

   --------------------
   -- Unit rewriting --
   --------------------

   function Handle (Unit : Lk_Unit) return Unit_Rewriting_Handle;
   --  Return the rewriting handle corresponding to ``Unit``

   function Unit (Handle : Unit_Rewriting_Handle) return Lk_Unit;
   --  Return the unit corresponding to ``Handle``

   function Language (Handle : Unit_Rewriting_Handle) return Language_Id
   is (Unit (Handle).Language);

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Return the node handle corresponding to the root of the unit which
   --  ``Handle`` designates.

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle);
   --  Set the root node for the unit Handle to ``Root``. This unties the
   --  previous root handle. If ``Root`` is not ``No_Node_Rewriting_Handle``,
   --  this also ties ``Root`` to ``Handle``.
   --
   --  ``Root`` must not already be tied to another analysis unit handle.

   function Unparse
     (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type;
   --  Return the text associated to the given unit

   --------------------
   -- Node rewriting --
   --------------------

   function Handle (Node : Lk_Node) return Node_Rewriting_Handle;
   --  Return the rewriting handle corresponding to ``Node``.
   --
   --  The owning unit of Node must be free of diagnostics.

   function Node (Handle : Node_Rewriting_Handle) return Lk_Node;
   --  Return the node which the given rewriting Handle relates to. This can
   --  be ``No_Lk_Node`` if this handle designates a new node.

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle;
   --  Return a handle for the rewriting context to which ``Handle`` belongs

   function Language (Handle : Node_Rewriting_Handle) return Language_Id
   is (Language (Context (Handle)));

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Turn the given rewritten node ``Handle`` designates into text. This is
   --  the text that is used in ``Apply`` in order to re-create an analysis
   --  unit.

   function Type_Of (Handle : Node_Rewriting_Handle) return Type_Ref;
   --  Return the type reference corresponding to ``Handle``'s node

   function Image (Handle : Node_Rewriting_Handle) return String;
   --  Return a representation of ``Handle`` as a string

   procedure Print
     (Handle : Node_Rewriting_Handle; Line_Prefix : String := "");
   --  Debug helper: print to standard output ``Handle`` and all its children.
   --
   --  ``Line_Prefix`` is prepended to each output line.

   function Tied (Handle : Node_Rewriting_Handle) return Boolean;
   --  Return whether ``Handle`` is tied to an analysis unit. If it is not, it
   --  can be passed as the ``Child`` parameter to ``Set_Child``.

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Return a handle for the node that is the parent of ``Handle``'s node.
   --  This is ``No_Rewriting_Handle`` for a node that is not tied to any tree
   --  yet.

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural;
   --  Return the number of children the node represented by ``Handle`` has

   function Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref) return Node_Rewriting_Handle;
   --  Return the node that is in the syntax ``Field`` for ``Handle``

   function Child
     (Handle : Node_Rewriting_Handle;
      Fields : Struct_Member_Ref_Array) return Node_Rewriting_Handle;
   --  Return a child deep in the tree ``Handle``.
   --
   --  Assuming ``Fields'Range`` is ``1 .. N``, this is a shortcut for::
   --
   --     C1 := Child (Handle, Fields (1));
   --     C2 := Child (C1, Fields (2));
   --     ...
   --     CN_1 := Child (CN_2, Fields (N - 1));
   --     CN := Child (CN_1, Fields (N));

   function Children
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle_Array;
   --  Return the list of children for ``Handle``

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref;
      Child  : Node_Rewriting_Handle);
   --  If ``Child`` is ``No_Rewriting_Node``, untie the syntax field in
   --  ``Handle`` corresponding to ``Field``, so it can be attached to another
   --  one. Otherwise, ``Child`` must have no parent as it will be tied to
   --  ``Handle``'s tree.

   function Text (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Return the text associated to the given token node

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type);
   --  Override text associated to the given token node

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle);
   --  If Handle is the root of an analysis unit, untie it and set ``New_Node``
   --  as its new root. Otherwise, replace ``Handle`` with ``New_Node`` in
   --  ``Handle``'s parent node.
   --
   --  Note that:
   --
   --  * Handle must be tied to an existing analysis unit handle.
   --  * ``New_Node`` must not already be tied to another analysis unit handle.

   procedure Rotate (Handles : Node_Rewriting_Handle_Array);
   --  Given a list of node rewriting handles ``H1``, ``H2``, ... ``HN``,
   --  replace ``H1`` by ``H2`` in the rewritten tree, replace ``H2`` by
   --  ``H3``, etc. and replace ``HN`` by ``H1``.
   --
   --  Note that this operation is atomic: if it fails, no replacement is
   --  actually performed.

   function Is_List_Node (Handle : Node_Rewriting_Handle) return Boolean;
   --  Return whether ``Handle`` represents a list node

   -------------------------
   -- List node rewriting --
   -------------------------

   function First_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Assuming ``Handle`` refers to a list node, return a handle to its first
   --  child, or ``No_Node_Rewriting_Handle``` if it has no child node.

   function Last_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Assuming ``Handle`` refers to a list node, return a handle to its last
   --  child, or ``No_Node_Rewriting_Handle``` if it has no child node.

   function Next_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Assuming ``Handle`` refers to the child of a list node, return a handle
   --  to its next sibling, or ``No_Node_Rewriting_Handle``` if it is the last
   --  sibling.

   function Previous_Child
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Assuming ``Handle`` refers to the child of a list node, return a handle
   --  to its previous sibling, or ``No_Node_Rewriting_Handle``` if it is the
   --  first sibling.

   procedure Insert_Before
     (Handle, New_Sibling : Node_Rewriting_Handle);
   --  Assuming ``Handle`` refers to the child of a list node, insert
   --  ``New_Sibling`` as a new child in this list, right before ``Handle``.

   procedure Insert_After
     (Handle, New_Sibling : Node_Rewriting_Handle);
   --  Assuming ``Handle`` refers to the child of a list node, insert
   --  ``New_Sibling`` as a new child in this list, right before ``Handle``.

   procedure Insert_First (Handle, New_Child : Node_Rewriting_Handle);
   --  Assuming ``Handle`` refers to a list node, insert ``New_Child`` to be
   --  the first child in this list.

   procedure Insert_Last (Handle, New_Child : Node_Rewriting_Handle);
   --  Assuming ``Handle`` refers to a list node, insert ``New_Child`` to be
   --  the last child in this list.

   procedure Remove_Child (Handle : Node_Rewriting_Handle);
   --  Assuming Handle refers to the child of a list node, remove it from that
   --  list.

   -------------------
   -- Node creation --
   -------------------

   function Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Create a clone of the Handle node tree. The result is not tied to any
   --  analysis unit tree.

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : Type_Ref) return Node_Rewriting_Handle;
   --  Create a new node of the given type, with empty text (for token nodes)
   --  or children (for regular nodes).

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : Type_Ref;
      Text   : Text_Type) return Node_Rewriting_Handle;
   --  Create a new token node with the given type and ``Text``

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : Type_Ref;
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle;
   --  Create a new regular node of the given type and assign it the given
   --  ``Children``.
   --
   --  Except for lists, which can have any number of children, the size of
   --  ``Children`` must match the number of children associated to the given
   --  type. Besides, all given children must not be tied.

   ---------------
   -- Templates --
   ---------------

   --  Templating is a way to create trees of node rewriting handles. It is
   --  intended to be more convenient than calling node constructors for each
   --  individual node in a tree.
   --
   --  A template is text that represents source code, including zero or
   --  multiple placeholders (stray "{}").
   --
   --  Create a tree of new nodes from a template is called instantiating a
   --  template: just call ``Create_From_Template``, passing to it the template
   --  itself, a sequence of nodes (the template arguments) to fill the
   --  template placeholders and a grammar rule to parse the resulting source
   --  code. This will unparse given nodes to replace placeholders in the
   --  template text, and then parse the resulting source code in order to
   --  create a tree of node rewriting handles.
   --
   --  In order not to interfer with the template DSL, stray "{" and "}"
   --  characters in the source code must be doubled: for instance "{{"
   --  represent "{" in the source code to be parsed.

   function Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : Text_Type;
      Arguments : Node_Rewriting_Handle_Array;
      Rule      : Grammar_Rule_Ref) return Node_Rewriting_Handle;
   --  Create a tree of new nodes from the given ``Template`` string, replacing
   --  placeholders with nodes in ``Arguments`` and parsed according to the
   --  given grammar ``Rule``.

private

   use Liblktlang_Support.Internal.Analysis;
   use Liblktlang_Support.Rewriting.Types;
   use Liblktlang_Support.Types;

   type Rewriting_Safety_Net is record
      Context         : Internal_Context;
      Context_Version : Version_Number;
      --  Analysis context and version number at the time this safety net was
      --  produced.

      Rewriting_Version : Version_Number;
      --  Serial number for the rewriting session at the time this safety net
      --  was produced (i.e. copy of ``Rewriting_Context_Reference.Version``).
   end record;

   No_Rewriting_Safety_Net : constant Rewriting_Safety_Net :=
     (No_Internal_Context, 0, 0);

   type Rewriting_Handle is record
      Ref        : Rewriting_Handle_Access;
      Safety_Net : Rewriting_Safety_Net;
   end record;

   type Unit_Rewriting_Handle is record
      Ref        : Unit_Rewriting_Handle_Access;
      Safety_Net : Rewriting_Safety_Net;
   end record;

   type Node_Rewriting_Handle is record
      Ref        : Node_Rewriting_Handle_Access;
      Safety_Net : Rewriting_Safety_Net;
   end record;

   No_Rewriting_Handle      : constant Rewriting_Handle :=
     (Ref => null, Safety_Net => No_Rewriting_Safety_Net);
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle :=
     (Ref => null, Safety_Net => No_Rewriting_Safety_Net);
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle :=
     (Ref => null, Safety_Net => No_Rewriting_Safety_Net);

end Liblktlang_Support.Generic_API.Rewriting;
