
--  This package provides support for tree-based source code rewriting.
--
--  .. ATTENTION:: This is an experimental feature, so even if it is exposed to
--  allow experiments, it is totally unsupported and the API is very likely to
--  change in the future.

with System;

with Liblktlang_Support.Generic_API.Introspection;
use Liblktlang_Support.Generic_API.Introspection;
private with Liblktlang_Support.Generic_API.Rewriting;

with Liblktlang.Analysis; use Liblktlang.Analysis;
with Liblktlang.Common;   use Liblktlang.Common;
with Liblktlang.Generic_API.Introspection;
use Liblktlang.Generic_API.Introspection;

package Liblktlang.Rewriting is

   use Support.Diagnostics, Support.Text;

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

   function Handle (Context : Analysis_Context) return Rewriting_Handle;
   --  Return the rewriting handle associated to Context, or
   --  No_Rewriting_Handle if Context is not being rewritten.

   function Context (Handle : Rewriting_Handle) return Analysis_Context;
   --  Return the analysis context associated to Handle

   function Start_Rewriting
     (Context : Analysis_Context) return Rewriting_Handle;
   --  Start a rewriting session for Context.
   --
   --  This handle will keep track of all changes to do on Context's analysis
   --  units. Once the set of changes is complete, call the Apply procedure to
   --  actually update Context. This makes it possible to inspect the "old"
   --  Context state while creating the list of changes.
   --
   --  There can be only one rewriting session per analysis context, so this
   --  will raise an Existing_Rewriting_Handle_Error exception if Context
   --  already has a living rewriting session.

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle);
   --  Discard all modifications registered in Handle and close Handle. This
   --  invalidates all related unit/node handles.

   type Apply_Result (Success : Boolean := True) is record
      case Success is
         when False =>
            Unit : Analysis_Unit;
            --  Reference to the analysis unit that was being processed when
            --  the error occurred.

            Diagnostics : Diagnostics_Vectors.Vector;
            --  Corresponding list of error messages
         when True => null;
      end case;
   end record;

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result;
   --  Apply all modifications to Handle's analysis context. If that worked,
   --  close Handle and return (Success => True). Otherwise, reparsing did not
   --  work, so keep Handle and its Context unchanged and return details about
   --  the error that happened.
   --
   --  Note that on success, this invalidates all related unit/node handles.

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array;
   --  Return the list of unit rewriting handles in the given context handle
   --  for units that the Apply primitive will modify.

   --------------------
   -- Unit rewriting --
   --------------------

   function Handle (Unit : Analysis_Unit) return Unit_Rewriting_Handle;
   --  Return the rewriting handle corresponding to Unit

   function Unit (Handle : Unit_Rewriting_Handle) return Analysis_Unit;
   --  Return the unit corresponding to Handle

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Return the node handle corresponding to the root of the unit which
   --  Handle designates.

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle);
   --  Set the root node for the unit Handle to Root. This unties the previous
   --  root handle. If Root is not No_Node_Rewriting_Handle, this also ties
   --  Root to Handle.
   --
   --  Root must not already be tied to another analysis unit handle.

   function Unparse (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type;
   --  Return the text associated to the given unit.

   --------------------
   -- Node rewriting --
   --------------------

   function Handle
     (Node : Lkt_Node'Class) return Node_Rewriting_Handle;
   --  Return the rewriting handle corresponding to Node.
   --
   --  The owning unit of Node must be free of diagnostics.

   function Node
     (Handle : Node_Rewriting_Handle) return Lkt_Node;
   --  Return the node which the given rewriting Handle relates to. This can be
   --  the null entity if this handle designates a new node.

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle;
   --  Return a handle for the rewriting context to which Handle belongs

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Turn the given rewritten node Handle designates into text. This is the
   --  text that is used in Apply in order to re-create an analysis unit.

   function Kind (Handle : Node_Rewriting_Handle) return Lkt_Node_Kind_Type;
   --  Return the kind corresponding to Handle's node

   function Type_Of
     (Handle : Node_Rewriting_Handle)
      return Liblktlang_Support.Generic_API.Introspection.Type_Ref
   is (Kind_To_Type (Kind (Handle)));
   --  Return the introspection type reference corresponding to ``Handle``'s
   --  node.

   function Image (Handle : Node_Rewriting_Handle) return String;
   --  Return a representation of ``Handle`` as a string.

   function Tied (Handle : Node_Rewriting_Handle) return Boolean;
   --  Return whether this node handle is tied to an analysis unit. If it is
   --  not, it can be passed as the Child parameter to Set_Child.

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Return a handle for the node that is the parent of Handle's node. This
   --  is ``No_Rewriting_Handle`` for a node that is not tied to any tree yet.

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural;
   --  Return the number of children the node represented by Handle has

   function Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref) return Node_Rewriting_Handle;
   --  Return the node that is in the syntax ``Field`` for ``Handle``

   function Child
     (Handle : Node_Rewriting_Handle;
      Fields : Struct_Member_Ref_Array) return Node_Rewriting_Handle;
   --  Return a child deep in the tree ``Handle``.
   --
   --  Assuming ``Fields'Range`` is ``1 .. N``, this is a shortcut for:
   --
   --  .. code::
   --
   --     C1 := Child (Handle, Fields (1));
   --     C2 := Child (C1, Fields (2));
   --     ...
   --     CN_1 := Child (CN_2, Fields (N - 1));
   --     CN := Child (CN_1, Fields (N));

   function Children
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle_Array;
   --  Return the list of children for ``Handle``.

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Field  : Struct_Member_Ref;
      Child  : Node_Rewriting_Handle);
   --  If ``Child`` is ``No_Rewriting_Node``, untie the syntax field in
   --  ``Handle`` corresponding to ``Field``, so it can be attached to another
   --  one. Otherwise, ``Child`` must have no parent as it will be tied to
   --  ``Handle``'s tree.

   function Text (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Return the text associated to the given token node.

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type);
   --  Override text associated to the given token node.

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle);
   --  If Handle is the root of an analysis unit, untie it and set New_Node as
   --  its new root. Otherwise, replace Handle with New_Node in Handle's parent
   --  node.
   --
   --  Note that: * Handle must be tied to an existing analysis unit handle. *
   --  New_Node must not already be tied to another analysis unit handle.

   procedure Rotate (Handles : Node_Rewriting_Handle_Array);
   --  Given a list of node rewriting handles ``H1``, ``H2``, ... ``HN``,
   --  replace ``H1`` by ``H2`` in the rewritten tree, replace ``H2`` by
   --  ``H3``, etc. and replace ``HN`` by ``H1``.
   --
   --  Note that this operation is atomic: if it fails, no replacement is
   --  actually performed.

   function Is_List_Node (Handle : Node_Rewriting_Handle) return Boolean;
   --  Return whether ``Handle`` represents a list node.

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
      Kind   : Lkt_Node_Kind_Type) return Node_Rewriting_Handle;
   --  Create a new node of the given Kind, with empty text (for token nodes)
   --  or children (for regular nodes).

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : Lkt_Node_Kind_Type;
      Text   : Text_Type) return Node_Rewriting_Handle;
   --  Create a new token node with the given Kind and Text

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : Lkt_Node_Kind_Type;
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle;
   --  Create a new regular node of the given Kind and assign it the given
   --  Children.
   --
   --  Except for lists, which can have any number of children, the size of
   --  Children must match the number of children associated to the given Kind.
   --  Besides, all given children must not be tied.

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
   --  template: just call Create_From_Template, passing to it the template
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
      Rule      : Grammar_Rule) return Node_Rewriting_Handle;
   --  Create a tree of new nodes from the given Template string, replacing
   --  placeholders with nodes in Arguments and parsed according to the given
   --  grammar Rule.

   -----------------------------
   -- Node creation shortcuts --
   -----------------------------



         function Create_Argument
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Value : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Lexer_Case_Rule_Cond_Alt
           (Handle : Rewriting_Handle
               ; F_Cond_Exprs : Node_Rewriting_Handle
               ; F_Send : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Lexer_Case_Rule_Default_Alt
           (Handle : Rewriting_Handle
               ; F_Send : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Binding_Pattern
           (Handle : Rewriting_Handle
               ; F_Binding : Node_Rewriting_Handle
               ; F_Value_Pattern : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Filtered_Pattern
           (Handle : Rewriting_Handle
               ; F_Pattern : Node_Rewriting_Handle
               ; F_Predicate : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_List_Pattern
           (Handle : Rewriting_Handle
               ; F_Patterns : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Extended_Node_Pattern
           (Handle : Rewriting_Handle
               ; F_Node_Pattern : Node_Rewriting_Handle
               ; F_Details : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Type_Pattern
           (Handle : Rewriting_Handle
               ; F_Type_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Not_Pattern
           (Handle : Rewriting_Handle
               ; F_Pattern : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Or_Pattern
           (Handle : Rewriting_Handle
               ; F_Left : Node_Rewriting_Handle
               ; F_Right : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Paren_Pattern
           (Handle : Rewriting_Handle
               ; F_Pattern : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Splat_Pattern
           (Handle : Rewriting_Handle
               ; F_Binding : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Tuple_Pattern
           (Handle : Rewriting_Handle
               ; F_Patterns : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Rule_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Synthetic_Lexer_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Node_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Self_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Enum_Lit_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Field_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
               ; F_Trait_Ref : Node_Rewriting_Handle
               ; F_Default_Val : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Fun_Param_Decl
           (Handle : Rewriting_Handle
               ; F_Decl_Annotations : Node_Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
               ; F_Default_Val : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Lambda_Param_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
               ; F_Default_Val : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Dyn_Var_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Match_Val_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Val_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Decl_Type : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Fun_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
               ; F_Return_Type : Node_Rewriting_Handle
               ; F_Trait_Ref : Node_Rewriting_Handle
               ; F_Body : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Env_Spec_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Actions : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Decl
           (Handle : Rewriting_Handle
               ; F_Generic_Param_Decls : Node_Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Rules : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Lexer_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Rules : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Lexer_Family_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Rules : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Synth_Fun_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Synth_Param_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Any_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Enum_Class_Alt_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Function_Type
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Param_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Has_Class : Node_Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Class_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Enum_Class_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Branches : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Enum_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
               ; F_Literals : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Struct_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Trait_Decl
           (Handle : Rewriting_Handle
               ; F_Syn_Name : Node_Rewriting_Handle
               ; F_Traits : Node_Rewriting_Handle
               ; F_Syn_Base_Type : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Decl_Annotation
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Decl_Annotation_Args
           (Handle : Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Elsif_Branch
           (Handle : Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
               ; F_Then_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Enum_Class_Case
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Any_Of
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Values : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Array_Literal
           (Handle : Rewriting_Handle
               ; F_Exprs : Node_Rewriting_Handle
               ; F_Element_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Call_Expr
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Logic_Predicate
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Logic_Propagate_Call
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Bin_Op
           (Handle : Rewriting_Handle
               ; F_Left : Node_Rewriting_Handle
               ; F_Op : Node_Rewriting_Handle
               ; F_Right : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Block_Expr
           (Handle : Rewriting_Handle
               ; F_Val_Defs : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Cast_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Null_Cond : Node_Rewriting_Handle
               ; F_Excludes_Null : Node_Rewriting_Handle
               ; F_Dest_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Dot_Expr
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Null_Cond : Node_Rewriting_Handle
               ; F_Suffix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Error_On_Null
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Instantiation
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Discard
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Dont_Skip
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Dont_Skip : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_List
           (Handle : Rewriting_Handle
               ; F_List_Type : Node_Rewriting_Handle
               ; F_Kind : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Sep : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Null
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Opt
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Opt_Error
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Opt_Error_Group
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Opt_Group
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Or_Expr
           (Handle : Rewriting_Handle
               ; F_Sub_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Pick
           (Handle : Rewriting_Handle
               ; F_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Implicit_Pick
           (Handle : Rewriting_Handle
               ; F_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Predicate
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Prop_Ref : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Rule_Ref
           (Handle : Rewriting_Handle
               ; F_Node_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Skip
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_Stop_Cut
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Parse_Node_Expr
           (Handle : Rewriting_Handle
               ; F_Node_Name : Node_Rewriting_Handle
               ; F_Sub_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Token_No_Case_Lit
           (Handle : Rewriting_Handle
               ; F_Lit : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Token_Pattern_Concat
           (Handle : Rewriting_Handle
               ; F_Left : Node_Rewriting_Handle
               ; F_Right : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Token_Ref
           (Handle : Rewriting_Handle
               ; F_Token_Name : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_If_Expr
           (Handle : Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
               ; F_Then_Expr : Node_Rewriting_Handle
               ; F_Alternatives : Node_Rewriting_Handle
               ; F_Else_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Isa
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Pattern : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Keep_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Null_Cond : Node_Rewriting_Handle
               ; F_Keep_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Lambda_Expr
           (Handle : Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
               ; F_Return_Type : Node_Rewriting_Handle
               ; F_Body : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Null_Lit
           (Handle : Rewriting_Handle
               ; F_Dest_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Block_String_Lit
           (Handle : Rewriting_Handle
               ; F_Lines : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Logic_Assign
           (Handle : Rewriting_Handle
               ; F_Dest_Var : Node_Rewriting_Handle
               ; F_Value : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Logic_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Logic_Propagate
           (Handle : Rewriting_Handle
               ; F_Dest_Var : Node_Rewriting_Handle
               ; F_Call : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Logic_Unify
           (Handle : Rewriting_Handle
               ; F_Lhs : Node_Rewriting_Handle
               ; F_Rhs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Match_Expr
           (Handle : Rewriting_Handle
               ; F_Match_Expr : Node_Rewriting_Handle
               ; F_Branches : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Not_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Paren_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Raise_Expr
           (Handle : Rewriting_Handle
               ; F_Dest_Type : Node_Rewriting_Handle
               ; F_Except_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Subscript_Expr
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Null_Cond : Node_Rewriting_Handle
               ; F_Index : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Try_Expr
           (Handle : Rewriting_Handle
               ; F_Try_Expr : Node_Rewriting_Handle
               ; F_Or_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Un_Op
           (Handle : Rewriting_Handle
               ; F_Op : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Full_Decl
           (Handle : Rewriting_Handle
               ; F_Doc : Node_Rewriting_Handle
               ; F_Decl_Annotations : Node_Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Grammar_List_Sep
           (Handle : Rewriting_Handle
               ; F_Token : Node_Rewriting_Handle
               ; F_Extra : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Import
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Langkit_Root
           (Handle : Rewriting_Handle
               ; F_Imports : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Lexer_Case_Rule
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Alts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Lexer_Case_Rule_Send
           (Handle : Rewriting_Handle
               ; F_Sent : Node_Rewriting_Handle
               ; F_Match_Size : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Match_Branch
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Node_Pattern_Field
           (Handle : Rewriting_Handle
               ; F_Id : Node_Rewriting_Handle
               ; F_Expected_Value : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Node_Pattern_Property
           (Handle : Rewriting_Handle
               ; F_Call : Node_Rewriting_Handle
               ; F_Expected_Value : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Node_Pattern_Selector
           (Handle : Rewriting_Handle
               ; F_Call : Node_Rewriting_Handle
               ; F_Pattern : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Selector_Call
           (Handle : Rewriting_Handle
               ; F_Quantifier : Node_Rewriting_Handle
               ; F_Binding : Node_Rewriting_Handle
               ; F_Selector_Call : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Function_Type_Ref
           (Handle : Rewriting_Handle
               ; F_Param_Types : Node_Rewriting_Handle
               ; F_Return_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Type_Ref
           (Handle : Rewriting_Handle
               ; F_Type_Name : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Simple_Type_Ref
           (Handle : Rewriting_Handle
               ; F_Type_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Var_Bind
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


private

   package G renames Liblktlang_Support.Generic_API.Rewriting;

   type Rewriting_Handle is new G.Rewriting_Handle;
   type Unit_Rewriting_Handle is new G.Unit_Rewriting_Handle;
   type Node_Rewriting_Handle is new G.Node_Rewriting_Handle;

   No_Rewriting_Handle : constant Rewriting_Handle :=
      Rewriting_Handle (G.No_Rewriting_Handle);
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle :=
      Unit_Rewriting_Handle (G.No_Unit_Rewriting_Handle);
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle :=
      Node_Rewriting_Handle (G.No_Node_Rewriting_Handle);

end Liblktlang.Rewriting;
