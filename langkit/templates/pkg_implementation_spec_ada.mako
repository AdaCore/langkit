## vim: filetype=makoada

<%namespace name="array_types"       file="array_types_ada.mako" />
<%namespace name="astnode_types"     file="astnode_types_ada.mako" />
<%namespace name="enum_types"        file="enum_types_ada.mako" />
<%namespace name="exts"              file="extensions.mako" />
<%namespace name="list_types"        file="list_types_ada.mako" />
<%namespace name="struct_types"      file="struct_types_ada.mako" />
<%namespace name="public_properties" file="public_properties_ada.mako" />
<%namespace name="memoization"       file="memoization_ada.mako" />

<%
   root_node_array = T.root_node.array
   no_builtins = lambda ts: filter(lambda t: not t.is_builtin(), ts)
%>

with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;

with System;

% if ctx.properties_logging:
   with GNATCOLL.Traces;
% endif

with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Eq_Same;

with Langkit_Support.Bump_Ptr;    use Langkit_Support.Bump_Ptr;
with Langkit_Support.Cheap_Sets;
with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Extensions;  use Langkit_Support.Extensions;
with Langkit_Support.Lexical_Env;
with Langkit_Support.Slocs;       use Langkit_Support.Slocs;
with Langkit_Support.Symbols;     use Langkit_Support.Symbols;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Langkit_Support.Vectors;

with ${ada_lib_name}.Analysis.Parsers;  use ${ada_lib_name}.Analysis.Parsers;
with ${ada_lib_name}.Lexer;    use ${ada_lib_name}.Lexer;
use ${ada_lib_name}.Lexer.Token_Data_Handlers;

% if ctx.separate_properties:
   with ${ada_lib_name}.Analysis.Properties;
   use ${ada_lib_name}.Analysis.Properties;
% endif

${exts.with_clauses(with_clauses)}

package ${ada_lib_name}.Analysis.Implementation is

   % if ctx.properties_logging:
      Properties_Traces : constant GNATCOLL.Traces.Trace_Handle :=
         GNATCOLL.Traces.Create
           ("Properties", GNATCOLL.Traces.On, Stream => "&1");
   % endif

   type ${root_node_value_type};
   --  This "by-value" type is public to expose the fact that the various
   --  AST nodes are a hierarchy of tagged types, but it is not intended to be
   --  used directly, hence the "_Type" suffix. Please use instead the
   --  class-wide types such at the one below.

   type ${root_node_type_name} is access all ${root_node_value_type}'Class;
   --  Most generic AST node type

   function Is_Null
     (Node : access ${root_node_value_type}'Class) return Boolean;

   function Short_Image
     (Node : access ${root_node_value_type}'Class) return Text_Type;
   --  Return a short representation of the node, containing just the kind
   --  name and the sloc.

   ----------------
   -- Extensions --
   ----------------

   --  Extensions are a way to associate arbitrary data (Extension_Type, i.e.
   --  pointers) to AST nodes.
   --
   --  In order to associate an extension to an AST node, one has first to
   --  register itself in Langkit_Support.Extensions to get an Extension_ID.
   --  Then, this ID must be passed to Get_Extension, which will create a slot
   --  to store this extension (or return an already existing one for the same
   --  ID). It is this slot that can be used to store arbitrary data.
   --
   --  As AST nodes can be deallocated later on, this abritrary data sometimes
   --  needs to be deallocated as well. The destructor mechanism was designed
   --  for this: when the AST node is about to be deallocated, the destructor
   --  callback is invoked so that one has a chance to release allocated
   --  resources.

   type Extension_Type is new System.Address;
   --  Data type storing arbitrary values in AST nodes

   type Extension_Access is access all Extension_Type;
   --  Access to the arbitrary values stored in AST nodes

   type Extension_Destructor is
     access procedure (Node      : access ${root_node_value_type}'Class;
                       Extension : Extension_Type)
     with Convention => C;
   --  Type for extension destructors. The parameter are the "Node" the
   --  extension was attached to and the "Extension" itself.

   function Get_Extension
     (Node : access ${root_node_value_type}'Class;
      ID   : Extension_ID;
      Dtor : Extension_Destructor) return Extension_Access;
   --  Get (and create if needed) the extension corresponding to ID for Node.
   --  If the extension is created, the Dtor destructor is associated to it.
   --  Note that the returned access is not guaranteed to stay valid after
   --  subsequent calls to Get_Extension.

   ---------------------------
   -- Environments handling --
   ---------------------------

   --  The following types and operations are implementation details we did not
   --  manage yet to put in a private part. Please don't use them.

   ${struct_types.public_incomplete_decl(T.env_md)}
   ${struct_types.public_decl(T.env_md)}

   function Combine
     (L, R : ${T.env_md.name}) return ${T.env_md.name};
   --  The combine function on environments metadata does a boolean Or on every
   --  boolean component of the env metadata.

   function Can_Reach (El, From : ${root_node_type_name}) return Boolean;
   --  Return whether El can reach From, from a sequential viewpoint. If
   --  elements are declared in different units, it will always return True,
   --  eg this does not handle general visibility issues, just sequentiality of
   --  declarations.

   function AST_Envs_Element_Image
     (Node  : ${root_node_type_name};
      Short : Boolean := True) return Text_Type;
   --  Return a "sourcefile:lineno:columnno" corresponding to the starting sloc
   --  of Node. Used to create a human-readable representation for env.
   --  rebindings.

   procedure Raise_Property_Error (Message : String := "");

   function Is_Rebindable (Node : ${root_node_type_name}) return Boolean;

   procedure Register_Rebinding
     (Node : ${root_node_type_name}; Rebinding : System.Address);
   --  Register a rebinding to be destroyed when Node's analysis unit is
   --  destroyed or reparsed.
   --
   --  TODO??? For now the rebinding must be represented as an untyped pointer
   --  because we probably need some big refactoring to provide to
   --  Langkit_Support.Lexical_Env a procedure that has visibility on both
   --  Env_Rebindings and on the analysis unit record.

   function Element_Parent
     (Node : ${root_node_type_name}) return ${root_node_type_name};

   function Hash
     (Node : access ${root_node_value_type}'Class) return Hash_Type;
   function Named_Hash (Node : ${root_node_type_name}) return Hash_Type is
     (Hash (Node));

   function Version (Unit : Analysis_Unit) return Natural;
   --  Return the version for Unit. Version is a number that is incremented
   --  every time Unit changes.

   package AST_Envs is new Langkit_Support.Lexical_Env
     (Unit_T               => Analysis_Unit,
      No_Unit              => No_Analysis_Unit,
      Get_Version          => Version,
      Element_T            => ${root_node_type_name},
      Element_Metadata     => ${T.env_md.name},
      No_Element           => null,
      Empty_Metadata       => No_Metadata,
      Element_Hash         => Named_Hash,
      Metadata_Hash        => Hash,
      Raise_Property_Error => Raise_Property_Error,
      Combine              => Combine,
      Element_Image        => AST_Envs_Element_Image,
      Register_Rebinding   => Register_Rebinding);

   use AST_Envs;

   function Create
     (El : ${root_node_type_name}; Info : Entity_Info)
      return Entity;

   ## Declare arrays of lexical environments here because we need them for the
   ## Group operation below.
   ${array_types.public_incomplete_decl(T.LexicalEnvType.array)}
   ${array_types.public_decl(T.LexicalEnvType.array)}

   ## See ASTNodeType.entity
   ${array_types.public_incomplete_decl(T.root_node.entity.array)}
   ${array_types.public_decl(T.root_node.entity.array)}

   ## Declare arrays of root nodes here since some primitives rely on it and
   ## since the declarations require AST_Envs.
   ${array_types.public_incomplete_decl(root_node_array)}
   ${array_types.public_decl(root_node_array)}

   ## Generate Hash functions for "built-in types" if need be
   % if T.BoolType.requires_hash_function:
      function Hash (B : Boolean) return Hash_Type;
   % endif

   % if T.LongType.requires_hash_function:
      function Hash (I : Integer) return Hash_Type;
   % endif

   % if T.entity_info.requires_hash_function:
      function Hash (Info : Entity_Info) return Hash_Type;
   % endif

   ------------------------------------------------------
   -- AST node derived types (incomplete declarations) --
   ------------------------------------------------------

   type ${generic_list_value_type};
   --  Base type for all lists of AST node subclasses

   type ${generic_list_type_name} is
      access all ${generic_list_value_type}'Class;

   % for astnode in no_builtins(ctx.astnode_types):
     % if not astnode.is_list_type:
       ${astnode_types.public_incomplete_decl(astnode)}
     % endif
   % endfor

   % for astnode in ctx.astnode_types:
      % if astnode.is_root_list_type:
         ${list_types.public_incomplete_decl(astnode.element_type)}
      % elif astnode.is_list_type:
         ${astnode_types.public_incomplete_decl(astnode)}
      % endif
   % endfor

   % if ctx.properties_logging:
      function Trace_Image
        (Node       : access ${root_node_value_type}'Class;
         Decoration : Boolean := True) return String;
   % endif

   function Kind_Name
     (Node : access ${root_node_value_type}'Class) return String;
   --  Return the concrete kind for Node

   function Is_Ghost
     (Node : access ${root_node_value_type}'Class) return Boolean;
   ${ada_doc('langkit.node_is_ghost', 3)}

   function Get_Unit
     (Node : access ${root_node_value_type}'Class)
      return Analysis_Unit;
   ${ada_doc('langkit.node_unit', 3)}

   -------------------------------
   -- Tree traversal operations --
   -------------------------------

   function Child_Count
     (Node : access ${root_node_value_type}'Class) return Natural;
   --  Return the number of children Node has

   function First_Child_Index
     (Node : access ${root_node_value_type}'Class) return Natural;
   --  Return the index of the first child Node has

   function Last_Child_Index
     (Node : access ${root_node_value_type}'Class) return Natural;
   --  Return the index of the last child Node has, or 0 if there is no child

   procedure Get_Child
     (Node            : access ${root_node_value_type}'Class;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out ${root_node_type_name});
   --  Get the Index'th child of Node, storing it into Result. Child indexing
   --  is 1-based. Store in Index_In_Bounds whether Node had such a child; if
   --  not, the content of Result is undefined.

   function Child
     (Node  : access ${root_node_value_type}'Class;
      Index : Positive) return ${root_node_type_name};
   --  Return the Index'th child of Node, or null if Node has no such child

   function Children
     (Node : access ${root_node_value_type}'Class)
      return ${root_node_array.api_name};
   --  Return an array containing all the children of Node.
   --  This is an alternative to the Child/Child_Count pair, useful if you want
   --  the convenience of Ada arrays, and you don't care about the small
   --  performance hit of creating an array.

   function Parents
     (Node         : access ${root_node_value_type}'Class;
      Include_Self : Boolean := True)
      return ${root_node_array.name};
   --  Return the list of parents for this node. This node included in the list
   --  iff Include_Self.

   function Parent
     (Node : access ${root_node_value_type}'Class)
      return ${root_node_type_name};

   function Traverse
     (Node  : access ${root_node_value_type}'Class;
      Visit : access function (Node : access ${root_node_value_type}'Class)
                               return Visit_Status)
      return Visit_Status;
   --  Given the parent node for a subtree, traverse all syntactic nodes of
   --  this tree, calling the given function on each node in pre order (ie.
   --  top-down). The order of traversing subtrees follows the order of
   --  declaration of the corresponding attributes in the grammar. The
   --  traversal is controlled as follows by the result returned by Visit:
   --
   --     Into   The traversal continues normally with the syntactic
   --            children of the node just processed.
   --
   --     Over   The children of the node just processed are skipped and
   --            excluded from the traversal, but otherwise processing
   --            continues elsewhere in the tree.
   --
   --     Stop   The entire traversal is immediately abandoned, and the
   --            original call to Traverse returns Stop.

   procedure Traverse
     (Node  : access ${root_node_value_type}'Class;
      Visit : access function (Node : access ${root_node_value_type}'Class)
                               return Visit_Status);
   --  This is the same as Traverse function except that no result is returned
   --  i.e. the Traverse function is called and the result is simply discarded.

   generic
      type Data_Type is private;
      Reset_After_Traversal : Boolean := False;
   function Traverse_With_Data
     (Node  : access ${root_node_value_type}'Class;
      Visit : access function (Node : access ${root_node_value_type}'Class;
                               Data : in out Data_Type)
                               return Visit_Status;
      Data  : in out Data_Type)
      return Visit_Status;
   --  This is the same as the first Traverse function except it accepts an
   --  argument that is passed to all Visit calls.
   --
   --  If Reset_After_Traversal is True, the Data formal is left unchanged when
   --  Traverse_With_Data returns no matter what Visit does. Visit can change
   --  it otherwise.

   function Child_Index
     (Node : access ${root_node_value_type}'Class)
      return Natural;
   --  Return the 0-based index for Node in its parent's children

   function Previous_Sibling
     (Node : access ${root_node_value_type}'Class)
      return ${root_node_type_name};
   --  Return the Node's previous sibling in the tree, if there is such a node

   function Next_Sibling
     (Node : access ${root_node_value_type}'Class)
      return ${root_node_type_name};
   --  Return the Node's next sibling in the tree, if there is such a node

   ----------------------------------------
   -- Source location-related operations --
   ----------------------------------------

   function Sloc_Range
     (Node : access ${root_node_value_type}'Class;
      Snap : Boolean := False) return Source_Location_Range;
   --  Return the source location range corresponding to the set of tokens from
   --  which Node was parsed.
   --
   --  TODO??? Document the Snap formal.

   function Compare
     (Node : access ${root_node_value_type}'Class;
      Sloc : Source_Location;
      Snap : Boolean := False) return Relative_Position;
   --  Compare Sloc to the sloc range of Node.
   --
   --  TODO??? Document the Snap formal.

   function Lookup
     (Node : access ${root_node_value_type}'Class;
      Sloc : Source_Location;
      Snap : Boolean := False) return ${root_node_type_name};
   --  Look for the bottom-most AST node whose sloc range contains Sloc. Return
   --  it, or null if no such node was found.
   --
   --  TODO??? Document the Snap formal.

   -------------------
   -- Debug helpers --
   -------------------

   procedure Print
     (Node        : access ${root_node_value_type}'Class;
      Show_Slocs  : Boolean;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children.
   --  Line_Prefix is prepended to each output line.

   procedure PP_Trivia
     (Node        : access ${root_node_value_type}'Class;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children along
   --  with the trivia associated to them. Line_Prefix is prepended to each
   --  output line.

   procedure Assign_Names_To_Logic_Vars
     (Node : access ${root_node_value_type}'Class);
   --  Debug helper: Assign names to every logical variable in the root node,
   --  so that we can trace logical variables.

   ---------------------------
   -- Adalog instantiations --
   ---------------------------

   function Image (Ent : ${T.entity.name}) return Text_Type;
   function Image (Ent : ${T.entity.name}) return String;
   ${ada_doc('langkit.entity_image', 3)}

   package Eq_Node is new Langkit_Support.Adalog.Eq_Same
     (LR_Type       => ${T.entity.name},
      Element_Image => Image);
   subtype Logic_Var is Eq_Node.Refs.Raw_Var;
   subtype Logic_Var_Record is Eq_Node.Refs.Var;
   Null_Var : constant Logic_Var := null;
   Null_Var_Record : constant Logic_Var_Record := (Reset => True, others => <>);

   subtype Logic_Equation is Relation;
   Null_Logic_Equation : constant Logic_Equation := null;

   % if ctx.properties_logging:
      function Trace_Image (B : Boolean) return String;
      function Trace_Image (I : Integer) return String;
      function Trace_Image (S : Symbol_Type) return String;
      function Trace_Image (Env : Lexical_Env) return String;
      function Trace_Image (E : Entity) return String;
      function Trace_Image (Info : Entity_Info) return String;
      function Trace_Image (R : Env_Rebindings) return String;
      function Trace_Image (Unit : Analysis_Unit) return String;
      function Trace_Image (Eq : Logic_Equation) return String;
      function Trace_Image (Var : Logic_Var) return String;
   % endif

   -----------------------------------------------
   -- Structure types (incomplete declarations) --
   -----------------------------------------------

   % for struct_type in no_builtins(ctx.struct_types):
   ${struct_types.public_incomplete_decl(struct_type)}
   % endfor

   -------------------------------------------
   -- Array types (incomplete declarations) --
   -------------------------------------------

   % for array_type in ctx.sorted_types(ctx.array_types):
   % if array_type.element_type.should_emit_array_type:
   ${array_types.public_incomplete_decl(array_type)}
   % endif
   % endfor

   ------------------------------
   -- Root AST node properties --
   ------------------------------

   % for prop in T.root_node.get_properties( \
         include_inherited=False, \
         predicate=lambda f: f.is_public):
      ${prop.prop_decl}
   % endfor

   -----------------------------------------
   -- Structure types (full declarations) --
   -----------------------------------------

   % for struct_type in no_builtins(ctx.struct_types):
   ${struct_types.public_decl(struct_type)}
   % endfor

   -----------------
   -- Array types --
   -----------------

   --  We implement array types as discriminated records so that binding to C
   --  can be done without copy.

   --  TODO??? This is likely to change in the near future: we would like to
   --  have here pure Ada arrays instead.

   % for array_type in ctx.sorted_types(ctx.array_types):
   % if array_type.element_type.should_emit_array_type:
   ${array_types.public_decl(array_type)}
   % endif
   % endfor

   --------------------------
   -- Extensions internals --
   --------------------------

   type Extension_Slot is record
      ID        : Extension_ID;
      Extension : Extension_Access;
      Dtor      : Extension_Destructor;
   end record;

   package Extension_Vectors is new Langkit_Support.Vectors
     (Element_Type => Extension_Slot);

   -------------------------------
   -- Root AST node (internals) --
   -------------------------------

   type ${root_node_value_type} is abstract tagged record
      Parent                 : ${root_node_type_name} := null;

      Unit                   : Analysis_Unit := No_Analysis_Unit;
      --  Reference to the analysis unit that owns this node

      Token_Start_Index      : Token_Index  := No_Token_Index;
      Token_End_Index        : Token_Index  := No_Token_Index;
      --  Reference to the start and end token that constitutes this node. If
      --  this node is a ghost, Token_Start_Index is the token that this AST
      --  node relates to and Token_End_Index is No_Token_Index. Otherwise,
      --  both tokens are inclusive, i.e. they both belong to this node.

      Extensions             : Extension_Vectors.Vector;

      Self_Env               : Lexical_Env;
      --  Hold the environment this node defines, or the parent environment
      --  otherwise.

      Kind : ${root_node_kind_name};
      --  Kind for this node. This must reflect the object tag

      ${astnode_types.node_fields(T.root_node, emit_null=False)}
   end record;

   function Pre_Env_Actions
     (Self                : access ${root_node_value_type}'Class;
      Bound_Env, Root_Env : Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return Lexical_Env;
   --  Internal procedure that will execute all necessary lexical env actions
   --  for Node. This is meant to be called by Populate_Lexical_Env, and not by
   --  the user.
   --
   --  The return value is the initial environment to be passed to
   --  Post_Env_Actions.

   procedure Post_Env_Actions
     (Self                : access ${root_node_value_type}'Class;
      Bound_Env, Root_Env : Lexical_Env);
   --  Internal procedure that will execute all post add to env actions for
   --  Node. This is meant to be called by Populate_Lexical_Env.

   ------------------------------
   -- Root AST node properties --
   ------------------------------

   % for prop in T.root_node.get_properties( \
         include_inherited=False, \
         predicate=lambda f: f.is_private):
      % if prop.dispatching:
         ${prop.prop_decl}
      % endif
   % endfor

   -----------------------
   -- Generic list type --
   -----------------------

   package Alloc_AST_List_Array is new Langkit_Support.Bump_Ptr.Array_Alloc
     (Element_T  => ${root_node_type_name},
      Index_Type => Positive);

   type ${generic_list_value_type} is
      abstract new ${root_node_value_type}
   with record
      Count : Natural;
      Nodes : Alloc_AST_List_Array.Element_Array_Access;
   end record;
   --  Base type for all lists of AST node subclasses

   function Length
     (Node : access ${generic_list_value_type}'Class) return Natural;

   function Children
     (Node : access ${root_node_value_type}'Class)
      return ${root_node_array.name};
   --  Return an array containing all the children of Node.
   --  This is an alternative to the Child/Child_Count pair, useful if you want
   --  the convenience of ada arrays, and you don't care about the small
   --  performance hit of creating an array.

   procedure Reset_Logic_Vars
     (Node : access ${root_node_value_type}'Class);
   --  Reset the logic variables attached to this node

   procedure Set_Parents (Node, Parent : access ${root_node_value_type}'Class);
   --  Set Node.Parent to Parent, and initialize recursively the parent of all
   --  child nodes.

   procedure Destroy (Node : access ${root_node_value_type}'Class);
   --  Free the resources allocated to this node and all its children

   --------------------------------------
   -- Environments handling (internal) --
   --------------------------------------

   No_Entity : constant Entity := (null, No_Entity_Info);

   function Get (A : AST_Envs.Entity_Array; Index : Integer) return Entity;
   --  Simple getter that raises Property_Error on out-of-bound accesses.
   --  Useful for code generation.

   function Group is new AST_Envs.Group
     (Index_Type        => Positive,
      Lexical_Env_Array => ${T.LexicalEnvType.array.api_name});

   function Group
     (Envs   : ${T.LexicalEnvType.array.name};
      Env_Md : ${T.env_md.name} := No_Metadata) return ${T.LexicalEnvType.name};
   --  Convenience wrapper for uniform types handling in code generation

   procedure Free_Extensions (Node : access ${root_node_value_type}'Class);
   --  Implementation helper to free the extensions associatde to Node

   ${array_types.private_decl(T.LexicalEnvType.array)}
   ${array_types.private_decl(T.root_node.entity.array)}
   ${array_types.private_decl(root_node_array)}

   package ${T.root_node.array.pkg_vector} is
      new Langkit_Support.Vectors (${T.root_node.name});

   function Is_Visible_From
     (Referenced_Env, Base_Env : Lexical_Env) return Boolean;
   --  Return whether the unit that Referenced_Env belongs to is visible from
   --  the unit that Base_Env belongs to. If at least one of these two lexical
   --  environments does not belong to a particular analysis unit, this raises
   --  a Property_Error.

   function Populate_Lexical_Env
     (Node     : access ${root_node_value_type}'Class;
      Root_Env : Lexical_Env) return Boolean;
   --  Populate the lexical environment for node and all its children. Return
   --  whether a Property_Error error occurred in the process.

   -----------------------------------
   -- Lexical utilities (internals) --
   -----------------------------------

   function Convert
     (TDH      : Token_Data_Handler;
      Token    : Token_Type;
      Raw_Data : Lexer.Token_Data_Type) return Analysis.Token_Data_Type;
   --  Turn data from TDH and Raw_Data into a user-ready token data record

   function Token
     (Node  : access ${root_node_value_type}'Class;
      Index : Token_Index) return Token_Type;
   --  Helper for properties. This is used to turn token indexes as stored in
   --  AST nodes into Token_Type values.

   function Stored_Token
     (Node  : access ${root_node_value_type}'Class;
      Token : Token_Type)
      return Token_Index;
   --  Helper for properties. This is used to turn a Token_Type value into a
   --  Token_Index value that can be stored as a field in Node. This raises a
   --  Property_Error if Node and Token don't belong to the same analysis unit
   --  or if Token is actually a Trivia.

   function Is_Synthetic
     (Node : access ${root_node_value_type}'Class) return Boolean;
   --  Returns whether the node is a synthetic node, i.e. whether it was
   --  generated for semantic analysis instead of parsing.

   % for array_type in ctx.sorted_types(ctx.array_types):
   % if array_type.element_type.should_emit_array_type:
   ${array_types.private_decl(array_type)}
   % endif
   % endfor

   % for astnode in no_builtins(ctx.astnode_types):
     % if astnode.is_root_list_type:
       ${list_types.private_decl(astnode.element_type)}
     % else:
       ${astnode_types.private_decl(astnode)}
     % endif
   % endfor

   type Exiled_Entry is record
      Env  : Lexical_Env;
      Key  : Symbol_Type;
      Node : ${root_node_type_name};
   end record;
   --  Tuple of values passed to AST_Envs.Add. Used in the lexical
   --  environment rerooting machinery: see Remove_Exiled_Entries and
   --  Reroot_Foreign_Nodes.

   package Exiled_Entry_Vectors is new Langkit_Support.Vectors (Exiled_Entry);

   function Token_Data (Unit : Analysis_Unit) return Token_Data_Handler_Access;

   procedure Register_Destroyable
     (Unit : Analysis_Unit; Node : ${root_node_type_name});
   --  Helper for synthetized nodes. We cannot used the generic
   --  Register_Destroyable because the root AST node is an abstract types, so
   --  this is implemented using the untyped (using System.Address)
   --  implementation helper.

   function Create
     (Node   : ${root_node_type_name};
      E_Info : Entity_Info := No_Entity_Info) return ${root_entity.api_name};

   function Bare_Node
     (Node : ${root_entity.api_name}'Class) return ${root_node_type_name};

   % if ctx.has_memoization:
   ------------------------
   --  Memoization state --
   ------------------------

   ${memoization.decl()}
   % endif

   -----------------------------
   -- Miscellanous operations --
   -----------------------------

   type Destroy_Procedure is access procedure (Object : System.Address);

   type Destroyable_Type is record
      Object  : System.Address;
      --  Object to destroy

      Destroy : Destroy_Procedure;
      --  Procedure to destroy Object
   end record;
   --  Simple holder to associate an object to destroy and the procedure to
   --  perform the destruction.

   package Destroyable_Vectors is new Langkit_Support.Vectors
     (Destroyable_Type);

   package Analysis_Unit_Sets is new Langkit_Support.Cheap_Sets
     (Analysis_Unit, null);

   package Units_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Analysis_Unit,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   % if ctx.symbol_literals:
      type Symbol_Literal_Type is (
         <%
            sym_items = ctx.sorted_symbol_literals
            last_i = len(sym_items) - 1
         %>
         % for i, (sym, name) in enumerate(sym_items):
            ${name}${',' if i < last_i else ''}
            --  ${sym}
         % endfor
      );

      type Symbol_Literal_Array is array (Symbol_Literal_Type) of Symbol_Type;
      type Symbol_Literal_Array_Access is access all Symbol_Literal_Array;
   % endif

   type Analysis_Context_Type is record
      Ref_Count : Natural;
      Units_Map : Units_Maps.Map;
      Symbols   : Symbol_Table;

      Charset : Unbounded_String;
      --  Default charset to use in analysis units

      With_Trivia : Boolean;
      --  Whether Trivia nodes were parsed and included in analysis units

      Root_Scope : Lexical_Env;
      --  The lexical scope that is shared amongst every compilation unit. Used
      --  to resolve cross file references.

      % if ctx.default_unit_provider:
      Unit_Provider : Unit_Provider_Access_Cst;
      --  Object to translate unit names to file names
      % endif

      % if ctx.symbol_literals:
      Symbol_Literals : Symbol_Literal_Array;
      --  List of pre-computed symbols in the Symbols table
      % endif

      Parser : Parser_Type;
      --  Main parser type. TODO: If we want to parse in several tasks, we'll
      --  replace that by an array of parsers.

      Discard_Errors_In_Populate_Lexical_Env : Boolean := True;
      --  See the eponym procedure

      In_Populate_Lexical_Env : Boolean := False;
      --  Flag to tell whether we are running the Populate_Lexical_Env pass.
      --  When it's on, we must not use the memoization map as the hash of
      --  lexical environment changes when their content changes.

      Logic_Resolution_Timeout : Natural := 100_000;
      --  If zero, inefficient. Otherwise, designates the maximal number of
      --  steps allowed in the resolution of logic equations before
      --  interrupting the resolution because of timeout. See the
      --  Set_Logic_Resolution_Timeout procedure.

      Cache_Version : Natural := 0;
      --  Version number used to invalidate memoization caches in a lazy
      --  fashion. If an analysis unit's version number is strictly inferior to
      --  this, its memoization map should be cleared.
   end record;

   type Analysis_Unit_Type is record
      Context : Analysis_Context;
      --  The owning context for this analysis unit

      Ref_Count : Natural;
      --  Ref count for the analysis unit. Note that in the Ada API you'll
      --  still have to call Inc_Ref/Dec_Ref manually.

      AST_Root : ${root_node_type_name};

      File_Name : Unbounded_String;
      --  The originating name for this analysis unit. This should be set even
      --  if the analysis unit was parsed from a buffer.

      Charset : Unbounded_String;
      --  The parsing charset for this analysis unit, as a string. If the
      --  charset used actually came from a byte order mark, this is
      --  nevertheless set to the one the user requested.

      TDH : aliased Token_Data_Handler;
      --  The token data handler that handles all token data during parsing and
      --  owns it afterwards.

      Diagnostics : Diagnostics_Vectors.Vector;
      --  The list of diagnostics produced for this analysis unit

      Is_Env_Populated : Boolean;
      --  Whether Populate_Lexical_Env was called on this unit. Used not to
      --  populate multiple times the same unit and hence avoid infinite
      --  populate recursions for circular dependencies.

      Rule : Grammar_Rule;
      --  The grammar rule used to parse this unit

      AST_Mem_Pool : Bump_Ptr_Pool;
      --  This memory pool shall only be used for AST parsing. Stored here
      --  because it is more convenient, but one shall not allocate from it.

      Destroyables : Destroyable_Vectors.Vector;
      --  Collection of objects to destroy when destroying the analysis unit

      Referenced_Units : Analysis_Unit_Sets.Set;
      --  Units that are referenced from this one. Useful for
      --  visibility/computation of the reference graph.

      Exiled_Entries : Exiled_Entry_Vectors.Vector;
      --  Lexical env population for this unit may have added AST nodes it owns
      --  to the lexical environments that belong to other units ("exiled"
      --  entries). For each of these AST nodes, this vector contains an entry
      --  that records the target environment, the AST node and the
      --  corresponding symbol.

      Foreign_Nodes : ${root_node_type_name}_Vectors.Vector;
      --  This unit owns a set of lexical environments. This vector contains
      --  the list of AST nodes that were added to these environments and that
      --  come from other units.

      Rebindings : aliased Env_Rebindings_Vectors.Vector;
      --  List of rebindings for which Old_Env and/or New_Env belong to this
      --  unit. When this unit gets destroyed or reparsed, these rebindings
      --  need to be destroyed too (see Destroy_Rebindings).

      % if ctx.has_memoization:
         Memoization_Map : Memoization_Maps.Map;
         --  Mapping of arguments tuple to property result for memoization
      % endif

      Cache_Version : Natural := 0;
      --  See the eponym field in Analysis_Context_Type

      Unit_Version : Natural := 0;
      --  Version for this particular unit. This will be incremented every time
      --  a reparse occurs.
   end record;

   procedure Reset_Caches (Context : Analysis_Context);
   --  Call Reset_Caches on all the units that Context contains. Note: this is
   --  is done lazily, just incrementing a version number.

   procedure Reset_Caches (Unit : Analysis_Unit);
   --  Destroy Unit's memoization cache. This resets Unit's version number to
   --  Unit.Context.Cache_Version.

   % if ctx.has_memoization:
      function Lookup_Memoization_Map
        (Unit   : Analysis_Unit;
         Key    : in out Mmz_Key;
         Cursor : out Memoization_Maps.Cursor) return Boolean;
      --  Look for a memoization entry in Unit.Memoization_Map that correspond
      --  to Key, creating one if none is found, and store it in Cursor. If one
      --  was created, return True. Otherwise, destroy Key and return False.
   % endif

   procedure Reference_Unit (From, Referenced : Analysis_Unit);
   --  Set the Referenced unit as being referenced from the From unit. This is
   --  useful for visibility purposes, and is mainly meant to be used in the
   --  env hooks.

   function Is_Referenced_From
     (Referenced, Unit : Analysis_Unit) return Boolean;

end ${ada_lib_name}.Analysis.Implementation;
