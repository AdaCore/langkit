## vim: filetype=makoada

<%namespace name="array_types"       file="array_types_ada.mako" />
<%namespace name="astnode_types"     file="astnode_types_ada.mako" />
<%namespace name="exts"              file="extensions.mako" />
<%namespace name="list_types"        file="list_types_ada.mako" />
<%namespace name="struct_types"      file="struct_types_ada.mako" />
<%namespace name="memoization"       file="memoization_ada.mako" />

<%
   root_node_array = T.root_node.array
   no_builtins = lambda ts: filter(lambda t: not t.is_builtin(), ts)
%>

with Ada.Containers;        use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;

with System;

% if ctx.properties_logging:
   with GNATCOLL.Traces;
% endif
with GNATCOLL.GMP.Integers;
with GNATCOLL.VFS; use GNATCOLL.VFS;

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
with Langkit_Support.Types;       use Langkit_Support.Types;
with Langkit_Support.Vectors;

with ${ada_lib_name}.Parsers;     use ${ada_lib_name}.Parsers;
with ${ada_lib_name}.Lexer;       use ${ada_lib_name}.Lexer;
use ${ada_lib_name}.Lexer.Token_Data_Handlers;
with ${ada_lib_name}.Common;     use ${ada_lib_name}.Common;

${exts.with_clauses(with_clauses)}

--  Internal package: low-level primitives to implement public types and
--  operations in ${ada_lib_name}.Analysis. Plesae don't use this package
--  directly.

private package ${ada_lib_name}.Implementation is

   type Analysis_Context_Type;
   type Internal_Context is access all Analysis_Context_Type;

   type Analysis_Unit_Type;
   type Internal_Unit is access all Analysis_Unit_Type;

   type ${root_node_value_type};
   --  This "by-value" type is public to expose the fact that the various
   --  AST nodes are a hierarchy of tagged types, but it is not intended to be
   --  used directly, hence the "_Type" suffix. Please use instead the
   --  class-wide types such at the one below.

   type ${root_node_type_name} is access all ${root_node_value_type}'Class;
   --  Most generic AST node type

   type Rewriting_Handle_Pointer is new System.Address;
   No_Rewriting_Handle_Pointer : constant Rewriting_Handle_Pointer :=
      Rewriting_Handle_Pointer (System.Null_Address);

   type Abstract_Node_Type is abstract tagged null record;
   type Abstract_Node is access all Abstract_Node_Type'Class;

   function Abstract_Kind
     (Node : access Abstract_Node_Type) return ${root_node_kind_name}
     is abstract;
   --  Return the kind for Node

   function Abstract_Children_Count
     (Node : access Abstract_Node_Type) return Natural is abstract;
   --  Return the number of children that Node has

   function Abstract_Child
     (Node  : access Abstract_Node_Type;
      Index : Positive) return Abstract_Node is abstract;
   --  Return the Node's child number Index. Index is a 1-based index. If it is
   --  out of bounds, a Constraint_Error is raised.

   function Abstract_Text
     (Node : access Abstract_Node_Type) return Text_Type is abstract;
   --  Assuming Node is a token node, return the associated text

   function Abstract_Rewritten_Node
     (Node : access Abstract_Node_Type)
      return ${root_node_type_name} is abstract;
   --  If Node is a rewritten node, return the original node (i.e. of which
   --  Node is a rewritten version). Return null otherwise.

   % if ctx.properties_logging:
      Properties_Traces : constant GNATCOLL.Traces.Trace_Handle :=
         GNATCOLL.Traces.Create
           ("Properties", GNATCOLL.Traces.On, Stream => "&1");
   % endif

   function Is_Null
     (Node : access ${root_node_value_type}'Class) return Boolean;

   function Short_Image
     (Self : access ${root_node_value_type}'Class) return Text_Type;
   --  Return a short representation of the node, containing just the kind
   --  name and the sloc.

   function Is_Token_Node
     (Node : access ${root_node_value_type}'Class) return Boolean;
   ${ada_doc('langkit.node_is_token_node', 3)}

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

   ${struct_types.incomplete_decl(T.env_md)}
   ${struct_types.decl(T.env_md)}

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

   No_Analysis_Unit : constant Internal_Unit := null;

   function Version (Unit : Internal_Unit) return Version_Number;
   --  Return the version for Unit. Version is a number that is incremented
   --  every time Unit changes.

   package AST_Envs is new Langkit_Support.Lexical_Env
     (Unit_T               => Internal_Unit,
      No_Unit              => No_Analysis_Unit,
      Get_Version          => Version,
      Node_Type            => ${root_node_type_name},
      Node_Metadata        => ${T.env_md.name},
      No_Node              => null,
      Empty_Metadata       => No_Metadata,
      Node_Hash            => Named_Hash,
      Metadata_Hash        => Hash,
      Raise_Property_Error => Raise_Property_Error,
      Combine              => Combine,
      Node_Image           => AST_Envs_Element_Image,
      Register_Rebinding   => Register_Rebinding);

   use AST_Envs;
   subtype Internal_Entity is AST_Envs.Entity;
   subtype Internal_Entity_Info is AST_Envs.Entity_Info;

   ${T.entity_info.nullexpr} : constant ${T.entity_info.name} :=
     (No_Metadata, null);
   ${root_entity.nullexpr} : constant ${root_entity.name} :=
     (null, ${T.entity_info.nullexpr});

   function ${root_entity.constructor_name}
     (Node : ${root_node_type_name};
      Info : ${T.entity_info.name}) return ${root_entity.name};

   function Hash_Entity (Self : ${root_entity.name}) return Hash_Type;
   --  Hash function to use in the public API. It's like the regular one, but
   --  disregards metadata.

   function Compare_Entity (Left, Right : ${root_entity.name}) return Boolean;
   --  Equality function to use in the public API. It's like the regular one,
   --  but disregards metadata.

   ## Declare arrays of lexical environments here because we need them for the
   ## Group operation below.
   ${array_types.incomplete_decl(T.LexicalEnvType.array)}
   ${array_types.decl(T.LexicalEnvType.array)}

   ## See ASTNodeType.entity
   ${array_types.incomplete_decl(T.root_node.entity.array)}
   ${array_types.decl(T.root_node.entity.array)}

   ## Declare arrays of root nodes here since some primitives rely on it and
   ## since the declarations require AST_Envs.
   ${array_types.incomplete_decl(root_node_array)}
   ${array_types.decl(root_node_array)}

   ## Generate Hash functions for "built-in types" if need be
   % if T.BoolType.requires_hash_function:
      function Hash (B : Boolean) return Hash_Type;
   % endif

   % if T.LongType.requires_hash_function:
      function Hash (I : Integer) return Hash_Type;
   % endif

   % if T.entity_info.requires_hash_function:
      function Hash (Info : ${T.entity_info.name}) return Hash_Type;
   % endif

   ${struct_types.decl_hash(T.entity)}

   --------------------------
   -- Big integers wrapper --
   --------------------------

   type Big_Integer_Record is limited record
      Value     : GNATCOLL.GMP.Integers.Big_Integer;
      Ref_Count : Integer;
      --  Number of owners. When it drops to 0, this record can be destroyed.
      --  If -1, this is a static big integer: Inc_Ref and Dec_Ref are no-ops.
   end record;

   type Big_Integer_Type is access all Big_Integer_Record;

   function Create_Big_Integer
     (Image : String; Base : Integer := 10) return Big_Integer_Type;
   function Create_Big_Integer
     (Big_Int : GNATCOLL.GMP.Integers.Big_Integer) return Big_Integer_Type;
   function Create_Big_Integer (Int : Integer) return Big_Integer_Type;
   function Create_Public_Big_Integer
     (Big_Int : Big_Integer_Type) return GNATCOLL.GMP.Integers.Big_Integer;

   No_Big_Integer_Record : aliased Big_Integer_Record :=
     (Value => <>, Ref_Count => -1);
   No_Big_Integer : constant Big_Integer_Type := No_Big_Integer_Record'Access;

   function To_Integer (Big_Int : Big_Integer_Type) return Integer;
   --  Convert Big_Int into a regular integer, raising a Property_Error if it
   --  is out of range.

   procedure Inc_Ref (Big_Int : Big_Integer_Type);
   procedure Dec_Ref (Big_Int : in out Big_Integer_Type);

   function Equivalent (Left, Right : Big_Integer_Type) return Boolean;
   function "<" (Left, Right : Big_Integer_Type) return Boolean;
   function "<=" (Left, Right : Big_Integer_Type) return Boolean;
   function ">" (Left, Right : Big_Integer_Type) return Boolean;
   function ">=" (Left, Right : Big_Integer_Type) return Boolean;

   function "+" (Left, Right : Big_Integer_Type) return Big_Integer_Type;
   function "-" (Left, Right : Big_Integer_Type) return Big_Integer_Type;

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

   function Is_Incomplete
     (Node : access ${root_node_value_type}'Class) return Boolean;
   --  Return whether this node is incomplete or not.  Incomplete nodes are a
   --  result of the parsing of a node failing as a result of a NoBacktrack
   --  parser annotation.

   function Kind_Name
     (Node : access ${root_node_value_type}'Class) return String;
   --  Return the concrete kind for Node

   -------------------------------
   -- Tree traversal operations --
   -------------------------------

   Kind_To_Node_Children_Count : array (${root_node_kind_name}) of Integer :=
     (${', \n'.join(
           '{} => {}'.format(
              cls.ada_kind_name,
              (len(cls.get_parse_fields(lambda f: f.type.is_ast_node))
               if not cls.is_list_type else -1)
           )
           for cls in ctx.astnode_types if not cls.abstract)});
   --  For each AST node kind, this array gives the number of AST node children
   --  it has. For AST node lists, this is -1 as this number varies from one
   --  list instance to another.

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
   --  Return the Index'th child of node, storing it into Result.
   --
   --  Child indexing is 1-based. Store in Index_In_Bounds whether Node had
   --  such a child: if not (i.e. Index is out-of-bounds), the content
   --  of Result is undefined.

   function Child
     (Node  : access ${root_node_value_type}'Class;
      Index : Positive) return ${root_node_type_name};
   --  Return the Index'th child of Node, or null if Node has no such child

   function Children
     (Node : access ${root_node_value_type}'Class)
      return ${root_node_array.array_type_name};
   --  Return an array containing all the children of Node.
   --  This is an alternative to the Child/Children_Count pair, useful if you
   --  want the convenience of Ada arrays, and you don't care about the small
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

   function Fetch_Sibling
     (Node   : access ${root_node_value_type}'Class;
      E_Info : ${T.entity_info.name};
      Offset : Integer) return ${root_entity.name};
   --  Assuming Node is the Nth child of its parent, return the (N + Offset)'th
   --  child of the same parent, or No_Entity if there is no such sibling.

   function Traverse
     (Node  : access ${root_node_value_type}'Class;
      Visit : access function (Node : access ${root_node_value_type}'Class)
                               return Visit_Status)
      return Visit_Status;
   --  Given the parent node for a subtree, traverse all syntactic nodes of
   --  this tree, calling the given function on each node in prefix order (i.e.
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

   ----------------------------------------
   -- Source location-related operations --
   ----------------------------------------

   function Sloc_Range
     (Node : access ${root_node_value_type}'Class) return Source_Location_Range;
   --  Return the source location range corresponding to the set of tokens from
   --  which Node was parsed.

   function Compare
     (Node : access ${root_node_value_type}'Class;
      Sloc : Source_Location) return Relative_Position;
   --  Compare Sloc to the sloc range of Node

   function Lookup
     (Node : access ${root_node_value_type}'Class;
      Sloc : Source_Location) return ${root_node_type_name};
   --  Look for the bottom-most AST node whose sloc range contains Sloc. Return
   --  it, or null if no such node was found.

   function Compare
     (Left, Right : access ${root_node_value_type}'Class;
      Relation    : Comparison_Relation) return Boolean;
   --  If Left and Right don't belong to the same analysis units or if one of
   --  them is null, raise a Property_Error. Otherwise, return the comparison
   --  of their starting source location according to Relation.

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
      function Trace_Image (K : Unit_Kind) return String;
      function Trace_Image (B : Boolean) return String;
      function Trace_Image (I : Integer) return String;
      function Trace_Image (S : Symbol_Type) return String;
      function Trace_Image (Env : Lexical_Env) return String;
      function Trace_Image (E : ${root_entity.name}) return String;
      function Trace_Image (Info : ${T.entity_info.name}) return String;
      function Trace_Image (R : Env_Rebindings) return String;
      function Trace_Image (Unit : Internal_Unit) return String;
      function Trace_Image (Eq : Logic_Equation) return String;
      function Trace_Image (Var : Logic_Var) return String;
   % endif

   -----------------------------------------------
   -- Structure types (incomplete declarations) --
   -----------------------------------------------

   % for struct_type in no_builtins(ctx.struct_types):
   ${struct_types.incomplete_decl(struct_type)}
   % endfor

   -------------------------------------------
   -- Array types (incomplete declarations) --
   -------------------------------------------

   % for array_type in ctx.sorted_types(ctx.array_types):
   % if array_type.element_type.should_emit_array_type:
   ${array_types.incomplete_decl(array_type)}
   % endif
   % endfor

   ------------------------------
   -- Root AST node properties --
   ------------------------------

   % for prop in T.root_node.get_properties(include_inherited=False):
      ${prop.prop_decl}
   % endfor

   -----------------------------------------
   -- Structure types (full declarations) --
   -----------------------------------------

   % for struct_type in no_builtins(ctx.struct_types):
   ${struct_types.decl(struct_type)}
   % endfor

   -----------------
   -- Array types --
   -----------------

   --  We implement array types as discriminated records so that binding to C
   --  can be done without copy.

   % for array_type in ctx.sorted_types(ctx.array_types):
   % if array_type.element_type.should_emit_array_type:
   ${array_types.decl(array_type)}
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

   type ${root_node_value_type} is new Abstract_Node_Type with record
      Parent : ${root_node_type_name} := null;
      --  Reference to the parent node, or null if this is the root one

      Unit : Internal_Unit := No_Analysis_Unit;
      --  Reference to the analysis unit that owns this node

      Token_Start_Index : Token_Index  := No_Token_Index;
      Token_End_Index   : Token_Index  := No_Token_Index;
      --  Reference to the start and end token that constitutes this node. If
      --  this node is a ghost, Token_Start_Index is the token that this AST
      --  node relates to and Token_End_Index is No_Token_Index. Otherwise,
      --  both tokens are inclusive, i.e. they both belong to this node.

      Extensions : Extension_Vectors.Vector;
      --  See documentation for the "Extensions" section above

      Self_Env : Lexical_Env;
      --  Hold the environment this node defines, or the parent environment
      --  otherwise.

      Kind : ${root_node_kind_name};
      --  Kind for this node. This must reflect the object tag

      ${astnode_types.node_fields(T.root_node, emit_null=False)}

      Last_Attempted_Child : Integer := -1;
   end record;

   overriding function Abstract_Kind
     (Node : access ${root_node_value_type}) return ${root_node_kind_name};

   overriding function Abstract_Children_Count
     (Node : access ${root_node_value_type}) return Natural;

   overriding function Abstract_Child
     (Node  : access ${root_node_value_type};
      Index : Positive) return Abstract_Node;

   overriding function Abstract_Text
     (Node : access ${root_node_value_type}) return Text_Type;

   overriding function Abstract_Rewritten_Node
     (Node : access ${root_node_value_type}) return ${root_node_type_name};

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

   function Get_Symbol
     (Node : access ${root_node_value_type}'Class) return Symbol_Type
      with Pre => Is_Token_Node (Node);
   --  Assuming Node is a token node, return the corresponding symbol for the
   --  token it contains.

   function Text
     (Node : access ${root_node_value_type}'Class) return Text_Type;
   --  Retun the fragment of text from which Node was parsed

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
   --  This is an alternative to the Child/Children_Count pair, useful if you
   --  want the convenience of ada arrays, and you don't care about the small
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

   function Get (A : AST_Envs.Entity_Array; Index : Integer) return Entity;
   --  Simple getter that raises Property_Error on out-of-bound accesses.
   --  Useful for code generation.

   function Group
     (Envs   : ${T.LexicalEnvType.array.name};
      Env_Md : ${T.env_md.name} := No_Metadata) return ${T.LexicalEnvType.name};
   --  Convenience wrapper for uniform types handling in code generation

   procedure Free_Extensions (Node : access ${root_node_value_type}'Class);
   --  Implementation helper to free the extensions associatde to Node

   package ${T.root_node.array.pkg_vector} is
      new Langkit_Support.Vectors (${T.root_node.name});

   function Is_Visible_From
     (Referenced_Env, Base_Env : Lexical_Env) return Boolean;
   --  Return whether the unit that Referenced_Env belongs to is visible from
   --  the unit that Base_Env belongs to. If at least one of these two lexical
   --  environments does not belong to a particular analysis unit, this raises
   --  a Property_Error.

   function Populate_Lexical_Env
     (Node : access ${root_node_value_type}'Class) return Boolean;
   --  Populate the lexical environment for node and all its children. Return
   --  whether a Property_Error error occurred in the process.

   -----------------------------------
   -- Lexical utilities (internals) --
   -----------------------------------

   function Token
     (Node  : access ${root_node_value_type}'Class;
      Index : Token_Index) return Token_Reference;
   --  Helper for properties. This is used to turn token indexes as stored in
   --  AST nodes into Token_Reference values.

   function Stored_Token
     (Node  : access ${root_node_value_type}'Class;
      Token : Token_Reference) return Token_Index;
   --  Helper for properties. This is used to turn a Token_Reference value into
   --  a Token_Index value that can be stored as a field in Node. This raises a
   --  Property_Error if Node and Token don't belong to the same analysis unit
   --  or if Token is actually a Trivia.

   type Bare_Child_Record (Kind : Child_Or_Trivia := Child) is record
      case Kind is
         when Child =>
            Node : ${root_node_type_name};
         when Trivia =>
            Trivia : Token_Reference;
      end case;
   end record;
   --  Variant that holds either an node or a token

   type Bare_Children_Array is array (Positive range <>) of Bare_Child_Record;

   function Children_With_Trivia
     (Node : access ${root_node_value_type}'Class) return Bare_Children_Array;
   --  Implementation for Analysis.Children_With_Trivia

   function Is_Synthetic
     (Node : access ${root_node_value_type}'Class) return Boolean;
   --  Returns whether the node is a synthetic node, i.e. whether it was
   --  generated for semantic analysis instead of parsing.

   % for astnode in no_builtins(ctx.astnode_types):
     % if astnode.is_root_list_type:
       ${list_types.private_decl(astnode.element_type)}
     % else:
       ${astnode_types.private_decl(astnode)}
     % endif
   % endfor

   function "<" (Left, Right : Internal_Unit) return Boolean;

   type Exiled_Entry is record
      Env  : Lexical_Env;
      Key  : Symbol_Type;
      Node : ${root_node_type_name};
   end record;
   --  Tuple of values passed to AST_Envs.Add. Used in the lexical
   --  environment rerooting machinery: see Remove_Exiled_Entries and
   --  Reroot_Foreign_Nodes.

   package Exiled_Entry_Vectors is new Langkit_Support.Vectors (Exiled_Entry);

   type Foreign_Node_Entry is record
      Node : ${root_node_type_name};
      --  The foreign node that has been added to an analysis unit's lexical
      --  environment.

      Unit : Internal_Unit;
      --  Analysis unit that owns Node
   end record;

   package Foreign_Node_Entry_Vectors is new Langkit_Support.Vectors
     (Foreign_Node_Entry);

   procedure Register_Destroyable
     (Unit : Internal_Unit; Node : ${root_node_type_name});
   --  Helper for synthetized nodes. We cannot use the generic
   --  Register_Destroyable because the root AST node is an abstract types, so
   --  this is implemented using the untyped (using System.Address)
   --  implementation helper.

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
     (Internal_Unit, null);

   package Units_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => GNATCOLL.VFS.Virtual_File,
      Element_Type    => Internal_Unit,
      Hash            => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Keys => GNATCOLL.VFS."=");

   type Symbol_Literal_Type is
      % if ctx.symbol_literals:
         (
            <%
               sym_items = ctx.sorted_symbol_literals
               last_i = len(sym_items) - 1
            %>
            % for i, (sym, name) in enumerate(sym_items):
               ${name}${',' if i < last_i else ''}
               --  ${sym}
            % endfor
         )
      % else:
         new Integer range 1 .. 0
      % endif
   ;

   type Symbol_Literal_Array is array (Symbol_Literal_Type) of Symbol_Type;
   type Symbol_Literal_Array_Access is access all Symbol_Literal_Array;

   function Token_Data (Unit : Internal_Unit) return Token_Data_Handler_Access;

   function Lookup_Symbol
     (Context : Internal_Context; Symbol : Text_Type) return Symbol_Type;
   --  Return the given symbol text as a symbol for this context. Raise an
   --  Invalid_Symbol_Error if it is invalid.

   function Create_Special_Unit
     (Context             : Internal_Context;
      Normalized_Filename : GNATCOLL.VFS.Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Internal_Unit;
   --  Create a new special analysis unit, i.e. a unit that is not registered
   --  in Context's unit map.

   function Templates_Unit (Context : Internal_Context) return Internal_Unit;
   --  Return the analysis unit to be used to parse tree rewriting templates.
   --  This creates it if it does not exists yet.

   procedure Set_Rule (Unit : Internal_Unit; Rule : Grammar_Rule);

   package Virtual_File_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => GNATCOLL.VFS.Virtual_File,
      Equivalent_Keys => "=",
      "="             => GNATCOLL.VFS."=",
      Hash            => Ada.Strings.Unbounded.Hash);

   function Normalized_Unit_Filename
     (Context : Internal_Context; Filename : String)
      return GNATCOLL.VFS.Virtual_File;
   --  Try to return a canonical filename. This is used to have an
   --  as-unique-as-possible analysis unit identifier.

   type Internal_Unit_Provider is limited interface;
   type Internal_Unit_Provider_Access is
      access all Internal_Unit_Provider'Class;

   function Get_Unit_Filename
     (Provider : Internal_Unit_Provider;
      Name     : Text_Type;
      Kind     : Unit_Kind) return String is abstract;
   ${ada_doc('langkit.unit_provider_get_unit_filename', 3)}

   function Get_Unit
     (Provider    : Internal_Unit_Provider;
      Context     : Internal_Context;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Internal_Unit is abstract;
   ${ada_doc('langkit.unit_provider_get_unit_from_name', 3)}

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Internal_Unit_Provider'Class, Internal_Unit_Provider_Access);

   type Analysis_Context_Type is limited record
      --  Start of ABI area. In order to perform fast checks from foreign
      --  languages, we maintain minimal ABI for analysis context: this allows
      --  us in language bindings to directly peek in this record rather than
      --  rely on (slow) calls to getters.

      Serial_Number : Version_Number;
      --  Serial number that is incremented each time this context allocation
      --  is re-used.

      --  End of ABI area

      Ref_Count : Natural;

      Units : Units_Maps.Map;
      --  Collection of analysis units loaded in this context

      Filenames : Virtual_File_Maps.Map;
      --  Cache for GNATCOLL.VFS.Virtual_File we create for String filenames.
      --  Re-using older Virtual_File values is useful as this reduces the need
      --  to normalize paths, which is a costly operation.

      Symbols : Symbol_Table;
      --  Symbol table used in this whole context

      Charset : Unbounded_String;
      --  Default charset to use in analysis units

      With_Trivia : Boolean;
      --  Whether Trivia nodes were parsed and included in analysis units

      Root_Scope : Lexical_Env;
      --  The lexical scope that is shared amongst every compilation unit. Used
      --  to resolve cross file references.

      Unit_Provider : Internal_Unit_Provider_Access;
      --  Object to translate unit names to file names

      Symbol_Literals : aliased Symbol_Literal_Array;
      --  List of pre-computed symbols in the Symbols table

      Parser : Parser_Type;
      --  Main parser type. TODO: If we want to parse in several tasks, we'll
      --  replace that by an array of parsers.

      Discard_Errors_In_Populate_Lexical_Env : Boolean;
      --  See the eponym procedure

      In_Populate_Lexical_Env : Boolean;
      --  Flag to tell whether we are running the Populate_Lexical_Env pass.
      --  When it's on, we must not use the memoization map as the hash of
      --  lexical environment changes when their content changes.

      Logic_Resolution_Timeout : Natural;
      --  If zero, inefficient. Otherwise, designates the maximal number of
      --  steps allowed in the resolution of logic equations before
      --  interrupting the resolution because of timeout. See the
      --  Set_Logic_Resolution_Timeout procedure.

      Cache_Version : Natural;
      --  Version number used to invalidate memoization caches in a lazy
      --  fashion. If an analysis unit's version number is strictly inferior to
      --  this, its memoization map should be cleared.

      Rewriting_Handle : Rewriting_Handle_Pointer :=
         No_Rewriting_Handle_Pointer;
      --  Rewriting handle for this context's current rewriting session.
      --  No_Rewriting_Handle_Pointer if there is no such session currently.

      Templates_Unit : Internal_Unit := No_Analysis_Unit;
      --  Special analysis unit used only as a containing unit to parse
      --  templates in the context of tree rewriting.

      Released : Boolean;
      --  Whether this context has been released and thus is available in
      --  Context_Pool.
   end record;

   type Analysis_Unit_Type is limited record
      --  Start of ABI area. In order to perform fast checks from foreign
      --  languages, we maintain minimal ABI for analysis context: this allows
      --  us in language bindings to directly peek in this record rather than
      --  rely on (slow) calls to getters.

      Unit_Version : Version_Number := 0;
      --  Version for this particular unit. This will be incremented every time
      --  a reparse occurs.

      --  End of ABI area

      Context : Internal_Context;
      --  The owning context for this analysis unit

      AST_Root : ${root_node_type_name};

      Filename : GNATCOLL.VFS.Virtual_File;
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

      Foreign_Nodes : Foreign_Node_Entry_Vectors.Vector;
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
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Context_Type, Internal_Context);

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Unit_Type, Internal_Unit);

   type Reparsed_Unit is record
      TDH          : Token_Data_Handler;
      Diagnostics  : Diagnostics_Vectors.Vector;
      AST_Mem_Pool : Bump_Ptr_Pool;
      AST_Root     : ${root_node_type_name};
   end record;
   --  Holder for fields affected by an analysis unit reparse. This makes it
   --  possible to separate the "reparsing" and the "replace" steps.

   procedure Destroy (Reparsed : in out Reparsed_Unit);
   --  Free all resources in Reparsed

   function Basename (Filename : String) return String;
   --  Return the base filename for String

   ----------------------------------------------------
   -- Implementation for analysis context primitives --
   ----------------------------------------------------

   function Create_Context
     (Charset       : String;
      With_Trivia   : Boolean;
      Unit_Provider : Internal_Unit_Provider_Access) return Internal_Context;
   --  Implementation for Analysis.Create_Context

   function Create_Unit
     (Context             : Internal_Context;
      Normalized_Filename : GNATCOLL.VFS.Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Internal_Unit
      with Pre => not Has_Unit (Context, +Normalized_Filename.Full_Name);
   --  Create a new analysis unit and register it in Context

   function Get_Unit
     (Context           : Internal_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Input             : Lexer_Input;
      Rule              : Grammar_Rule) return Internal_Unit;
   --  Helper for Get_From_File and Get_From_Buffer. Return the resulting
   --  analysis unit.

   function Has_Unit
     (Context       : Internal_Context;
      Unit_Filename : String) return Boolean;
   --  Implementation for Analysis.Has_Unit

   function Get_From_File
     (Context  : Internal_Context;
      Filename : String;
      Charset  : String;
      Reparse  : Boolean;
      Rule     : Grammar_Rule) return Internal_Unit
      with Pre => not Reparse or else not Has_Rewriting_Handle (Context);
   --  Implementation for Analysis.Get_From_File

   function Get_From_Buffer
     (Context  : Internal_Context;
      Filename : String;
      Charset  : String;
      Buffer   : String;
      Rule     : Grammar_Rule) return Internal_Unit
      with Pre => not Has_Rewriting_Handle (Context);
   --  Implementation for Analysis.Get_From_Buffer

   function Get_With_Error
     (Context  : Internal_Context;
      Filename : String;
      Error    : String;
      Charset  : String;
      Rule     : Grammar_Rule) return Internal_Unit;
   --  Implementation for Analysis.Get_With_Error

   % if ctx.default_unit_provider:

   function Get_From_Provider
     (Context : Internal_Context;
      Name    : Text_Type;
      Kind    : Unit_Kind;
      Charset : String;
      Reparse : Boolean) return Internal_Unit
      with Pre => not Reparse or else not Has_Rewriting_Handle (Context);
   --  Implementation for Analysis.Get_From_Provider

   % endif

   function Unit_Provider
     (Context : Internal_Context) return Internal_Unit_Provider_Access;
   --  Implementation for Analysis.Unit_Provider

   function Hash (Context : Internal_Context) return Hash_Type;
   --  Implementation for Analysis.Hash

   function Has_With_Trivia (Context : Internal_Context) return Boolean;
   --  Implementation for Analysis.Has_With_Trivia

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Internal_Context; Discard : Boolean);
   --  Implementation for Analysis.Discard_Errors_In_Populate_Lexical_Env

   procedure Set_Logic_Resolution_Timeout
     (Context : Internal_Context; Timeout : Natural);
   --  Implementation for Analysis.Set_Logic_Resolution_Timeout

   function Has_Rewriting_Handle (Context : Internal_Context) return Boolean;
   --  Implementation for Analysis.Has_Rewriting_Handle

   procedure Inc_Ref (Context : Internal_Context);
   --  Increment the ref-count of Context. This does nothing if Context is
   --  null.

   procedure Dec_Ref (Context : in out Internal_Context);
   --  Decrement the ref-count of Context, destroying it if the ref-count
   --  reaches zero. This does nothing if Context is null.

   procedure Destroy (Context : in out Internal_Context)
      with Pre => not Has_Rewriting_Handle (Context);
   --  Free all resources allocated for Context

   -------------------------------------------------
   -- Implementation for analysis unit primitives --
   -------------------------------------------------

   function Context (Unit : Internal_Unit) return Internal_Context;
   --  Implementation for Analysis.Context

   function Hash (Unit : Internal_Unit) return Hash_Type;
   --  Implementation for Analysis.Hash

   procedure Reparse (Unit : Internal_Unit; Charset : String);
   --  Implementation for Analysis.Reparse

   procedure Reparse
     (Unit : Internal_Unit; Charset : String; Buffer  : String);
   --  Implementation for Analysis.Reparse

   procedure Populate_Lexical_Env (Unit : Internal_Unit);
   --  Implementation for Analysis.Populate_Lexical_Env

   function Get_Filename (Unit : Internal_Unit) return String;
   --  Implementation for Analysis.Get_Filename

   function Get_Charset (Unit : Internal_Unit) return String;
   --  Implementation for Analysis.Get_Charset

   function Has_Diagnostics (Unit : Internal_Unit) return Boolean;
   --  Implementation for Analysis.Has_Diagnostics

   function Diagnostics (Unit : Internal_Unit) return Diagnostics_Array;
   --  Implementation for Analysis.Diagnostics

   function Format_GNU_Diagnostic
     (Unit : Internal_Unit; D : Diagnostic) return String;
   --  Implementation for Analysis.Format_GNU_Diagnostic

   function Root (Unit : Internal_Unit) return ${root_node_type_name};
   --  Implementation for Analysis.Root

   function First_Token (Unit : Internal_Unit) return Token_Reference;
   --  Implementation for Analysis.First_Token

   function Last_Token (Unit : Internal_Unit) return Token_Reference;
   --  Implementation for Analysis.Last_Token

   function Token_Count (Unit : Internal_Unit) return Natural;
   --  Implementation for Analysis.Token_Count

   function Trivia_Count (Unit : Internal_Unit) return Natural;
   --  Implementation for Analysis.Trivia_Count

   function Text (Unit : Internal_Unit) return Text_Type;
   --  Implementation for Analysis.Text

   function Lookup_Token
     (Unit : Internal_Unit; Sloc : Source_Location) return Token_Reference;
   --  Implementation for Analysis.Lookup_Token

   procedure Dump_Lexical_Env (Unit : Internal_Unit);
   --  Implementation for Analysis.Dump_Lexical_Env

   procedure Print (Unit : Internal_Unit; Show_Slocs : Boolean);
   --  Implementation for Analysis.Print

   procedure PP_Trivia (Unit : Internal_Unit);
   --  Implementation for Analysis.PP_Trivia

   procedure Destroy (Unit : in out Internal_Unit);
   --  TODO???

   function Basename (Unit : Internal_Unit) return String;
   --  Return the base filename for Unit

   procedure Invalidate_Caches (Context : Internal_Context);
   --  Invalidate all caches (memoization and envs)

   procedure Reset_Caches (Unit : Internal_Unit);
   --  Destroy Unit's memoization cache. This resets Unit's version number to
   --  Unit.Context.Cache_Version.

   % if ctx.has_memoization:
      function Lookup_Memoization_Map
        (Unit   : Internal_Unit;
         Key    : in out Mmz_Key;
         Cursor : out Memoization_Maps.Cursor) return Boolean;
      --  Look for a memoization entry in Unit.Memoization_Map that correspond
      --  to Key, creating one if none is found, and store it in Cursor. If one
      --  was created, return True. Otherwise, destroy Key and return False.
   % endif

   procedure Reference_Unit (From, Referenced : Internal_Unit);
   --  Set the Referenced unit as being referenced from the From unit. This is
   --  useful for visibility purposes, and is mainly meant to be used in the
   --  env hooks.

   function Is_Referenced_From
     (Referenced, Unit : Internal_Unit) return Boolean;

   procedure Do_Parsing
     (Unit : Internal_Unit; Input : Lexer_Input; Result : out Reparsed_Unit);
   --  Parse text for Unit using Input and store the result in Result. This
   --  leaves Unit unchanged.

   procedure Update_After_Reparse
     (Unit : Internal_Unit; Reparsed : in out Reparsed_Unit);
   --  Update Unit's AST from Reparsed and update stale lexical environment
   --  data after the reparsing of Unit.

   procedure Destroy_Unit_Destroyables (Unit : Internal_Unit);
   --  Destroy all destroyables objects in Unit and clear this list in Unit

   procedure Remove_Exiled_Entries (Unit : Internal_Unit);
   --  Remove lexical environment entries that reference some of Unit's nodes,
   --  in lexical environments it does not own.

   procedure Extract_Foreign_Nodes
     (Unit          : Internal_Unit;
      Foreign_Nodes : in out ${root_node_type_name}_Vectors.Vector);
   --  Collect from Unit all the foreign nodes that belong to an analysis unit
   --  which is not in the populate lexical env queue, appending them to
   --  Foreign_Nodes. Clear Unit.Foreign_Nodes afterwards.

   procedure Reroot_Foreign_Node (Node : access ${root_node_value_type}'Class);
   --  Re-create the lexical env entry for Node. This is to be used in
   --  Flush_Populate_Lexical_Env_Queue, after reparsing removed the target
   --  lexical environment.

   procedure Destroy_Rebindings
     (Rebindings : access Env_Rebindings_Vectors.Vector);
   --  Destroy all rebindings in Rebindings, plus their child rebindings. Note
   --  that children can belong to various analysis units, so this takes care
   --  of removing the destroyed rebindings from each concerned analysis unit's
   --  Rebindings vector.
   --
   --  This require an access parameter in order to avoid aliasing issues in
   --  the body.

   function Get_Rewriting_Handle
     (Context : Internal_Context) return Rewriting_Handle_Pointer;
   --  Return the Rewriting_Handle component of Context

   procedure Set_Rewriting_Handle
     (Context : Internal_Context; Handle : Rewriting_Handle_Pointer);
   --  Set the Rewriting_Handle component of Context

   type Node_Safety_Net is record
      Context        : Internal_Context;
      Context_Serial : Version_Number;
      --  Analysis context and serial number at the time this safety net was
      --  produced.

      Unit         : Internal_Unit;
      Unit_Version : Version_Number;
      --  Analysis unit and unit version at the time this safety net was
      --  produced.
   end record;
   --  Information to embed in public APIs, used to check before accessing data
   --  that the said-data is still valid.

   No_Node_Safety_Net : constant Node_Safety_Net := (null, 0, null, 0);

   procedure Check_Safety_Net (Self : Node_Safety_Net);
   --  Check that Self's node is still valid, raising a Stale_Reference_Error
   --  if it is not.

end ${ada_lib_name}.Implementation;
