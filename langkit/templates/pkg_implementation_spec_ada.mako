## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="exts"          file="extensions.mako" />
<%namespace name="struct_types"  file="struct_types_ada.mako" />
<%namespace name="memoization"   file="memoization_ada.mako" />

<% root_node_array = T.root_node.array %>

with Ada.Containers;                  use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Conversion;
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
with Langkit_Support.Lexical_Env;
with Langkit_Support.Types;       use Langkit_Support.Types;
with Langkit_Support.Vectors;

with ${ada_lib_name}.Parsers; use ${ada_lib_name}.Parsers;
with ${ada_lib_name}.Common;  use ${ada_lib_name}.Common;
use ${ada_lib_name}.Common.Symbols;
use ${ada_lib_name}.Common.Token_Data_Handlers;
with ${ada_lib_name}.Lexer_Implementation;
use ${ada_lib_name}.Lexer_Implementation;

${exts.with_clauses(with_clauses)}

--  Internal package: low-level primitives to implement public types and
--  operations in ${ada_lib_name}.Analysis.

private package ${ada_lib_name}.Implementation is

   use Support.Diagnostics, Support.Slocs, Support.Text;

   type Analysis_Context_Type;
   type Internal_Context is access all Analysis_Context_Type;

   Unexpected_Call_Depth : exception;
   --  Raised when the Call_Depth for two matching calls to Enter_Call and
   --  Exit_Call don't match, i.e. when there is a bug in the counting of
   --  recursive calls.

   procedure Enter_Call
     (Context : Internal_Context; Call_Depth : access Natural);
   --  Increment the call depth in Context. If the depth exceeds Context's
   --  maximum, raise a Property_Error for "stack overflow".
   --
   --  Note that in the case of an exception, the depth is still incremented.
   --  This means that all calls to Enter_Call must be wrapped in an exception
   --  handler which calls Exit_Call on exception.
   --
   --  Put in Call_Depth the incremented call depth.

   procedure Exit_Call (Context : Internal_Context; Call_Depth : Natural);
   --  Decrement the call depth in Context. If Call_Depth does not match the
   --  current call depth, raise an Unexpected_Call_Depth.

   type Analysis_Unit_Type;
   type Internal_Unit is access all Analysis_Unit_Type;

   type ${T.root_node.value_type_name};
   type ${T.root_node.name} is access all ${T.root_node.value_type_name};
   ${T.root_node.null_constant} : constant ${T.root_node.name} := null;
   --  Most generic AST node type

   function "<" (Left, Right : ${T.root_node.name}) return Boolean;
   --  Abritrary but deterministic ordering criteria for parsing nodes. This
   --  handles null nodes as well. Raise a Property_Error for synthetic nodes.

   function Is_Null (Node : ${T.root_node.name}) return Boolean;
   function Kind (Node : ${T.root_node.name}) return ${T.node_kind};

   % for node in ctx.astnode_types:
      % if not node.is_root_node:
         subtype ${node.name} is ${T.root_node.name}
            with Dynamic_Predicate =>
               Is_Null (${node.name})
               or else Kind (${node.name}) in ${node.ada_kind_range_name};
      % endif
   % endfor

   package Alloc_AST_List_Array is new Langkit_Support.Bump_Ptr.Array_Alloc
     (Element_T  => ${T.root_node.name},
      Index_Type => Positive);
   --  Allocator for array of nodes, used in list nodes

   type Rewriting_Handle_Pointer is new System.Address;
   No_Rewriting_Handle_Pointer : constant Rewriting_Handle_Pointer :=
      Rewriting_Handle_Pointer (System.Null_Address);

   % if ctx.properties_logging:
      Properties_Traces : constant GNATCOLL.Traces.Trace_Handle :=
         GNATCOLL.Traces.Create
           ("LANGKIT.PROPERTIES", GNATCOLL.Traces.On
           % if ctx.show_property_logging:
           , Stream => "&1"
           % endif
           );
   % endif

   function Short_Text_Image (Self : ${T.root_node.name}) return Text_Type;
   --  Return a short representation of the node, containing just the kind
   --  name and the sloc, or "None" if Self is null.

   function Is_Token_Node (Node : ${T.root_node.name}) return Boolean;
   ${ada_doc('langkit.node_is_token_node', 3)}

   function Is_Synthetic (Node : ${T.root_node.name}) return Boolean;
   ${ada_doc('langkit.node_is_synthetic', 3)}

   ---------------------------
   -- Environments handling --
   ---------------------------

   ${struct_types.incomplete_decl(T.env_md)}
   ${struct_types.decl(T.env_md, incomplete_nullexpr=False)}
   ${struct_types.nullexpr_decl(T.env_md)}

   function Combine
     (L, R : ${T.env_md.name}) return ${T.env_md.name};
   --  The combine function on environments metadata does a boolean Or on every
   --  boolean component of the env metadata.

   function Can_Reach (El, From : ${T.root_node.name}) return Boolean;
   --  Return whether El can reach From, from a sequential viewpoint. If
   --  elements are declared in different units, it will always return True,
   --  eg this does not handle general visibility issues, just sequentiality of
   --  declarations.

   function AST_Envs_Node_Text_Image
     (Node  : ${T.root_node.name};
      Short : Boolean := True) return Text_Type;
   --  Return a "sourcefile:lineno:columnno" corresponding to the starting sloc
   --  of Node. Used to create a human-readable representation for env.
   --  rebindings.

   function Is_Rebindable (Node : ${T.root_node.name}) return Boolean;

   procedure Register_Rebinding
     (Node : ${T.root_node.name}; Rebinding : System.Address);
   --  Register a rebinding to be destroyed when Node's analysis unit is
   --  destroyed or reparsed.
   --
   --  TODO??? For now the rebinding must be represented as an untyped pointer
   --  because we probably need some big refactoring to provide to
   --  Langkit_Support.Lexical_Env a procedure that has visibility on both
   --  Env_Rebindings and on the analysis unit record.

   function Element_Parent
     (Node : ${T.root_node.name}) return ${T.root_node.name};

   function Hash (Node : ${T.root_node.name}) return Hash_Type;
   function Node_Unit (Node : ${T.root_node.name}) return Internal_Unit;
   function Named_Hash (Node : ${T.root_node.name}) return Hash_Type is
     (Hash (Node));

   No_Analysis_Unit : constant Internal_Unit := null;

   function Unit_Version (Unit : Internal_Unit) return Version_Number;
   --  Return the version for Unit. Version is a number that is incremented
   --  every time Unit changes.

   function Context_Version (Unit : Internal_Unit) return Integer;
   --  Return the version of the analysis context associated with Unit

   type Ref_Category is
     (${", ".join(sorted(str(cat) for cat in ctx.ref_cats))});
   type Ref_Categories is array (Ref_Category) of Boolean;
   pragma Pack (Ref_Categories);

   package AST_Envs is new Langkit_Support.Lexical_Env
     (Precomputed_Symbol_Index => Precomputed_Symbol_Index,
      Precomputed_Symbol       => Precomputed_Symbol,
      Symbols                  => Symbols,
      Unit_T                   => Internal_Unit,
      No_Unit                  => No_Analysis_Unit,
      Get_Unit_Version         => Unit_Version,
      Get_Context_Version      => Context_Version,
      Node_Type                => ${T.root_node.name},
      Node_Metadata            => ${T.env_md.name},
      No_Node                  => null,
      Empty_Metadata           => No_Metadata,
      Node_Unit                => Node_Unit,
      Node_Hash                => Named_Hash,
      Metadata_Hash            => Hash,
      Combine                  => Combine,
      Node_Text_Image          => AST_Envs_Node_Text_Image,
      Register_Rebinding       => Register_Rebinding,
      Ref_Category             => Ref_Category,
      Ref_Categories           => Ref_Categories);

   use AST_Envs;
   subtype Internal_Entity is AST_Envs.Entity;
   subtype Internal_Entity_Info is AST_Envs.Entity_Info;

   ${T.entity_info.nullexpr} : constant ${T.entity_info.name} :=
     (No_Metadata, null, False);
   ${root_entity.nullexpr} : constant ${root_entity.name} :=
     (null, ${T.entity_info.nullexpr});

   function ${root_entity.constructor_name}
     (Node : ${T.root_node.name};
      Info : ${T.entity_info.name}) return ${root_entity.name};

   function Hash_Entity (Self : ${root_entity.name}) return Hash_Type;
   --  Hash function to use in the public API. It's like the regular one, but
   --  disregards metadata.

   function Compare_Entity (Left, Right : ${root_entity.name}) return Boolean;
   --  Equality function to use in the public API. It's like the regular one,
   --  but disregards metadata.

   ## Declare arrays of lexical environments here because we need them for the
   ## Group operation below.
   ${array_types.incomplete_decl(T.LexicalEnv.array)}
   ${array_types.decl(T.LexicalEnv.array)}

   ## See ASTNodeType.entity
   ${array_types.incomplete_decl(T.root_node.entity.array)}
   ${array_types.decl(T.root_node.entity.array)}

   ## Declare arrays of root nodes here since some primitives rely on it and
   ## since the declarations require AST_Envs.
   ${array_types.incomplete_decl(root_node_array)}
   ${array_types.decl(root_node_array)}

   ## Generate Hash functions for "built-in types" if need be
   % if T.Bool.requires_hash_function:
      function Hash (B : Boolean) return Hash_Type;
   % endif

   % if T.Int.requires_hash_function:
      function Hash (I : Integer) return Hash_Type;
   % endif

   % if T.Character.requires_hash_function:
      function Hash (I : Character_Type) return Hash_Type;
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

   % if ctx.properties_logging:
   function Trace_Image (I : Big_Integer_Type) return String;
   % endif

   % if ctx.properties_logging:
      function Trace_Image
        (Node       : ${T.root_node.name};
         Decoration : Boolean := True) return String;
   % endif

   function Is_Incomplete (Node : ${T.root_node.name}) return Boolean;
   --  Return whether this node is incomplete or not.  Incomplete nodes are a
   --  result of the parsing of a node failing as a result of a NoBacktrack
   --  parser annotation.

   function Kind_Name (Node : ${T.root_node.name}) return String;
   --  Return the concrete kind for Node

   -------------------------------
   -- Tree traversal operations --
   -------------------------------

   Kind_To_Node_Children_Count : constant array (${T.node_kind}) of Integer :=
     (${', \n'.join(
           '{} => {}'.format(
              cls.ada_kind_name,
              (len(cls.get_parse_fields(lambda f: not f.null))
               if not cls.is_list_type else -1)
           )
           for cls in ctx.astnode_types if not cls.abstract)});
   --  For each AST node kind, this array gives the number of AST node children
   --  it has. For AST node lists, this is -1 as this number varies from one
   --  list instance to another.

   function First_Child_Index (Node : ${T.root_node.name}) return Natural;
   --  Return the index of the first child Node has

   function Last_Child_Index (Node : ${T.root_node.name}) return Natural;
   --  Return the index of the last child Node has, or 0 if there is no child

   function Children_Count (Node : ${T.root_node.name}) return Natural;
   --  Return the number of children that Node has

   procedure Get_Child
     (Node            : ${T.root_node.name};
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out ${T.root_node.name});
   --  Return the Index'th child of node, storing it into Result.
   --
   --  Child indexing is 1-based. Store in Index_In_Bounds whether Node had
   --  such a child: if not (i.e. Index is out-of-bounds), the content
   --  of Result is undefined.

   function Child
     (Node  : ${T.root_node.name};
      Index : Positive) return ${T.root_node.name};
   --  Return the Index'th child of Node, or null if Node has no such child

   function Children
     (Node : ${T.root_node.name}) return ${root_node_array.array_type_name};
   --  Return an array containing all the children of Node.
   --  This is an alternative to the Child/Children_Count pair, useful if you
   --  want the convenience of Ada arrays, and you don't care about the small
   --  performance hit of creating an array.

   function Parents
     (Node         : ${T.root_node.name};
      Include_Self : Boolean := True)
      return ${root_node_array.name};
   --  Return the list of parents for this node. This node included in the list
   --  iff Include_Self.

   function Parent (Node : ${T.root_node.name}) return ${T.root_node.name};

   function Fetch_Sibling
     (Node   : ${T.root_node.name};
      E_Info : ${T.entity_info.name};
      Offset : Integer) return ${root_entity.name};
   --  Assuming Node is the Nth child of its parent, return the (N + Offset)'th
   --  child of the same parent, or No_Entity if there is no such sibling.

   function Traverse
     (Node  : ${T.root_node.name};
      Visit : access function (Node : ${T.root_node.name}) return Visit_Status)
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
     (Node  : ${T.root_node.name};
      Visit : access function (Node : ${T.root_node.name})
                               return Visit_Status);
   --  This is the same as Traverse function except that no result is returned
   --  i.e. the Traverse function is called and the result is simply discarded.

   generic
      type Data_Type is private;
      Reset_After_Traversal : Boolean := False;
   function Traverse_With_Data
     (Node  : ${T.root_node.name};
      Visit : access function (Node : ${T.root_node.name};
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

   ----------------------------------------
   -- Source location-related operations --
   ----------------------------------------

   function Sloc_Range
     (Node : ${T.root_node.name}) return Source_Location_Range;
   --  Return the source location range corresponding to the set of tokens from
   --  which Node was parsed.

   function Compare
     (Node : ${T.root_node.name};
      Sloc : Source_Location) return Relative_Position;
   --  Compare Sloc to the sloc range of Node

   function Lookup
     (Node : ${T.root_node.name};
      Sloc : Source_Location) return ${T.root_node.name};
   --  Look for the bottom-most AST node whose sloc range contains Sloc. Return
   --  it, or null if no such node was found.

   function Compare
     (Left, Right : ${T.root_node.name};
      Relation    : Comparison_Relation) return Boolean;
   --  If Left and Right don't belong to the same analysis units or if one of
   --  them is null, raise a Property_Error. Otherwise, return the comparison
   --  of their starting source location according to Relation.

   -------------------
   -- Debug helpers --
   -------------------

   procedure Print
     (Node        : ${T.root_node.name};
      Show_Slocs  : Boolean;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children.
   --  Line_Prefix is prepended to each output line.

   procedure PP_Trivia
     (Node        : ${T.root_node.name};
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children along
   --  with the trivia associated to them. Line_Prefix is prepended to each
   --  output line.

   procedure Assign_Names_To_Logic_Vars (Node : ${T.root_node.name});
   --  Debug helper: Assign names to every logical variable in the root node,
   --  so that we can trace logical variables.

   ---------------------------
   -- Adalog instantiations --
   ---------------------------

   function Text_Image (Ent : ${T.entity.name}) return Text_Type;
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
      function Trace_Image (K : Analysis_Unit_Kind) return String;
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

   % for array_type in ctx.array_types:
   % if array_type.element_type.should_emit_array_type:
   ${array_types.incomplete_decl(array_type)}
   % endif
   % endfor

   ------------------------------
   -- Root AST node properties --
   ------------------------------

   % for prop in T.root_node.get_properties(include_inherited=False):
      % if not prop.user_external:
         ${prop.prop_decl}
      % endif
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

   % for array_type in ctx.array_types:
   % if array_type.element_type.should_emit_array_type:
   ${array_types.decl(array_type)}
   % endif
   % endfor

   -------------------------------
   -- Root AST node (internals) --
   -------------------------------

   type ${T.root_node.value_type_name} (Kind : ${T.node_kind}) is record
      Parent : ${T.root_node.name};
      --  Reference to the parent node, or null if this is the root one

      Unit : Internal_Unit;
      --  Reference to the analysis unit that owns this node

      Token_Start_Index : Token_Index;
      Token_End_Index   : Token_Index;
      --  Reference to the start and end token that constitutes this node. If
      --  this node is a ghost, Token_Start_Index is the token that this AST
      --  node relates to and Token_End_Index is No_Token_Index. Otherwise,
      --  both tokens are inclusive, i.e. they both belong to this node.

      Self_Env : Lexical_Env;
      --  Hold the environment this node defines, or the parent environment
      --  otherwise.

      Last_Attempted_Child : Integer;
      --  0-based index for the last child we tried to parse for this node. -1
      --  if parsing for all children was successful.

      <%def name="node_fields(cls, or_null=True)">
         <%
            is_generic_list = cls is ctx.generic_list_type
            fields = cls.get_fields(
               include_inherited=False,
               predicate=lambda f: (f.should_emit and
                                    not f.abstract and
                                    not f.null)
            )
            ext = ctx.ext('nodes', cls.raw_name, 'components')

            null_required = (or_null and
                             not is_generic_list and
                             not fields and
                             not cls.subclasses and
                             not ext)
         %>

         % if is_generic_list:
            Count : Natural;
            Nodes : Alloc_AST_List_Array.Element_Array_Access;
         % endif

         % for f in fields:
            ${f.name} : aliased ${f.type.storage_type_name} :=
               ${f.type.storage_nullexpr};
         % endfor

         ${exts.include_extension(ext)}

         % if cls.subclasses:
            case Kind is
               % for subcls in cls.subclasses:
                  when ${subcls.ada_kind_range_name} =>
                     ${node_fields(subcls)}
               % endfor
               when others => null;
            end case;
         % endif

         % if null_required:
            null;
         % endif
      </%def>

      ${node_fields(T.root_node, or_null=False)}
   end record;

   procedure Initialize
     (Self              : ${T.root_node.name};
      Kind              : ${T.node_kind};
      Unit              : Internal_Unit;
      Token_Start_Index : Token_Index;
      Token_End_Index   : Token_Index;
      Parent            : ${T.root_node.name} := null;
      Self_Env          : Lexical_Env := AST_Envs.Empty_Env);
   --  Helper for parsers, to initialize a freshly allocated node

   type PLE_State is record
      Current_Env : Lexical_Env;
      --  Current environment when processing the node: initially inheritted
      --  from the Current_Env of the parent node (or Root_Scope on the root
      --  node), SetInitialEnv actions can change this.
      --
      --  Other environment actions such as AddEnv or AddToEnv can use this.
   end record;
   --  State of PLE on a specific node

   procedure Pre_Env_Actions
     (Self            : ${T.root_node.name};
      State           : in out PLE_State;
      Add_To_Env_Only : Boolean := False);
   --  Internal procedure that will execute all necessary lexical env actions
   --  for Node. This is meant to be called by Populate_Lexical_Env, and not by
   --  the user.

   procedure Post_Env_Actions
     (Self : ${T.root_node.name}; State : in out PLE_State);
   --  Internal procedure that will execute all post add to env actions for
   --  Node. This is meant to be called by Populate_Lexical_Env.

   function Get_Symbol (Node : ${T.root_node.name}) return Symbol_Type
      with Pre => Node = null or else Is_Token_Node (Node);
   --  Assuming Node is a token node, return the corresponding symbol for the
   --  token it contains.

   function Image (Self : Symbol_Type) return ${T.String.name};
   --  Transform a Symbol into an internal String

   function Text (Node : ${T.root_node.name}) return Text_Type;
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

   ## Helpers generated for properties code. Used in CollectionGet's and
   ## Map/Quantifier's code.

   function Length (Node : ${ctx.generic_list_type.name}) return Natural;

   function Children
     (Node : ${T.root_node.name}) return ${root_node_array.name};
   --  Return an array containing all the children of Node.
   --  This is an alternative to the Child/Children_Count pair, useful if you
   --  want the convenience of ada arrays, and you don't care about the small
   --  performance hit of creating an array.

   function Item
     (Node  : ${ctx.generic_list_type.name};
      Index : Positive) return ${T.root_node.name} renames Child;

   function Get
     (Node    : ${ctx.generic_list_type.name};
      Index   : Integer;
      Or_Null : Boolean := False) return ${T.root_node.name};
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.

   procedure Reset_Logic_Vars (Node : ${T.root_node.name});
   --  Reset the logic variables attached to this node

   procedure Set_Parents (Node, Parent : ${T.root_node.name});
   --  Set Node.Parent to Parent, and initialize recursively the parent of all
   --  child nodes.

   procedure Destroy (Node : ${T.root_node.name});
   --  Free the resources allocated to this node and all its children

   --------------------------------------
   -- Environments handling (internal) --
   --------------------------------------

   function Get (A : AST_Envs.Entity_Array; Index : Integer) return Entity;
   --  Simple getter that raises Property_Error on out-of-bound accesses.
   --  Useful for code generation.

   function Group
     (Envs   : ${T.LexicalEnv.array.name};
      Env_Md : ${T.env_md.name} := No_Metadata) return ${T.LexicalEnv.name};
   --  Convenience wrapper for uniform types handling in code generation

   package ${T.root_node.array.pkg_vector} is
      new Langkit_Support.Vectors (${T.root_node.name});

   function Is_Visible_From
     (Referenced_Env, Base_Env : Lexical_Env) return Boolean;
   --  Return whether the unit that Referenced_Env belongs to is visible from
   --  the unit that Base_Env belongs to. If at least one of these two lexical
   --  environments does not belong to a particular analysis unit, this raises
   --  a Property_Error.

   function Populate_Lexical_Env (Node : ${T.root_node.name}) return Boolean;
   --  Populate the lexical environment for node and all its children. Return
   --  whether a Property_Error error occurred in the process.

   -----------------------------------
   -- Lexical utilities (internals) --
   -----------------------------------

   function Token
     (Node  : ${T.root_node.name};
      Index : Token_Index) return Token_Reference;
   --  Helper for properties. This is used to turn token indexes as stored in
   --  AST nodes into Token_Reference values.

   function Stored_Token
     (Node  : ${T.root_node.name};
      Token : Token_Reference) return Token_Index;
   --  Helper for properties. This is used to turn a Token_Reference value into
   --  a Token_Index value that can be stored as a field in Node. This raises a
   --  Property_Error if Node and Token don't belong to the same analysis unit
   --  or if Token is actually a Trivia.

   type Bare_Child_Record (Kind : Child_Or_Trivia := Child) is record
      case Kind is
         when Child =>
            Node : ${T.root_node.name};
         when Trivia =>
            Trivia : Token_Reference;
      end case;
   end record;
   --  Variant that holds either an node or a token

   type Bare_Children_Array is array (Positive range <>) of Bare_Child_Record;

   function Children_And_Trivia
     (Node : ${T.root_node.name}) return Bare_Children_Array;
   --  Implementation for Analysis.Children_And_Trivia

   % for astnode in no_builtins(ctx.astnode_types):
      ${astnode_types.private_decl(astnode)}
   % endfor

   function "<" (Left, Right : Internal_Unit) return Boolean;

   type Exiled_Entry is record
      Env  : Lexical_Env;
      Key  : Symbol_Type;
      Node : ${T.root_node.name};
   end record;
   --  Tuple of values passed to AST_Envs.Add. Used in the lexical
   --  environment rerooting machinery: see Remove_Exiled_Entries and
   --  Reroot_Foreign_Nodes.

   package Exiled_Entry_Vectors is new Langkit_Support.Vectors (Exiled_Entry);

   type Foreign_Node_Entry is record
      Node : ${T.root_node.name};
      --  The foreign node that has been added to an analysis unit's lexical
      --  environment.

      Unit : Internal_Unit;
      --  Analysis unit that owns Node
   end record;

   package Foreign_Node_Entry_Vectors is new Langkit_Support.Vectors
     (Foreign_Node_Entry);

   procedure Register_Destroyable
     (Unit : Internal_Unit; Node : ${T.root_node.name});
   --  Register Node to be destroyed when Unit is deallocated/reparsed

   procedure Register_Destroyable
     (Unit : Internal_Unit; Env : AST_Envs.Lexical_Env_Access);
   --  Register Env to be destroyed when Unit is deallocated/reparsed

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

   procedure Inc_Ref (Provider : in out Internal_Unit_Provider) is abstract;
   ${ada_doc('langkit.unit_provider_inc_ref', 3)}

   function Dec_Ref (Provider : in out Internal_Unit_Provider) return Boolean
   is abstract;
   ${ada_doc('langkit.unit_provider_dec_ref', 3)}

   function Get_Unit_Filename
     (Provider : Internal_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String is abstract;
   ${ada_doc('langkit.unit_provider_get_unit_filename', 3)}

   function Get_Unit
     (Provider    : Internal_Unit_Provider;
      Context     : Internal_Context;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Internal_Unit is abstract;
   ${ada_doc('langkit.unit_provider_get_unit_from_name', 3)}

   procedure Dec_Ref (Provider : in out Internal_Unit_Provider_Access);

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

      Tab_Stop : Positive;
      --  Tab stop for the lexer to correctly interpret ASCII.HT input
      --  characters.

      With_Trivia : Boolean;
      --  Whether Trivia nodes were parsed and included in analysis units

      Root_Scope : Lexical_Env;
      --  The lexical scope that is shared amongst every compilation unit. Used
      --  to resolve cross file references.

      Unit_Provider : Internal_Unit_Provider_Access;
      --  Object to translate unit names to file names

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

      Reparse_Cache_Version : Natural;
      --  Version number used to invalidate referenced envs caches. It is
      --  incremented only when a unit is reparsed in the context.

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

      Current_Call_Depth : Natural := 0;
      --  Number of recursive property calls currently running. This counter is
      --  used as a mitigation against infinite recursions.

      Call_Depth_High_Water_Mark : Natural := 0;
      --  Maximum number of recursive calls seen in this context so far

      Max_Call_Depth : Natural := 0;
      --  Maximum number of recursive calls allowed
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

      AST_Root : ${T.root_node.name};

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
      AST_Root     : ${T.root_node.name};
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
     (Charset        : String;
      Unit_Provider  : Internal_Unit_Provider_Access;
      With_Trivia    : Boolean;
      Tab_Stop       : Positive;
      Max_Call_Depth : Natural := ${ctx.default_max_call_depth})
      return Internal_Context;
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
      Input             : Internal_Lexer_Input;
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
      Error    : Text_Type;
      Charset  : String;
      Rule     : Grammar_Rule) return Internal_Unit;
   --  Implementation for Analysis.Get_With_Error

   % if ctx.default_unit_provider:

   function Get_From_Provider
     (Context : Internal_Context;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
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

   function Root (Unit : Internal_Unit) return ${T.root_node.name};
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

   function Text (Unit : Internal_Unit) return Text_Cst_Access;
   --  Overload for Analysis.Text that returns an access

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

   procedure Invalidate_Caches
     (Context : Internal_Context; Invalidate_Envs : Boolean);
   --  Invalidate memoization caches. If Invalidate_Envs is true, also
   --  invalidate referenced envs caches.

   procedure Reset_Caches (Unit : Internal_Unit);
   --  Destroy Unit's memoization cache. This resets Unit's version number to
   --  Unit.Context.Cache_Version.

   procedure Reference_Unit (From, Referenced : Internal_Unit);
   --  Set the Referenced unit as being referenced from the From unit. This is
   --  useful for visibility purposes, and is mainly meant to be used in the
   --  env hooks.

   function Get_Line
     (Unit : Internal_Unit; Line_Number : Positive) return Text_Type;
   --  Return the line of text at line number ``Line_Number``

   function Is_Referenced_From
     (Self, Unit : Internal_Unit) return Boolean;

   procedure Do_Parsing
     (Unit   : Internal_Unit;
      Input  : Internal_Lexer_Input;
      Result : out Reparsed_Unit);
   --  Parse text for Unit using Input and store the result in Result. This
   --  leaves Unit unchanged.

   procedure Update_After_Reparse
     (Unit : Internal_Unit; Reparsed : in out Reparsed_Unit);
   --  Update Unit's AST from Reparsed and update stale lexical environment
   --  data after the reparsing of Unit.

   procedure Destroy_Unit_Destroyables (Unit : Internal_Unit);
   --  Destroy all destroyables objects in Unit and clear this list in Unit

   procedure Remove_Exiled_Entries (Unit : Internal_Unit);
   --  Remove lexical environment entries referencing nodes in Unit from
   --  lexical environments Unit does not own. Remove foreign node entries in
   --  foreign units that correspond to these exiled entries. Clear
   --  Unit.Exiled_Entries afterwards.

   procedure Extract_Foreign_Nodes
     (Unit          : Internal_Unit;
      Foreign_Nodes : in out ${T.root_node.name}_Vectors.Vector);
   --  Collect in Foreign_Nodes all foreign nodes in Unit's lexical
   --  environments (i.e. lexical env entries that refer to nodes which belongs
   --  to other analysis units). Remove the exiled entries in foreign units
   --  that correspond to these foreign nodes. Clear Unit.Foreign_Nodes
   --  afterwards.

   procedure Reroot_Foreign_Node (Node : ${T.root_node.name});
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

private
   --  We only have a private part to defer the initialization of struct
   --  constants. This allows us to circumvent circularity problems between
   --  arrays and structs.

   % for struct_type in no_builtins(ctx.struct_types):
   ${struct_types.nullexpr_decl(struct_type)}
   % endfor

end ${ada_lib_name}.Implementation;
