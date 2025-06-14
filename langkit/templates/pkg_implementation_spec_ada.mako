## vim: filetype=makoada

<%namespace name="array_types"    file="array_types_ada.mako" />
<%namespace name="iterator_types" file="iterator_types_ada.mako" />
<%namespace name="set_types"      file="set_types_ada.mako" />
<%namespace name="astnode_types"  file="astnode_types_ada.mako" />
<%namespace name="exts"           file="extensions.mako" />
<%namespace name="struct_types"   file="struct_types_ada.mako" />
<%namespace name="memoization"    file="memoization_ada.mako" />

<%
root_node_array = T.root_node.array
root_node_iterator = T.root_node.iterator
cache_collection_enabled = cfg.library.cache_collection is not None
%>

--  To facilitate use from a -gnatX project, since we don't use the [] syntax
pragma Warnings (Off, "obsolescent");

with Ada.Containers;              use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System; use System;

with GNATCOLL.GMP.Integers;
with GNATCOLL.Traces;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Adalog.Logic_Var;
with Langkit_Support.Adalog.Solver;
with Langkit_Support.Adalog.Solver_Interface;

with Langkit_Support.Bump_Ptr;     use Langkit_Support.Bump_Ptr;
with Langkit_Support.Cheap_Sets;
with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;
with Langkit_Support.Internal.Analysis;
with Langkit_Support.Lexical_Envs; use Langkit_Support.Lexical_Envs;
with Langkit_Support.Lexical_Envs_Impl;
with Langkit_Support.Symbols;      use Langkit_Support.Symbols;
with Langkit_Support.Symbols.Precomputed;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;
with Langkit_Support.Types;        use Langkit_Support.Types;
with Langkit_Support.Vectors;

with ${ada_lib_name}.Parsers; use ${ada_lib_name}.Parsers;
with ${ada_lib_name}.Common;  use ${ada_lib_name}.Common;

${exts.with_clauses(with_clauses)}

--  Internal package: low-level primitives to implement public types and
--  operations in ${ada_lib_name}.Analysis.

private package ${ada_lib_name}.Implementation is

   pragma Suppress (Container_Checks);

   use Support.Diagnostics, Support.Slocs, Support.Text;

   ------------
   -- Traces --
   ------------

   Main_Trace : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("${ctx.lib_name.upper}.MAIN_TRACE", GNATCOLL.Traces.From_Config);

   PLE_Errors_Trace : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("${ctx.lib_name.upper}.PLE_ERRORS", GNATCOLL.Traces.From_Config);

   Cache_Invalidation_Trace : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("${ctx.lib_name.upper}.CACHE_INVALIDATION",
        GNATCOLL.Traces.From_Config);

   -------------------------------------
   -- Symbols and token data handlers --
   -------------------------------------

   type Precomputed_Symbol_Index is
      % if ctx.symbol_literals:
         (
            <%
               sym_items = ctx.sorted_symbol_literals
               last_i = len(sym_items) - 1
            %>
            % for i, (sym, name) in enumerate(sym_items):
               ${name}${',' if i < last_i else ''} --  ${sym}
            % endfor
         )
      % else:
         new Integer range 1 .. 0
      % endif
   ;

   function Precomputed_Symbol
     (Index : Precomputed_Symbol_Index) return Text_Type;

   --  GNAT emits an incorrect value not in range in instantiation warning...
   --  So deactivate them at the instantiation point.
   pragma Warnings (Off, "value not in range");
   package Precomputed_Symbols is new Langkit_Support.Symbols.Precomputed
     (Precomputed_Symbol_Index, Precomputed_Symbol);
   pragma Warnings (On, "value not in range");

   --------------------
   -- Analysis types --
   --------------------

   type Analysis_Context_Type;
   type Internal_Context is access all Analysis_Context_Type;

   type Analysis_Unit_Type;
   type Internal_Unit is access all Analysis_Unit_Type;

   type ${T.root_node.value_type_name};
   type ${T.root_node.name} is access all ${T.root_node.value_type_name};
   ${T.root_node.null_constant} : constant ${T.root_node.name} := null;
   --  Most generic AST node type

   pragma No_Strict_Aliasing (Internal_Context);
   pragma No_Strict_Aliasing (Internal_Unit);
   pragma No_Strict_Aliasing (${T.root_node.name});

   function "<" (Left, Right : ${T.root_node.name}) return Boolean;
   --  Abritrary but deterministic ordering criteria for parsing nodes. This
   --  handles null nodes as well. Raise a Property_Error for synthetic nodes.

   function Is_Null (Node : ${T.root_node.name}) return Boolean;
   function Kind (Node : ${T.root_node.name}) return ${T.node_kind};

   % for node in ctx.node_types:
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

   % if ctx.properties_logging:
      Properties_Traces : constant GNATCOLL.Traces.Trace_Handle :=
         GNATCOLL.Traces.Create
           ("LANGKIT.PROPERTIES", GNATCOLL.Traces.On
           % if cfg.emission.show_property_logging:
           , Stream => "&1"
           % endif
           );
   % endif

   function Node_Sloc_Image (Self : ${T.root_node.name}) return Text_Type;
   --  Return the image of the sloc for ``Self``, to be used in
   --  ``Short_Text_Image``.

   function Short_Text_Image (Self : ${T.root_node.name}) return Text_Type;
   --  Return a short representation of the node, containing just the kind
   --  name and the sloc, or "None" if Self is null.

   function Is_Token_Node (Node : ${T.root_node.name}) return Boolean;
   ${ada_doc('langkit.node_is_token_node', 3)}

   function Is_Synthetic (Node : ${T.root_node.name}) return Boolean;
   ${ada_doc('langkit.node_is_synthetic', 3)}

   procedure Raise_Property_Exception
     (Node    : ${T.root_node.name};
      Exc     : Ada.Exceptions.Exception_Id;
      Message : String)
     with No_Return;
   --  Raise an exception of the given type and with the given message. Prepend
   --  the sloc of the given node to the exception message.

   ---------------------------
   -- Iterators safety nets --
   ---------------------------

   type Iterator_Safety_Net is record
      Context         : Internal_Context;
      Context_Serial  : Version_Number;
      Context_Version : Version_Number;
      --  Analysis context, its serial number and version number at the time
      --  this safety net was produced.
   end record;

   No_Iterator_Safety_Net : constant Iterator_Safety_Net := (null, 0, 0);

   function Create_Safety_Net
     (Context : Internal_Context) return Iterator_Safety_Net;
   --  Create an iterator safety net from the given Context

   procedure Check_Safety_Net (Self : Iterator_Safety_Net);
   --  Check that the given iterator safety net is still valid, raising a
   --  Stale_Reference_Error if it is not.

   -----------------
   -- String type --
   -----------------

   type String_Record (Length : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Content : Text_Type (1 .. Length);
   end record;

   type String_Type is access all String_Record;

   Empty_String_Record : aliased String_Record :=
     (Length => 0, Ref_Count => -1, Content => (others => <>));
   Empty_String        : constant String_Type := Empty_String_Record'Access;

   procedure Inc_Ref (Self : String_Type);
   procedure Dec_Ref (Self : in out String_Type);
   procedure Free is new Ada.Unchecked_Deallocation
     (String_Record, String_Type);

   function Create_String (Content : Text_Type) return String_Type;
   function Create_String (Content : Unbounded_Text_Type) return String_Type;
   --  Create string values from their content. The overload for unbounded
   --  strings makes it easier for callers to avoid using the secondary stack,
   --  which can be a problem for big strings.

   function Concat_String (Left, Right : String_Type) return String_Type;
   --  Return a new string that is the concatenation of ``Left`` and ``Right``

   function Equivalent (Left, Right : String_Type) return Boolean;
   --  Return whether ``Left`` and ``Right`` contain equal strings

   ---------------------------
   -- Environments handling --
   ---------------------------

   subtype Long_Long_Natural is Long_Long_Integer
      range 0 .. Long_Long_Integer'Last;

   ${struct_types.incomplete_decl(T.env_md)}
   ${struct_types.decl(T.env_md, incomplete_nullexpr=False)}
   ${struct_types.nullexpr_decl(T.env_md)}

   ## Special case for the env metadata struct, where the Hash function is
   ## explicitly declared because it has specific logic.
   function Hash (Self : ${T.env_md.name}) return Hash_Type;

   ${struct_types.incomplete_decl(T.InnerEnvAssoc)}
   ${struct_types.decl(T.InnerEnvAssoc, incomplete_nullexpr=False)}
   ${struct_types.nullexpr_decl(T.InnerEnvAssoc)}
   function Get_Key (Self : ${T.InnerEnvAssoc.name}) return Thin_Symbol
   is (Thin (Self.Key));
   function Get_Node
     (Self : ${T.InnerEnvAssoc.name}) return ${T.root_node.name}
   is (Self.Value);
   function Get_Rebindings
     (Self : ${T.InnerEnvAssoc.name}) return Env_Rebindings
   is (Self.Rebindings);
   function Get_Metadata
     (Self : ${T.InnerEnvAssoc.name}) return ${T.env_md.name}
   is (Self.Metadata);

   ${array_types.incomplete_decl(T.InnerEnvAssoc.array)}
   % if T.InnerEnvAssoc.iterator.is_used:
      ${iterator_types.incomplete_decl(T.InnerEnvAssoc.iterator)}
   % endif

   ${array_types.decl(T.InnerEnvAssoc.array)}
   % if T.InnerEnvAssoc.iterator.is_used:
      ${iterator_types.decl(T.InnerEnvAssoc.iterator)}
   % endif

   function Inner_Env_Assoc_Get
     (Self  : ${T.InnerEnvAssoc.array.name};
      Index : Positive) return ${T.InnerEnvAssoc.name}
   is (Self.Items (Index));

   function Combine
     (L, R : ${T.env_md.name}) return ${T.env_md.name};
   --  The combine function on environments metadata does a boolean Or on every
   --  boolean component of the env metadata.

   function Can_Reach (El, From : ${T.root_node.name}) return Boolean
     with Inline;
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

   function Acquire_Rebinding
     (Node             : ${T.root_node.name};
      Parent           : Env_Rebindings;
      Old_Env, New_Env : Lexical_Env) return Env_Rebindings;
   --  Initialize and return a fresh rebinding

   procedure Release_Rebinding (Self : in out Env_Rebindings);
   --  Mark the rebinding as unused, so that a future call to Acquire_Rebinding
   --  can return it.

   procedure Register_Rebinding
     (Node : ${T.root_node.name}; Rebinding : Env_Rebindings);
   --  Register a rebinding to be destroyed when Node's analysis unit is
   --  destroyed or reparsed.

   % if cache_collection_enabled:
      procedure Lexical_Env_Cache_Updated
        (Node         : ${T.root_node.name};
         Delta_Amount : Long_Long_Integer);
      --  Callback for Langkit_Support.Lexical_Envs_Impl.Notify_Cache_Updated

      procedure Lexical_Env_Cache_Looked_Up (Node : ${T.root_node.name});
      --  Callback for Langkit_Support.Lexical_Envs_Impl.Notify_Cache_Looked_Up

      procedure Lexical_Env_Cache_Hit (Node : ${T.root_node.name});
      --  Callback for Langkit_Support.Lexical_Envs_Impl.Notify_Cache_Hit
   % endif

   function Element_Parent
     (Node : ${T.root_node.name}) return ${T.root_node.name};

   function Hash (Node : ${T.root_node.name}) return Hash_Type;
   function Node_Unit (Node : ${T.root_node.name}) return Generic_Unit_Ptr;
   function Named_Hash (Node : ${T.root_node.name}) return Hash_Type is
     (Hash (Node));

   No_Analysis_Unit : constant Internal_Unit := null;

   function Convert_Unit is new Ada.Unchecked_Conversion
     (Generic_Unit_Ptr, Internal_Unit);
   function Convert_Unit is new Ada.Unchecked_Conversion
     (Internal_Unit, Generic_Unit_Ptr);

   function Unit_Version (Unit : Generic_Unit_Ptr) return Version_Number;
   --  Return the version for Unit. Version is a number that is incremented
   --  every time Unit changes.

   function Get_Context_Version
     (Node : ${T.root_node.name}) return Version_Number;
   --  Assuming that Node is not null, return the version number for Node's
   --  context, which is incremented every time a unit in this context is
   --  parsed.

   function Self_Env (Node : ${T.root_node.name}) return Lexical_Env;

   type Ref_Category is
     (${", ".join(sorted(str(cat) for cat in ctx.ref_cats))});
   type Ref_Categories is array (Ref_Category) of Boolean;
   pragma Pack (Ref_Categories);

   function Properties_May_Raise
     (Exc : Ada.Exceptions.Exception_Occurrence) return Boolean;
   --  Return if ``Exc`` is one of the exceptions that properties are allowed
   --  to raise.

   package AST_Envs is new Langkit_Support.Lexical_Envs_Impl
     (Get_Unit_Version         => Unit_Version,
      Node_Type                => ${T.root_node.name},
      Node_Metadata            => ${T.env_md.name},
      No_Node                  => null,
      Empty_Metadata           => No_Metadata,
      Node_Unit                => Node_Unit,
      Node_Hash                => Named_Hash,
      Metadata_Hash            => Hash,
      Combine                  => Combine,
      Node_Text_Image          => AST_Envs_Node_Text_Image,
      Acquire_Rebinding        => Acquire_Rebinding,
      Register_Rebinding       => Register_Rebinding,
   % if cache_collection_enabled:
      Notify_Cache_Updated     => Lexical_Env_Cache_Updated,
      Notify_Cache_Looked_Up   => Lexical_Env_Cache_Looked_Up,
      Notify_Cache_Hit         => Lexical_Env_Cache_Hit,
   % endif
      Ref_Category             => Ref_Category,
      Ref_Categories           => Ref_Categories,
      Inner_Env_Assoc          => ${T.InnerEnvAssoc.name},
      Inner_Env_Assoc_Array    => ${T.InnerEnvAssoc.array.name},
      Get                      => Inner_Env_Assoc_Get);

   use AST_Envs;
   subtype Internal_Entity is AST_Envs.Entity;
   subtype Internal_Entity_Info is AST_Envs.Entity_Info;

   ${T.EntityInfo.nullexpr} : constant ${T.EntityInfo.name} :=
     (No_Metadata, null, False);
   ${root_entity.nullexpr} : constant ${root_entity.name} :=
     (null, ${T.EntityInfo.nullexpr});

   function Hash_Entity (Self : ${root_entity.name}) return Hash_Type;
   --  Hash function to use in the public API. It is like the regular one, but
   --  compares metadata according to the user specification in the DSL.

   function Compare_Entity (Left, Right : ${root_entity.name}) return Boolean;
   --  Equality function to use in the public API. It is like the regular one,
   --  but compares metadata according to the user specification in the DSL.

   function Compare_Metadata (L, R : ${T.env_md.name}) return Boolean;
   --  Compare metadata ``L`` and ``R`` for public entity comparison

   function Create_Dynamic_Lexical_Env
     (Self              : ${T.root_node.name};
      Assocs_Getter     : Inner_Env_Assocs_Resolver;
      Assoc_Resolver    : Entity_Resolver;
      Transitive_Parent : Boolean;
      Sym_Table         : Symbol_Table) return Lexical_Env;
   --  Helper for properties code generation: wrapper around
   --  AST_Envs.Create_Dynamic_Lexical_Env.

   ## Generate Hash functions for "built-in types" if need be
   % if T.Bool.requires_hash_function:
      function Hash (B : Boolean) return Hash_Type;
   % endif

   % if T.Int.requires_hash_function:
      function Hash (I : Integer) return Hash_Type;
   % endif

   % if T.Char.requires_hash_function:
      function Hash (I : Character_Type) return Hash_Type;
   % endif

   % if T.String.requires_hash_function:
      function Hash (I : String_Type) return Hash_Type;
   % endif

   ## Also generate hash functions for enum types that need one
   % for enum_type in ctx.enum_types:
      % if enum_type.requires_hash_function:
         function Hash (Self : ${enum_type.name}) return Hash_Type;
      % endif
   % endfor

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
   pragma No_Strict_Aliasing (Big_Integer_Type);

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

   function To_Integer
     (Self    : ${T.root_node.name};
      Big_Int : Big_Integer_Type) return Integer;
   --  Convert ``Big_Int`` into a regular integer, raising a ``Property_Error``
   --  if it is out of range (using ``Self`` to provide context for this
   --  error).

   procedure Inc_Ref (Big_Int : Big_Integer_Type);
   procedure Dec_Ref (Big_Int : in out Big_Integer_Type);

   function Equivalent (Left, Right : Big_Integer_Type) return Boolean;
   function "<" (Left, Right : Big_Integer_Type) return Boolean;
   function "<=" (Left, Right : Big_Integer_Type) return Boolean;
   function ">" (Left, Right : Big_Integer_Type) return Boolean;
   function ">=" (Left, Right : Big_Integer_Type) return Boolean;

   function "+" (Left, Right : Big_Integer_Type) return Big_Integer_Type;
   function "-" (Left, Right : Big_Integer_Type) return Big_Integer_Type;
   function "-" (Value : Big_Integer_Type) return Big_Integer_Type;

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
   --  result of the parsing of a node failing as a result of a Cut parser
   --  annotation.

   function Kind_Name (Node : ${T.root_node.name}) return String;
   --  Return the concrete kind for Node

   -------------------
   -- Node Builders --
   -------------------

   --  A node builder is basically a functor that takes one argument (a "parent
   --  node") and that returns a node (either an existing node or a node that
   --  the node builder creates).

   type Node_Builder_Record is abstract tagged record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.
   end record;
   type Node_Builder_Type is access all Node_Builder_Record'Class;

   function Build
     (Self              : Node_Builder_Record;
      Parent, Self_Node : ${T.root_node.name}) return ${T.root_node.name}
   is abstract;
   --  Return the node that ``Self`` must create.
   --
   --  If actual node synthetization occurs, ``Parent`` is used to initialize
   --  the parent link of the returned node.
   --
   --  This function is meant to be called in a property: ``Self_Node`` must be
   --  the ``Self`` of the calling property.

   function Trace_Image (Self : Node_Builder_Record) return String is abstract;

   procedure Release (Self : in out Node_Builder_Record) is null;
   --  Free resources for this node builder

   function Trace_Image (Self : Node_Builder_Type) return String
   is (if Self = null
       then "<NodeBuilder null>"
       else Self.Trace_Image);

   type Copy_Node_Builder_Record is new Node_Builder_Record with record
      Value : ${T.root_node.name};
      --  Existing node that this builder must yield
   end record;

   overriding function Build
     (Self              : Copy_Node_Builder_Record;
      Parent, Self_Node : ${T.root_node.name}) return ${T.root_node.name}
   is (Self.Value);

   overriding function Trace_Image
     (Self : Copy_Node_Builder_Record) return String
   is ("<NodeBuilder to copy " & Trace_Image (Self.Value) & ">");

   Null_Node_Builder_Record : aliased Copy_Node_Builder_Record :=
     (Ref_Count => -1, Value => null);
   Null_Node_Builder        : constant Node_Builder_Type :=
     Null_Node_Builder_Record'Access;

   procedure Free is new Ada.Unchecked_Deallocation
     (Node_Builder_Record'Class, Node_Builder_Type);

   procedure Inc_Ref (Self : Node_Builder_Type);
   procedure Dec_Ref (Self : in out Node_Builder_Type);

   function Create_Copy_Node_Builder
     (Value : ${T.root_node.name}) return Node_Builder_Type;

   % for t in ctx.node_builder_types:
      subtype ${t.name} is Node_Builder_Type;
   % endfor

   --  Constructors for synthetizing node builders may rely on other types, so
   --  they are declared later.

   -----------------------------------------------
   -- Structure types (incomplete declarations) --
   -----------------------------------------------

   % for struct_type in ctx.struct_types:
      % if not struct_type.has_early_decl:
         ${struct_types.incomplete_decl(struct_type)}
      % endif
   % endfor

   -------------------------------------------
   -- Array types (incomplete declarations) --
   -------------------------------------------

   % for array_type in ctx.array_types:
      % if not array_type.has_early_decl:
         ${array_types.incomplete_decl(array_type)}
      % endif
   % endfor

   ----------------------------------------------
   -- Iterator types (incomplete declarations) --
   ----------------------------------------------

   % for iterator_type in ctx.iterator_types:
      % if not iterator_type.has_early_decl and iterator_type.is_used:
         ${iterator_types.incomplete_decl(iterator_type)}
      % endif
   % endfor

   -----------------------------------------
   -- Set types (incomplete declarations) --
   -----------------------------------------

   % for set_type in ctx.set_types:
      % if not set_type.has_early_decl:
         ${set_types.incomplete_decl(set_type)}
      % endif
   % endfor

   ---------------------------
   -- Adalog instantiations --
   ---------------------------

   function Text_Image (Ent : ${T.entity.name}) return Text_Type;
   function Image (Ent : ${T.entity.name}) return String;
   ${ada_doc('langkit.node_image', 3)}

   type Internal_Logic_Context_Access is access Internal_Logic_Context;

   function Allocate_Logic_Context
     (Ctx : Internal_Logic_Context) return Internal_Logic_Context_Access;
   --  Return an access on a heap allocated copy of the given context

   function Trace_Logic_Context
     (Ctx : Internal_Logic_Context_Access) return String;
   --  Return a trace representation of the context after dereference

   function Deep_Equals
     (X, Y : Internal_Logic_Context_Access) return Boolean;
   --  Return whether the two logic contexts after dereference are equal

   procedure Free_Logic_Context
     (Ctx : in out Internal_Logic_Context_Access);
   --  Release memory allocated by ``Allocate_Logic_Context``

   package Entity_Vars is new Langkit_Support.Adalog.Logic_Var
     (Value_Type => ${T.entity.name}, Value_Image => Image);
   package Solver_Ifc is new Langkit_Support.Adalog.Solver_Interface
     (Entity_Vars,
      Internal_Logic_Context, Internal_Logic_Context_Access,
      Trace_Logic_Context, Deep_Equals,
      Free_Logic_Context, Internal_Solver_Diagnostic);
   package Solver is new Langkit_Support.Adalog.Solver (Solver_Ifc);

   subtype Logic_Var is Entity_Vars.Logic_Var;
   subtype Logic_Var_Record is Entity_Vars.Logic_Var_Record;
   Null_Var : constant Logic_Var := null;
   Null_Var_Record : constant Logic_Var_Record := (Reset => True, others => <>);

   subtype Logic_Equation is Solver.Relation;
   Null_Logic_Equation : constant Logic_Equation := Solver.No_Relation;

   % if ctx.properties_logging:
      function Trace_Image (K : Analysis_Unit_Kind) return String;
      function Trace_Image (B : Boolean) return String;
      function Trace_Image (I : Integer) return String;
      function Trace_Image (S : Symbol_Type) return String;
      function Trace_Image (C : Character_Type) return String;
      function Trace_Image (S : String_Type) return String;
      function Trace_Image (Env : Lexical_Env) return String;
      function Trace_Image (R : Env_Rebindings) return String;
      function Trace_Image (Unit : Internal_Unit) return String;
      function Trace_Image (Eq : Logic_Equation) return String;
      function Trace_Image (Var : Logic_Var) return String;
      function Trace_Image (T : Token_Reference) return String renames Image;
      function Trace_Image (T : Source_Location) return String renames Image;
      function Trace_Image (Self : Ref_Categories) return String;
   % endif

   -----------------------------------------
   -- Structure types (full declarations) --
   -----------------------------------------

   % for struct_type in ctx.struct_types:
      % if not struct_type.has_early_decl:
         ${struct_types.decl(struct_type)}
      % endif
   % endfor

   -----------------
   -- Array types --
   -----------------

   --  We implement array types as discriminated records so that binding to C
   --  can be done without copy.

   % for array_type in ctx.array_types:
      % if not array_type.has_early_decl:
         ${array_types.decl(array_type)}
      % endif
   % endfor

   --------------------
   -- Iterator types --
   --------------------

   % for iterator_type in ctx.iterator_types:
      % if not iterator_type.has_early_decl and iterator_type.is_used:
         ${iterator_types.decl(iterator_type)}
      % endif
   % endfor

   ---------------
   -- Set types --
   ---------------

   % for set_type in ctx.set_types:
      % if not set_type.has_early_decl:
         ${set_types.decl(set_type)}
      % endif
   % endfor

   ---------------------
   -- Extension specs --
   ---------------------

   ${exts.include_extension(ctx.ext('analysis', 'implem_decls'))}

   ---------------------------------------------
   -- Synthetizing node builders constructors --
   ---------------------------------------------

   % for t in ctx.node_builder_types:
      % if t.synth_node_builder_needed:
         <% constructor_args = t.synth_constructor_args %>

         function ${t.synth_constructor}
           % if constructor_args:
           ${ada_block_with_parens(
              [
                 f"{arg.codegen_name} : {arg.type.name}"
                 for arg in constructor_args
              ],
              12,
              separator=";",
           )}
           % endif
           return ${t.name};
      % endif
   % endfor

   ------------------------
   -- Named environments --
   ------------------------

   --  The goal of named environments is to provide a sound mechanism to
   --  associate nodes and environments across analysis units: nodes whose
   --  Self_Env comes from another unit ("foreign env"), environments whose
   --  parent comes from another unit (also foreign env), or that contain
   --  symbol/node mappings for nodes coming from other units ("foreign
   --  nodes").
   --
   --  This mechanism comes with the following requirements:
   --
   --  * Ensure that, after unit reparsing, all cross-unit associations are
   --    still valid. For instance, no node's Self_Env can refer to a lexical
   --    environment that has been deallocated.
   --
   --  * Ensure that regardless of the sequence of unit parsing/reparsing that
   --    led to a given set of units (considering only unit filename and source
   --    buffer), the node/env graph (i.e. the result of PLE) is always the
   --    same, i.e. make incremental PLE idempotent.
   --
   --  Note that even though the end goal for named envs is to replace the
   --  previous mechanism (proved to be unsound, as violating the second
   --  requirement), both still coexist during the transition period.
   --
   --  Here is how this mechanism works:
   --
   --  1. Environments can be assigned zero, one or several names (i.e. one or
   --     several symbols). Name(s) assignment happens at environment
   --     construction.
   --
   --  2. As a consequence, multiple environments can be associated to a given
   --     env name. Using a total and deterministic ordering predicate, only
   --     one of them is said to have "precedence": looking up an environment
   --     using that name will return this unique environment.
   --
   --  3. For a given env name, we keep track of all uses of the environment
   --     that is looked up by its name: environment parent link, symbol/node
   --     mapping addition, node's Self_Env assignment. This info is
   --     tracked using the Named_Env_Descriptor record type below, often
   --     abbreviated NED. Note that this tracking happens even when there is
   --     no environment associated to the env name, as we need to do such
   --     updates when an environment gets associated to that env name.
   --
   --  4. Unit reparsing can destroy existing environments and/or create new
   --     ones. This means that, depending on their "ranking" using the
   --     ordering predicate, environments can earn or lose precedence for a
   --     given name.
   --
   --  5. When the precedence changes for a given name, we use the info
   --     collected as per 3. to perform relocation: relevant environment
   --     parent links are updated, symbol/node mappings are removed from the
   --     env that lost precedence and added to the env that earned precedence,
   --     etc.

   --  Tables to populate lexical entries in named envs

   package NED_Assoc_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Internal_Map_Node_Vectors.Vector,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => Internal_Map_Node_Vectors."=");
   --  Symbol/lexical env entry mappings for a given named env descriptor.
   --  Symbols are not unique in all mappings, so the lexical env entries are
   --  stored in a vector.

   procedure Add
     (Self : in out NED_Assoc_Maps.Map;
      Key  : Symbol_Type;
      Node : AST_Envs.Internal_Map_Node);
   --  Add a symbol/lexical env entry mapping in Self

   procedure Remove
     (Self : in out NED_Assoc_Maps.Map;
      Key  : Symbol_Type;
      Node : ${T.root_node.name});
   --  Remove a symbol/lexical env entry mapping from Self

   --  Global table for named environments

   package Sorted_Env_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => ${T.root_node.name},
      Element_Type => Lexical_Env);
   --  List of lexical environments, sorted by owning node. This means that the
   --  following must be true for all cursors in such maps::
   --
   --     Key (Cur) = Element (Cur).Env.Node

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => ${T.root_node.name},
      Hash                => Hash,
      Equivalent_Elements => "=");

   type Named_Env_Descriptor is record
      Name : Symbol_Type;
      --  Name corresponding to this descriptor. Useful during debugging.

      Envs : Sorted_Env_Maps.Map;
      --  For each env name, we can have one or several environments
      --  (concurrent definitions). Just like foreign nodes in lexical
      --  environments, we keep them sorted by node to preserve determinism:
      --  given a set of loaded units, we will always have the same set of
      --  name:env associations sorted in the same order and thus always the
      --  same results at lookup time.

      Env_With_Precedence : Lexical_Env;
      --  Named environment that has precedence for this name.
      --
      --  Most of the time, if Envs is empty, this is Empty_Env and otherwise,
      --  shortcut to Envs.First_Element. However, when a change in Envs
      --  invalidates Env_With_Precedence, we reset it to Empty_Env momentarily
      --  during PLE as a way to tag the temprorary inconsistency. Later on, we
      --  recompute it and perform the needed relocations.

      Foreign_Nodes : NED_Assoc_Maps.Map;
      --  This maps symbols to lists of env entries for all the foreign nodes
      --  in Env_With_Precedence.
      --
      --  This set allows efficient relocation of env entries when
      --  Env_With_Precedence changes.

      Foreign_Envs : Sorted_Env_Maps.Map;
      --  This maps the owning node to env mapping for all lexical environments
      --  whose parent must be Env_With_Precedence. Envs are indexed by owning
      --  node for quick lookup during updates.
      --
      --  This set allows efficient env parent link updates when
      --  Env_With_Precedence changes.

      Nodes_With_Foreign_Env : Node_Sets.Set;
      --  Set of nodes whose env (Self_Env) must be Env_With_Precedence.
      --
      --  This set allows efficient Self_Env updates when Env_With_Precedence
      --  changes.

      --  Note that during the updating process of a reparsed unit
      --  (Update_After_Reparse procedure), these data structures become
      --  temporarily inconsistent: Env_With_Precedence can become Empty_Env
      --  even though Envs is not empty.  This is fine, because when it does,
      --  Update_After_Reparse keeps track of it as to be updated
      --  (Named_Envs_Needing_Update map).
   end record;
   type Named_Env_Descriptor_Access is access Named_Env_Descriptor;
   procedure Destroy is new Ada.Unchecked_Deallocation
     (Named_Env_Descriptor, Named_Env_Descriptor_Access);

   package NED_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Named_Env_Descriptor_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");
   --  Context-wide table that tracks for all env names the set of lexical envs
   --  that define it.

   type Exiled_Entry_In_NED is record
      Named_Env : Named_Env_Descriptor_Access;
      --  Named env descriptor in which Node is registered

      Key : Symbol_Type;
      --  Key in that Env's internal map that leads to the env descriptor that
      --  contains Node.

      Node : ${T.root_node.name};
      --  Exiled node
   end record;

   package Exiled_Entry_In_NED_Vectors is new
      Langkit_Support.Vectors (Exiled_Entry_In_NED);

   type Exiled_Env is record
      Named_Env : Named_Env_Descriptor_Access;
      --  Named env descriptor in which Env is registered

      Env : Lexical_Env;
      --  Exiled environment
   end record;

   package Exiled_Env_Vectors is new Langkit_Support.Vectors (Exiled_Env);

   type Named_Env_Pair is record
      Name : Symbol_Type;
      --  Name on the lexical environment

      Env  : Lexical_Env;
      --  Named lexical environment
   end record;

   package Named_Env_Vectors is new Langkit_Support.Vectors (Named_Env_Pair);

   --  High-level primitives to handle the life cycle of named environment

   function Get_Named_Env_Descriptor
     (Context : Internal_Context;
      Name    : Symbol_Type) return Named_Env_Descriptor_Access;
   --  Return the named env descriptor in Context corresponding to Name. Create
   --  it first, if needed.

   procedure Register_Named_Env
     (Context                   : Internal_Context;
      Name                      : Symbol_Type;
      Env                       : Lexical_Env;
      Named_Envs_Needing_Update : in out NED_Maps.Map);
   --  Register Name as the environment name for Env. If Env takes the
   --  precedence for this name, add Name/its named env descriptor to
   --  Named_Envs_Needing_Update.

   procedure Update_Named_Envs
     (Context : Internal_Context; Named_Envs : NED_Maps.Map);
   --  For each named environment in Named_Envs, update Env_With_Precedence and
   --  do the necessary adjustments: relocate exiled entries, etc.

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
           for cls in ctx.node_types if not cls.abstract)});
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
   --  such a child: if not (i.e. ``Index`` is out-of-bounds), set ``Result``
   --  to a null node.

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
     (Node      : ${T.root_node.name};
      With_Self : Boolean := True)
      return ${root_node_array.name};
   --  Return an array that contains the lexical parents, this node included
   --  iff ``with_self`` is True. Nearer parents are first in the list.

   function Parent (Node : ${T.root_node.name}) return ${T.root_node.name};

   function Fetch_Sibling
     (Node   : ${T.root_node.name};
      Offset : Integer) return ${T.root_node.name};
   function Fetch_Sibling
     (Node   : ${T.root_node.name};
      E_Info : ${T.EntityInfo.name};
      Offset : Integer) return ${root_entity.name};
   --  Assuming Node is the Nth child of its parent, return the (N + Offset)'th
   --  child of the same parent, or null/No_Entity if there is no such sibling.

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
     (Self, Left, Right : ${T.root_node.name};
      Relation          : Comparison_Relation) return Boolean;
   --  If ``Left`` and ``Right`` don't belong to the same analysis units or if
   --  one of them is null, raise a ``Property_Error`` (use ``Self`` to provide
   --  error context). Otherwise, return the comparison of their starting
   --  source location according to Relation.

   -------------------
   -- Debug helpers --
   -------------------

   function Image (Value : Boolean) return String;
   --  Image for a Boolean, for debugging/logging purposes

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

   -------------------------------
   -- Root AST node (internals) --
   -------------------------------

   type Initialization_State is
   ${ada_block_with_parens(
      ["Uninitialized", "Initialized"]
      + [f"Raised_{exc}" for exc in ctx.property_exceptions],
      3,
   )};
   --  Initialization status:
   --
   --  * ``Uninitialized``: initialization still needed to get a value;
   --  * ``Initialized``: initialization completed, value is available;
   --  * ``Raise_X``: initialization raised exception ``X``, value will never
   --    be available.

   subtype Error_Initialization_State is
     Initialization_State range
       Raised_${ctx.property_exceptions[0]}
       .. Raised_${ctx.property_exceptions[-1]};

   function Initialization_Error
     (Exc : Ada.Exceptions.Exception_Occurrence)
      return Error_Initialization_State;
   --  Assuming that ``Exc`` is an exception allowed to be raised in
   --  properties, return the corresponding initialization state.

   procedure Reraise_Initialization_Error
     (Node    : ${T.root_node.name};
      State   : Error_Initialization_State;
      Message : String);
   --  Raise the exception that ``State`` describes. ``Node`` and ``Message``
   --  are used to add contextual information to the exception.

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
            ext = ctx.ext('nodes', cls.raw_name.lower, 'components')

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
            ${f.names.codegen} : aliased ${f.type.storage_type_name} :=
               ${f.ada_default_value};
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

   function Allocate_Synthetic_List_Children
     (Self  : ${ctx.generic_list_type.name};
      Count : Natural) return Alloc_AST_List_Array.Element_Array_Access;
   --  Assuming that ``Self`` is a synthetized list node, allocate an array of
   --  ``Count`` elements for it and return it. This also takes care of
   --  initializing ``Self`` to use that array. It is then up to the caller to
   --  initialize each list child.

   procedure Free_Synthetic_List_Children
     (Self : ${ctx.generic_list_type.name});
   --  Assuming that ``Self`` is a synthetized list node initialized with
   --  ``Allocate_Synthetic_List_Children``, free the corresponding array of
   --  children.

   type PLE_Unit_State is record
      Named_Envs_Needing_Update : NED_Maps.Map;
      --  Set of named env entries whose Env_With_Precedence needs to be
      --  updated.
   end record;
   --  State of PLE on a specific unit

   type PLE_Unit_State_Access is access all PLE_Unit_State;

   type PLE_Node_State is record
      Unit_State : PLE_Unit_State_Access;
      --  State of PLE on the unit that owns this node

      Current_Env : Lexical_Env;
      --  Current environment when processing the node: initially inheritted
      --  from the Current_Env of the parent node (or Root_Scope on the root
      --  node), SetInitialEnv actions can change this.
      --
      --  Other environment actions such as AddEnv or AddToEnv can use this.

      Current_NED : Named_Env_Descriptor_Access;
      --  If the current environment was looked up by name, reference to the
      --  named environment descriptor. Null otherwise.
   end record;
   --  State of PLE on a specific node

   procedure Use_Direct_Env (State : in out PLE_Node_State; Env : Lexical_Env);
   --  Change State so that the current environment is Env, and record that it
   --  was *not* looked up by name.

   procedure Use_Named_Env
     (State   : in out PLE_Node_State;
      Context : Internal_Context;
      Name    : Symbol_Type);
   --  Change State so that the current environment comes from the named
   --  environment looked up with Name.

   procedure Set_Initial_Env
     (Self         : ${T.root_node.name};
      State        : in out PLE_Node_State;
      Env          : ${T.DesignatedEnv.name};
      DSL_Location : String);
   --  Helper for ``Populate_Lexical_Env``: fetch the initial environment for
   --  ``Self`` according to ``Env`` and update ``State`` accordingly.

   procedure Add_To_Env
     (Self         : ${T.root_node.name};
      State        : PLE_Node_State;
      Key          : Symbol_Type;
      Value        : ${T.root_node.name};
      Md           : ${T.env_md.name};
      Resolver     : Entity_Resolver;
      Dest_Env     : ${T.DesignatedEnv.name};
      DSL_Location : String);
   --  Helper for Populate_Lexical_Env: insert the Key/Value/MD/Resolver entry
   --  in the appropriate lexical env.
   --
   --  The destination environment is:
   --
   --  * If Dest_Env_Name is not null, this is the corresponding named
   --    environment.
   --
   --  * Otherwise, use Dest_Env_Fallback if is not the empty environment.
   --
   --  * Finally, use State's current environment.
   --
   --  If the destination environment is foreign and not fetched from its name
   --  while DSL_Location is not empty, raise a Property_Error.

   procedure Ref_Env
     (Self                : ${T.root_node.name};
      Dest_Env            : Lexical_Env;
      Ref_Env_Nodes       : in out ${T.root_node.array.name};
      Resolver            : Lexical_Env_Resolver;
      Kind                : Ref_Kind;
      Cats                : Ref_Categories;
      Shed_Rebindings     : Boolean);
   --  Helper for Populate_Lexical_Env: add referenced environments to
   --  Dest_Env. Calling this takes an ownership share for Ref_Env_Nodes.

   procedure Add_Env
     (Self              : ${T.root_node.name};
      State             : in out PLE_Node_State;
      No_Parent         : Boolean;
      Transitive_Parent : Boolean;
      Names             : in out ${T.Symbol.array.name});
   --  Helper for Populate_Lexical_Env: create a new environment for Self, and
   --  update State accordingly.
   --
   --  State and No_Parent participate to the computation of the parent for
   --  this new environment. Transitive_Parent is directly forwarded to the
   --  lexical environment constructor.
   --
   --  If Names is not null, this also registers the new environment as a named
   --  env for all the given names. For PLE code brevity, Add_Env takes care of
   --  freeing Names before returning.

   procedure Pre_Env_Actions
     (Self            : ${T.root_node.name};
      State           : in out PLE_Node_State;
      Add_To_Env_Only : Boolean := False);
   --  Internal procedure that will execute all necessary lexical env actions
   --  for Node. This is meant to be called by Populate_Lexical_Env, and not by
   --  the user.

   procedure Post_Env_Actions
     (Self : ${T.root_node.name}; State : in out PLE_Node_State);
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
     (Self    : ${T.root_node.name};
      Node    : ${ctx.generic_list_type.name};
      Index   : Integer;
      Or_Null : Boolean := False) return ${T.root_node.name};
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --
   --  ``Self`` is used to provide context to the ``Property_Error`` that is
   --  raised when the index is invalid.

   procedure Free_User_Fields (Node : ${T.root_node.name});
   --  Free resources associated to user fields in ``Node``

   procedure Set_Parents (Node, Parent : ${T.root_node.name});
   --  Set Node.Parent to Parent, and initialize recursively the parent of all
   --  child nodes.

   procedure Destroy (Node : ${T.root_node.name});
   --  Free the resources allocated to this node and all its children

   --------------------------------------
   -- Environments handling (internal) --
   --------------------------------------

   function Create_Static_Lexical_Env
     (Parent            : Lexical_Env;
      Node              : ${T.root_node.name};
      Sym_Table         : Symbol_Table;
      Transitive_Parent : Boolean := False) return Lexical_Env;
   --  Wrapper around AST_Envs.Create_Lexical_Env. Create the environment and,
   --  if Node is not null, register the result for destruction in Node's
   --  analysis unit.

   function Get
     (Self  : ${T.root_node.name};
      A     : AST_Envs.Entity_Array;
      Index : Integer) return ${root_entity.name};
   --  Simple getter that raises a ``Property_Error`` on out-of-bound accesses
   --  (using ``Self`` to provide context for this error). Useful for code
   --  generation.

   function Group
     (Envs   : ${T.LexicalEnv.array.name};
      Env_Md : ${T.env_md.name} := No_Metadata) return ${T.LexicalEnv.name};
   --  Convenience wrapper for uniform types handling in code generation

   package ${T.root_node.array.pkg_vector} is
      new Langkit_Support.Vectors (${T.root_node.name});

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

   package Bare_Children_Record_Vectors is new Ada.Containers.Vectors
     (Positive, Bare_Child_Record);

   subtype Bare_Children_Vector is Bare_Children_Record_Vectors.Vector;

   function Children_And_Trivia
     (Node : ${T.root_node.name}) return Bare_Children_Vector;
   --  Implementation for Analysis.Children_And_Trivia

   % for astnode in ctx.node_types:
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

   ------------------------------------
   -- File reader internal interface --
   ------------------------------------

   type Internal_File_Reader is limited interface;
   type Internal_File_Reader_Access is access all Internal_File_Reader'Class;
   pragma No_Strict_Aliasing (Internal_File_Reader_Access);

   procedure Inc_Ref (Self : in out Internal_File_Reader) is abstract;
   ${ada_doc('langkit.file_reader_inc_ref', 3)}

   function Dec_Ref (Self : in out Internal_File_Reader) return Boolean
   is abstract;
   ${ada_doc('langkit.file_reader_dec_ref', 3)}

   procedure Read
     (Self        : Internal_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector) is abstract;
   ${ada_doc('langkit.file_reader_read', 3)}

   procedure Dec_Ref (File_Reader : in out Internal_File_Reader_Access);
   --  Call Dec_Ref on File_Reader.all and, if the ref-count reaches 0,
   --  dealloacte it.

   --------------------------------------
   -- Unit provider internal interface --
   --------------------------------------

   type Internal_Unit_Provider is limited interface;
   type Internal_Unit_Provider_Access is
      access all Internal_Unit_Provider'Class;
   pragma No_Strict_Aliasing (Internal_Unit_Provider_Access);

   procedure Inc_Ref (Provider : in out Internal_Unit_Provider) is abstract;
   ${ada_doc('langkit.unit_provider_inc_ref', 3)}

   function Dec_Ref (Provider : in out Internal_Unit_Provider) return Boolean
   is abstract;
   ${ada_doc('langkit.unit_provider_dec_ref', 3)}

   procedure Get_Unit_Location
     (Provider       : Internal_Unit_Provider;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Filename       : out Unbounded_String;
      PLE_Root_Index : out Positive) is abstract;
   --  See the public ``Get_Unit_Location`` procedure

   procedure Get_Unit_And_PLE_Root
     (Provider       : Internal_Unit_Provider;
      Context        : Internal_Context;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Charset        : String := "";
      Reparse        : Boolean := False;
      Unit           : out Internal_Unit;
      PLE_Root_Index : out Positive) is abstract;
   --  See the public ``Get_Unit_And_PLE_Root`` procedure

   procedure Dec_Ref (Provider : in out Internal_Unit_Provider_Access);

   type Resolved_Unit is record
      Unit           : Internal_Unit;
      Filename       : String_Access;
      PLE_Root_Index : Positive;
   end record;
   --  Cache entry for requests to unit providers

   type Resolved_Unit_Array is array (Analysis_Unit_Kind) of Resolved_Unit;
   --  One cache entry per unit kind, i.e. all cache entries needed for a given
   --  unit name.

   package Unit_Provider_Cache_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Resolved_Unit_Array,
      Equivalent_Keys => "=",
      Hash            => Hash);

   --------------------------------------
   -- Event handler internal interface --
   --------------------------------------

   type Internal_Event_Handler is limited interface;
   type Internal_Event_Handler_Access is
      access all Internal_Event_Handler'Class;
   pragma No_Strict_Aliasing (Internal_Event_Handler_Access);

   procedure Inc_Ref (Self : in out Internal_Event_Handler) is abstract;
   ${ada_doc('langkit.event_handler_inc_ref', 3)}

   function Dec_Ref (Self : in out Internal_Event_Handler) return Boolean
   is abstract;
   ${ada_doc('langkit.event_handler_dec_ref', 3)}

   procedure Unit_Requested_Callback
     (Self               : in out Internal_Event_Handler;
      Context            : Internal_Context;
      Name               : Text_Type;
      From               : Internal_Unit;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean) is null;

   procedure Unit_Parsed_Callback
     (Self     : in out Internal_Event_Handler;
      Context  : Internal_Context;
      Unit     : Internal_Unit;
      Reparsed : Boolean) is null;

   procedure Dec_Ref (Self : in out Internal_Event_Handler_Access);

   -----------------------------
   -- Lexical env cache stats --
   -----------------------------

   % if cache_collection_enabled:

   type Context_Env_Caches_Stats is record
      Entry_Count : Long_Long_Integer := 0;
      --  Current number of entries stored in all lexical env caches of all
      --  the analysis units owned by this context. A ``Long_Long_Integer``
      --  is enough to not have to worry about overflow, as it would mean
      --  reaching a cache capacity of hundreds of thousands of terabytes.

      Lookup_Count : Long_Long_Natural := 0;
      --  Current number of cache lookups done in any lexical env of any
      --  analysis unit owned by this context since its creation. A
      --  ``Long_Long_Natural`` is enough to ensure we will not overflow,
      --  in any realistic scenario.

      Previous_Lookup_Count : Long_Long_Natural := 0;
      --  Snapshot of the total number of cache lookups that were done in the
      --  lexical envs of any analysis unit of this context at the time the
      --  last collection was attempted.
   end record;

   type Unit_Env_Caches_Stats is record
      Entry_Count : Long_Long_Integer := 0;
      --  Current number of entries stored in lexical env caches of this
      --  analysis unit.

      Lookup_Count : Long_Long_Natural := 0;
      --  Current number of cache lookups done in any lexical env of this
      --  analysis unit since this unit was last collected.

      Hit_Count : Long_Long_Natural := 0;
      --  Current number of cache hits that have occurred in any lexical env of
      --  this analysis unit since this unit was last collected.

      Previous_Lookup_Count : Long_Long_Natural := 0;
      --  Snapshot of the total number of cache lookups that were done in any
      --  lexical env of this particular analysis unit when the last collection
      --  was *attempted* (it is updated even if this unit was not collected).

      Last_Overall_Lookup_Count : Long_Long_Natural := 0;
      --  Snapshot of the total number of cache lookups that were done in any
      --  lexical env of any analysis unit belonging to the same context as
      --  this one when this unit was last collected.
   end record;

   % endif

   ---------------------------------
   -- Analysis context definition --
   ---------------------------------

   type Analysis_Context_Type is limited record
      --  Start of ABI area. In order to perform fast checks from foreign
      --  languages, we maintain minimal ABI for analysis context: this allows
      --  us in language bindings to directly peek in this record rather than
      --  rely on (slow) calls to getters.

      Serial_Number : Version_Number;
      --  Serial number that is incremented each time this context allocation
      --  is released.

      Rewriting_Handle : System.Address;
      --  Pointer to the ``Rewriting_Handle_Record`` allocated for the current
      --  rewriting session, if there is one, ``Null_Address`` otherwise.

      Rewriting_Version : Version_Number;
      --  Serial number that is incremented each time a rewriting session
      --  associated to this context is destroyed.

      --  End of ABI area

      Initialized : Boolean;
      Ref_Count   : Natural;
      --  Whether this context is fully initialized, and when it is allocated,
      --  its number of ownership shares. Allocated contexts have 3 possible
      --  states:
      --
      --  * Acquired (not yet initialized, Ref_Count => 1, Initialized =>
      --    False): it can be either initialized or released.
      --
      --  * Initialized (Ref_Count > 0, Initialized => True): it can only be
      --    destroyed and released.
      --
      --  * Released (Ref_Count = 0, Initialized => False): it can only be
      --    acquired again.

      Units : Units_Maps.Map;
      --  Collection of analysis units loaded in this context

      Filenames : Langkit_Support.Internal.Analysis.Virtual_File_Cache;
      --  Cache for GNATCOLL.VFS.Virtual_File values

      Symbols : Symbol_Table;
      --  Symbol table used in this whole context

      Charset : Unbounded_String;
      --  Default charset to use in analysis units

      Tab_Stop : aliased Positive;
      --  Tab stop for the lexer to correctly interpret ASCII.HT input
      --  characters.

      With_Trivia : Boolean;
      --  Whether Trivia nodes were parsed and included in analysis units

      Root_Scope : Lexical_Env;
      --  The lexical scope that is shared amongst every compilation unit. Used
      --  to resolve cross file references.

      Named_Envs : NED_Maps.Map;
      --  Map env names to the corresponding named environment descriptors

      File_Reader : Internal_File_Reader_Access;
      --  Object to override the reading and decoding of source files

      Event_Handler : Internal_Event_Handler_Access;
      --  Object to provide event callbacks

      Unit_Provider : Internal_Unit_Provider_Access;
      --  Object to translate unit names to file names

      Unit_Provider_Cache : Unit_Provider_Cache_Maps.Map;
      --  Cache for the Unit_Provider.Get_Unit_And_PLE_Root primitive

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

      Cache_Version : Version_Number;
      --  Version number used to invalidate memoization caches in a lazy
      --  fashion. If an analysis unit's version number is strictly inferior to
      --  this, its memoization map should be cleared.

      Reparse_Cache_Version : Version_Number;
      --  Version number used to invalidate referenced envs caches. It is
      --  incremented only when a unit is reparsed in the context.

      Templates_Unit : Internal_Unit := No_Analysis_Unit;
      --  Special analysis unit used only as a containing unit to parse
      --  templates in the context of tree rewriting.

      Available_Rebindings : Env_Rebindings_Vectors.Vector;
      --  List of allocated-but-unused Env_Rebinding_Type records.
      --
      --  Each rebinding we allocate for an analysis context is deallocated
      --  only when the whole context is released, so when this list is not
      --  empty, we pick one of its element instead of allocating another
      --  rebinding (see the Acquire_Rebindings and Release_Rebindings
      --  subprograms).
      --
      --  Thanks to this mechanism, we have a very simple way to implement
      --  rebindings validity checking for nodes: once we have established that
      --  the node reference is valid regarding its context, we know that the
      --  rebindings pointer is valid, and thus we can just check the rebinding
      --  version number.

      % if cache_collection_enabled:

      Env_Caches_Stats : Context_Env_Caches_Stats;
      --  Holds stats about the usage of lexical env caches of all units
      --  belonging to this context.

      Env_Caches_Collection_Threshold : Long_Long_Integer :=
        ${ctx.config.library.cache_collection.threshold_increment};
      --  The number of total cache entries that ``Entry_Count`` in
      --  ``Env_Caches_Stats`` must reach before a new collection is attempted.

      % endif

      ${exts.include_extension(ctx.ext("analysis", "context", "components"))}
   end record;

   --  TODO??? (eng/toolchain/gnat#1400) Add the pragma No_Component_Reordering
   --  to Internal_Context_Stable_ABI to ensure that the ABI is respected.

   package Node_To_Named_Env_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => ${T.root_node.name},
      Element_Type    => Named_Env_Descriptor_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");

   package Boolean_Vectors is new Langkit_Support.Vectors (Boolean);

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

      Is_Internal : Boolean;
      --  Whether this unit is internal.
      --
      --  The use of file readers for parsing is disabled for internal units,
      --  which allows in-memory parsing for them even when a file reader is
      --  active.
      --
      --  It is illegal for users of public APIs to reparse an internal unit.
      --  Setting this flag allows generated libraries to create internal units
      --  to implement language internals and forbid library users to mess with
      --  this unit.

      Ast_Root : ${T.root_node.name};

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

      Rule : Grammar_Rule;
      --  The grammar rule used to parse this unit

      Ast_Mem_Pool : Bump_Ptr_Pool;
      --  This memory pool shall only be used for AST parsing. Stored here
      --  because it is more convenient, but one shall not allocate from it.

      Destroyables : Destroyable_Vectors.Vector;
      --  Collection of objects to destroy when destroying the analysis unit

      PLE_Roots_Starting_Token : Token_Index_Vectors.Vector;
      --  If this unit contains a list of PLE roots, then for each PLE root,
      --  this vector contains a reference to the first token that is part of
      --  it. Otherwise, this vector is empty.
      --
      --  This table is initialized after each parsing and allows to quickly
      --  look for the PLE root corresponding to some token, and thus to some
      --  node in this unit (see the ``Lookup_PLE_Root`` function).

      Env_Populated_Roots : Boolean_Vectors.Vector;
      --  For each PLE root in this unit, indicates whether
      --  Populate_Lexical_Env was called on it.
      --
      --  Note that this vector may contain less or more elements than the
      --  number of PLE roots in this unit: this allows not to run PLE twice on
      --  each root, and to keep track on which roots PLE should be run after a
      --  reparse. "Missing" elements in this vector are considered False.

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

      Exiled_Entries_In_NED : Exiled_Entry_In_NED_Vectors.Vector;
      --  Like Exiled_Entries, but for symbol/node associations exclusively
      --  handled by the named environments mechanism.
      --
      --  This list allows efficient removal of these entries from
      --  Named_Env_Descriptor.Foreign_Nodes components when unloading this
      --  unit.

      Exiled_Envs : Exiled_Env_Vectors.Vector;
      --  List of lexical environments created in this unit and whose parent is
      --  a named environment.
      --
      --  This list allows efficient removal for these envs from
      --  Named_Env_Descriptor.Foreign_Envs components when unloading this
      --  unit.

      Named_Envs : Named_Env_Vectors.Vector;
      --  List of named environment created in this unit.
      --
      --  This list allows efficient removal for these envs from the
      --  Named_Env_Descriptor.Envs components when unloading this unit.

      Nodes_With_Foreign_Env : Node_To_Named_Env_Maps.Map;
      --  Mapping from a node to its Self_Env's named env descriptor, for each
      --  node in this unit whose Self_Env is a named environment.
      --
      --  This mapping allows efficient removal for these nodes from the
      --  Named_Env_Descriptor.Nodes_With_Foreign_Env components when unloading
      --  this unit.

      Rebindings : aliased Env_Rebindings_Vectors.Vector;
      --  List of rebindings for which Old_Env and/or New_Env belong to this
      --  unit. When this unit gets destroyed or reparsed, these rebindings
      --  need to be destroyed too (see Destroy_Rebindings).

      % if ctx.has_memoization:
         Memoization_Map : Memoization_Maps.Map;
         --  Mapping of arguments tuple to property result for memoization
      % endif

      Cache_Version : Version_Number := 0;
      --  See the eponym field in Analysis_Context_Type

      % if cache_collection_enabled:

      Env_Caches_Stats : Unit_Env_Caches_Stats;
      --  Holds stats about the lookup cache usage of all lexical envs
      --  belonging to this context.

      % endif

      ${exts.include_extension(ctx.ext('analysis', 'unit', 'components'))}
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Context_Type, Internal_Context);

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Unit_Type, Internal_Unit);

   subtype Reparsed_Unit is Langkit_Support.Internal.Analysis.Reparsed_Unit;
   procedure Destroy (Reparsed : in out Reparsed_Unit)
     renames Langkit_Support.Internal.Analysis.Destroy;

   function Basename (Filename : String) return String;
   --  Return the base filename for String

   ----------------------------------------------------
   -- Implementation for analysis context primitives --
   ----------------------------------------------------

   function Allocate_Context return Internal_Context;
   ${ada_doc('langkit.allocate_context', 3)}

   procedure Initialize_Context
     (Context        : Internal_Context;
      Charset        : String;
      File_Reader    : Internal_File_Reader_Access;
      Unit_Provider  : Internal_Unit_Provider_Access;
      Event_Handler  : Internal_Event_Handler_Access;
      With_Trivia    : Boolean;
      Tab_Stop       : Positive);
   ${ada_doc('langkit.initialize_context', 3)}
   --  Implementation for ``Analysis.Create_Context``: call
   --  ``Allocate_Context`` to allocate an ``Internal_Context`` value, then
   --  call ``Initialize_Context`` to initialize it.
   --
   --  Having separate primitives for allocation/initialization allows library
   --  bindings to have a context wrapper (created between the two calls) ready
   --  when callbacks that happen during context initialization (for instance
   --  "unit parsed" events).

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
      Input             : Langkit_Support.Internal.Analysis.Lexer_Input;
      Rule              : Grammar_Rule;
      Is_Internal       : Boolean := False) return Internal_Unit;
   --  Helper for Get_From_File and Get_From_Buffer. Return the resulting
   --  analysis unit.
   --
   --  If ``Is_Internal`` is True, allow parsing from buffer even if
   --  ``Context`` has a file reader, and forbid later calls to
   --  Get_From_File/Get_From_Buffer/Reparse on the returned unit.

   function Has_Unit
     (Context : Internal_Context; Unit_Filename : String) return Boolean;
   --  Implementation for Analysis.Has_Unit

   function Get_From_File
     (Context  : Internal_Context;
      Filename : String;
      Charset  : String;
      Reparse  : Boolean;
      Rule     : Grammar_Rule) return Internal_Unit;
   --  Implementation for Analysis.Get_From_File

   function Get_From_Buffer
     (Context  : Internal_Context;
      Filename : String;
      Charset  : String;
      Buffer   : String;
      Rule     : Grammar_Rule) return Internal_Unit;
   --  Implementation for Analysis.Get_From_Buffer

   function Get_With_Error
     (Context  : Internal_Context;
      Filename : String;
      Error    : Text_Type;
      Charset  : String;
      Rule     : Grammar_Rule) return Internal_Unit;
   --  Implementation for Analysis.Get_With_Error

   % if cfg.library.defaults.unit_provider:

   function Get_From_Provider
     (Context : Internal_Context;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
      Charset : String;
      Reparse : Boolean) return Internal_Unit;
   --  Implementation for Analysis.Get_From_Provider

   % endif

   function Unit_Provider
     (Context : Internal_Context) return Internal_Unit_Provider_Access;
   --  Implementation for Analysis.Unit_Provider

   procedure Resolve_Unit
     (Context : Internal_Context;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
      Unit    : out Resolved_Unit);
   --  Completely resolve the requested unit. The result is cached: later calls
   --  for the same name/kind will have constant complexity.

   procedure Get_Unit_Location
     (Context        : Internal_Context;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Filename       : out String_Access;
      PLE_Root_Index : out Positive);
   --  Caching wrapper around Context.Unit_Provider.Get_Unit_Location

   procedure Get_Unit_And_PLE_Root
     (Context        : Internal_Context;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Unit           : out Internal_Unit;
      PLE_Root_Index : out Positive);
   --  Caching wrapper around Context.Unit_Provider.Get_Unit_And_PLE_Root

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

   procedure Destroy (Context : Internal_Context)
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

   procedure Populate_Lexical_Env
     (Unit           : Internal_Unit;
      PLE_Root_Index : Positive
      ## To simplify code generation, generate the PLE_Root_Index argument even
      ## though we always expect it to be 1 when there are no PLE root for this
      ## language spec. However, still to simplify code generation, give it a
      ## default expression in that case.
      % if not ctx.ple_unit_root:
         := 1
      % endif
      );
   --  Implementation for Analysis.Populate_Lexical_Env

   procedure Populate_Lexical_Env_For_Unit (Node : ${T.root_node.name});
   --  Populate the lexical environment for the PLE root that owns ``Node``, or
   --  for the whole unit if there is no PLE root.

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

   function Lookup_Token
     (Unit : Internal_Unit; Sloc : Source_Location) return Token_Reference;
   --  Implementation for Analysis.Lookup_Token

   procedure Lookup_PLE_Root
     (Node  : ${T.root_node.name};
      Root  : out ${T.root_node.name};
      Index : out Natural);
   --  Look for the PLE root that owns this node. If there is one, assign it to
   --  ``Root`` and assign its index in the list of PLE roots to ``Index``. If
   --  there is none, set ``Root`` to the unit root node and ``Index`` to 0.

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

   function Get_Line
     (Unit : Internal_Unit; Line_Number : Positive) return Text_Type;
   --  Return the line of text at line number ``Line_Number``

   procedure Do_Parsing
     (Unit   : Internal_Unit;
      Input  : Langkit_Support.Internal.Analysis.Lexer_Input;
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

   procedure Remove_Named_Envs
     (Unit                      : Internal_Unit;
      Named_Envs_Needing_Update : in out NED_Maps.Map);
   --  Remove envs that belong to Unit from all relevant NEDs, and keep track
   --  in Named_Env_Needing_Update of the env names whose env with precedence
   --  must change because of this.

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

   type Node_Safety_Net is record
      Context        : Internal_Context;
      Context_Serial : Version_Number;
      --  Analysis context and serial number at the time this safety net was
      --  produced.

      Unit         : Internal_Unit;
      Unit_Version : Version_Number;
      --  Analysis unit and unit version at the time this safety net was
      --  produced.

      Rebindings_Version : Version_Number;
      --  Version of the associated rebinding at the time this safety net was
      --  procuded.
   end record;
   --  Information to embed in public APIs, used to check before accessing data
   --  that the said-data is still valid.

   No_Node_Safety_Net : constant Node_Safety_Net := (null, 0, null, 0, 0);

   function String_To_Symbol
     (Self    : ${T.root_node.name};
      Context : Internal_Context;
      S       : ${T.String.name}) return Symbol_Type;
   --  Convert ``S`` into the corresponding symbol, raising a
   --  ``Property_Error`` if symbol canonicalization fails (using ``Self`` to
   --  provide context for this error). If ``S`` is empty, just return
   --  ``null``.

   function Solve_Wrapper
     (R            : Solver.Relation;
      Context_Node : ${T.root_node.name}) return Boolean;
   --  Wrapper for Langkit_Support.Adalog.Solve; will handle setting the debug
   --  strings in the equation if in debug mode.

   function Solve_With_Diagnostics
     (R            : Solver.Relation;
      Context_Node : ${T.root_node.name}) return Internal_Solver_Result;
   --  Like ``Solve_Wrapper``, but returns a ``Internal_Solver_Result`` which
   --  contains solver diagnostics in case of resolution failure.

   generic
      type T (<>) is limited private;
      type T_Access is access all T;
      with procedure Destroy (Object : in out T_Access);
   procedure Register_Destroyable_Gen
     (Unit : Internal_Unit; Object : T_Access);
   --  Generic procedure to register an object so that it is automatically
   --  destroyed when Unit is destroyed.

   function New_Unit_String
     (Unit : Internal_Unit; Str : String) return String_Access;
   --  This function allocates a string whose lifetime will be associated with
   --  ``Unit``.

private
   --  We only have a private part to defer the initialization of struct
   --  constants. This allows us to circumvent circularity problems between
   --  arrays and structs.

   % for struct_type in ctx.struct_types:
      % if not struct_type.has_early_decl:
         ${struct_types.nullexpr_decl(struct_type)}
      % endif
   % endfor

end ${ada_lib_name}.Implementation;
