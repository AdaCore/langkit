## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="enum_types"    file="enum_types_ada.mako" />
<%namespace name="list_types"    file="list_types_ada.mako" />
<%namespace name="struct_types"  file="struct_types_ada.mako" />
<%
   root_node_array = T.root_node.array_type()
   no_builtins = lambda ts: filter(lambda t: not t.is_builtin(), ts)
   library_private_field = lambda f: not library_public_field(f)
%>

with Ada.Containers.Hashed_Maps;
with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;

with System;

with Adalog.Abstract_Relation;   use Adalog.Abstract_Relation;
with Adalog.Eq_Same;

with Langkit_Support.Bump_Ptr;    use Langkit_Support.Bump_Ptr;
with Langkit_Support.Bump_Ptr.Vectors;
with Langkit_Support.Cheap_Sets;
with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Extensions;  use Langkit_Support.Extensions;
with Langkit_Support.Iterators;
with Langkit_Support.Lexical_Env;
with Langkit_Support.Slocs;       use Langkit_Support.Slocs;
with Langkit_Support.Symbols;     use Langkit_Support.Symbols;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Langkit_Support.Tree_Traversal_Iterator;
with Langkit_Support.Vectors;

with ${_self.ada_api_settings.lib_name}.Lexer;
use ${_self.ada_api_settings.lib_name}.Lexer;
use ${_self.ada_api_settings.lib_name}.Lexer.Token_Data_Handlers;

--  This package provides types and primitives to analyze source files as
--  analysis units.
--
--  This is the entry point to parse and process an unit: first create an
--  analysis context with Create, then get analysis units out of it using
--  Get_From_File and/or Get_From_Buffer.

package ${_self.ada_api_settings.lib_name}.Analysis is

   type Analysis_Context is private;
   ${ada_doc('langkit.analysis_context_type', 3)}

   type Analysis_Unit is private;
   ${ada_doc('langkit.analysis_unit_type', 3)}

   type Grammar_Rule is (
      % for i, name in enumerate(_self.user_rule_names):
         % if i > 0:
            ,
         % endif
         ${Name.from_lower(name)}_Rule
      % endfor
   );
   ${ada_doc('langkit.grammar_rule_type')}

   type ${root_node_value_type} is abstract tagged private;
   --  This "by-value" type is public to expose the fact that the various
   --  AST nodes are a hierarchy of tagged types, but it is not intended to be
   --  used directly, hence the "_Type" suffix. Please use instead the
   --  class-wide types such at the one below.

   type ${root_node_type_name} is access all ${root_node_value_type}'Class;
   --  Most generic AST node type

   Property_Error : exception;
   ${ada_doc('langkit.property_error', 3)}

   type Token_Type is private;
   ${ada_doc('langkit.token_type')}

   No_Token : constant Token_Type;

   -------------------------
   -- Unit file providers --
   -------------------------

   type Unit_Kind is (Unit_Specification, Unit_Body);
   ${ada_doc('langkit.unit_kind_type', 3)}

   Invalid_Unit_Name_Error : exception;
   ${ada_doc('langkit.invalid_unit_name_error')}

   type Unit_File_Provider_Interface is limited interface;
   type Unit_File_Provider_Access is
      access Unit_File_Provider_Interface'Class;
   type Unit_File_Provider_Access_Cst is
      access constant Unit_File_Provider_Interface'Class;
   ${ada_doc('langkit.unit_file_provider_type', 3)}

   function Get_File
     (Provider : Unit_File_Provider_Interface;
      Node     : ${root_node_type_name};
      Kind     : Unit_Kind)
      return String is abstract;
   ${ada_doc('langkit.unit_file_provider_get_file_from_node', 3)}

   function Get_File
     (Provider : Unit_File_Provider_Interface;
      Name     : Text_Type;
      Kind     : Unit_Kind)
      return String is abstract;
   ${ada_doc('langkit.unit_file_provider_get_file_from_name', 3)}

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Unit_File_Provider_Interface'Class, Unit_File_Provider_Access);

   ---------------------------------
   -- Analysis context primitives --
   ---------------------------------

   function Create
     (Charset : String := ${string_repr(_self.default_charset)}
      % if _self.default_unit_file_provider:
         ; Unit_File_Provider : Unit_File_Provider_Access_Cst := null
      % endif
     ) return Analysis_Context;
   ${ada_doc('langkit.create_context', 3)}

   procedure Inc_Ref (Context : Analysis_Context);
   ${ada_doc('langkit.context_incref')}

   procedure Dec_Ref (Context : in out Analysis_Context);
   ${ada_doc('langkit.context_decref')}

   function Get_From_File
     (Context     : Analysis_Context;
      Filename    : String;
      Charset     : String := "";
      Reparse     : Boolean := False;
      With_Trivia : Boolean := False;
      Rule        : Grammar_Rule :=
         ${Name.from_lower(_self.main_rule_name)}_Rule)
      return Analysis_Unit;
   ${ada_doc('langkit.get_unit_from_file', 3)}

   function Get_From_Buffer
     (Context     : Analysis_Context;
      Filename    : String;
      Charset     : String := "";
      Buffer      : String;
      With_Trivia : Boolean := False;
      Rule        : Grammar_Rule :=
         ${Name.from_lower(_self.main_rule_name)}_Rule)
      return Analysis_Unit;
   ${ada_doc('langkit.get_unit_from_buffer', 3)}

   % if _self.default_unit_file_provider:

   function Get_From_Provider
     (Context     : Analysis_Context;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False;
      With_Trivia : Boolean := False;
      Rule        : Grammar_Rule :=
         ${Name.from_lower(_self.main_rule_name)}_Rule)
      return Analysis_Unit;
   ${ada_doc('langkit.get_unit_from_provider', 3)}

   function Unit_File_Provider
     (Context : Analysis_Context)
      return Unit_File_Provider_Access_Cst;
   --  Object to translate unit names to file names
   % endif

   procedure Remove (Context   : Analysis_Context;
                     File_Name : String);
   ${ada_doc('langkit.remove_unit', 3)}

   procedure Destroy (Context : in out Analysis_Context);
   ${ada_doc('langkit.destroy_context', 3)}

   procedure Inc_Ref (Unit : Analysis_Unit);
   ${ada_doc('langkit.unit_incref')}

   procedure Dec_Ref (Unit : Analysis_Unit);
   ${ada_doc('langkit.unit_decref')}

   function Get_Context (Unit : Analysis_Unit) return Analysis_Context;
   ${ada_doc('langkit.unit_context')}

   procedure Reparse (Unit : Analysis_Unit; Charset : String := "");
   ${ada_doc('langkit.unit_reparse_file', 3)}

   procedure Reparse
     (Unit    : Analysis_Unit;
      Charset : String := "";
      Buffer  : String);
   ${ada_doc('langkit.unit_reparse_buffer', 3)}

   procedure Populate_Lexical_Env (Unit : Analysis_Unit);
   ${ada_doc('langkit.unit_populate_lexical_env')}

   function Get_Filename (Unit : Analysis_Unit) return String;
   ${ada_doc('langkit.unit_filename')}

   function Has_Diagnostics (Unit : Analysis_Unit) return Boolean;
   ${ada_doc('langkit.unit_has_diagnostics', 3)}

   function Diagnostics (Unit : Analysis_Unit) return Diagnostics_Array;
   ${ada_doc('langkit.unit_diagnostics', 3)}

   function Root (Unit : Analysis_Unit) return ${root_node_type_name};
   ${ada_doc('langkit.unit_root', 3)}

   function First_Token (Unit : Analysis_Unit) return Token_Type;
   ${ada_doc('langkit.unit_first_token', 3)}

   function Last_Token (Unit : Analysis_Unit) return Token_Type;
   ${ada_doc('langkit.unit_last_token', 3)}

   procedure Dump_Lexical_Env (Unit : Analysis_Unit);
   --  Debug helper: output the lexical envs for given analysis unit

   procedure Print (Unit : Analysis_Unit);
   --  Debug helper: output the AST and eventual diagnostic for this unit on
   --  standard output.

   procedure PP_Trivia (Unit : Analysis_Unit);
   --  Debug helper: output a minimal AST with mixed trivias

   procedure Reference_Unit (From, Referenced : Analysis_Unit);
   --  Set the Referenced unit as being referenced from the From unit. This is
   --  useful for visibility purposes, and is mainly meant to be used in the
   --  env hooks.

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

   % if ctx.env_metadata:
   ${struct_types.public_incomplete_decl(ctx.env_metadata)}
   ${struct_types.public_decl(ctx.env_metadata)}

   function Combine
     (L, R : ${ctx.env_metadata.name()}) return ${ctx.env_metadata.name()};
   --  The combine function on environments metadata does a boolean Or on every
   --  boolean component of the env metadata.

   % else:
   type Dummy_Metadata is new Integer;
   No_Metadata : constant Dummy_Metadata := 0;
   function Combine (L, R : Dummy_Metadata) return Dummy_Metadata is (0);
   --  This type and constants are added waiting for a real metadata type
   % endif

   function Can_Reach (El, From : ${root_node_type_name}) return Boolean;
   --  Return whether El can reach From, from a sequential viewpoint. If
   --  elements are declared in different units, it will always return True,
   --  eg. this does not handle general visibility issues, just sequentiality of
   --  declarations.

   type Env_Getter_State_T is record
      Node        : ${root_node_type_name};
   end record;

   package AST_Envs is new Langkit_Support.Lexical_Env
     (Element_T        => ${root_node_type_name},
      Element_Metadata =>
         ${ctx.env_metadata.name() if ctx.env_metadata else "Dummy_Metadata" },
      No_Element       => null,
      Empty_Metadata   => No_Metadata,
      Combine          => Combine,
      Getter_State_T   => Env_Getter_State_T);

   --  The following subtypes are introduced to ease code generation, so we
   --  don't have to deal with the AST_Envs suffix.

   subtype Lexical_Env is AST_Envs.Lexical_Env;
   subtype Env_Element is AST_Envs.Env_Element;

   ## Declare arrays of lexical environments here because we need them for the
   ## Group operation below.
   ${array_types.public_decl(LexicalEnvType.array_type())}

   ## See ASTNode.env_element
   ${array_types.public_decl(T.root_node.env_element().array_type())}

   ## Declare arrays of root nodes here since some primitives rely on it and
   ## since the declarations require AST_Envs.
   ${array_types.public_decl(root_node_array)}

   -----------------------------
   -- Miscellanous operations --
   -----------------------------

   ## Output enumerators so that all concrete AST_Node subclasses get their own
   ## kind. Nothing can be an instance of an abstract subclass, so these do not
   ## need their own kind.
   type ${root_node_kind_name} is
     (${', '.join(cls.ada_kind_name()
                  for cls in _self.astnode_types
                  if not cls.abstract)});
   --  AST node concrete types

   for ${root_node_kind_name} use
     (${', '.join('{} => {}'.format(cls.ada_kind_name(),
                                    ctx.node_kind_constants[cls])
                  for cls in _self.astnode_types
                  if not cls.abstract)});

   ## Output subranges to materialize abstract classes as sets of their
   ## concrete subclasses.
   % for cls in _self.astnode_types:
      <% subclasses = cls.concrete_subclasses() %>
      % if cls.abstract and subclasses:
         subtype ${cls.ada_kind_name()} is
            ${root_node_kind_name} range
               ${subclasses[0].ada_kind_name()}
               .. ${subclasses[-1].ada_kind_name()};
      % endif
   % endfor

   function Kind (Node : access ${root_node_value_type})
                  return ${root_node_kind_name} is abstract;
   function Kind_Name
     (Node : access ${root_node_value_type}) return String is abstract;
   --  Return the concrete kind for Node

   function Is_Empty_List
     (Node : access ${root_node_value_type}) return Boolean
   is (False);
   --  Return whether Node is an empty list (so this is wrong for all nodes
   --  that are not lists).

   function Node_Env
     (Node : access ${root_node_value_type})
      return AST_Envs.Lexical_Env;
   ${ada_doc(T.root_node._fields['node_env'], 3)}

   function Children_Env
     (Node : access ${root_node_value_type})
      return AST_Envs.Lexical_Env;
   ${ada_doc(T.root_node._fields['children_env'], 3)}

   function Get_Unit
     (Node : access ${root_node_value_type}'Class)
      return Analysis_Unit;
   ${ada_doc('langkit.node_unit', 3)}

   -------------------------------
   -- Tree traversal operations --
   -------------------------------

   function Child_Count (Node : access ${root_node_value_type})
                         return Natural is abstract;
   --  Return the number of children Node has

   function First_Child_Index
     (Node : access ${root_node_value_type}'Class)
      return Natural
   is (1);
   --  Return the index of the first child Node has

   function Last_Child_Index
     (Node : access ${root_node_value_type}'Class)
      return Natural
   is (Node.Child_Count);
   --  Return the index of the last child Node has, or 0 if there is no child

   procedure Get_Child
     (Node            : access ${root_node_value_type};
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out ${root_node_type_name}) is abstract;
   --  Get the Index'th child of Node, storing it into Result. Child indexing
   --  is 1-based. Store in Index_In_Bounds whether Node had such a child; if
   --  not, the content of Result is undefined.

   function Child (Node  : access ${root_node_value_type}'Class;
                   Index : Positive) return ${root_node_type_name};
   --  Return the Index'th child of Node, or null if Node has no such child

   function Children
     (Node : access ${root_node_value_type}'Class)
     return ${root_node_type_name}_Arrays.Array_Type;
   --  Return an array containing all the children of Node.
   --  This is an alternative to the Child/Child_Count pair, useful if you want
   --  the convenience of Ada arrays, and you don't care about the small
   --  performance hit of creating an array.

   function Parents
     (Node         : access ${root_node_value_type}'Class;
      Include_Self : Boolean := True)
      return ${root_node_array.name()};
   --  Return the list of parents for this node. This node included in the list
   --  iff Include_Self.

   function Parent
     (Node : access ${root_node_value_type}'Class)
     return ${root_node_type_name};

   type Visit_Status is (Into, Over, Stop);
   --  Helper type to control the AST node traversal process. See Traverse.

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
                               Data : in out Data_type)
                               return Visit_Status;
      Data  : in out Data_Type)
      return Visit_Status;
   --  This is the same as the first Traverse function except it accepts an
   --  argument that is passed to all Visit calls.
   --
   --  If Reset_After_Traversal is True, the Data formal is left unchanged when
   --  Traverse_With_Data returns no matter what Visit does. Visit can change
   --  it otherwise.

   package ${root_node_type_name}_Iterators is new Langkit_Support.Iterators
     (Element_Type => ${root_node_type_name},
      Element_Vectors => ${root_node_type_name}_Vectors);

   type Traverse_Iterator is
     limited new ${root_node_type_name}_Iterators.Iterator
     with private;

   function Traverse
     (Root : access ${root_node_value_type}'Class)
      return Traverse_Iterator;
   --  Return an iterator that yields all AST nodes under Root (included) in a
   --  prefix DFS (depth first search) fasion.

   type ${root_node_type_name}_Predicate_Type is interface;
   type ${root_node_type_name}_Predicate is
      access all ${root_node_type_name}_Predicate_Type'Class;
   --  Predicate on AST nodes.
   --
   --  Useful predicates often rely on values from some context, so predicates
   --  that are mere accesses to a function are not powerful enough. Having a
   --  full interface for this makes it possible to package both the predicate
   --  code and some data it needs.

   function Evaluate
     (P : access ${root_node_type_name}_Predicate_Type;
      N : ${root_node_type_name})
      return Boolean is abstract;
   --  Return the value of the predicate for the N node

   procedure Destroy is new Ada.Unchecked_Deallocation
     (${root_node_type_name}_Predicate_Type'Class,
      ${root_node_type_name}_Predicate);

   type Find_Iterator is limited
     new ${root_node_type_name}_Iterators.Iterator
     with private;
   --  Iterator type for Find (see below)

   overriding function Next
     (It       : in out Find_Iterator;
      Element  : out ${root_node_type_name}) return Boolean;

   type Local_Find_Iterator is limited
      new ${root_node_type_name}_Iterators.Iterator
   with private;
   --  Iterator type for the Find function that takes an access to function. It
   --  is called Local_Find_Iterator because if you use a locally declared
   --  function, the iterator itself will only be valid in the scope of the
   --  function.

   overriding function Next
     (It      : in out Local_Find_Iterator;
      Element : out ${root_node_type_name})
      return Boolean;

   function Find
     (Root      : access ${root_node_value_type}'Class;
      Predicate : access function (N : ${root_node_type_name}) return Boolean)
      return Local_Find_Iterator;
   --  Return an iterator that yields all AST nodes under Root (included) that
   --  satisfy the Predicate predicate.

   function Find
     (Root      : access ${root_node_value_type}'Class;
      Predicate : ${root_node_type_name}_Predicate)
      return Find_Iterator;
   --  Return an iterator that yields all AST nodes under Root (included) that
   --  satisfy the Predicate predicate. Predicate will be destroyed when
   --  Find_Iterator is exhausted.

   function Find_First
     (Root      : access ${root_node_value_type}'Class;
      Predicate : ${root_node_type_name}_Predicate)
      return ${root_node_type_name};
   --  Return the first found AST node under Root (included) that satisfies the
   --  Pred, or return null if there is no such node.

   type ${root_node_type_name}_Kind_Filter is
      new ${root_node_type_name}_Predicate_Type with
   record
      Kind : ${root_node_kind_name};
   end record;
   --  Predicate that returns true for all AST nodes of some kind

   function Evaluate
     (P : access ${root_node_type_name}_Kind_Filter;
      N : ${root_node_type_name})
      return Boolean;

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

   function Sloc_Range (Node : access ${root_node_value_type}'Class;
                        Snap : Boolean := False) return Source_Location_Range;
   --  Return the source location range corresponding to the set of tokens from
   --  which Node was parsed.
   --
   --  TODO??? Document the Snap formal.

   function Compare (Node : access ${root_node_value_type}'Class;
                     Sloc : Source_Location;
                     Snap : Boolean := False) return Relative_Position;
   --  Compare Sloc to the sloc range of Node.
   --
   --  TODO??? Document the Snap formal.

   function Lookup (Node : access ${root_node_value_type}'Class;
                    Sloc : Source_Location;
                    Snap : Boolean := False) return ${root_node_type_name};
   --  Look for the bottom-most AST node whose sloc range contains Sloc. Return
   --  it, or null if no such node was found.
   --
   --  TODO??? Document the Snap formal.

   -----------------------
   -- Lexical utilities --
   -----------------------

   type Token_Data_Type is record
      Kind       : Token_Kind;
      --  Kind for this token

      Text       : Text_Cst_Access;
      --  Text as found in the original source file, or null if not available

      Sloc_Range : Source_Location_Range;
      --  Source location range for this token. Note that the end bound is
      --  exclusive.
   end record;

   function "<" (Left, Right : Token_Type) return Boolean;
   --  Assuming Left and Right belong to the same analysis unit, return whether
   --  Left came before Right in the source file.

   function Next (Token : Token_Type) return Token_Type;
   ${ada_doc('langkit.token_next')}

   function Previous (Token : Token_Type) return Token_Type;
   ${ada_doc('langkit.token_previous')}

   function Data (T : Token_Type) return Token_Data_Type;
   --  Return the data associated to T

   function Is_Equivalent (L, R : Token_Type) return Boolean;
   ${ada_doc('langkit.token_is_equivalent')}

   function Image (Token : Token_Type) return String;
   --  Debug helper: return a human-readable text to represent a token

   function Text (Token : Token_Type) return Text_Type
   is (Data (Token).Text.all);
   --  Return the text of the token as Text_Type

   function Text (Token : Token_Type) return String
   is (Image (Data (Token).Text.all));
   --  Return the text of the token as String

   type Child_Or_Trivia is (Child, Trivia);
   --  Discriminator for the Child_Record type

   type Child_Record (Kind : Child_Or_Trivia := Child) is record
      case Kind is
         when Child =>
            Node : ${root_node_type_name};
         when Trivia =>
            Trivia : Token_Data_Type;
      end case;
   end record;
   --  Variant that holds either an AST node or a token

   package Children_Vectors is new Langkit_Support.Vectors (Child_Record);
   package Children_Arrays renames Children_Vectors.Elements_Arrays;

   function Children_With_Trivia
     (Node : access ${root_node_value_type}'Class)
      return Children_Arrays.Array_Type;
   --  Return the children of this node interleaved with Trivia token nodes, so
   --  that:
   --  - Every trivia contained between Node.Start_Token and Node.End_Token - 1
   --    will be part of the returned array;
   --  - Nodes and trivias will be lexically ordered.

   function Token_Start
     (Node : access ${root_node_value_type}'Class)
      return Token_Type;
   --  Return the first token used to parse Node

   function Token_End
     (Node : access ${root_node_value_type}'Class)
      return Token_Type;
   --  Return the last token used to parse Node

   --------------------
   -- Token Iterator --
   --------------------

   type Token_Iterator is private
      with Iterable => (First       => First_Token,
                        Next        => Next_Token,
                        Has_Element => Has_Element,
                        Element     => Element);
   --  This type allows iteration on a range of tokens

   function First_Token (Self : Token_Iterator) return Token_Type;
   function Next_Token
     (Self : Token_Iterator; Tok : Token_Type) return Token_Type;
   function Has_Element
     (Self : Token_Iterator; Tok : Token_Type) return Boolean;
   function Element (Self : Token_Iterator; Tok : Token_Type) return Token_Type;

   function Token_Range
     (Node : access ${root_node_value_type}'Class)
      return Token_Iterator;
   --  Return an iterator on the range of tokens encompassed by Node

   -------------------
   -- Debug helpers --
   -------------------

   function Image
     (Node : access ${root_node_value_type}) return String is abstract;
   --  Debug helper: return a textual representation of this node and all its
   --  children.

   function Short_Image
     (Node : access ${root_node_value_type})
      return Text_Type;
   --  Debug helper: return a short representation of the string, containing
   --  just the kind name and the sloc.

   procedure Print (Node   : access ${root_node_value_type};
                    Prefix : String := "") is abstract;
   --  Debug helper: print to standard output Node and all its children. Prefix
   --  is prepended to each output line.

   procedure PP_Trivia
     (Node  : access ${root_node_value_type}'Class;
      Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children along
   --  with the trivia associated to them. Prefix is prepended to each output
   --  line.


   procedure Dump_Lexical_Env
     (Node     : access ${root_node_value_type}'Class;
      Root_Env : AST_Envs.Lexical_Env);
   --  Debug helper: dump the lexical environment of Node, and consequently any
   --  nested lexical environment. Used for debugging/testing purpose. Pass the
   --  root env explicitly so that we can tag it properly in the output.

   procedure Dump_One_Lexical_Env
     (Self          : AST_Envs.Lexical_Env;
      Env_Id        : String := "";
      Parent_Env_Id : String := "");
   --  Debug helper: Dumps one lexical env. You can supply ids for env and its
   --  parent, so that they will be identified in the output.

   procedure Assign_Names_To_Logic_Vars
     (Node : access ${root_node_value_type}'Class);
   --  Debug helper: Assign names to every logical variable in the root node,
   --  so that we can trace logical variables.

   ------------------------------
   -- Root AST node properties --
   ------------------------------

   % for prop in T.root_node.get_properties( \
         include_inherited=False, \
         predicate=library_public_field):
      ${prop.prop_decl}
   % endfor

   ---------------------------
   -- Adalog instantiations --
   ---------------------------

   function El_Image (N : ${root_node_type_name}) return String is
   (if N /= null then Image (N.Short_Image) else "None");

   package Eq_Node is new Adalog.Eq_Same (${root_node_type_name}, El_Image);
   subtype Logic_Var is Eq_Node.Refs.Raw_Var;
   subtype Logic_Var_Record is Eq_Node.Refs.Var;
   Null_Var : constant Logic_Var := null;
   Null_Var_Record : constant Logic_Var_Record := (Reset => True, others => <>);

   subtype Logic_Equation is Relation;
   Null_Logic_Equation : constant Logic_Equation := null;

   -----------------------
   -- Enumeration types --
   -----------------------

   function Image (Value : Boolean) return String is
     (if Value then "True" else "False");

   % for cls in _self.sorted_types(_self.enum_types):
   ${enum_types.public_decl(cls)}
   % endfor

   -----------------------------------------------
   -- Structure types (incomplete declarations) --
   -----------------------------------------------

   % for struct_type in no_builtins(_self.struct_types):
   ${struct_types.public_incomplete_decl(struct_type)}
   % endfor

   -----------------------------------------------------
   -- ASTNode derived types (incomplete declarations) --
   -----------------------------------------------------

   type ${generic_list_value_type} is
      abstract new ${root_node_value_type}
      with private;
   --  Base type for all lists of AST node subclasses

   type ${generic_list_type_name} is
      access all ${generic_list_value_type}'Class;

   overriding function Image
     (Node : access ${generic_list_value_type}) return String;

   overriding function Child_Count
     (Node : access ${generic_list_value_type}) return Natural;

   overriding procedure Get_Child
     (Node            : access ${generic_list_value_type};
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out ${root_node_type_name});

   overriding procedure Print
     (Node : access ${generic_list_value_type}; Prefix : String := "");

   overriding function Is_Empty_List
     (Node : access ${generic_list_value_type})
      return Boolean
   is
     (Child_Count (${root_node_type_name} (Node)) = 0);

   % for astnode in no_builtins(_self.astnode_types):
     % if not astnode.is_list_type:
       ${astnode_types.public_incomplete_decl(astnode)}
     % endif
   % endfor

   % for astnode in _self.astnode_types:
      % if astnode.is_root_list_type:
         ${list_types.public_incomplete_decl(astnode.element_type())}
      % elif astnode.is_list_type:
         ${astnode_types.public_incomplete_decl(astnode)}
      % endif
   % endfor

   -----------------------------------------
   -- Structure types (full declarations) --
   -----------------------------------------

   % for struct_type in no_builtins(_self.struct_types):
   ${struct_types.public_decl(struct_type)}
   % endfor

   -----------------
   -- Array types --
   -----------------

   --  We implement array types as discriminated records so that binding to C
   --  can be done without copy.

   --  TODO??? This is likely to change in the near future: we would like to
   --  have here pure Ada arrays instead.

   % for array_type in _self.sorted_types(_self.array_types):
   % if array_type.element_type().should_emit_array_type:
   ${array_types.public_decl(array_type)}
   % endif
   % endfor

   -----------------------------------------------
   -- ASTNode derived types (full declarations) --
   -----------------------------------------------

   --  See above for overriden primitive operations documentations

   % for astnode in no_builtins(_self.astnode_types):
     % if not astnode.is_list_type:
       ${astnode_types.public_decl(astnode)}
     % endif
   % endfor

   % for astnode in _self.astnode_types:
      % if astnode.is_root_list_type:
         ${list_types.public_decl(astnode.element_type())}
      % elif astnode.is_list_type:
         ${astnode_types.public_decl(astnode)}
      % endif
   % endfor

private

   use AST_Envs;

   type Analysis_Context_Type;
   type Analysis_Unit_Type;

   type Analysis_Context is access all Analysis_Context_Type;
   type Analysis_Unit is access all Analysis_Unit_Type;

   package Units_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Analysis_Unit,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   type Analysis_Context_Type is record
      Ref_Count  : Natural;
      Units_Map  : Units_Maps.Map;
      Symbols    : Symbol_Table;

      Charset    : Unbounded_String;
      --  Default charset to use in analysis units

      Root_Scope : AST_Envs.Lexical_Env;
      --  The lexical scope that is shared amongst every compilation unit. Used
      --  to resolve cross file references.

      % if _self.default_unit_file_provider:
      Unit_File_Provider : Unit_File_Provider_Access_Cst;
      --  Object to translate unit names to file names
      % endif
   end record;

   procedure Reset_Property_Caches (Context : Analysis_Context);
   --  Call Reset_Property_Caches on all units Context contains

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

   package Analysis_Unit_Sets
   is new Langkit_Support.Cheap_Sets (Analysis_Unit, null);

   type Containing_Env_Element is record
      Env  : Lexical_Env;
      Key  : Symbol_Type;
      Node : ${root_node_type_name};
   end record;

   package Containing_Envs is new Langkit_Support.Vectors
     (Containing_Env_Element);

   type Lex_Env_Data_Type is record
      Is_Contained_By : Containing_Envs.Vector;
      Contains        : ${root_node_type_name}_Vectors.Vector;
   end record;
   type Lex_Env_Data is access all Lex_Env_Data_Type;

   procedure Destroy (Self : in out Lex_Env_Data_Type);
   --  Destroy data associated to lexical environments

   procedure Destroy (Self : in out Lex_Env_Data);
   --  Likewise, but also free the memory allocated to Self

   procedure Remove_Exiled_Entries (Self : in out Lex_Env_Data_Type);
   --  Remove lex env entries that references some of the unit's nodes, in
   --  lexical environments not owned by the unit.

   procedure Reroot_Foreign_Nodes
     (Self : in out Lex_Env_Data_Type; Root_Scope : Lexical_Env);
   --  Re-create entries for nodes that are keyed in one of the unit's lexical
   --  envs.

   type Analysis_Unit_Type is record
      Context           : Analysis_Context;
      --  The owning context for this analysis unit

      Ref_Count         : Natural;
      --  Ref count for the analysis unit. Note that in the Ada API you'll
      --  still have to call Inc_Ref/Dec_Ref manually.

      AST_Root          : ${root_node_type_name};

      File_Name         : Unbounded_String;
      --  The originating name for this analysis unit. This should be set even
      --  if the analysis unit was parsed from a buffer.

      Charset           : Unbounded_String;
      --  The parsing charset for this analysis unit, as a string. If the
      --  charset used actually came from a byte order mark, this is
      --  nevertheless set to the one the user requested.

      TDH               : aliased Token_Data_Handler;
      --  The token data handler that handles all token data during parsing and
      --  owns it afterwards.

      Diagnostics       : Diagnostics_Vectors.Vector;
      --  The list of diagnostics produced for this analysis unit

      With_Trivia       : Boolean;
      --  Whether Trivia nodes were parsed and included in this analysis unit

      Is_Env_Populated  : Boolean;
      --  Whether Populate_Lexical_Env was called on this unit. Used not to
      --  populate multiple times the same unit and hence avoid infinite
      --  populate recursions for circular dependencies.

      Has_Filled_Caches : Boolean;
      --  Set iff at least one memoized property has been evaluated
      --  successfully in one of the nodes, i.e. whether we need to invalidate
      --  the cache on the AST_Root tree.

      Rule              : Grammar_Rule;
      --  The grammar rule used to parse this unit

      AST_Mem_Pool      : Bump_Ptr_Pool;
      --  This memory pool shall only be used for AST parsing. Stored here
      --  because it is more convenient, but one shall not allocate from it.

      Destroyables      : Destroyable_Vectors.Vector;
      --  Collection of objects to destroy when destroying the analysis unit

      Referenced_Units  : Analysis_Unit_Sets.Set;
      --  Units that are referenced from this one. Useful for
      --  visibility/computation of the reference graph.

      Lex_Env_Data_Acc  : Lex_Env_Data;
   end record;

   % if _self.default_unit_file_provider:
   function Unit_File_Provider
     (Context : Analysis_Context)
      return Unit_File_Provider_Access_Cst is (Context.Unit_File_Provider);
   % endif

   function Token_Data
     (Unit : access Analysis_Unit_Type)
      return Token_Data_Handler_Access
   is
     (Unit.TDH'Access);

   procedure Register_Destroyable_Helper
     (Unit    : access Analysis_Unit_Type;
      Object  : System.Address;
      Destroy : Destroy_Procedure);

   procedure Set_Filled_Caches (Unit : access Analysis_Unit_Type);
   --  Tag Unit as having filled caches for properties memoization

   procedure Reset_Property_Caches (Unit : access Analysis_Unit_Type);
   --  If AST_Node is not null, invoke Reset_Property_Caches primitives on all
   --  the nodes it contains.

   function Is_Referenced
     (Unit, Referenced : access Analysis_Unit_Type) return Boolean;
   --  Check whether the Referenced unit is referenced from Unit

   function Root (Unit : Analysis_Unit) return ${root_node_type_name} is
     (Unit.AST_Root);

   function Get_Context (Unit : Analysis_Unit) return Analysis_Context is
     (Unit.Context);

   function Get_Filename (Unit : Analysis_Unit) return String is
     (To_String (Unit.File_Name));
   --  Check whether Referenced's unit is referenced from Env's unit. Used for
   --  property generation purposes.

   generic
      type T (<>) is limited private;
      type T_Access is access all T;
      with procedure Destroy (Object : in out T_Access);
   procedure Register_Destroyable_Gen
     (Unit : access Analysis_Unit_Type; Object : T_Access);
   --  Generic procedure to register an object so that it is automatically
   --  destroyed when Unit is destroyed.

   function Children
     (Node : access ${root_node_value_type}'Class)
     return ${root_node_array.name()};
   --  Return an array containing all the children of Node.
   --  This is an alternative to the Child/Child_Count pair, useful if you want
   --  the convenience of ada arrays, and you don't care about the small
   --  performance hit of creating an array.

   procedure Reset_Property_Caches (Node : access ${root_node_value_type})
      is null;
   --  Reset the properties memoization caches attached to this node

   procedure Destroy_Node (Node : access ${root_node_value_type}) is null;
   --  Free the resources allocated to this node. This is conceptually abstract
   --  but we can't have private abstract primitives in Ada.

   procedure Destroy (Node : access ${root_node_value_type}'Class);
   --  Free the resources allocated to this node and all its children

   ------------------------------
   -- Root AST node properties --
   ------------------------------

   % for prop in T.root_node.get_properties( \
         include_inherited=False, \
         predicate=library_private_field):
      ${prop.prop_decl}
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

   --------------------------------------
   -- Environments handling (internal) --
   --------------------------------------

   No_Env_Element : constant Env_Element := (null, No_Metadata, True);
   procedure Inc_Ref (Self : Lexical_Env) renames AST_Envs.Inc_Ref;
   procedure Dec_Ref (Self : in out Lexical_Env) renames AST_Envs.Dec_Ref;

   function Get
     (A     : AST_Envs.Env_Element_Array;
      Index : Integer)
      return Env_Element;
   --  Simple getter that raises Property_Error on out-of-bound accesses.
   --  Useful for code generation.

   function Group is new AST_Envs.Group
     (Index_Type        => Positive,
      Lexical_Env_Array => ${LexicalEnvType.array_type().api_name()});

   function Group
     (Envs : ${LexicalEnvType.array_type().name()})
      return ${LexicalEnvType.name()}
   is (Group (Envs.Items));
   --  Convenience wrapper for uniform types handling in code generation

   -------------------------------
   -- Root AST node (internals) --
   -------------------------------

   type ${root_node_value_type} is abstract tagged record
      Parent                 : ${root_node_type_name} := null;

      Unit                   : Analysis_Unit := null;
      --  Reference to the analysis unit that owns this node

      Token_Start, Token_End : Token_Index  := No_Token_Index;
      --  Reference to the start and end token that constitutes this node.
      --  If this node is a ghost, Token_Start is the token that this AST node
      --  relates to and Token_End is No_Token_Index. Otherwise, both tokens
      --  are inclusive, i.e. they both belong to this node.

      Extensions             : Extension_Vectors.Vector;

      Self_Env               : AST_Envs.Lexical_Env;
      --  Hold the environment this node defines, or the parent environment
      --  otherwise.

      ${astnode_types.node_fields(T.root_node, emit_null=False)}
   end record;

   procedure Free_Extensions (Node : access ${root_node_value_type}'Class);
   --  Implementation helper to free the extensions associatde to Node

   ${array_types.private_decl(LexicalEnvType.array_type())}
   ${array_types.private_decl(T.root_node.env_element().array_type())}
   ${array_types.private_decl(root_node_array)}

   function Pre_Env_Actions
     (Self                  : access ${root_node_value_type};
      Current_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only       : Boolean := False) return AST_Envs.Lexical_Env
   is (null);
   --  Internal procedure that will execute all necessary lexical env actions
   --  for Node. This is meant to be called by Populate_Lexical_Env, and not by
   --  the user.
   --  The return value is the initial environment to be passed to
   --  Post_Env_Actions.

   procedure Post_Env_Actions
     (Self        : access ${root_node_value_type};
      Current_Env, Root_Env : AST_Envs.Lexical_Env) is null;
   --  Internal procedure that will execute all post add to env actions for
   --  Node. This is meant to be called by Populate_Lexical_Env.

   function Is_Visible_From
     (Env, Referenced : AST_Envs.Lexical_Env) return Boolean
   is
     (Is_Referenced (Env.Node.Unit, Referenced.Node.Unit));

   procedure Populate_Lexical_Env
     (Node     : access ${root_node_value_type}'Class;
      Root_Env : AST_Envs.Lexical_Env);
   --  Populate the lexical environment for node and all its children

   --------------------------------
   -- Tree traversal (internals) --
   --------------------------------

   function Get_Parent
     (N : ${root_node_type_name}) return ${root_node_type_name}
   is (N.Parent);

   function First_Child_Index_For_Traverse
     (N : ${root_node_type_name})
      return Natural
   is (N.First_Child_Index);

   function Last_Child_Index_For_Traverse
     (N : ${root_node_type_name})
      return Natural
   is (N.Last_Child_Index);

   function Get_Child
     (N : ${root_node_type_name}; I : Natural) return ${root_node_type_name}
   is (N.Child (I));

   package Traversal_Iterators is new Langkit_Support.Tree_Traversal_Iterator
     (Element_type      => ${root_node_type_name},
      Null_Value        => null,
      First_Child_Index => First_Child_Index_For_Traverse,
      Last_Child_Index  => Last_Child_Index_For_Traverse,
      Element_Vectors   => ${root_node_type_name}_Vectors,
      Iterators         => ${root_node_type_name}_Iterators);

   type Traverse_Iterator is
      limited new Traversal_Iterators.Traverse_Iterator with null record;

   type Find_Iterator is limited
      new Ada.Finalization.Limited_Controlled
      and ${root_node_type_name}_Iterators.Iterator with
   record
      Traverse_It : Traverse_Iterator;
      --  Traverse iterator to fetch all nodes

      Predicate   : ${root_node_type_name}_Predicate;
      --  Predicate used to filter the nodes Traverse_It yields
   end record;

   overriding procedure Finalize (It : in out Find_Iterator);

   type Local_Find_Iterator is limited
      new Ada.Finalization.Limited_Controlled
      and ${root_node_type_name}_Iterators.Iterator with
   record
      Traverse_It : Traverse_Iterator;
      --  Traverse iterator to fetch all nodes

      Predicate   : access function (N : ${root_node_type_name})
                                     return Boolean;
      --  Predicate used to filter the nodes Traverse_It yields
   end record;
   --  Iterator type for Find (see below)

   -----------------------------------
   -- Lexical utilities (internals) --
   -----------------------------------

   type Token_Type is record
      TDH           : Token_Data_Handler_Access;
      --  Token data handler that owns this token

      Token, Trivia : Token_Index;
      --  Indices that identify what this token refers to.
      --
      --  * If this references a token, then Token is the corresponding index
      --    in TDH.Tokens and Trivia is No_Token_Index.
      --
      --  * If this references a trivia that comes before the first token,
      --    Token is No_Token_Index while Trivia is the corresponding index in
      --    TDH.Trivias.
      --
      --  * If this references a trivia that comes after some token, Token is
      --    the index for this token and Trivia is the corresponding index for
      --    this trivia.
      --
      --  * If this references no token, both Token and Trivia are
      --    No_Token_Index.
   end record;

   No_Token : constant Token_Type := (null, No_Token_Index, No_Token_Index);

   function First_Token (TDH : Token_Data_Handler_Access) return Token_Type;
   --  Internal helper. Return a reference to the first token in TDH.

   function Last_Token (TDH : Token_Data_Handler_Access) return Token_Type;
   --  Internal helper. Return a reference to the last token in TDH.

   function First_Token (Unit : Analysis_Unit) return Token_Type is
     (First_Token (Unit.TDH'Access));

   function Last_Token (Unit : Analysis_Unit) return Token_Type is
     (Last_Token (Unit.TDH'Access));

   function Token
     (Node  : access ${root_node_value_type}'Class;
      Index : Token_Index)
      return Token_Type
   is
     ((TDH    => Token_Data (Node.Unit),
       Token  => Index,
       Trivia => No_Token_Index));
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

   function Token_Start
     (Node : access ${root_node_value_type}'Class)
      return Token_Type
   is
     ((TDH    => Token_Data (Node.Unit),
       Token  => Node.Token_Start,
       Trivia => No_Token_Index));

   function Token_End
     (Node : access ${root_node_value_type}'Class)
      return Token_Type
   is
     (if Node.Token_End = No_Token_Index
      then Token_Start (Node)
      else (TDH    => Token_Data (Node.Unit),
            Token  => Node.Token_End,
            Trivia => No_Token_Index));

   function Is_Ghost
     (Node : access ${root_node_value_type}'Class)
      return Boolean
   is (Node.Token_End = No_Token_Index);
   --  Returns whether the node is a ghost node, i.e. whether it corresponds to
   --  a real chain of tokens in the source.

   function Is_Synthetic
     (Node : access ${root_node_value_type}'Class)
      return Boolean
   is (Node.Token_Start = No_Token_Index);
   --  Returns whether the node is a synthetic node, i.e. whether it was
   --  generated for semantic analysis instead of parsing.

   --------------------------------
   -- Token Iterator (internals) --
   --------------------------------

   type Token_Iterator is record
      Node : ${root_node_type_name};
      Last : Token_Index;
   end record;

   function First_Token (Self : Token_Iterator) return Token_Type
   is (Token_Start (Self.Node));

   function Next_Token
     (Self : Token_Iterator; Tok : Token_Type) return Token_Type
   is (Next (Tok));

   function Has_Element
     (Self : Token_Iterator; Tok : Token_Type) return Boolean
   is (Tok.Token <= Self.Last);

   function Element (Self : Token_Iterator; Tok : Token_Type) return Token_Type
   is (Tok);

   function Token_Range
     (Node : access ${root_node_value_type}'Class)
      return Token_Iterator
   is
     (Token_Iterator'(${root_node_type_name} (Node), Node.Token_End));

   function Get_Symbol (Token : Token_Type) return Symbol_Type is
     (Symbol_Type (Data (Token).Text));
   --  Assuming that Token refers to a token that contains a symbol, return the
   --  corresponding symbol. This is an internal helper for properties code
   --  generation.

   type Memoization_State is
     (Not_Computed,
      Computed,
      Raise_Property_Error);
   --  Implementation detail for properties memoization. Values describe if the
   --  property is still to be evaluated (Not_Computed), if its result value is
   --  already available (Comptuted) or if it is known to raise a
   --  Property_Error (Raise_Property_Error).

   % for array_type in _self.sorted_types(_self.array_types):
   % if array_type.element_type().should_emit_array_type:
   ${array_types.private_decl(array_type)}
   % endif
   % endfor

   package Node_Bump_Ptr_Vectors is new Langkit_Support.Bump_Ptr.Vectors
     (Element_Type => ${root_node_type_name});

   type ${generic_list_value_type} is
      abstract new ${root_node_value_type}
   with record
      Vec : Node_Bump_Ptr_Vectors.Vector;
   end record;

   overriding procedure Destroy_Node
     (Node : access ${generic_list_value_type});

   % for astnode in no_builtins(_self.astnode_types):
     % if not astnode.is_list_type:
       ${astnode_types.private_decl(astnode)}
     % endif
   % endfor

   % for astnode in _self.astnode_types:
      % if astnode.is_root_list_type:
         ${list_types.private_decl(astnode.element_type())}
      % elif astnode.is_list_type:
         ${astnode_types.private_decl(astnode)}
      % endif
   % endfor

end ${_self.ada_api_settings.lib_name}.Analysis;
