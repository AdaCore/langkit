## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="struct_types"  file="struct_types_ada.mako" />

<%
   root_node_array = T.root_node.array_type()
   no_builtins = lambda ts: filter(lambda t: not t.is_builtin(), ts)
   library_private_field = lambda f: not library_public_field(f)
%>

with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Unchecked_Deallocation;

with System;

with Adalog.Abstract_Relation;   use Adalog.Abstract_Relation;
with Adalog.Eq_Same;

with Langkit_Support.Extensions;  use Langkit_Support.Extensions;
with Langkit_Support.Iterators;
with Langkit_Support.Lexical_Env;
with Langkit_Support.Slocs;       use Langkit_Support.Slocs;
with Langkit_Support.Symbols;     use Langkit_Support.Symbols;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Langkit_Support.Tree_Traversal_Iterator;
with Langkit_Support.Vectors;

with ${_self.ada_api_settings.lib_name}.Analysis_Interfaces;
use ${_self.ada_api_settings.lib_name}.Analysis_Interfaces;
with ${_self.ada_api_settings.lib_name}.Lexer;
use ${_self.ada_api_settings.lib_name}.Lexer;
use ${_self.ada_api_settings.lib_name}.Lexer.Token_Data_Handlers;

--  This package defines the base ("root") type for AST nodes. All node types
--  that appear in the AST derive from it.

package ${_self.ada_api_settings.lib_name}.AST is

   -------------------
   -- Root AST node --
   -------------------

   type ${root_node_value_type} is abstract tagged private
     with Default_Iterator  => Iterate,
          Iterator_Element  => ${root_node_type_name},
          Constant_Indexing => Element_Value;
   --  This "by-value" type is public to expose the fact that the various
   --  AST nodes are a hierarchy of tagged types, but it is not intended to be
   --  used directly, hence the "_Type" suffix. Please use instead the
   --  class-wide types such at the one below.

   type ${root_node_type_name} is access all ${root_node_value_type}'Class;
   --  Most generic AST node type

   ## This type is defined here so as to be accessible to the AST array Get
   ## helper.
   Property_Error : exception;
   ${ada_doc('langkit.property_error', 3)}

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

   ----------------------------
   --  Environments handling --
   ----------------------------

   --  The following types and operations are implementation details we did not
   --  manage yet to put in a private part. Please don't use them.

   % if ctx.env_metadata:
   ${struct_types.public_incomplete_decl(ctx.env_metadata)}
   ${struct_types.public_decl(ctx.env_metadata)}

   function Combine
     (L, R : ${ctx.env_metadata.name()}) return ${ctx.env_metadata.name()};
   ## The combine function on environments metadata does a boolean Or on every
   ## boolean component of the env metadata.

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

   ## The following subtypes are introduced to ease code generation, so we
   ## don't have to deal with the AST_Envs suffix.
   subtype Lexical_Env is AST_Envs.Lexical_Env;
   subtype Env_Element is AST_Envs.Env_Element;
   No_Env_Element : constant Env_Element := (null, No_Metadata, True);
   procedure Inc_Ref (Self : Lexical_Env) renames AST_Envs.Inc_Ref;
   procedure Dec_Ref (Self : in out Lexical_Env) renames AST_Envs.Dec_Ref;

   function Get
     (A     : AST_Envs.Env_Element_Array;
      Index : Integer)
      return Env_Element;
   --  Simple getter that raises Property_Error on out-of-bound accesses.
   --  Useful for code generation.

   ## Declare arrays of lexical environments here because we need them for the
   ## Group operation below.
   ${array_types.public_decl(LexicalEnvType.array_type())}

   ## See EnvElement.should_emit_array_type
   ${array_types.public_decl(EnvElement.array_type())}

   function Group is new AST_Envs.Group
     (Index_Type        => Positive,
      Lexical_Env_Array => ${LexicalEnvType.array_type().api_name()});

   function Group
     (Envs : ${LexicalEnvType.array_type().name()})
      return ${LexicalEnvType.name()}
   is (Group (Envs.Items));
   --  Convenience wrapper for uniform types handling in code generation

   ## Declare arrays of root nodes here since some primitives rely on it and
   ## since the declarations require AST_Envs.
   ${array_types.public_decl(root_node_array)}

   procedure Populate_Lexical_Env
     (Node     : access ${root_node_value_type}'Class;
      Root_Env : AST_Envs.Lexical_Env);
   --  Populate the lexical environment for node and all its children

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

   procedure Destroy
     (Node : access ${root_node_value_type}) is abstract;
   --  Free the resources allocated to this node and all its children.
   --
   --  This is an internal implementation detail, please don't use this.
   --  TODO??? Hide it somehow: destruction is done automatically when the
   --  owning analysis unit is destroyed itself.

   function Node_Env
     (Node : access ${root_node_value_type})
      return AST_Envs.Lexical_Env;
   ${ada_doc(T.root_node._fields['node_env'], 3)}

   function Children_Env
     (Node : access ${root_node_value_type})
      return AST_Envs.Lexical_Env;
   ${ada_doc(T.root_node._fields['children_env'], 3)}

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
   --  the convenience of ada arrays, and you don't care about the small
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
     new ${root_node_type_name}_Iterators.Iterator
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
     (It       : in out Local_Find_Iterator;
      Element  : out ${root_node_type_name})
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

   type Token_Type is private;
   ${ada_doc('langkit.token_type')}

   No_Token : constant Token_Type;

   function First_Token (TDH : Token_Data_Handler_Access) return Token_Type;
   --  Internal helper. Return a reference to the first token in TDH.

   function Last_Token (TDH : Token_Data_Handler_Access) return Token_Type;
   --  Internal helper. Return a reference to the last token in TDH.

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

   ----------------------------------------
   -- Tree traversal (Ada 2012 iterator) --
   ----------------------------------------

   type Children_Cursor is private;
   --  Cursor for AST node children iteration

   No_Children : constant Children_Cursor;

   function Has_Element (C : Children_Cursor) return Boolean;
   --  Whether C references a valid AST node child

   package ${root_node_type_name}_Ada2012_Iterators is
     new Ada.Iterator_Interfaces (Children_Cursor, Has_Element);

   function Iterate
     (Node : ${root_node_value_type})
      return
      ${root_node_type_name}_Ada2012_Iterators.Reversible_Iterator'Class;

   function Element_Value
     (Node : ${root_node_value_type}; C : Children_Cursor)
      return ${root_node_type_name};

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

   function Get_Unit
     (Node : access ${root_node_value_type}'Class)
      return Analysis_Unit_Interface;
   --  Internal helper to get the unit that owns an AST node

private

   use AST_Envs;

   function Is_Visible_From
     (Env, Referenced : AST_Envs.Lexical_Env) return Boolean
   is
     (Is_Referenced (Get_Unit (Env.Node), Get_Unit (Referenced.Node)));
   --  Check whether Referenced's unit is referenced from Env's unit. Used for
   --  property generation purposes.

   function Children
     (Node : access ${root_node_value_type}'Class)
     return ${root_node_array.name()};
   --  Return an array containing all the children of Node.
   --  This is an alternative to the Child/Child_Count pair, useful if you want
   --  the convenience of ada arrays, and you don't care about the small
   --  performance hit of creating an array.

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
   --  TODO??? Remove this from the public API

   package Extension_Vectors is new Langkit_Support.Vectors
     (Element_Type => Extension_Slot);

   -------------------------------
   -- Root AST node (internals) --
   -------------------------------

   type ${root_node_value_type} is abstract tagged record
      Parent                 : ${root_node_type_name} := null;

      Unit                   : Analysis_Unit_Interface := null;
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
   ${array_types.private_decl(EnvElement.array_type())}
   ${array_types.private_decl(root_node_array)}

   function Pre_Env_Actions
     (Self        : access ${root_node_value_type};
      Current_Env : AST_Envs.Lexical_Env) return AST_Envs.Lexical_Env
   is (null);
   --  Internal procedure that will execute all necessary lexical env actions
   --  for Node. This is meant to be called by Populate_Lexical_Env, and not by
   --  the user.
   --  The return value is the initial environment to be passed to
   --  Post_Env_Actions.

   procedure Post_Env_Actions
     (Self        : access ${root_node_value_type};
      Current_Env : AST_Envs.Lexical_Env) is null;
   --  Internal procedure that will execute all post add to env actions for
   --  Node. This is meant to be called by Populate_Lexical_Env.

   --------------------------------
   -- Tree traversal (internals) --
   --------------------------------

   function Get_Parent
     (N : ${root_node_type_name}) return ${root_node_type_name}
   is (N.Parent);

   function Children_Count (N : ${root_node_type_name}) return Natural
   is (N.Child_Count);

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

   type Traverse_Iterator
   is new Traversal_Iterators.Traverse_Iterator with null record;

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

   function Token
     (Node  : access ${root_node_value_type}'Class;
      Index : Token_Index)
      return Token_Type
   is
     ((TDH => Node.Unit.Token_Data, Token => Index, Trivia => No_Token_Index));
   --  Helper for properties. This is used to turn token indexes as stored in
   --  AST nodes into Token_Type values.

   function Token_Start
     (Node : access ${root_node_value_type}'Class)
      return Token_Type
   is
     ((Node.Unit.Token_Data, Node.Token_Start, No_Token_Index));

   function Token_End
     (Node : access ${root_node_value_type}'Class)
      return Token_Type
   is
     (if Node.Token_End = No_Token_Index
      then Token_Start (Node)
      else (Node.Unit.Token_Data, Node.Token_End, No_Token_Index));

   function Is_Ghost
     (Node : access ${root_node_value_type}'Class)
      return Boolean
   is (Node.Token_End = No_Token_Index);
   --  Returns whether the node is a ghost node or not, eg. whether it
   --  corresponds to a real chain of tokens in the source or not.

   --------------------
   -- Token Iterator --
   --------------------

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

   ----------------------------------------
   -- Tree traversal (Ada 2012 iterator) --
   ----------------------------------------

   type Children_Cursor is record
      Node             : ${root_node_type_name};
      --  This cursor references a children in Node

      Child_Index : Natural;
      --  1-based index of Node's children this cursor references, or zero when
      --  this cursor does not reference a valid child.
   end record;

   No_Children : constant Children_Cursor := (null, 0);

   function Has_Element (C : Children_Cursor) return Boolean is
     (C.Child_Index /= 0);

   function Element_Value
     (Node : ${root_node_value_type}; C : Children_Cursor)
      return ${root_node_type_name} is
     (C.Node.Child (C.Child_Index));

   type Iterator is new
      ${root_node_type_name}_Ada2012_Iterators.Reversible_Iterator with
   record
      Node : ${root_node_type_name};
   end record;

   overriding function First (Object : Iterator) return Children_Cursor;
   overriding function Last (Object : Iterator) return Children_Cursor;
   overriding function Next
     (Object : Iterator;
      C      : Children_Cursor)
      return Children_Cursor;
   overriding function Previous
     (Object : Iterator;
      C      : Children_Cursor)
      return Children_Cursor;

   function Get_Unit
     (Node : access ${root_node_value_type}'Class)
      return Analysis_Unit_Interface
   is (Node.Unit);

end ${_self.ada_api_settings.lib_name}.AST;
