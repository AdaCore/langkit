## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<% root_node_array = ctx.root_grammar_class.array_type() %>

with System;

with Langkit_Support.Extensions;         use Langkit_Support.Extensions;
with Langkit_Support.Lexical_Env;
with Langkit_Support.Token_Data_Handler; use Langkit_Support.Token_Data_Handler;
with Langkit_Support.Tokens;             use Langkit_Support.Tokens;
with Langkit_Support.Vectors;

--  This package defines the base ("root") type for AST nodes. All node types
--  that appear in the AST derive from it.

package ${_self.ada_api_settings.lib_name}.AST_Root is

   -------------------
   -- Root AST node --
   -------------------

   type ${root_node_value_type} is tagged;
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
     access procedure (Node      : ${root_node_type_name};
                       Extension : Extension_Type)
     with Convention => C;
   --  Type for extension destructors. The parameter are the "Node" the
   --  extension was attached to and the "Extension" itself.

   type Extension_Slot is record
      ID        : Extension_ID;
      Extension : Extension_Access;
      Dtor      : Extension_Destructor;
   end record;
   --  TODO??? Remove this from the public API

   package Extension_Vectors is new Langkit_Support.Vectors
     (Element_Type => Extension_Slot);

   ----------------------------
   --  Environments handling --
   ----------------------------

   type Dummy_Metadata is new Integer;
   No_Metadata : constant Dummy_Metadata := 0;
   function Combine (L, R : Dummy_Metadata) return Dummy_Metadata is (0);
   --  This type and constants are added waiting for a real metadata type.
   --  TODO??? Use a real metadata type.

   package AST_Envs is new Langkit_Support.Lexical_Env
     (${root_node_type_name}, Dummy_Metadata, No_Metadata, Combine);

   -------------------------------
   -- Root AST node (internals) --
   -------------------------------

   type ${root_node_value_type} is abstract tagged record
      Parent                 : ${root_node_type_name} := null;
      Token_Data             : Token_Data_Handler_Access := null;
      Token_Start, Token_End : Natural  := 0;
      Parent_Env             : AST_Envs.Lexical_Env;
      Extensions             : Extension_Vectors.Vector;
   end record;
   --  TODO??? Remove this from the public API

   type Child_Or_Trivia is (Child, Trivia);
   --  Discriminator for the Child_Record type

   type Child_Record (Kind : Child_Or_Trivia := Child) is record
      case Kind is
         when Child =>
            Node : ${root_node_type_name};
         when Trivia =>
            Trivia : Token;
      end case;
   end record;
   --  Variant that holds either an AST node or a token

   package Children_Vectors is new Langkit_Support.Vectors (Child_Record);
   package Children_Arrays renames Children_Vectors.Elements_Arrays;

   type ${root_node_kind_name} is new Natural;
   --  Describe the concrete type (aka dynamic type) of an AST node (i.e. from
   --  which concrete derivation it comes from).
   --  See ${_self.ada_api_settings.lib_name}.AST for possible values.

   ## Declare arrays of root nodes here since some primitives rely on it
   ${array_types.public_decl(root_node_array)}

   package ${root_node_type_name}_Arrays renames
     ${root_node_type_name}_Vectors.Elements_Arrays;

   function Kind (Node : access ${root_node_value_type})
                  return ${root_node_kind_name} is abstract;
   function Kind_Name
     (Node : access ${root_node_value_type}) return String is abstract;
   --  Return the concrete kind for Node

   function Image
     (Node : access ${root_node_value_type}) return String is abstract;
   --  Debug helper: return a textual representation of this node and all its
   --  children.

   function Short_Image (Node : ${root_node_type_name}) return String;
   --  Return a short representation of the string, containing just the kind
   --  name and the sloc.

   function Child_Count (Node : access ${root_node_value_type})
                         return Natural is abstract;
   --  Return the number of children Node has

   procedure Get_Child (Node   : access ${root_node_value_type};
                        Index  : Natural;
                        Exists : out Boolean;
                        Result : out ${root_node_type_name}) is abstract;
   --  Get the Index'th child of Node, storing it into Result. Store in Exists
   --  whether Node had such a child (if not, the content of Result is
   --  undefined).

   function Child (Node  : access ${root_node_value_type}'Class;
                   Index : Natural) return ${root_node_type_name};
   --  Return the Index'th child of Node, or null if Node has no such child

   function Children
     (Node : access ${root_node_value_type}'Class)
     return ${root_node_type_name}_Arrays.Array_Type;
   --  Return an array containing all the children of Node.
   --  This is an alternative to the Child/Child_Count pair, useful if you want
   --  the convenience of ada arrays, and you don't care about the small
   --  performance hit of creating an array.

   function Children_With_Trivia
     (Node : ${root_node_type_name}) return Children_Arrays.Array_Type;
   --  Return the children of this node interleaved with Trivia token nodes, so
   --  that:
   --  - Every trivia contained between Node.Start_Token and Node.End_Token - 1
   --    will be part of the returned array;
   --  - Nodes and trivias will be lexically ordered.

   type Visit_Status is (Into, Over, Stop);
   --  Helper type to control the AST node traversal process. See Traverse.

   function Traverse
     (Node  : ${root_node_type_name};
      Visit : access function (Node : ${root_node_type_name})
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
     (Node  : ${root_node_type_name};
      Visit : access function (Node : ${root_node_type_name})
              return Visit_Status);
   --  This is the same as Traverse function except that no result is returned
   --  i.e. the Traverse function is called and the result is simply discarded.

   procedure Validate (Node   : access ${root_node_value_type};
                       Parent : ${root_node_type_name} := null) is abstract;
   --  Perform consistency checks on Node. Check that Parent is Node's parent.

   function Sloc_Range (Node : ${root_node_type_name};
                        Snap : Boolean := False) return Source_Location_Range;
   --  Return the source location range corresponding to the set of tokens from
   --  which Node was parsed.
   --
   --  TODO??? Document the Snap formal.

   function Lookup (Node : ${root_node_type_name};
                    Sloc : Source_Location;
                    Snap : Boolean := False) return ${root_node_type_name};
   --  Look for the bottom-most AST node whose sloc range contains Sloc. Return
   --  it, or null if no such node was found.
   --
   --  TODO??? Document the Snap formal.

   function Compare (Node : ${root_node_type_name};
                     Sloc : Source_Location;
                     Snap : Boolean := False) return Relative_Position;
   --  Compare Sloc to the sloc range of Node.
   --
   --  TODO??? Document the Snap formal.

   function Lookup_Children
     (Node : access ${root_node_value_type};
      Sloc : Source_Location;
      Snap : Boolean := False) return ${root_node_type_name} is abstract;
   --  Implementation helper for the looking up process. TODO??? Do not expose
   --  it in the public API.

   procedure Lookup_Relative (Node       : ${root_node_type_name};
                              Sloc       : Source_Location;
                              Position   : out Relative_Position;
                              Node_Found : out ${root_node_type_name};
                              Snap       : Boolean := False);
   --  Implementation helper for the looking up process. TODO??? Do not expose
   --  it in the public API.

   function Get_Extension
     (Node : ${root_node_type_name};
      ID   : Extension_ID;
      Dtor : Extension_Destructor) return Extension_Access;
   --  Get (and create if needed) the extension corresponding to ID for Node.
   --  If the extension is created, the Dtor destructor is associated to it.
   --  Note that the returned access is not guaranteed to stay valid after
   --  subsequent calls to Get_Extension.

   procedure Print (Node  : access ${root_node_value_type};
                    Level : Natural := 0) is abstract;
   --  Debug helper: print to standard output Node and all its children. Level
   --  indicates the indentation level for the output.

   procedure PP_Trivia (Node : ${root_node_type_name}; Level : Integer := 0);
   --  Debug helper: print to standard output Node and all its children along
   --  with the trivia associated to them. Level indicates the indentation
   --  level for the output.

   procedure Free_Extensions (Node : access ${root_node_value_type});
   --  Implementation helper to free the extensions associatde to Node. TODO???
   --  Do not expose it in the public API.

   procedure Destroy
     (Node : access ${root_node_value_type}) is abstract;
   --  Free the resources allocated to this node and all its children
   --  TODO??? We probably don't want this to be exposed in the public API, as
   --  destruction is supposed to be done automatically when the refcount
   --  reaches zero.

   function Is_Empty_List
     (Node : access ${root_node_value_type})
      return Boolean is
     (False);
   --  Return whether Node is an empty list (so this is wrong for all nodes
   --  that are not lists).

   procedure Populate_Lexical_Env
     (Node : ${root_node_type_name}; Root_Env : AST_Envs.Lexical_Env);
   --  Populate the lexical environment for node and all its children.
   --
   --  TODO??? This is probably an internal implementation detail, so this
   --  should not appear in the public API.

   function Do_Env_Actions
     (Self       : access ${root_node_value_type};
      Parent_Env : in out AST_Envs.Lexical_Env) return AST_Envs.Lexical_Env
   is (null);
   --  Internal procedure that will execute all necessary lexical env actions
   --  for Node. This is meant to be called by Populate_Lexical_Env, and not by
   --  the user.
   --
   --  Parent_Env is the environment that is the parent scope for Self when
   --  entering the function. It is an in out parameter because the
   --  implementation can replace Parent_Env by a new Lexical_Env derived from
   --  it.
   --
   --  The return value can be either null, or a new Lexical_Env that represent
   --  a new scope that will be used by Self's children.
   --  The difference between replacing Parent_Env and returning a new env, is
   --  that replacing Parent_Env will affect the env that the following
   --  siblings of Self see, while returning a new env will only affect the
   --  environment seen by Self's children.

   procedure Dump_Lexical_Env
     (Node : ${root_node_type_name}; Root_Env : AST_Envs.Lexical_Env);
   --  Debug helper: dump the lexical environment of Node, and consequently any
   --  nested lexical environment. Used for debugging/testing purpose. Pass the
   --  root env explicitly so that we can tag it properly in the output.

   function Parents
     (Node : access ${root_node_value_type})
      return ${root_node_array.name()};
   --  Return the list of parents for this node (this node included)

end ${_self.ada_api_settings.lib_name}.AST_Root;
