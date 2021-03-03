## vim: filetype=makoada

<%namespace name="array_types"    file="array_types_ada.mako" />
<%namespace name="iterator_types" file="iterator_types_ada.mako" />
<%namespace name="astnode_types"  file="astnode_types_ada.mako" />
<%namespace name="exts"           file="extensions.mako" />
<%namespace name="public_properties"
            file="properties/public_wrappers_ada.mako" />
<%namespace name="struct_types"   file="struct_types_ada.mako" />

with Ada.Containers;
private with Ada.Finalization;
with Ada.Strings.Unbounded;
% if any(a.used_in_public_struct for a in ctx.array_types):
   private with Ada.Unchecked_Deallocation;
% endif

with GNATCOLL.Refcount;

% if any(s.exposed and not s.is_entity_type for s in ctx.struct_types):
   private with Langkit_Support.Boxes;
% endif

with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

with Langkit_Support.Symbols; use Langkit_Support.Symbols;

with ${ada_lib_name}.Common; use ${ada_lib_name}.Common;
private with ${ada_lib_name}.Implementation;
private with ${ada_lib_name}.Debug;

${exts.with_clauses(with_clauses)}

--  This package provides types and primitives to analyze source files as
--  analysis units.
--
--  This is the entry point to parse and process a unit: first create an
--  analysis context with ``Create_Context``, then get analysis units out of it
--  using the ``Get_From_*`` functions.

package ${ada_lib_name}.Analysis is

   use Support.Diagnostics, Support.Slocs, Support.Text;

   type Analysis_Context is tagged private;
   ${ada_doc('langkit.analysis_context_type', 3)}

   type Analysis_Unit is new Langkit_Support.Text.Text_Buffer_Ifc with private;
   ${ada_doc('langkit.analysis_unit_type', 3)}

   No_Analysis_Context : constant Analysis_Context;
   --  Special value to mean the absence of analysis context

   No_Analysis_Unit : constant Analysis_Unit;
   --  Special value to mean the absence of analysis unit. No analysis units
   --  can be passed this value.

   ---------------
   -- AST nodes --
   ---------------

   % for e in ctx.entity_types:
      % if e.is_root_type:
      type ${e.api_name} is tagged private;
      ${ada_doc('langkit.node_type', 6)}
      --
      % else:
      type ${e.api_name} is new ${e.base.api_name} with private
      % if e.element_type.is_root_list_type:
         with Iterable => (First       => ${e.api_name}_First,
                           Next        => ${e.api_name}_Next,
                           Has_Element => ${e.api_name}_Has_Element,
                           Element     => ${e.api_name}_Element)
      % endif
      ;
      % endif
      ${ada_doc(e.astnode, 6)}
   % endfor

   % for e in ctx.entity_types:
      No_${e.api_name} : constant ${e.api_name};
      % if e == root_entity:
      --  Special value to represent the absence of a node. Note that every
      --  node type derived from the root type has a similar ``No_Node``
      --  constant.
      % else:
      --% no-document: True
      % endif
   % endfor

   function Is_Null (Node : ${root_entity.api_name}'Class) return Boolean;
   ${ada_doc('langkit.node_is_null', 3)}

   function Is_Token_Node
     (Node : ${root_entity.api_name}'Class) return Boolean;
   ${ada_doc('langkit.node_is_token_node', 3)}

   function Is_Synthetic
     (Node : ${root_entity.api_name}'Class) return Boolean;
   ${ada_doc('langkit.node_is_synthetic', 3)}

   function "=" (L, R : ${root_entity.api_name}'Class) return Boolean;
   --  Return whether L and R designate the same entity

   function Image (Node : ${root_entity.api_name}'Class) return String;
   --  Return a short string describing Node, or "None" if Node.Is_Null is
   --  true.

   --------------------
   -- Unit providers --
   --------------------

   type Unit_Provider_Interface is interface;
   ${ada_doc('langkit.unit_provider_type', 3)}

   function Get_Unit_Filename
     (Provider : Unit_Provider_Interface;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String is abstract;
   ${ada_doc('langkit.unit_provider_get_unit_filename', 3)}

   function Get_Unit
     (Provider    : Unit_Provider_Interface;
      Context     : Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Analysis_Unit'Class is abstract;
   ${ada_doc('langkit.unit_provider_get_unit_from_name', 3)}

   procedure Release (Provider : in out Unit_Provider_Interface) is abstract;
   --  Actions to perform when releasing resources associated to Provider

   procedure Do_Release (Provider : in out Unit_Provider_Interface'Class);
   --  Helper for the instantiation below

   package Unit_Provider_References is new GNATCOLL.Refcount.Shared_Pointers
     (Unit_Provider_Interface'Class, Do_Release);

   subtype Unit_Provider_Reference is Unit_Provider_References.Ref;
   No_Unit_Provider_Reference : Unit_Provider_Reference renames
      Unit_Provider_References.Null_Ref;

   function Create_Unit_Provider_Reference
     (Provider : Unit_Provider_Interface'Class) return Unit_Provider_Reference;
   --  Simple wrapper around the GNATCOLL.Refcount API to create unit provider
   --  references.

   ---------------------------------
   -- Analysis context primitives --
   ---------------------------------

   function Create_Context
     (Charset       : String := Default_Charset;
      Unit_Provider : Unit_Provider_Reference := No_Unit_Provider_Reference;
      With_Trivia   : Boolean := True;
      Tab_Stop      : Positive := ${ctx.default_tab_stop})
      return Analysis_Context;
   ${ada_doc('langkit.create_context', 3)}
   --% belongs-to: Analysis_Context

   function Has_Unit
     (Context       : Analysis_Context'Class;
      Unit_Filename : String) return Boolean;
   --  Return whether ``Context`` contains a unit correponding to
   --  ``Unit_Filename``.

   function Get_From_File
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit;
   ${ada_doc('langkit.get_unit_from_file', 3)}

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit;
   ${ada_doc('langkit.get_unit_from_buffer', 3)}

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : Ada.Strings.Unbounded.Unbounded_String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit;
   --  Likewise, but working on an unbounded string

   function Get_With_Error
     (Context  : Analysis_Context'Class;
      Filename : String;
      Error    : Text_Type;
      Charset  : String := "";
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit;
   --  If a Unit for ``Filename`` already exists, return it unchanged.
   --  Otherwise, create an empty analysis unit for ``Filename`` with a
   --  diagnostic that contains the ``Error`` message.

   % if ctx.default_unit_provider:

   function Get_From_Provider
     (Context : Analysis_Context'Class;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
      Charset : String := "";
      Reparse : Boolean := False) return Analysis_Unit;
   ${ada_doc('langkit.get_unit_from_provider', 3)}

   % endif

   function Unit_Provider
     (Context : Analysis_Context'Class) return Unit_Provider_Reference;
   --  Return the unit provider for ``Context``
   --
   --% belongs-to: Analysis_Context

   function Hash (Context : Analysis_Context) return Ada.Containers.Hash_Type;
   ${ada_doc('langkit.context_hash', 3)}

   function Has_With_Trivia (Context : Analysis_Context'Class) return Boolean;
   --  Return whether ``Context`` keeps trivia when parsing units

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Analysis_Context'Class; Discard : Boolean);
   ${ada_doc('langkit.context_discard_errors_in_populate_lexical_env', 3)}

   procedure Set_Logic_Resolution_Timeout
     (Context : Analysis_Context'Class; Timeout : Natural);
   ${ada_doc('langkit.context_set_logic_resolution_timeout', 3)}

   procedure Disable_Lookup_Cache (Disable : Boolean := True);
   --  Debug helper: if ``Disable`` is true, disable the use of caches in
   --  lexical environment lookups. Otherwise, activate it.

   function Has_Rewriting_Handle
     (Context : Analysis_Context'Class) return Boolean;
   --  Return whether ``Context`` has a rewriting handler (see
   --  ``${ada_lib_name}.Rewriting``), i.e. whether it is in the process of
   --  rewriting. If true, this means that the set of currently loaded analysis
   --  units is frozen until the rewriting process is done.

   function Get_Symbol_Table
     (Context : Analysis_Context'Class) return Symbol_Table;
   --  Return the symbol table attached to this context. Useful for users
   --  needing their own symbolization and wanting to share it with their
   --  language frontend.
   --
   --  WARNING: EXPERIMENTAL & UNSAFE - The Symbol_Table exposes an unsafe API,
   --  that might be subject to some changes, use with caution.

   ------------------------------
   -- Analysis unit primitives --
   ------------------------------

   function Context (Unit : Analysis_Unit'Class) return Analysis_Context;
   ${ada_doc('langkit.unit_context', 3)}

   function Hash (Unit : Analysis_Unit) return Ada.Containers.Hash_Type;
   ${ada_doc('langkit.unit_hash', 3)}

   procedure Reparse (Unit : Analysis_Unit'Class; Charset : String := "");
   ${ada_doc('langkit.unit_reparse_file', 3)}

   procedure Reparse
     (Unit    : Analysis_Unit'Class;
      Charset : String := "";
      Buffer  : String);
   ${ada_doc('langkit.unit_reparse_buffer', 3)}

   procedure Populate_Lexical_Env (Unit : Analysis_Unit'Class);
   ${ada_doc('langkit.unit_populate_lexical_env', 3)}

   function Get_Filename (Unit : Analysis_Unit'Class) return String;
   ${ada_doc('langkit.unit_filename', 3)}

   function Get_Charset (Unit : Analysis_Unit'Class) return String;
   --  Return the charset that was used to parse Unit

   function Has_Diagnostics (Unit : Analysis_Unit'Class) return Boolean;
   ${ada_doc('langkit.unit_has_diagnostics', 3)}

   function Diagnostics (Unit : Analysis_Unit'Class) return Diagnostics_Array;
   ${ada_doc('langkit.unit_diagnostics', 3)}

   function Format_GNU_Diagnostic
     (Unit : Analysis_Unit'Class; D : Diagnostic) return String;
   --  Format a diagnostic in a GNU fashion. See
   --  <https://www.gnu.org/prep/standards/html_node/Errors.html>.

   pragma Warnings (Off, "defined after private extension");
   function Root (Unit : Analysis_Unit'Class) return ${root_entity.api_name};
   ${ada_doc('langkit.unit_root', 3)}
   pragma Warnings (On, "defined after private extension");

   function First_Token (Unit : Analysis_Unit'Class) return Token_Reference;
   ${ada_doc('langkit.unit_first_token', 3)}

   function Last_Token (Unit : Analysis_Unit'Class) return Token_Reference;
   ${ada_doc('langkit.unit_last_token', 3)}

   function Token_Count (Unit : Analysis_Unit'Class) return Natural;
   ${ada_doc('langkit.unit_token_count', 3)}

   function Trivia_Count (Unit : Analysis_Unit'Class) return Natural;
   ${ada_doc('langkit.unit_trivia_count', 3)}

   function Text (Unit : Analysis_Unit'Class) return Text_Type;
   ${ada_doc('langkit.unit_text', 3)}

   function Lookup_Token
     (Unit : Analysis_Unit'Class; Sloc : Source_Location)
      return Token_Reference;
   ${ada_doc('langkit.unit_lookup_token', 3)}

   procedure Dump_Lexical_Env (Unit : Analysis_Unit'Class);
   ${ada_doc('langkit.unit_dump_lexical_env', 3)}

   procedure Trigger_Envs_Debug (Is_Active : Boolean);
   --  Debug helper: activate debug traces for lexical envs lookups

   procedure Print (Unit : Analysis_Unit'Class; Show_Slocs : Boolean := True);
   --  Debug helper: output the AST and eventual diagnostic for this unit on
   --  standard output.
   --
   --  If Show_Slocs, include AST nodes' source locations in the output.

   procedure PP_Trivia (Unit : Analysis_Unit'Class);
   --  Debug helper: output a minimal AST with mixed trivias

   overriding function Get_Line
     (Unit : Analysis_Unit; Line_Number : Positive) return Text_Type;
   --  Return the line of text at line number ``Line_Number``

   type Child_Record (Kind : Child_Or_Trivia := Child) is record
      case Kind is
         when Child =>
            Node : ${root_entity.api_name};
         when Trivia =>
            Trivia : Token_Reference;
      end case;
   end record;
   --  Variant that holds either an AST node or a token

   type Children_Array is array (Positive range <>) of Child_Record;

   function Children_And_Trivia
     (Node : ${root_entity.api_name}'Class) return Children_Array;
   --  Return the children of this node interleaved with Trivia token nodes, so
   --  that:
   --
   --  - Every trivia contained between ``Node.Start_Token`` and
   --    ``Node.End_Token - 1`` will be part of the returned array.
   --
   --  - Nodes and trivias will be lexically ordered.

   ---------------------
   -- Composite types --
   ---------------------

   % for t in ctx.composite_types:
      % if t.is_array_type:
         % if t.exposed:
            ${array_types.public_api_decl(t)}
         % endif
      % elif t.is_iterator_type:
        % if t.exposed:
            ${iterator_types.public_api_decl(t)}
        % endif
      % elif t.is_struct_type:
         % if t.exposed and not t.is_entity_type:
            ${struct_types.public_api_decl(t)}
         % endif
      % endif
   % endfor

   --------------------
   -- Token Iterator --
   --------------------

   type Token_Iterator is private
      with Iterable => (First       => First_Token,
                        Next        => Next_Token,
                        Has_Element => Has_Element,
                        Element     => Element);
   --  Allow iteration on a range of tokens corresponding to a node

   function First_Token (Self : Token_Iterator) return Token_Reference;
   --  Return the first token corresponding to the node

   function Next_Token
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference;
   --  Return the token that follows Tok in the token stream

   function Has_Element
     (Self : Token_Iterator; Tok : Token_Reference) return Boolean;
   --  Return if Tok is in Self's iteration range

   function Element
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference;
   --  Identity function: helper for the Iterable aspect

   -------------------------
   -- AST Node primitives --
   -------------------------

   function Kind
     (Node : ${root_entity.api_name}'Class) return ${T.node_kind};
   function Kind_Name (Node : ${root_entity.api_name}'Class) return String;
   --  Return the concrete kind for Node

   pragma Warnings (Off, "defined after private extension");
   % for e in ctx.entity_types:

      ## Generate specific children accessors for root lists so that users can
      ## get list children with their specific type.
      % if (e.element_type.is_root_list_type and \
            not e.element_type.element_type.is_root_node):
         function List_Child
           (Node : ${e.api_name}'Class; Index : Positive)
            return ${e.element_type.element_type.entity.api_name};
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.
      % endif

      % if e.element_type.is_root_list_type:
         function ${e.api_name}_First (Node : ${e.api_name}) return Positive;
         --  Implementation detail for the Iterable aspect

         function ${e.api_name}_Next
           (Node : ${e.api_name}; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function ${e.api_name}_Has_Element
           (Node : ${e.api_name}; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function ${e.api_name}_Element
           (Node : ${e.api_name}; Cursor : Positive)
            return ${e.element_type.element_type.entity.api_name}'Class;
         --  Implementation detail for the Iterable aspect
      % endif

      % for f in e.element_type.get_parse_fields( \
         include_inherited=False, \
         predicate=lambda f: f.is_public and not f.overriding, \
      ):
         ${astnode_types.field_decl(f)}
      % endfor

      % for p in e.element_type.get_properties( \
         include_inherited=False, \
         predicate=lambda p: p.is_public and not p.overriding \
      ):
         ${public_properties.decl(p)}
      % endfor

   % endfor
   pragma Warnings (On, "defined after private extension");

   -------------------------------
   -- Tree traversal operations --
   -------------------------------

   function Children_Count
     (Node : ${root_entity.api_name}'Class) return Natural;
   --  Return the number of children Node has

   function First_Child_Index
     (Node : ${root_entity.api_name}'Class) return Natural;
   --  Return the index of the first child Node has

   function Last_Child_Index
     (Node : ${root_entity.api_name}'Class) return Natural;
   --  Return the index of the last child Node has, or 0 if there is no child

   pragma Warnings (Off, "defined after private extension");
   procedure Get_Child
     (Node            : ${root_entity.api_name}'Class;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out ${root_entity.api_name});
   --  Return the Index'th child of node, storing it into Result.
   --
   --  Child indexing is 1-based. Store in Index_In_Bounds whether Node had
   --  such a child: if not (i.e. Index is out-of-bounds), the content
   --  of Result is undefined.

   function Child
     (Node  : ${root_entity.api_name}'Class;
      Index : Positive)
      return ${root_entity.api_name};
   --  Return the Index'th child of Node, or null if Node has no such child
   pragma Warnings (On, "defined after private extension");

   function Traverse
     (Node  : ${root_entity.api_name}'Class;
      Visit : access function (Node : ${root_entity.api_name}'Class)
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
     (Node  : ${root_entity.api_name}'Class;
      Visit : access function (Node : ${root_entity.api_name}'Class)
                               return Visit_Status);
   --  This is the same as Traverse function except that no result is returned
   --  i.e. the Traverse function is called and the result is simply discarded.

   ----------------------------------------
   -- Source location-related operations --
   ----------------------------------------

   function Sloc_Range
     (Node : ${root_entity.api_name}'Class) return Source_Location_Range;
   --  Return the source location range corresponding to the set of tokens from
   --  which Node was parsed.

   function Compare
     (Node : ${root_entity.api_name}'Class;
      Sloc : Source_Location) return Relative_Position;
   --  Compare Sloc to the sloc range of Node

   pragma Warnings (Off, "defined after private extension");
   function Lookup
     (Node : ${root_entity.api_name}'Class;
      Sloc : Source_Location) return ${root_entity.api_name};
   --  Look for the bottom-most AST node whose sloc range contains Sloc. Return
   --  it, or null if no such node was found.
   pragma Warnings (On, "defined after private extension");

   -----------------------
   -- Lexical utilities --
   -----------------------

   function Text (Node : ${root_entity.api_name}'Class) return Text_Type;
   ${ada_doc('langkit.node_text', 3)}

   function Token_Range
     (Node : ${root_entity.api_name}'Class) return Token_Iterator;
   --  Return an iterator on the range of tokens encompassed by Node

   ${exts.include_extension(ctx.ext('analysis', 'public_decls'))}

   -------------------
   -- Debug helpers --
   -------------------

   procedure Print
     (Node        : ${root_entity.api_name}'Class;
      Show_Slocs  : Boolean := True;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children.
   --
   --  If Show_Slocs, include AST nodes' source locations in the output.
   --
   --  Line_Prefix is prepended to each output line.

   procedure PP_Trivia
     (Node        : ${root_entity.api_name}'Class;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children along
   --  with the trivia associated to them. Line_Prefix is prepended to each
   --  output line.

   procedure Assign_Names_To_Logic_Vars (Node : ${root_entity.api_name}'Class);
   --  Debug helper: Assign names to every logical variable in the root node,
   --  so that we can trace logical variables.

   --  The following As_* functions convert references to nodes from one type
   --  to another (${root_entity.api_name} can refer to any node type). They
   --  raise a Constraint_Error if the conversion is invalid.

   pragma Warnings (Off, "defined after private extension");
   % for e in ctx.entity_types:
      function As_${e.element_type.kwless_raw_name}
        (Node : ${root_entity.api_name}'Class) return ${e.api_name};
      --% no-document: True
   % endfor

   function Hash
     (Node : ${root_entity.api_name}) return Ada.Containers.Hash_Type;
   --  Generic hash function, to be used for nodes as keys in hash tables
   pragma Warnings (On, "defined after private extension");

private

   type Internal_Context_Access is
      access all Implementation.Analysis_Context_Type;
   type Internal_Unit_Access is
      access all Implementation.Analysis_Unit_Type;

   type Analysis_Context is new Ada.Finalization.Controlled with record
      Internal : Internal_Context_Access;
   end record;

   overriding procedure Initialize (Context : in out Analysis_Context);
   overriding procedure Adjust (Context : in out Analysis_Context);
   overriding procedure Finalize (Context : in out Analysis_Context);

   type Analysis_Unit is new Langkit_Support.Text.Text_Buffer_Ifc with record
      Internal : Internal_Unit_Access;

      Context : Analysis_Context;
      --  Keep a reference to the owning context so that the context lives as
      --  long as there is at least one reference to one of its units.
   end record;

   No_Analysis_Context : constant Analysis_Context :=
     (Ada.Finalization.Controlled with Internal => null);
   No_Analysis_Unit    : constant Analysis_Unit :=
     (Internal => null,
      Context  => (Ada.Finalization.Controlled with Internal => null));

   --------------------------
   -- AST nodes (internal) --
   --------------------------

   % for e in ctx.entity_types:
      % if e.is_root_type:
         type ${e.api_name} is tagged record
            Internal   : Implementation.AST_Envs.Entity;
            Safety_Net : Implementation.Node_Safety_Net;
         end record;
      % else:
         type ${e.api_name} is new ${e.base.api_name} with null record;
      % endif
      No_${e.api_name} : constant ${e.api_name} :=
        (Internal   => Implementation.${root_entity.nullexpr},
         Safety_Net => Implementation.No_Node_Safety_Net);
   % endfor

   procedure Check_Safety_Net (Self : ${T.root_node.entity.api_name}'Class);
   --  Check that Self's node and rebindings are still valid, raising a
   --  Stale_Reference_Error if one is not.

   --------------------------------
   -- Token Iterator (internals) --
   --------------------------------

   type Token_Iterator is record
      Node : ${root_entity.api_name};
      Last : Token_Index;
   end record;

   ---------------------------------
   -- Composite types (internals) --
   ---------------------------------

   % for t in ctx.composite_types:
      % if t.is_array_type:
         % if t.exposed:
            ${array_types.public_api_private_decl(t)}
         % endif
      % elif t.is_iterator_type:
        % if t.exposed:
            ${iterator_types.public_api_private_decl(t)}
        % endif
      % elif t.is_struct_type:
         % if t.exposed and not t.is_entity_type:
            ${struct_types.public_api_private_decl(t)}
         % endif
      % endif
   % endfor

   --  The dummy references to these packages forces them to be included in
   --  statically linked builds (thanks to the binder). This benefits the GDB
   --  helpers at no cost.

   Version : String renames ${ada_lib_name}.Version;
   procedure RN (Node : ${ada_lib_name}.Implementation.${T.root_node.name})
      renames ${ada_lib_name}.Debug.PN;

end ${ada_lib_name}.Analysis;
