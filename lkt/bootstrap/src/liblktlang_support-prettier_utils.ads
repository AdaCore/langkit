--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  .. note:: This unit is internal: only Langkit and Langkit-generated
--  libraries are supposed to use it.
--
--  This package provides helper on top of the Prettier_Ada library to build
--  documents incremenally: create a document, inspect it, possibly modify it,
--  and at the end produce the final Prettier_Ada document.

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Traces;
with Prettier_Ada.Documents;

with Liblktlang_Support.Generic_API; use Liblktlang_Support.Generic_API;
with Liblktlang_Support.Generic_API.Analysis;
use Liblktlang_Support.Generic_API.Analysis;
with Liblktlang_Support.Generic_API.Introspection;
use Liblktlang_Support.Generic_API.Introspection;
with Liblktlang_Support.Symbols;     use Liblktlang_Support.Symbols;
with Liblktlang_Support.Text;        use Liblktlang_Support.Text;

private package Liblktlang_Support.Prettier_Utils is

   package Prettier renames Prettier_Ada.Documents;
   use type Prettier.Symbol_Type;

   package Type_Vectors is new Ada.Containers.Vectors (Positive, Type_Ref);

   function Node_Matches
     (Node : Lk_Node; Types : Type_Vectors.Vector) return Boolean
   with Pre => not Node.Is_Null;
   --  Return whether ``Node`` matches at least one type in ``Types``

   --  The Document_Type data structure serves two joint purposes:
   --
   --  * Represent unparsing configuration templates: these contain pure
   --    formatting directives and Recurse items (i.e. anything but Token
   --    items).
   --
   --  * Represent actual unparsing documents: tokens and formatting directives
   --    (i.e. anything but Recurse items).
   --
   --  Formatting directives map the Prettier IR/commands as closely as
   --  possible, with exceptions (for instance there is no Token or Whitespace
   --  command in Prettier) that allow us to refine raw unparsing documents,
   --  for example insert necessary whitespaces/newlines between tokens.

   -----------------------
   --  Template symbols --
   -----------------------

   type Template_Symbol is new Natural;
   subtype Some_Template_Symbol is
     Template_Symbol range 1 ..  Template_Symbol'Last;
   No_Template_Symbol : constant Template_Symbol := 0;

   --  The following map type is used during templates parsing to validate the
   --  names used as symbols in JSON templates, and to turn them into their
   --  internal representation: ``Template_Symbol``.

   type Symbol_Info is record
      Source_Name : Unbounded_String;
      --  Name for this symbol as found in the unparsing configuration

      Template_Sym : Some_Template_Symbol;
      --  Unique identifier for this symbol

      Has_Definition : Boolean;
      --  Whether we have found one definition for this symbol

      Is_Referenced : Boolean;
      --  Whether we have found at least one reference to this symbol
   end record;

   package Symbol_Parsing_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Symbol_Info,
      Hash            => Hash,
      Equivalent_Keys => "=");

   Duplicate_Symbol_Definition : exception;

   function Declare_Symbol
     (Source_Name : Unbounded_String;
      Symbols     : Symbol_Table;
      Symbol_Map  : in out Symbol_Parsing_Maps.Map)
      return Some_Template_Symbol;
   --  Return the template symbol corresponding to ``Source_Name`` (creating it
   --  if needed) and mark it as being declared in ``Symbol_Map``.
   --
   --  Raise a ``Duplicate_Symbol_Definition`` exception if that symbol was
   --  already marked as declared.

   function Reference_Symbol
     (Source_Name : Unbounded_String;
      Symbols     : Symbol_Table;
      Symbol_Map  : in out Symbol_Parsing_Maps.Map)
      return Some_Template_Symbol;
   --  Return the template symbol corresponding to ``Source_Name`` (creating it
   --  if needed) and mark it as being referenced in ``Symbol_Map``.

   function Extract_Definitions
     (Source : Symbol_Parsing_Maps.Map) return Symbol_Parsing_Maps.Map;
   --  Return a new map that contains only entries from ``Source`` that have
   --  the ``Has_Definition`` component set to true, resetting their
   --  ``Is_Referenced`` component to False.
   --
   --  This is useful when creating the initial symbol map to parse templates
   --  for (A) node fields or (B) list separators from the map obtained after
   --  parsing the corresponding (C) node template: symbols defined in (C) can
   --  be referenced from both (A) and (B), but symbols referenced in (C) must
   --  be marked as referenced in (A) or (B) only if these templates do
   --  reference them.

   type Document_Record;
   type Document_Type is access all Document_Record;

   package Document_Vectors is new Ada.Containers.Vectors
     (Positive, Document_Type);

   package Type_Ref_Vectors is new Ada.Containers.Vectors (Positive, Type_Ref);

   type Matcher_Record is record
      Matched_Types : Type_Ref_Vectors.Vector;
      Document      : Document_Type;
   end record;

   function Matches (Node : Lk_Node; Matcher : Matcher_Record) return Boolean;
   --  Return if ``Node`` matches at least one type in
   --  ``Matcher.Matched_Types``.

   package Matcher_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Matcher_Record);

   package Trivias_Bubble_Up is

      --  Trivias bubbling up behavior for align/fill/group/indent documents
      --  (see the Bubble_Up_Trivias procedure).
      --
      --  Each kind of document has default behavior (the constants below), but
      --  unparsing configuration may override it.

      type Config is record
         Leading, Trailing : Boolean;
         --  For leading and trailing trivas, whether trivias are hoisted up in
         --  the document tree (True) or whether they are retained (False).
      end record;

      Align_Default_Config  : constant Config := (True, False);
      Fill_Default_Config   : constant Config := (True, True);
      Group_Default_Config  : constant Config := (True, True);
      Indent_Default_Config : constant Config := (True, False);

   end Trivias_Bubble_Up;

   type Document_Kind is
     (Align,
      Break_Parent,
      Empty_Table_Separator,
      Expected_Line_Breaks,
      Expected_Whitespaces,
      Fill,
      Flush_Line_Breaks,
      Group,
      Hard_Line,
      Hard_Line_Without_Break_Parent,
      If_Break,
      If_Empty,
      If_Kind,
      Indent,
      Line,
      List,
      Literal_Line,
      Recurse,
      Recurse_Field,
      Recurse_Flatten,
      Recurse_Left,
      Recurse_Right,
      Soft_Line,
      Table,
      Table_Separator,
      Token,
      Trim,
      Whitespace);

   subtype Template_Document_Kind is Document_Kind
   with Static_Predicate =>
     Template_Document_Kind not in
       Expected_Line_Breaks
     | Expected_Whitespaces
     | Table;

   subtype Instantiated_Template_Document_Kind is Document_Kind
   with Static_Predicate =>
     Instantiated_Template_Document_Kind not in
       If_Empty
     | If_Kind
     | Recurse
     | Recurse_Field
     | Recurse_Flatten
     | Recurse_Left
     | Recurse_Right;

   subtype Final_Document_Kind is Instantiated_Template_Document_Kind
   with Static_Predicate =>
     Final_Document_Kind not in
       Expected_Line_Breaks
     | Expected_Whitespaces
     | Flush_Line_Breaks;

   type Document_Record (Kind : Document_Kind := Document_Kind'First) is record

      --  Note that for "stateless leaf" documents which occur very frequently
      --  (Hard_Line, Token, ...), we precompute the corresponding Prettier
      --  ``Document_Type`` value here so that the ``To_Prettier_Document``
      --  pass does not re-create them over and over, both to avoid memory
      --  allocactions and to improve run time performance. This is the purpose
      --  of the ``..._Prettier_Doc`` components below.

      case Kind is
         when Align =>
            Align_Data      : Prettier.Alignment_Data_Type;
            Align_Contents  : Document_Type;
            Align_Bubble_Up : Trivias_Bubble_Up.Config;

         when Break_Parent =>
            null;

         when Empty_Table_Separator =>
            Empty_Table_Separator_Prettier_Doc : Prettier.Document_Type;

         when Expected_Line_Breaks =>
            Expected_Line_Breaks_Count : Positive;

         when Expected_Whitespaces =>
            Expected_Whitespaces_Count : Positive;

         when Fill =>
            Fill_Document  : Document_Type;
            Fill_Bubble_Up : Trivias_Bubble_Up.Config;

         when Flush_Line_Breaks =>
            null;

         when Group =>
            Group_Document     : Document_Type;
            Group_Should_Break : Boolean;
            Group_Id           : Template_Symbol;
            Group_Bubble_Up    : Trivias_Bubble_Up.Config;

         when Hard_Line =>
            Hard_Line_Prettier_Doc : Prettier.Document_Type;

         when Hard_Line_Without_Break_Parent =>
            Hard_Line_Without_Break_Parent_Prettier_Doc :
              Prettier.Document_Type;

         when If_Break =>
            If_Break_Contents      : Document_Type;
            If_Break_Flat_Contents : Document_Type;
            If_Break_Group_Id      : Template_Symbol;

         when If_Empty =>
            If_Empty_Then : Document_Type;
            If_Empty_Else : Document_Type;

         when If_Kind =>
            If_Kind_Field    : Struct_Member_Ref;
            If_Kind_Matchers : Matcher_Vectors.Vector;
            If_Kind_Default  : Document_Type;
            If_Kind_Absent   : Document_Type;

         when Indent =>
            Indent_Document  : Document_Type;
            Indent_Bubble_Up : Trivias_Bubble_Up.Config;

         when Line =>
            null;

         when List =>
            List_Documents : Document_Vectors.Vector;

         when Literal_Line =>
            null;

         when Recurse =>
            null;

         when Recurse_Field =>
            Recurse_Field_Ref : Struct_Member_Ref;
            --  Node field on which to recurse

            Recurse_Field_Position : Positive;
            --  1-based index for this field in the list of fields for the
            --  owning node.
            --
            --  This information is in theory redundant with the field
            --  reference, but using an index allows template instantantiation
            --  code to use an array rather than a map to store information
            --  related to fields: more simple and probably more efficient.

         when Recurse_Flatten =>
            Recurse_Flatten_Types : Type_Vectors.Vector;

         when Recurse_Left | Recurse_Right =>
            null;

         when Soft_Line =>
            null;

         when Table =>
            Table_Rows       : Document_Vectors.Vector;
            Table_Must_Break : Boolean;

         when Table_Separator | Token =>
            Token_Kind         : Token_Kind_Ref;
            Token_Text         : Unbounded_Text_Type;
            Token_Prettier_Doc : Prettier.Document_Type;

         when Trim =>
            null;

         when Whitespace =>
            Whitespace_Length       : Positive;
            Whitespace_Prettier_Doc : Prettier.Document_Type;
      end case;
   end record;

   function To_Prettier_Document
     (Document : Document_Type) return Prettier.Document_Type;
   --  Turn an unparsing document into an actual Prettier document

   --  Templates have different kinds depending on how they should be
   --  instantiated:
   --
   --  * No_Template_Kind: Special value to designate the absence of template.
   --
   --  * With_Recurse: Node or field template to instantiate with a single node
   --    document argument. For node templates, the argument must embed all the
   --    tokens for that node. For field templates, the argument must embed
   --    only the unparsing of the node that the field contains.
   --
   --  * With_Recurse_Field: Node template to instantiate with one argument per
   --    field.
   --
   --  * With_Text_Recurse: Field template to instantiate with a single node
   --    argument (the field). The argument must embed all the tokens for the
   --    field (pre/post tokens plus the unparsing of the node that the field
   --    contains).

   type Template_Kind is
     (No_Template_Kind,
      With_Recurse,
      With_Recurse_Field,
      With_Text_Recurse,
      Join_Template);
   subtype Some_Template_Kind is Template_Kind
     range Template_Kind'Succ (Template_Kind'First) ..  Template_Kind'Last;
   type Template_Type (Kind : Template_Kind := No_Template_Kind) is record
      case Kind is
         when No_Template_Kind =>
            null;

         when Some_Template_Kind =>
            Root : Document_Type;
            --  Root node for this template

            Symbols : Symbol_Parsing_Maps.Map;
            --  Symbols that are referenced and defined in this template
      end case;
   end record;
   --  Template document extended with information about how to instantiate it

   No_Template : constant Template_Type := (Kind => No_Template_Kind);

   type Document_Pool is limited private;
   --  Allocation pool for ``Document_Type`` nodes

   procedure Refresh_Prettier_Documents (Pool : in out Document_Pool);
   --  Recompute the Prettier ``Document_Type`` values for all nodes in
   --  ``Pool``. Doing so is necessary between each call to Prettier's
   --  ``Format`` function, as each Prettier document contains an ID, and that
   --  ``Format`` resets the ID generator: we need to create new documents with
   --  fresh IDs past this point.

   procedure Release (Self : in out Document_Pool);
   --  Free all the Document_Type nodes allocated in ``Self``

   function Deep_Copy
     (Pool : in out Document_Pool; Self : Document_Type) return Document_Type;
   --  Create a deep copy of the given document.
   --
   --  Note: as a light optimization, leaf nodes that are considered as
   --  constant (singletons and tokens) are not duplicated: they are returned
   --  as-is instead.

   function Create_Align
     (Self      : in out Document_Pool;
      Data      : Prettier.Alignment_Data_Type;
      Contents  : Document_Type;
      Bubble_Up : Trivias_Bubble_Up.Config) return Document_Type;
   --  Return an ``Align`` node

   function Create_Break_Parent
     (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Break_Parent`` node

   function Create_Empty_Table_Separator
     (Self : in out Document_Pool) return Document_Type;
   --  Return an ``Empty_Table_Separator`` node

   function Create_Expected_Line_Breaks
     (Self : in out Document_Pool; Count : Positive) return Document_Type;
   --  Return an ``Expected_Line_Breaks`` node

   function Create_Expected_Whitespaces
     (Self : in out Document_Pool; Count : Positive) return Document_Type;
   --  Return an ``Expected_Whitespaces`` node

   function Create_Fill
     (Self      : in out Document_Pool;
      Document  : Document_Type;
      Bubble_Up : Trivias_Bubble_Up.Config) return Document_Type;
   --  Return a ``Fill`` node

   function Create_Flush_Line_Breaks
     (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Flush_Line_Breaks`` node

   function Create_Group
     (Self         : in out Document_Pool;
      Document     : Document_Type;
      Should_Break : Boolean;
      Id           : Template_Symbol;
      Bubble_Up    : Trivias_Bubble_Up.Config) return Document_Type;
   --  Return a ``Group`` node

   function Create_Hard_Line
     (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Hard_Line`` node

   function Create_Hard_Line_Without_Break_Parent
     (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Hard_Line_Without_Break_Parent`` node

   function Create_If_Break
     (Self          : in out Document_Pool;
      Contents      : Document_Type;
      Flat_Contents : Document_Type := null;
      Group_Id      : Template_Symbol := No_Template_Symbol)
      return Document_Type;
   --  Return an ``If_Break`` node

   function Create_If_Empty
     (Self          : in out Document_Pool;
      Then_Contents : Document_Type;
      Else_Contents : Document_Type) return Document_Type;
   --  Return an ``If_Empty`` node

   function Create_If_Kind
     (Self             : in out Document_Pool;
      If_Kind_Field    : Struct_Member_Ref;
      If_Kind_Matchers : in out Matcher_Vectors.Vector;
      If_Kind_Default  : Document_Type;
      If_Kind_Absent   : Document_Type) return Document_Type;
   --  Return an ``If_Kind`` node

   function Create_Indent
     (Self      : in out Document_Pool;
      Document  : Document_Type;
      Bubble_Up : Trivias_Bubble_Up.Config) return Document_Type;
   --  Return an ``Indent`` node

   function Create_Line (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Line`` node

   function Create_List
     (Self      : in out Document_Pool;
      Documents : in out Document_Vectors.Vector) return Document_Type;
   --  Transfer all nodes in ``Documents`` to a new ``List`` node and return
   --  that new node.

   function Create_Literal_Line
     (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Literal_Line`` node

   function Create_Empty_List
     (Self : in out Document_Pool) return Document_Type;
   --  Return a new empty ``List`` node

   function Create_Recurse (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Recurse`` node

   function Create_Recurse (Self : in out Document_Pool) return Template_Type;
   --  Return a ``Recurse`` node wrapped in a ``With_Recurse`` template

   function Create_Recurse_Field
     (Self     : in out Document_Pool;
      Field    : Struct_Member_Ref;
      Position : Positive) return Document_Type;
   --  Return a ``Recurse_Field`` node

   function Create_Recurse_Flatten
     (Self  : in out Document_Pool;
      Types : in out Type_Vectors.Vector) return Document_Type;
   --  Return a ``Recurse_Flatten`` node

   function Create_Recurse_Left
     (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Recurse_Left`` node

   function Create_Recurse_Right
     (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Recurse_Right`` node

   function Create_Table
     (Self       : in out Document_Pool;
      Rows       : in out Document_Vectors.Vector;
      Must_Break : Boolean) return Document_Type;
   --  Return a ``Table`` node

   function Create_Table_Separator
     (Self : in out Document_Pool;
      Kind : Token_Kind_Ref;
      Text : Unbounded_Text_Type) return Document_Type;
   --  Return a ``Table_Separator`` node

   function Create_Soft_Line
     (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Soft_Line`` node

   function Create_Token
     (Self : in out Document_Pool;
      Kind : Token_Kind_Ref;
      Text : Unbounded_Text_Type) return Document_Type;
   --  Return a ``Token`` node

   function Create_Trim (Self : in out Document_Pool) return Document_Type;
   --  Return a ``Trim`` node

   function Create_Whitespace
     (Self   : in out Document_Pool;
      Length : Positive := 1) return Document_Type;
   --  Return a ``Whitespace`` node for the given length

   procedure Bubble_Up_Trivias
     (Pool : in out Document_Pool; Document : in out Document_Type);
   --  Recursively hoist the trivias up in the document tree in
   --  align/fill/group/indent documents (controlled by each document bubble up
   --  configuration.
   --
   --  For instance, if Document.Group_Bubble_Up.Leading is true, apply
   --  the following transformation::
   --
   --    group([<trivia>, ...]) -> [<trivia>, group([...])]
   --
   --  and if Document.Group_Bubble_Up.Trailing is true, apply::
   --
   --    group([..., <trivia>]) -> [group([...]), <trivia>]

   procedure Detect_Broken_Groups
     (Self : in out Document_Type; Max_Empty_Lines : Integer);
   --  Set the Group_Should_Break flag for all groups that can be statically
   --  proven to be broken.
   --
   --  See ``Unparsing_Configuration_Record.Max_Empty_Lines`` for the semantics
   --  of ``Max_Empty_Lines``.

   procedure Dump
     (Document : Document_Type; Trace : GNATCOLL.Traces.Trace_Handle := null);
   --  Debug helper: dump a textual representation of ``Document`` on the given
   --  trace (do nothing if the trace is disabled), or on the standard output
   --  (if ``Trace`` is null).

   -------------
   -- Spacing --
   -------------

   type Spacing_Kind is (None, Whitespaces, Line_Breaks);
   --  Spacing required between two tokens:
   --
   --  * ``None``: no spacing required, the two tokens can be unparsed next to
   --    each other in the source buffer (spacing is permitted, but not
   --    necessary).
   --
   --  * ``Whitespaces``: a given number of whitespaces is required after the
   --    first token. Note that one line break satisfies an arbitrary number of
   --    required whitespaces.
   --
   --  * ``Line_Breaks``: a given number of line breaks is required right after
   --    the first token.  Extra spacing is permitted after that line break.

   subtype Some_Spacing_Kind is Spacing_Kind range Whitespaces .. Line_Breaks;

   type Spacing_Type (Kind : Spacing_Kind := Spacing_Kind'First) is record
      case Kind is
         when None                      => null;
         when Whitespaces | Line_Breaks => Count : Positive;
      end case;
   end record;

   No_Spacing             : constant Spacing_Type := (Kind => None);
   One_Whitespace_Spacing : constant Spacing_Type :=
     (Kind => Whitespaces, Count => 1);
   One_Line_Break_Spacing : constant Spacing_Type :=
     (Kind => Line_Breaks, Count => 1);

   --  There is a total order for all possible Spacing_Type values:
   --
   --  * ``No_Spacing`` is the weakest spacing requirement.
   --  * ``One_Whitespace_Spacing`` is the second weakest.
   --  * ``(Whitespaces, 2)`` comes third.
   --  * ...
   --  * ``One_Line_Break_Spacing`` is stronger than all whitespaces
   --    requirements.
   --  * Then comes ``(Line_Breaks, 2)``.
   --  * ... and so on.

   function "<" (Left, Right : Spacing_Type) return Boolean;
   function "<=" (Left, Right : Spacing_Type) return Boolean
   is (Left < Right or else Left = Right);

   function Max_Spacing (Left, Right : Spacing_Type) return Spacing_Type
   is (if Left < Right then Right else Left);

   function Min_Spacing (Left, Right : Spacing_Type) return Spacing_Type
   is (if Left < Right then Left else Right);

   function Required_Spacing
     (Left, Right : Token_Kind_Ref) return Spacing_Type;
   --  Return the spacing that is required when unparsing a token of kind
   --  ``Right`` just after a token of kind ``Left`` to a source buffer.
   --
   --  For convenience, ``Required_Spacing`` is allowed to be
   --  ``No_Token_Kind_Ref``: the result is always ``None`` in this case. The
   --  intended use case for this is when processing the first token to unparse
   --  to a source buffer: ``Left`` is ``No_Token_Kind_Ref`` (no token were
   --  unprase in the source buffer yet) and ``Right`` is the first token to
   --  unparse to the source buffer.

   function Required_Line_Breaks
     (Self : Spacing_Type; Max_Empty_Lines : Integer) return Natural;
   --  Return the number of line breaks that ``Self`` implies, within the limit
   --  implied by ``Max_Empty_Lines``.

   procedure Extend_Spacing
     (Self : in out Spacing_Type; Requirement : Spacing_Type);
   --  Shortcut for::
   --
   --     Self := Max_Spacing (Self, Requirement);

   procedure Insert_Required_Spacing
     (Pool            : in out Document_Pool;
      Document        : in out Document_Type;
      Max_Empty_Lines : Integer);
   --  Adjust the tree of nodes in ``Document`` so that formatting that
   --  unparsing document will leave the mandatory spacing between tokens (i.e.
   --  so that the formatted document can be re-parsed correctly).
   --
   --  See ``Unparsing_Configuration_Record.Max_Empty_Lines`` for the semantics
   --  of ``Max_Empty_Lines``.

private

   package Text_To_Document_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_Text_Type,
      Element_Type    => Document_Type,
      Hash            => Hash,
      Equivalent_Keys => "=");
   --  Memoization tables for Prettier documents corresponding to token texts

   function "+" (Text : Unbounded_Text_Type) return Unbounded_String
   is (To_Unbounded_String (To_UTF8 (To_Text (Text))));

   procedure Precompute_Prettier_Document (Self : Document_Type);
   --  Assuming that ``Self`` is a node that is supposed to carry a
   --  pre-computed Prettier document, do the pre-computation.

   type Document_Pool is limited record
      Documents : Document_Vectors.Vector;

      --  Some leaf nodes are used so often that allocating singletons rather
      --  than allocating once instance per use saves a lot of memory. We store
      --  them here, and the ``Create_*`` pool primitives are supposed to use
      --  these components to implement caching.

      Empty_Table_Separator          : Document_Type;
      Flush_Line_Breaks              : Document_Type;
      Hard_Line                      : Document_Type;
      Hard_Line_Without_Break_Parent : Document_Type;
      Line                           : Document_Type;
      Soft_Line                      : Document_Type;

      --  Do the same for leaf nodes parametrized by a positive number: store
      --  them in vectors, so that each number parameter has the corresponding
      --  node stored in the vector.

      Whitespaces          : Document_Vectors.Vector;
      Expected_Whitespaces : Document_Vectors.Vector;
      Expected_Line_Breaks : Document_Vectors.Vector;

      --  And finally do the same for token nodes: store them in maps, indexed
      --  by the token text.

      Token_Seps : Text_To_Document_Maps.Map;
      Texts      : Text_To_Document_Maps.Map;
   end record;

   procedure Register (Self : in out Document_Pool; Document : Document_Type);
   --  Register ``Document`` as allocated by ``Self``

end Liblktlang_Support.Prettier_Utils;
