--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  .. note:: At this stage, this package is considered as internal.
--
--  This package provides a generic API so that programs can work with the
--  $.Analysis packages of all Langkit-generated libraries.
--
--  .. todo::
--
--     Create a language-agnostic documentation for all the concepts
--     referenced here (context, unit, node, token, trivia, ...).

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
private with Ada.Finalization;

with Langkit_Support.Diagnostics;  use Langkit_Support.Diagnostics;
with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;
private with Langkit_Support.Internal.Analysis;
with Langkit_Support.Slocs;        use Langkit_Support.Slocs;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

package Langkit_Support.Generic_API.Analysis is

   type Lk_Context is tagged private;
   type Lk_Unit is new Langkit_Support.Text.Text_Buffer_Ifc with private;
   type Lk_Node is tagged private;
   type Lk_Token is tagged private;
   --  Reference types to actual context/unit/node/token objects. Only
   --  ``Lk_Context`` and ``Lk_Unit`` are strong references (designated object
   --  lives as long as the reference exists): ``Lk_Node`` and ``Lk_Token_Ref``
   --  are weak references (designated object may be destroyed even if there
   --  are still references to it, but usage of such a stale reference is
   --  properly rejected).

   No_Lk_Context : constant Lk_Context;
   --  Special value to mean the absence of analysis context

   No_Lk_Unit : constant Lk_Unit;
   --  Special value to mean the absence of analysis unit

   No_Lk_Node : constant Lk_Node;
   --  Special value to mean the absence of analysis node

   No_Lk_Token : constant Lk_Token;

   --  TODO: add unit providers handling

   ------------------------
   -- Context operations --
   ------------------------

   function Create_Context
     (Language      : Language_Id;
      Charset       : String := "";
      File_Reader   : File_Reader_Reference := No_File_Reader_Reference;
      With_Trivia   : Boolean := True;
      Tab_Stop      : Natural := 0) return Lk_Context;
   --  Create a new analysis context.
   --
   --  ``Charset`` will be used as a default charset to decode input sources
   --  in analysis units. Please see ``GNATCOLL.Iconv`` for several supported
   --  charsets. Be careful: passing an unsupported charset is not guaranteed
   --  to raise an error here. If no charset is provided, use the language
   --  default.
   --
   --  When ``With_Trivia`` is true, the parsed analysis units will contain
   --  trivias.
   --
   --  If provided, ``File_Reader`` will be used to fetch the contents of
   --  source files instead of the default, which is to just read it from the
   --  filesystem and decode it using the regular charset rules. Note that if
   --  provided, all parsing APIs that provide a buffer are forbidden, and any
   --  use of the rewriting API with the returned context is rejected.
   --
   --  ``Tab_Stop`` is a positive number to describe the effect of tabulation
   --  characters on the column number in source files.

   function Language (Self : Lk_Context) return Language_Id;
   --  Return the unique identifier for the Langkit-generated library that
   --  implements ``Context``.

   function Hash (Self : Lk_Context) return Hash_Type;
   --  Hash function to use ``Lk_Context`` in hashed containers

   function Has_With_Trivia (Self : Lk_Context) return Boolean;
   --  Return whether ``Context`` keeps trivia when parsing units

   function Has_Unit
     (Self : Lk_Context; Unit_Filename : String) return Boolean;
   --  Return whether ``Context`` contains a unit corresponding to
   --  ``Unit_Filename``.

   function Get_From_File
     (Self     : Lk_Context'Class;
      Filename : String;
      Charset  : String := "";
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule_Ref := No_Grammar_Rule_Ref) return Lk_Unit;
   --  Create a new analysis unit for ``Filename`` or return the existing one
   --  if any. If ``Reparse`` is true and the analysis unit already exists,
   --  reparse it from ``Filename``.
   --
   --  ``Rule`` controls which grammar rule is used to parse the unit. If
   --  ``No_Grammar_Rule_Ref``, use the default grammar rule for this language.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as file opening, decoding, lexing or parsing
   --  failure, return an analysis unit anyway: errors are described as
   --  diagnostics of the returned analysis unit.
   --
   --  It is invalid to pass ``True`` to ``Reparse`` if a rewriting context is
   --  active.

   function Get_From_Buffer
     (Self     : Lk_Context'Class;
      Filename : String;
      Buffer   : String;
      Charset  : String := "";
      Rule     : Grammar_Rule_Ref := No_Grammar_Rule_Ref) return Lk_Unit;
   --  Create a new analysis unit for ``Filename`` or return the existing one
   --  if any. Whether the analysis unit already exists or not, (re)parse it
   --  from the source code in ``Buffer``.
   --
   --  ``Rule`` controls which grammar rule is used to parse the unit. If
   --  ``No_Grammar_Rule_Ref``, use the default grammar rule for this language.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as file opening, decoding, lexing or parsing
   --  failure, return an analysis unit anyway: errors are described as
   --  diagnostics of the returned analysis unit.
   --
   --  Calling this is invalid if a rewriting context is active.

   ------------------------------
   -- Analysis unit operations --
   ------------------------------

   function Language (Self : Lk_Unit) return Language_Id;
   --  Return the unique identifier corresponding to ``Self``

   function Hash (Self : Lk_Unit) return Hash_Type;
   --  Hash function to use ``Lk_Unit`` in hashed containers

   procedure Reparse_From_File (Self : Lk_Unit; Charset : String := "");
   --  Reparse ``Self`` from the associated file.
   --
   --   Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --   then use the context's default charset.

   procedure Reparse_From_Buffer
     (Self    : Lk_Unit;
      Buffer  : String;
      Charset : String := "");
   --  Reparse ``Self`` from an in-memory buffer.
   --
   --   Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --   then use the context's default charset.

   overriding function Get_Line
     (Self : Lk_Unit; Line_Number : Positive) return Text_Type;
   --  Return the line of text at line number ``Line_Number``

   function Context (Self : Lk_Unit'Class) return Lk_Context;
   --  Return the context that owns this unit

   function Filename (Self : Lk_Unit) return String;
   --  Return the filename this unit is associated to

   function Charset (Self : Lk_Unit) return String;
   --  Return the charset that was used to parse ``Self``

   function Has_Diagnostics (Self : Lk_Unit) return Boolean;
   --  Return whether this unit has associated diagnostics

   function Diagnostics (Self : Lk_Unit) return Diagnostics_Array;
   --  Return an array that contains the diagnostics associated to this unit

   function Format_GNU_Diagnostic
     (Self : Lk_Unit; D : Diagnostic) return String;
   --  Format a diagnostic in a GNU fashion. See
   --  <https://www.gnu.org/prep/standards/html_node/Errors.html>.

   function Root (Self : Lk_Unit'Class) return Lk_Node;
   --  Return the root node for this unit, or ``No_Lk_Node`` if there is
   --  none.

   function First_Token (Self : Lk_Unit'Class) return Lk_Token;
   --  Return a reference to the first token scanned in this unit

   function Last_Token (Self : Lk_Unit'Class) return Lk_Token;
   --  Return a reference to the last token scanned in this unit

   function Token_Count (Self : Lk_Unit'Class) return Natural;
   --  Return the number of tokens in this unit

   function Trivia_Count (Self : Lk_Unit'Class) return Natural;
   --  Return the number of trivias in this unit. This is 0 for units that were
   --  parsed with trivia analysis disabled.

   function Text (Self : Lk_Unit) return Text_Type;
   --  Return the source buffer associated to this unit

   function Lookup_Token
     (Self : Lk_Unit'Class; Sloc : Source_Location) return Lk_Token;
   --  Look for a token in ``Self`` that contains the given source location.
   --  If this falls before the first token, return the first token. If this
   --  falls between two tokens, return the token that appears before. If this
   --  falls after the last token, return the last token. If there is no token
   --  in this unit, return ``No_Lk_Token``.

   procedure Print (Self : Lk_Unit; Show_Slocs : Boolean := True);
   --  Debug helper: output this unit's parse tree and any diagnostics it has
   --  on standard output.
   --
   --  If ``Show_Slocs`` is true, include the nodes source locations in the
   --  output.

   -------------------------------
   -- Analysis nodes operations --
   -------------------------------

   function Language (Self : Lk_Node) return Language_Id;
   --  Return the unique identifier corresponding to ``Self``

   function Hash (Self : Lk_Node) return Hash_Type;
   --  Hash function to use ``Lk_Node`` in hashed containers

   function Unit (Self : Lk_Node'Class) return Lk_Unit;
   --  Return the unit that owns this node

   function Is_Null (Self : Lk_Node) return Boolean;
   --  Return whether ``Node`` is a null node reference

   function "=" (Left, Right : Lk_Node) return Boolean;
   --  Return whether ``Left`` and ``Right`` designate the same node.
   --
   --  Note that, unlike other operations accepting multiple generic types,
   --  checking equality is allowed for nodes coming from different languages.

   function Image (Self : Lk_Node) return String;
   --  Return a short string describing ``Node``, or ``"None"`` if
   --  ``Node.Is_Null`` is true.

   procedure Print
     (Node        : Lk_Node;
      Show_Slocs  : Boolean := True;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output ``Node`` and all its children.
   --
   --  If ``Show_Slocs``, include the source location of each node in the
   --  output.
   --
   --  ``Line_Prefix`` is prepended to each output line.

   type Lk_Node_Array is array (Positive range <>) of Lk_Node;

   function Parent (Self : Lk_Node'Class) return Lk_Node;
   --  Return the syntactic parent for ``Node``. Return a null node for the
   --  root one.

   function Parents
     (Self : Lk_Node; With_Self : Boolean := True) return Lk_Node_Array;
   --  Return an array that contains the lexical parents, this node included
   --  iff ``with_self`` is True. Nearer parents are first in the list.

   function Children_Count (Self : Lk_Node) return Natural;
   --  Return the number of children Node has

   procedure Get_Child
     (Self            : Lk_Node'Class;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Lk_Node);
   --  Return the ``Index``'th child of node, storing it into ``Result``.
   --
   --  Child indexing is 1-based. Store in ``Index_In_Bounds`` whether ``Node``
   --  had such a child: if not (i.e. ``Index`` is out-of-bounds), set
   --  ``Result`` to a null node.

   function Child (Self : Lk_Node'Class; Index : Positive) return Lk_Node;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Children (Self : Lk_Node) return Lk_Node_Array;
   --  Return the children in ``Node``

   function Next_Sibling (Self : Lk_Node'Class) return Lk_Node;
   --  Return the ``Node``'s next sibling, or null if there is no such sibling

   function Previous_Sibling (Self : Lk_Node'Class) return Lk_Node;
   --  Return the ``Node``'s previous sibling, or null if there is no such
   --  sibling.

   function Closest_Common_Parent (Self, Other : Lk_Node'Class) return Lk_Node;
   --  If ``Self`` and ``Other`` do not belong to the same analysis unit,
   --  return ``No_Lk_Node``. Otherwise, return the deepest node in the tree
   --  that is a parent for both ``Self`` and ``Other``.

   function Is_Ghost (Self : Lk_Node) return Boolean;
   --  Return whether the node is a ghost.
   --
   --  Unlike regular nodes, ghost nodes cover no token in the input source:
   --  they are logically located instead between two tokens.  Both the
   --  ``token_start`` and the ``token_end`` of all ghost nodes is the token
   --  right after this logical position.

   function Is_Token_Node (Self : Lk_Node'Class) return Boolean;
   --  Shortcut for ``Is_Token_Node (Type_Of (Self))``

   function Is_List_Node (Self : Lk_Node'Class) return Boolean;
   --  Shortcut for ``Is_List_Node (Type_Of (Self))``

   type Visit_Status is (Into, Over, Stop);
   --  Helper type to control the node traversal process. See the ``Traverse``
   --  subprograms below.

   function Traverse
     (Self  : Lk_Node;
      Visit : access function (Node : Lk_Node) return Visit_Status)
      return Visit_Status;
   --  Call ``Visit`` on ``Node`` and all its children, transitively. Calls
   --  happen in prefix order (i.e. top-down and left first). The traversal is
   --  controlled as follows by the result returned by Visit:
   --
   --  ``Into``
   --     The traversal continues normally with the syntactic children of the
   --     node just processed.
   --
   --  ``Over``
   --     The children of the node just processed are skipped and excluded from
   --     the traversal, but otherwise processing continues elsewhere in the
   --     tree.
   --
   --  ``Stop``
   --     The entire traversal is immediately abandoned, and the original call
   --     to ``Traverse`` returns ``Stop``.

   procedure Traverse
     (Self  : Lk_Node;
      Visit : access function (Node : Lk_Node) return Visit_Status);
   --  This is the same as ``Traverse`` function except that no result is
   --  returned i.e. the ``Traverse`` function is called and the result is
   --  simply discarded.

   function Token_Start (Self : Lk_Node'Class) return Lk_Token;
   --  Return the first token used to parse this node

   function Token_End (Self : Lk_Node'Class) return Lk_Token;
   --  Return the last token used to parse this node

   function Text (Self : Lk_Node) return Text_Type;
   --  Return the source buffer slice corresponding to the text that spans
   --  between the first and the last tokens of this node.
   --
   --  Note that this returns the empty string for synthetic nodes.

   function Sloc_Range (Self : Lk_Node) return Source_Location_Range;
   --  Return the spanning source location range for this node.
   --
   --  Note that this returns the sloc of the parent for synthetic nodes.

   function Compare
     (Node : Lk_Node; Sloc : Source_Location) return Relative_Position;
   --  Compare Sloc to the sloc range of Node

   function Lookup
     (Self : Lk_Node'Class; Sloc : Source_Location) return Lk_Node;
   --  Look for the bottom-most node in the ``Self`` subtree whose sloc range
   --  contains ``Sloc``. Return it, or ``No_Lk_Node`` if no such node was
   --  found.

   function Is_Incomplete (Self : Lk_Node) return Boolean;
   --  Return whether this node is incomplete, i.e. whether its parsing
   --  partially failed.

   type Node_Or_Token (Is_Node : Boolean := False) is record
      case Is_Node is
         when False => Token : Lk_Token;
         when True  => Node  : Lk_Node;
      end case;
   end record;
   --  Variant that holds either a parse node or a token

   type Node_Or_Token_Sequence is private
      with Iterable => (First       => First,
                        Next        => Next,
                        Has_Element => Has_Element,
                        Element     => Element,
                        Last        => Last,
                        Previous    => Previous);
   --  Iterable over a sequence of nodes and tokens.
   --
   --  Note that in principle, we would like to expose a vector of
   --  ``Node_Or_Token`` here. However, Ada validy rules prevent us from
   --  instantiating a generic with Lk_Node and Lk_Token, as they are private
   --  types.

   function First (Self : Node_Or_Token_Sequence) return Natural;
   --  Return the first item in ``Self`` (helper for the ``Iterable`` aspect)

   function Last (Self : Node_Or_Token_Sequence) return Natural;
   --  Return the last item in ``Self`` (helper for the ``Iterable`` aspect)

   function Next (Self : Node_Or_Token_Sequence; Pos : Natural) return Natural;
   --  Return the next item in ``Self`` (helper for the ``Iterable`` aspect)

   function Previous
     (Self : Node_Or_Token_Sequence; Pos : Natural) return Natural;
   --  Return the previous item in ``Self`` (helper for the ``Iterable``
   --  aspect).

   function Has_Element
     (Self : Node_Or_Token_Sequence; Pos : Natural) return Boolean;
   --  Return if ``Pos`` is in ``Self``'s iteration range (elper for the
   --  ``Iterable`` aspect).

   function Element
     (Self : Node_Or_Token_Sequence; Pos : Natural) return Node_Or_Token;
   --  Return the element at position ``Pos`` in ``Self`` (helper for the
   --  ``Iterable`` aspect).

   function Children_And_Trivia (Self : Lk_Node) return Node_Or_Token_Sequence;
   --  Return the children of this node interleaved with Trivia token nodes, so
   --  that:
   --
   --  * Every trivia contained between ``Node.Start_Token`` and
   --    ``Node.End_Token.Previous`` will be part of the returned array.
   --
   --  * Nodes and trivias will be lexically ordered.

   ----------------------
   -- Token operations --
   ----------------------

   function Language (Self : Lk_Token) return Language_Id;
   --  Return the unique identifier corresponding to ``Self``

   function Hash (Self : Lk_Token) return Hash_Type;
   --  Hash function to use ``Lk_Token`` in hashed containers

   function Unit (Self : Lk_Token'Class) return Lk_Unit;
   --  Return the unit that owns this token

   function Is_Null (Self : Lk_Token) return Boolean;
   --  Return whether ``Self`` is a null token reference

   function Kind (Self : Lk_Token) return Token_Kind_Ref;
   --  Return the token kind for ``Self``

   function "<" (Left, Right : Lk_Token) return Boolean;
   --  Assuming ``Left`` and ``Right`` belong to the same analysis unit, return
   --  whether ``Left`` came before ``Right`` in the source file.
   --
   --  Raise a ``Precondition_Failure`` error if they do not belong to the same
   --  analysis unit.

   function Next
     (Self           : Lk_Token'Class;
      Exclude_Trivia : Boolean := False) return Lk_Token;
   --  Return a reference to the next token in the corresponding analysis unit

   function Previous
     (Self           : Lk_Token'Class;
      Exclude_Trivia : Boolean := False) return Lk_Token;
   --  Return a reference to the previous token in the corresponding analysis
   --  unit.

   function Image (Self : Lk_Token) return String;
   --  Debug helper: return a human-readable text to represent a token

   function Text (Self : Lk_Token) return Text_Type;
   --  Return the text of the token as ``Text_Type``

   function Text (First, Last : Lk_Token) return Text_Type;
   --  Compute the source buffer slice corresponding to the text that spans
   --  between the ``First`` and ``Last`` tokens (both included). This yields
   --  an empty slice if ``Last`` actually appears before ``First``.
   --
   --  This raises a ``Precondition_Failure`` if ``First`` and ``Last`` don't
   --  belong to the same analysis unit.

   function Is_Comment (Self : Lk_Token) return Boolean;
   --  Return whether unparsing must treat the given token kind as a comment,
   --  i.e. a trivia to preserve in unparsed sources. Return False if passed a
   --  null token.

   function Is_Trivia (Self : Lk_Token) return Boolean;
   --  Return whether this token is not null and is a trivia

   function Index (Self : Lk_Token) return Token_Index;
   --  One-based index for this token/trivia. Tokens and trivias get their own
   --  index space.

   function Sloc_Range (Self : Lk_Token) return Source_Location_Range;
   --  Source location range for this token. Note that the end bound is
   --  exclusive.

   function Is_Equivalent (Left, Right : Lk_Token) return Boolean;
   --  Return whether ``Left`` and ``Right`` are structurally equivalent
   --  tokens. This means that their position in the stream won't be taken into
   --  account, only the kind and text of the token.

   function Origin_Filename (Self : Lk_Token) return String;
   --  Return the name of the file whose content was scanned to create Token.
   --  Return an empty string if the source comes from a memory buffer instead
   --  of a file.

   function Origin_Charset (Self : Lk_Token) return String;
   --  Return the charset used to decode the source that was scanned to create
   --  Token. Return an empty string if the source was already decoded during
   --  the scan.

private

   use Langkit_Support.Internal.Analysis;

   type Lk_Context is new Ada.Finalization.Controlled with record
      Desc     : Any_Language_Id;
      Internal : Internal_Context;
   end record;

   overriding procedure Initialize (Self : in out Lk_Context);
   overriding procedure Adjust (Self : in out Lk_Context);
   overriding procedure Finalize (Self : in out Lk_Context);

   type Lk_Unit is new Langkit_Support.Text.Text_Buffer_Ifc with record
      Internal : Internal_Unit;
      --  Note that this is a System.Address derivation, not an access type, so
      --  it is not automatically initialized. We want to consider
      --  uninitialized Lk_Unit objects as null ones, so null checks must be
      --  done on the Context component instead.

      Context : Lk_Context;
      --  Keep a reference to the owning context so that the context lives as
      --  long as there is at least one reference to one of its units.
   end record;

   type Lk_Node is new Ada.Finalization.Controlled with record
      Desc       : Any_Language_Id;
      Internal   : Internal_Entity;
      Safety_Net : Node_Safety_Net;
   end record;

   overriding procedure Initialize (Self : in out Lk_Node);
   overriding procedure Adjust (Self : in out Lk_Node);
   overriding procedure Finalize (Self : in out Lk_Node);

   type Lk_Token is tagged record
      Desc       : Any_Language_Id;
      TDH        : Token_Data_Handler_Access;
      Index      : Token_Or_Trivia_Index;
      Safety_Net : Token_Safety_Net;
   end record;

   No_Lk_Context : constant Lk_Context :=
     (Ada.Finalization.Controlled with
      Desc     => null,
      Internal => No_Internal_Context);
   No_Lk_Unit    : constant Lk_Unit :=
     (Internal => No_Internal_Unit,
      Context  => (Ada.Finalization.Controlled with
                   Desc     => null,
                   Internal => No_Internal_Context));
   No_Lk_Node    : constant Lk_Node :=
     (Ada.Finalization.Controlled with
      Desc       => null,
      Internal   => No_Internal_Entity,
      Safety_Net => No_Node_Safety_Net);

   No_Lk_Token : constant Lk_Token :=
     (null, null, No_Token_Or_Trivia_Index, No_Token_Safety_Net);

   package Node_Or_Token_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Node_Or_Token);

   type Node_Or_Token_Sequence is record
      Elements : Node_Or_Token_Vectors.Vector;
   end record;

end Langkit_Support.Generic_API.Analysis;
