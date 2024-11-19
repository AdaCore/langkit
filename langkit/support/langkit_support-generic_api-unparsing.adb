--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Directories;          use Ada.Directories;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Unchecked_Deallocation;

pragma Warnings (Off, "internal");
with Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "internal");

with GNAT.Regpat;
with GNAT.Strings;

with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.Opt_Parse;
with GNATCOLL.VFS;  use GNATCOLL.VFS;
with Prettier_Ada.Documents.Json;

with Langkit_Support.Errors;         use Langkit_Support.Errors;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;
with Langkit_Support.Internal.Unparsing;
use Langkit_Support.Internal.Unparsing;
with Langkit_Support.Prettier_Utils; use Langkit_Support.Prettier_Utils;
with Langkit_Support.Slocs;          use Langkit_Support.Slocs;
with Langkit_Support.Symbols;        use Langkit_Support.Symbols;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

package body Langkit_Support.Generic_API.Unparsing is

   use type Ada.Containers.Count_Type;

   function Image_With_Sloc (T : Lk_Token) return String
   is (T.Image & " (" & Image (Start_Sloc (T.Sloc_Range)) & ")");

   function Load_Unparsing_Config_From_Buffer
     (Language        : Language_Id;
      Buffer          : String;
      Diagnostics     : in out Diagnostics_Vectors.Vector;
      Check_All_Nodes : Boolean)
      return Unparsing_Configuration;
   --  Like ``Load_Unparsing_Config``, but loading the unparsing configuration
   --  from an in-memory buffer rather than from a file.

   package String_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Unbounded_String,
      Equivalent_Elements => "=",
      Hash                => Hash);
   Traces_To_Enable : String_Sets.Set;

   procedure Process_Enable_Traces (Trace : GNATCOLL.Traces.Trace_Handle);
   --  Callback for GNATCOLL.Traces.For_Each_Handle. If Traces's name matches
   --  an element of Traces_To_Enable, activate it.

   function Is_Field_Present
     (Field          : Lk_Node;
      Field_Unparser : Field_Unparser_Impl) return Boolean;
   --  Return whether, according to ``Field_Unparser``, the field ``Field``
   --  must be considered as present for unparsing.

   function Token_Matches
     (Token : Lk_Token; Unparser : Token_Unparser) return Boolean
   is (To_Index (Token.Kind) = Unparser.Kind);
   --  Return whether the given ``Token`` has a kind that matches the one found
   --  in ``Unparser``.

   type Any_List_Sep_Template_Kind is
     (None, Sep_Template, Leading_Sep_Template, Trailing_Sep_Template);
   subtype List_Sep_Template_Kind is
     Any_List_Sep_Template_Kind range Sep_Template .. Trailing_Sep_Template;
   --  Kind of template for list node separators

   procedure Check_Same_Tokens (Original, Reformatted : Lk_Unit);
   --  Check that both units have the same sequence of tokens + comments. Print
   --  an error message and set the exit status to Failure if this not the
   --  case.

   --------------
   -- Fragment --
   --------------

   --  We define here a fragment as a slice of source code to unparse. Token
   --  fragments are atomic and directly translate to the corresponding source
   --  excerpt while node fragments must be decomposed into further fragments.

   type Unparsing_Fragment_Kind is
     (Token_Fragment,
      List_Separator_Fragment,
      Field_Fragment,
      List_Child_Fragment,
      Suffix_Comment,
      Line_Comment,
      Whitespaces,
      Line_Breaks);
   subtype Non_Trivia_Fragment_Kind is
     Unparsing_Fragment_Kind range Token_Fragment .. List_Child_Fragment;
   subtype Trivia_Fragment_Kind is
     Unparsing_Fragment_Kind range Suffix_Comment .. Line_Breaks;
   type Unparsing_Fragment
     (Kind : Unparsing_Fragment_Kind := Unparsing_Fragment_Kind'First)
   is record
      case Kind is
         when Token_Fragment | List_Separator_Fragment =>
            Token_Kind : Token_Kind_Ref;
            --  Token kind corresponding to this token fragment. This component
            --  is used to determine the spacing required in the source buffer
            --  between two consecutive tokens.

            Token_Text : Unbounded_Text_Type;
            --  Text to emit when unparsing this token fragment

            case Kind is
               when List_Separator_Fragment =>
                  List_Sep_Kind : List_Sep_Template_Kind;
                  --  Separator template to use for this separator fragment

               when others =>
                  null;
            end case;

         when Field_Fragment | List_Child_Fragment =>
            Node : Lk_Node;
            --  Node for this fragment

            case Kind is
               when Token_Fragment | List_Separator_Fragment =>
                  null;

               when Field_Fragment =>
                  Field : Struct_Member_Ref;
                  --  Syntax field for this fragment (the parent node is a
                  --  regular node).

                  Field_Unparser_Ref : Field_Unparser;
                  --  Unparser for this field

               when List_Child_Fragment =>
                  Child_Index : Positive;
                  --  Index of this field for this fragment (the parent node is
                  --  a list node).

               when Trivia_Fragment_Kind =>
                  null;
            end case;

         when Suffix_Comment | Line_Comment =>

            --  Fragment for:
            --
            --  *  ``Suffix_Comment``: a comment that appears as a line suffix
            --      (i.e. following a non-comment token on the same line).
            --
            --  * ``Line_Comment``: a comment that is alone on its line.

            Comment_Token : Lk_Token;

         when Whitespaces =>

            --  Fragment to record the presence of whitespaces before a suffix
            --  comment. This fragment is materialized only before a suffix
            --  comment: other whitespaces are ignored.

            Whitespaces_Count : Positive;
            --  Number of whitespaces found

         when Line_Breaks =>

            --  Fragment to record the presence of line breaks in the original
            --  source code.

            Line_Breaks_Count : Positive;
            --  Number of line breaks found
      end case;
   end record;
   --  Source code unparsing fragment used to unparse source code. Fragments
   --  are either tokens or nodes (that must be decomposed themselves into
   --  fragments).

   package Token_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Lk_Token,
      Hash                => Hash,
      Equivalent_Elements => "=");

   package Token_To_Node_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Lk_Token,
      Element_Type    => Lk_Node,
      Equivalent_Keys => "=",
      Hash            => Hash);

   --  Trivias from the original source code to unparse are generally processed
   --  in the same context as the token that preceeds them. There is one
   --  exception to this: trivias that precede or come after a list node, so
   --  that they are processed as part of the unparsing of that list node.
   --  Here, we call such trivias "reattached".
   --
   --  To implement this, a first pass (``Compute_Trivias_Info``) looks for
   --  trivias that precede/come after list tokens and add them to the
   --  ``First_Reattached_Trivias`` set. Each time we are about to process a
   --  sequence of trivias because they follow a token, we return early (skip
   --  them) if ``First_Reattached_Trivias`` contains the first trivia.
   --
   --  In addition, each time we unparse a list node, we force the processing
   --  of the sequence of trivias that precede/come after it. In order to avoid
   --  processing these trivias multiple times, we add them to the
   --  ``Processed_Reattached_Trivias`` set.

   type Any_Unparsing_Fragment_Index is new Natural;
   subtype Unparsing_Fragment_Index is
     Any_Unparsing_Fragment_Index
     range 1 ..  Any_Unparsing_Fragment_Index'Last;

   package Unparsing_Fragment_Vectors is new Ada.Containers.Vectors
     (Unparsing_Fragment_Index, Unparsing_Fragment);

   type Unparsing_Fragment_Range is record
      First : Unparsing_Fragment_Index;
      Last  : Any_Unparsing_Fragment_Index;
   end record;
   --  Range of unparsing fragments in an
   --  ``Unparsing_Fragment_Vectors.Vector``. As in Ada, ``First`` and ``Last``
   --  indexes are inclusive so ``First`` > ``Last`` designates the empty
   --  range.

   Empty_Unparsing_Fragment_Range : constant Unparsing_Fragment_Range :=
     (1, 0);

   type Unparsing_Fragment_Array is
     array (Token_Index range <>) of Unparsing_Fragment_Range;
   type Unparsing_Fragment_Array_Access is access Unparsing_Fragment_Array;

   type Trivias_Fragments is record
      Vector              : Unparsing_Fragment_Vectors.Vector;
      Trivia_To_Fragments : Unparsing_Fragment_Array_Access;
   end record;
   --  Mapping from sequences of trivias to the corresponding unparsing
   --  fragments.
   --
   --  For a sequence of consecutive trivias whose indexes are ``TI1 .. I2``,
   --  the corresponding unparsing fragments are stored in
   --  ``Trivias_Fragments.Vector`` as the range corresponding to
   --  ``Trivias_Fragments.Trivia_To_Fragments (TI1)``.

   type Trivias_Info is record
      First_Reattached_Trivias : Token_To_Node_Maps.Map;
      --  Mapping from first trivia to reattachment node for all sequences of
      --  reattached trivias.

      Processed_Reattached_Trivias : Token_Sets.Set;
      --  Subset of ``First_Reattached_Trivias`` keys for the trivias that were
      --  already processed (included in the unparsing).

      Fragments : Trivias_Fragments;
      --  Unparsing fragments for all trivias
   end record;

   procedure Free (Self : in out Trivias_Info);
   --  Release resources allocated for ``Self``

   function Fragment_For
     (Id       : Language_Id;
      Token    : Token_Unparser;
      List_Sep : Any_List_Sep_Template_Kind := None) return Unparsing_Fragment;
   --  Return the unparsing fragment corresponding to the given token.
   --
   --  ``List_Sep`` must be ``None`` if this fragment is not for a list
   --  separator, and the corresponding separate kind otherwise.

   procedure Compute_Trivia_Fragments
     (Node : Lk_Node; Fragments : out Trivias_Fragments);
   --  Compute unparsing fragments for all trivias in ``Node`` and store them
   --  in ``Fragments``.

   procedure Compute_Trivias_Info (Node : Lk_Node; Info : out Trivias_Info);
   --  Preparatory pass for the unparsing of trivias (empty lines, comments):
   --  compute information about trivias found in the ``Node`` subtree and fill
   --  ``Info``'s data structures accordingly.

   function Reattached_Trivia_Needs_Processing
     (Token : Lk_Token; Node : Lk_Node; Info : Trivias_Info) return Boolean;
   --  Return whether ``Info.First_Reattached_Trivias`` contains a ``Token ->
   --  Node`` entry.

   procedure Iterate_On_Trivia_Fragments
     (First_Trivia : Lk_Token;
      Info         : Trivias_Info;
      Process      : access procedure (Fragment : Unparsing_Fragment));
   --  Assuming that ``First_Trivia`` designates a trivia token (the first in a
   --  sequence of consecutive trivias), call ``Process`` on all the
   --  corresponding unparsing fragments.

   function First_Trivia_Before (Node : Lk_Node) return Lk_Token;
   --  Return the first trivia (i.e. the trivia with the lowest index) in the
   --  contiguous sequence of trivias that precedes the given node's first
   --  token, or ``No_Lk_Token`` if no trivia immediately precedes that first
   --  token.

   function Count_Line_Breaks (Token : Lk_Token) return Natural;
   --  Return the number of line breaks found in ``Token``

   function Is_Empty_List (Node : Lk_Node) return Boolean;
   --  Return whether ``Node`` is a list node that can be considered empty for
   --  the purpose of unparsing (i.e. for the ``ifEmpty`` command).
   --
   --  In this context, we consider that a list node with no child but with
   --  attached comments is *not* empty. This makes more sense for formatting
   --  concerns, as we unparse these comments as list children.

   procedure Process_Trivias
     (Token              : in out Lk_Token;
      Items              : in out Document_Vectors.Vector;
      Pool               : in out Document_Pool;
      Trivias            : Trivias_Info;
      Skip_Token         : Boolean;
      Skip_If_Reattached : Boolean := True)
   with Pre  => (if Skip_Token
                 then not Is_Trivia (Token)
                 else Is_Trivia (Token)),
        Post => not Is_Trivia (Token);
   --  Process all trivias that follow the given ``Token``. Add the
   --  corresponding internal documents to ``Items`` (this is delegated to the
   --  ``Append_Trivia_Fragment`` procedure). Update ``Token`` to get past all
   --  processed tokens/trivias (so it is supposed to refer to a non-trivia
   --  token upon return).
   --
   --  ``Pool`` is used to allocate the new internal document nodes.
   --
   --  ``Trivias`` is used to skip reattached trivias.
   --
   --  If ``Skip_Token`` is true, ``Token`` is assumed to be the token that
   --  precedes the trivias to process. Otherwise, ``Token`` is assumed to be
   --  the first trivia to process.
   --
   --  If ``Skip_If_Reattached``, this returns early if the first trivia to
   --  process is reattached according to ``Trivias``. Otherwise, that first
   --  trivia is processed unconditionally.

   procedure Append_Trivia_Fragment
     (Fragment : Unparsing_Fragment;
      Items    : in out Document_Vectors.Vector;
      Pool     : in out Document_Pool)
   with Pre => Fragment.Kind in Trivia_Fragment_Kind;
   --  Create the internal document node that corresponds to the given trivia
   --  fragment and append it to ``Items``.

   procedure Process_Reattached_Trivias
     (First_Trivia : Lk_Token;
      Node         : Lk_Node;
      Items        : in out Document_Vectors.Vector;
      Pool         : in out Document_Pool;
      Trivias      : in out Trivias_Info);
   --  If ``First_Trivia`` is unprocessed trivia reattached to ``Node``,
   --  process them now and add the corresponding internal document nodes to
   --  ``Items``.

   procedure Iterate_On_Fragments
     (Node          : Lk_Node;
      Current_Token : in out Lk_Token;
      Process       : access procedure (Fragment      : Unparsing_Fragment;
                                        Current_Token : in out Lk_Token));
   --  Decompose ``Node`` into a list of unparsing fragments
   --  (``Non_Trivia_Fragment_Kind`` ones only) and call ``Process`` on each
   --  fragment.
   --
   --  ``Current_Token`` must be the first token for ``Node``, and is updated
   --  to account for all the tokens that are processed. The ``Process``
   --  callback is expected to do the same.

   -----------------------------------
   -- Template symbol instantiation --
   -----------------------------------

   package Symbol_Instantiation_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Some_Template_Symbol,
      Element_Type    => Some_Template_Symbol,
      Hash            => Ada.Containers.Hash_Type'Mod,
      Equivalent_Keys => "=");

   type Symbol_Instantiation_Context is record
      Map         : Symbol_Instantiation_Maps.Map;
      Next_Symbol : not null access Some_Template_Symbol;
   end record;
   --  Information necessary to generate independent symbols when instantiating
   --  templates.
   --
   --  ``Next_Symbol`` is used to generate unique new symbols (incremented each
   --  time a new symbol is created).
   --
   --  ``Map`` is used to keep track of already generated symbols.

   function Instantiate_Symbol
     (Self   : in out Symbol_Instantiation_Context;
      Symbol : Template_Symbol) return Template_Symbol;
   --  Return the "instantiated" symbol for ``Symbol``, creating one if needed.
   --  Update ``Self`` accordingly.
   --
   --  As a convenient special case, if ``Symbol`` is ``No_Template_Symbol``,
   --  just return it unchanged.

   ----------------------
   -- Linear templates --
   ----------------------

   --  While templates encoded with Template_Type/Document_Type are recursive
   --  data structures (they more or less map to prettier documents) with
   --  disjunction semantics, what we call "linear templates" here are either:
   --
   --  * lists of tokens (Token_Item) and recurse_field (Field_Item) items used
   --    to unparse a concrete regular node type.
   --
   --  * lists of tokens (Token_Item) and recurse (Recurse_Item) items used to
   --    unparse a specific field in a concrete node.
   --
   --  * lists of recurses for join templates (Recurse_Left, then
   --    Recurse_Right).
   --
   --  They are used to process templates that embed text.

   type Linear_Template_Item_Kind is
     (Token_Item,
      Field_Item,
      Recurse_Item,
      Recurse_Left,
      Recurse_Right);
   type Linear_Template_Item
     (Kind : Linear_Template_Item_Kind := Linear_Template_Item_Kind'First)
   is record
      case Kind is
         when Token_Item =>
            Token_Kind : Token_Kind_Ref;
            Token_Text : Unbounded_Text_Type;
            --  Same semantics as the homonym Unparsing_Fragment components

         when Field_Item =>
            Field_Ref      : Struct_Member_Ref;
            Field_Position : Positive;
            --  Same semantics as the corresponding Document_Type components

         when Recurse_Item | Recurse_Left | Recurse_Right =>
            null;
      end case;
   end record;

   function Image (Item : Linear_Template_Item) return String;
   function Is_Equivalent (Left, Right : Linear_Template_Item) return Boolean;

   package Linear_Template_Vectors is new Ada.Containers.Vectors
     (Positive, Linear_Template_Item);

   procedure Linear_Template_From_Unparser
     (Linear_Template : in out Linear_Template_Vectors.Vector;
      Id              : Language_Id;
      Tokens          : Token_Sequence);
   --  Append all items in ``Tokens`` to ``Linear_Template``

   function Linear_Template
     (Node : Type_Ref) return Linear_Template_Vectors.Vector;
   --  Return the linear template that correspond to ``Node``

   function Linear_Template
     (Node  : Type_Ref;
      Field : Struct_Member_Ref) return Linear_Template_Vectors.Vector;
   --  Return the linear template that correspond to the ``Field`` member of
   --  ``Node``.

   Linear_Template_For_Join : constant Linear_Template_Vectors.Vector :=
     [(Kind => Recurse_Left), (Kind => Recurse_Right)];
   --  Linear template for table row join templates

   function Hash (Self : Struct_Member_Index) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type'Mod (Self));

   function Hash (Self : Type_Index) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type'Mod (Self));

   package Field_Config_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Struct_Member_Index,
      Element_Type    => Template_Type,
      Hash            => Hash,
      Equivalent_Keys => "=");

   function JSON_Key_For (Kind : List_Sep_Template_Kind) return String
   is (case Kind is
       when Sep_Template          => "sep",
       when Leading_Sep_Template  => "leading_sep",
       when Trailing_Sep_Template => "trailing_sep");

   type Sep_Templates is array (List_Sep_Template_Kind) of Template_Type;

   type List_Table_Split_Kind is (Empty_Line, Line_Comment);
   type List_Table_Split_Options is array (List_Table_Split_Kind) of Boolean;
   --  Flags to determine what trivias trigger a table split:
   --
   --  * Empty_Line: at least one empty line splits the current table.
   --  * Line_Comment: at least one line comment splits the current table.

   type Table_Config_Record (Enabled : Boolean := False) is record
      case Enabled is
         when False =>
            null;

         when True =>
            Sep_Before : Boolean;
            --  If we generate a table for this list node, whether list
            --  separators must be inserted at the end of the previous row
            --  (``Table_Sep_Before => True``) or at the beginning of the next
            --  row (``Table_Sep_Before => False``).

            Split : List_Table_Split_Options;
            --  If we generate a table for this list node, conditions to split
            --  a table in two.

            Must_Break : Boolean;
            --  If we generate a table for this list node, whether its rows
            --  must break the current group.

            Join_Predicate : Struct_Member_Ref;
            --  If table row join behavior is enabled, reference to the
            --  predicate that determines whether a list element must be joined
            --  to the previous one. ``No_Struct_Member_Ref`` otherwise.

            Join_Template : Template_Type;
            --  Table row join template
      end case;
   end record;

   type List_Config_Record is record
      Seps : Sep_Templates;
      --  Templates to decorate the unparsing of list separators

      Table_Config : Table_Config_Record;
      --  Table config for this list type

      Flush_Before_Children : Boolean;
      --  Whether to insert a Flush_Line_Breaks document before each list child
   end record;

   function Table_Needs_Split
     (Inter_Trivia : Lk_Token;
      Config       : List_Config_Record;
      Trivias      : Trivias_Info) return Boolean
   with Pre => Config.Table_Config.Enabled;
   --  Assuming that we are unparsing a list node (``Config`` is its unparsing
   --  configuration, ``Config.Table_Config.Enabled`` is assumed to be true),
   --  return whether the sequence of trivias starting at ``Inter_Trivia`` must
   --  trigger a table split.

   function Table_Needs_Split
     (Next_Child : Lk_Node;
      Last_Sep   : Lk_Token;
      Config     : List_Config_Record;
      Trivias    : Trivias_Info) return Boolean
   with Pre => Config.Table_Config.Enabled;
   --  Assuming that we are unparsing a list node (``Config`` is its unparsing
   --  configuration, ``Config.Table_Config.Enabled`` is assumed to be true),
   --  and given the next list child to process (``Next_Child``), return
   --  whether the table needs to be split before adding a row for
   --  ``Next_Child``.

   No_List_Config : constant List_Config_Record :=
     (Seps                  => (others => No_Template),
      Table_Config          => (Enabled => False),
      Flush_Before_Children => True);

   type Node_Config_Record is limited record
      Is_Automatic : Boolean;
      --  Whether this configuration was automatically created (not present in
      --  the JSON configuration and not derived from a non-automatic
      --  configuration).

      Node_Template : Template_Type;
      --  Template to decorate the unparsing of the whole node

      Field_Configs : Field_Config_Maps.Map;
      --  For each non-null syntax field in this node, template to decorate the
      --  unparsing of the field.

      List_Config : List_Config_Record;
      --  Configuration bits that are specific to list nodes
   end record;
   type Node_Config_Access is access all Node_Config_Record;
   --  Unparsing configuration for a given node type

   procedure Free is new Ada.Unchecked_Deallocation
     (Node_Config_Record, Node_Config_Access);

   package Node_Config_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Type_Index,
      Element_Type    => Node_Config_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Unparsing_Configuration_Record is limited record
      Ref_Count : Natural;
      --  Number of references to this unparsing configuration. When it drops
      --  to 0, we can deallocate it.

      Language : Language_Id;
      --  Language for which this unparsing configuration was created

      Pool : Document_Pool;
      --  Allocation pool for all Document_Type values, to be released when the
      --  unparsing configuration is destroyed.

      Symbols : Symbol_Table;
      --  Symbol table for group ids

      Node_Configs : Node_Config_Maps.Map;
      --  Node configurations for all node types in Language

      Max_Empty_Lines : Integer;
      --  Maximum number of consecutive empty lines to preserve during
      --  source code reformatting. If negative, all empty lines are preserved.
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Unparsing_Configuration_Record, Unparsing_Configuration_Access);

   procedure Release (Self : in out Unparsing_Configuration_Access);
   --  Release all the memory that was allocated for ``Self``

   type Single_Template_Instantiation_Argument is record
      Document : Document_Type;
      --  Document to substitute to "recurse*" nodes when instantiating a
      --  template.

      Node : Lk_Node;
      --  Node from which ``Document`` was generated. Keeping track of this is
      --  necessary in order to implement instantiation for "recurse_flatten".

      Next_Token : Lk_Token;
      --  Token that follows ``Node``, i.e. the token to assign to
      --  ``Current_Token`` after this template argument has been processed.
   end record;

   package Template_Instantiation_Arg_Vectors is new Ada.Containers.Vectors
     (Positive, Single_Template_Instantiation_Argument);

   type Template_Instantiation_Args (Kind : Some_Template_Kind) is record
      case Kind is
         when With_Recurse | With_Text_Recurse =>
            With_Recurse_Doc : Single_Template_Instantiation_Argument;
            --  Document to use in order to replace "recurse"/"recurse_flatten"
            --  templates.

         when With_Recurse_Field =>
            Field_Docs : Template_Instantiation_Arg_Vectors.Vector;
            --  Documents to use in order to replace "recurse_field" templates

         when Join_Template =>
            Join_Left, Join_Right : Document_Type;
            --  Documents for the rows to join in a table
      end case;
   end record;

   function Instantiate_Template
     (Pool          : in out Document_Pool;
      Symbols       : in out Symbol_Instantiation_Context;
      Node          : Lk_Node;
      Current_Token : in out Lk_Token;
      Trivias       : Trivias_Info;
      Template      : Template_Type;
      Arguments     : Template_Instantiation_Args) return Document_Type;
   --  Instantiate the given template, i.e. create a copy of it, replacing
   --  "recurse*" documents with the relevant documents in ``Arguments``.
   --
   --  ``Node`` must be the node for which we instantiate this template: it is
   --  used to correctly initialize the ``Node`` component of instantiated
   --  documents.
   --
   --  ``Current_Token`` must be the first token for ``Node``, and is updated
   --  to account for all the tokens that are processed by this template.
   --
   --  Information in ``Trivias`` is used to process trivias as expected.

   type Instantiation_State is record
      Node : Lk_Node;
      --  Node for which we instantiate a template (see the ``Node`` argument
      --  of ``Instantiate_Template``).

      Current_Token : Lk_Token;
      --  Token that is about to be unparsed by this template instantiation.
      --  It is updated as tokens are processed during the instantiation.

      Symbols : not null access Symbol_Instantiation_Context;
      --  Mapping for the symbols to create during the instantiation

      Arguments : not null access constant Template_Instantiation_Args;
      --  Template arguments for the instantiation

      Trivias : not null access constant Trivias_Info;
      --  Information used to process trivias as expected
   end record;
   --  Group of common parameters for ``Instantiate_Template_Helper``, to have
   --  a single argument to pass down to recursion.

   function Instantiate_Template_Helper
     (Pool     : in out Document_Pool;
      State    : in out Instantiation_State;
      Template : Document_Type) return Document_Type;
   --  Helper for ``Instantiate_Template_Helper``. Implement the recursive part
   --  of template instantiation: ``Instantiate_Template_Helper`` takes care of
   --  the template unwrapping.

   -----------------------
   -- Check_Same_Tokens --
   -----------------------

   procedure Check_Same_Tokens (Original, Reformatted : Lk_Unit) is
      use Ada.Command_Line;

      procedure Skip_Formatting (T : in out Lk_Token);
      --  Advance T until it is null, a token or a comment

      procedure Skip_Formatting (T : in out Lk_Token) is
      begin
         while not T.Is_Null and then T.Is_Trivia and then not T.Is_Comment
         loop
            T := T.Next;
         end loop;
      end Skip_Formatting;

      T1, T2 : Lk_Token;
   begin
      T1 := Original.First_Token;
      T2 := Reformatted.First_Token;
      loop
         --  Skip non-comment trivia

         Skip_Formatting (T1);
         Skip_Formatting (T2);

         --  Complain if we reached the end of the token stream for one unit
         --  but not the other (teey are supposed to stay in sync).

         exit when T1.Is_Null and then T2.Is_Null;
         if T1.Is_Null then
            Put_Line
              ("Reformatted source has at least one extra token: "
               & Image (T2));
            Set_Exit_Status (Failure);
            exit;
         elsif T2.Is_Null then
            Put_Line
              ("Reformatted source lacks at least one token: " & Image (T1));
            Set_Exit_Status (Failure);
            exit;
         end if;

         if T1.Text /= T2.Text then
            Put_Line ("Unexpected change for " & T1.Image & ":");
            Put_Line ("  " & Image (T1.Text));
            Put_Line ("became:");
            Put_Line ("  " & Image (T2.Text));
            Set_Exit_Status (Failure);
            exit;
         end if;

         T1 := T1.Next;
         T2 := T2.Next;
      end loop;
   end Check_Same_Tokens;

   ---------------------------
   -- Process_Enable_Traces --
   ---------------------------

   procedure Process_Enable_Traces (Trace : GNATCOLL.Traces.Trace_Handle) is
   begin
      if Traces_To_Enable.Contains (To_Unbounded_String (Trace.Unit_Name)) then
         Trace.Set_Active (True);
      end if;
   end Process_Enable_Traces;

   ----------------------
   -- Is_Field_Present --
   ----------------------

   function Is_Field_Present
     (Field          : Lk_Node;
      Field_Unparser : Field_Unparser_Impl) return Boolean
   is
   begin
      --  Unparsing tables (Empty_List_Is_Absent component) determine whether a
      --  non-null child with no children of its own must be treated as absent.

      return not Field.Is_Null
             and then (not Field_Unparser.Empty_List_Is_Absent
                       or else Field.Children_Count > 0);
   end Is_Field_Present;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Trivias_Info) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Unparsing_Fragment_Array, Unparsing_Fragment_Array_Access);
   begin
      Free (Self.Fragments.Trivia_To_Fragments);
   end Free;

   ------------------
   -- Fragment_For --
   ------------------

   function Fragment_For
     (Id       : Language_Id;
      Token    : Token_Unparser;
      List_Sep : Any_List_Sep_Template_Kind := None) return Unparsing_Fragment
   is
      Kind : constant Token_Kind_Ref := From_Index (Id, Token.Kind);
      Text : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Token.Text.all);
   begin
      return
        (case List_Sep is
         when None                   => (Token_Fragment, Kind, Text),
         when List_Sep_Template_Kind =>
           (List_Separator_Fragment, Kind, Text, List_Sep));
   end Fragment_For;

   ------------------------------
   -- Compute_Trivia_Fragments --
   ------------------------------

   procedure Compute_Trivia_Fragments
     (Node : Lk_Node; Fragments : out Trivias_Fragments)
   is
      Id    : constant Language_Descriptor_Access := +Node.Language;
      Trace : constant Boolean := Trivias_Trace.Is_Active;

      T, Exit_Token : Lk_Token;
      --  Token/trivia cursor, and token to stop trivia fragments computation

      First_Trivia : Token_Index := No_Token_Index;
      --  When we are processing a sequence of trivias, this contains the index
      --  of the first trivia of that sequence. Set to ``No_Token_Index`` the
      --  rest of the time (i.e. when ``T`` is not a trivia).

      First_Line : Boolean;
      --  Whether the currently processed trivia starts on the first line for
      --  this sequence of trivias.

      Columns : Natural;
      --  If processing the first line, this is set to the number of
      --  whitespaces found so far. This is used to track how many whitespaces
      --  precede the suffix comment (if any).

      procedure Append (Fragment : Unparsing_Fragment);
      --  Append ``Fragment`` as a new unparsing fragment for the sequence of
      --  trivias we are currently processing.

      procedure Finalize_Sequence;
      --  If we are processing a sequence of trivias, close it:
      --
      --  * Set ``First_Trivia`` to ``No_Token_Index``.
      --
      --  * If the last trivia in this sequence is a comment, and if these
      --    comments require a newline before the next token, append a line
      --    break at the end of this sequence of trivia. This is necessary to
      --    avoid letting the mandatory line break to be inserted "too" late,
      --    inside the next group for instance, which would cause unwanted
      --    formatting.

      ------------
      -- Append --
      ------------

      procedure Append (Fragment : Unparsing_Fragment) is

         --  Get the range for the current sequence of trivias

         Fragment_Range : Unparsing_Fragment_Range renames
           Fragments.Trivia_To_Fragments (First_Trivia);
      begin
         --  Append the new fragment to the storage vector and update the range
         --  to include it.

         Fragments.Vector.Append (Fragment);
         if Fragment_Range = Empty_Unparsing_Fragment_Range then
            Fragment_Range.First := Fragments.Vector.Last_Index;
         end if;
         Fragment_Range.Last := Fragments.Vector.Last_Index;
      end Append;

      -----------------------
      -- Finalize_Sequence --
      -----------------------

      procedure Finalize_Sequence is
      begin
         if First_Trivia = No_Token_Index then
            return;
         end if;

         declare
            Fragment_Range : Unparsing_Fragment_Range renames
              Fragments.Trivia_To_Fragments (First_Trivia);
            Last_Fragment  : Unparsing_Fragment;
            K              : Token_Kind_Ref;
         begin
            --  Is this trivia fragment sequence empty?

            if Fragment_Range.First <= Fragment_Range.Last then
               Last_Fragment := Fragments.Vector (Fragment_Range.Last);

               --  Is the last trivia fragment a comment?

               if Last_Fragment.Kind in Suffix_Comment | Line_Comment then
                  K := Last_Fragment.Comment_Token.Kind;

                  --  Does this comment requires a line break?

                  if Id.Unparsers.Token_Newlines (To_Index (K)) then

                     --  Then add this line break now

                     Append ((Line_Breaks, 1));
                  end if;
               end if;
            end if;
         end;

         --  Finally, mark close this sequence

         First_Trivia := No_Token_Index;
      end Finalize_Sequence;

      Unit         : constant Lk_Unit := Node.Unit;
      Node_Is_Root : constant Boolean := Node = Unit.Root;
   begin
      --  Determine the token iteration range

      if Node_Is_Root then
         T := Unit.First_Token;
         Exit_Token := No_Lk_Token;
      else
         T := Node.Token_Start;
         Exit_Token := Node.Token_End.Next;
      end if;

      --  Allocate a map from trivia index to unparsing fragment range that is
      --  big enough for all trivias in ``Unit``. Only some trivias start a
      --  trivia sequence, so some slots will be unused: put a buggy range for
      --  them, just in case that (could help debugging).

      Fragments.Trivia_To_Fragments :=
        new Unparsing_Fragment_Array (1 .. Token_Index (Unit.Trivia_Count));
      for R of Fragments.Trivia_To_Fragments.all loop
         R := Empty_Unparsing_Fragment_Range;
      end loop;

     --  Iterate through all tokens/trivias in this unit to find sequences of
     --  trivias: create unparsing fragments for them.

      while T /= Exit_Token loop
         if T.Is_Trivia then
            if Trace then
               Trivias_Trace.Trace ("Found " & Image_With_Sloc (T));
            end if;

            --  If ``T`` starts a new sequence of trivias, reset the "trivia
            --  parsing state".

            if First_Trivia = No_Token_Index then
               if Trace then
                  Trivias_Trace.Trace
                    ("  ... it starts a sequence of trivias");
               end if;
               First_Trivia := T.Index;
               First_Line := True;
               Columns := 0;
            end if;

            if not T.Is_Comment then
               declare
                  --  This is a non-comment trivia, so treat it as a sequence
                  --  of whitespaces/line breaks: count the number of line
                  --  breaks it contains, and the number of prefix columns.

                  Line_Breaks_Count : constant Natural :=
                    Count_Line_Breaks (T);
               begin
                  if Trace then
                     Trivias_Trace.Trace
                       ("  ... it has" & Line_Breaks_Count'Image
                        & " line breaks");
                  end if;

                  --  If we are still processing the first line in this
                  --  sequence of trivias, compute the number of spaces that
                  --  come before the potential suffix comment that comes next.

                  if First_Line and then Line_Breaks_Count = 0 then
                     declare
                        Sloc_Range : constant Source_Location_Range :=
                          T.Sloc_Range;
                     begin
                        pragma Assert
                          (Sloc_Range.Start_Line = Sloc_Range.End_Line);
                        Columns := Positive
                          (Sloc_Range.End_Column - Sloc_Range.Start_Column);
                     end;
                  end if;

                  --  If there are N line breaks with N > 2, there are N - 1
                  --  empty lines to account for, and so N line breaks to
                  --  preserve.

                  if Line_Breaks_Count >= 2 then
                     Append
                       ((Kind              => Line_Breaks,
                         Line_Breaks_Count => Line_Breaks_Count));
                  end if;

                  --  If this trivia contained at least one line break, the
                  --  next trivia to process will not start on the first line.

                  if Line_Breaks_Count > 0 then
                     First_Line := False;
                  end if;
               end;

            elsif First_Line then
               if Trace then
                  Trivias_Trace.Trace ("  ... it is a suffix comment");
               end if;

               --  Propagate spaces that appear before this suffix comment, if
               --  any.

               if Columns > 0 then
                  Append ((Whitespaces, Whitespaces_Count => Columns));
               end if;

               Append ((Suffix_Comment, T));

            else
               if Trace then
                  Trivias_Trace.Trace ("  ... it is a line comment");
               end if;

               Append ((Line_Comment, T));
            end if;

         else
            --  ``T`` is not a trivia: update our state to represent that we
            --  are not in a sequence of trivias.

            Finalize_Sequence;
         end if;
         T := T.Next;
      end loop;

      Finalize_Sequence;
   end Compute_Trivia_Fragments;

   --------------------------
   -- Compute_Trivias_Info --
   --------------------------

   procedure Compute_Trivias_Info (Node : Lk_Node; Info : out Trivias_Info)
   is
      Trace : constant Boolean := Trivias_Trace.Is_Active;

      procedure Process (Node : Lk_Node);
      --  Reattach relevant trivias to ``Node``. This processes ``Node``'s
      --  children recursively.

      procedure Reattach (T : Lk_Token; To : Lk_Node; What : String);
      --  Reattach ``T`` to the ``To`` node. ``What`` is used to qualify this
      --  trivia in debug logs.

      -------------
      -- Process --
      -------------

      procedure Process (Node : Lk_Node) is
         Is_List : constant Boolean := Node.Is_List_Node;
      begin
         --  Register reattached trivias that come before list nodes

         if Is_List then
            declare
               First_Trivia : constant Lk_Token := First_Trivia_Before (Node);
            begin
               if not First_Trivia.Is_Null then
                  Reattach
                    (First_Trivia, Node, "leading trivias before list node");
               end if;
            end;
         end if;

         --  Reattach trivias to children

         for C of Node.Children loop
            if not C.Is_Null then
               Process (C);
            end if;
         end loop;
         --
         --  Note that what follows is done after the recursion so that
         --  reattaching to children has priority over reattaching to their
         --  parents.

         --  Register reattached trivias that come after list children:
         --  they must be processed during the unparsing of the parent list.

         if not Node.Parent.Is_Null and then Node.Parent.Is_List_Node then
            declare
               T : constant Lk_Token := Node.Token_End.Next;
            begin
               if T.Is_Trivia then
                  Reattach
                    (T, Node.Parent, "trailing trivias after list child");
               end if;
            end;
         end if;

         --  Register reattached trivias that come after list nodes

         if Is_List then
            if Node.Children_Count > 0 then
               declare
                  T : constant Lk_Token := Node.Token_End.Next;
               begin
                  if T.Is_Trivia then
                     Reattach (T, Node, "trailing trivias after list node");
                  end if;
               end;
            end if;
         end if;
      end Process;

      --------------
      -- Reattach --
      --------------

      procedure Reattach (T : Lk_Token; To : Lk_Node; What : String) is
         Dummy_Position : Token_To_Node_Maps.Cursor;
         Dummy_Inserted : Boolean;
      begin
         Info.First_Reattached_Trivias.Insert
           (T, To, Dummy_Position, Dummy_Inserted);
         if Trace then
            Trivias_Trace.Trace
              ("Reattaching " & What & ": " & Image_With_Sloc (T) & " to "
               & To.Image);
         end if;
      end Reattach;
   begin
      --  Determine which tokens have reattached trivias

      Info.First_Reattached_Trivias.Clear;
      Process (Node);

      --  Scan all tokens and create the corresponding trivias

      Compute_Trivia_Fragments (Node, Info.Fragments);
   end Compute_Trivias_Info;

   ----------------------------------------
   -- Reattached_Trivia_Needs_Processing --
   ----------------------------------------

   function Reattached_Trivia_Needs_Processing
     (Token : Lk_Token; Node : Lk_Node; Info : Trivias_Info) return Boolean
   is
      use Token_To_Node_Maps;

      Pos : constant Cursor := Info.First_Reattached_Trivias.Find (Token);
   begin
      return Has_Element (Pos) and then Element (Pos) = Node;
   end Reattached_Trivia_Needs_Processing;

   ---------------------------------
   -- Iterate_On_Trivia_Fragments --
   ---------------------------------

   procedure Iterate_On_Trivia_Fragments
     (First_Trivia : Lk_Token;
      Info         : Trivias_Info;
      Process      : access procedure (Fragment : Unparsing_Fragment))
   is
      Fragment_Range : Unparsing_Fragment_Range renames
        Info.Fragments.Trivia_To_Fragments.all (First_Trivia.Index);
   begin
      for I in Fragment_Range.First .. Fragment_Range.Last loop
         Process.all (Info.Fragments.Vector (I));
      end loop;
   end Iterate_On_Trivia_Fragments;

   -------------------------
   -- First_Trivia_Before --
   -------------------------

   function First_Trivia_Before (Node : Lk_Node) return Lk_Token is
      First_Trivia, T : Lk_Token;
   begin
      First_Trivia := Node.Token_Start.Previous;
      if First_Trivia.Is_Trivia then
         loop
            T := First_Trivia.Previous;
            if not T.Is_Trivia then
               return First_Trivia;
            end if;
            First_Trivia := T;
         end loop;
      else
         return No_Lk_Token;
      end if;
   end First_Trivia_Before;

   -----------------------
   -- Count_Line_Breaks --
   -----------------------

   function Count_Line_Breaks (Token : Lk_Token) return Natural is
   begin
      return Result : Natural := 0 do
         for C of Token.Text loop
            if C = Chars.LF then
               Result := Result + 1;
            end if;
         end loop;
      end return;
   end Count_Line_Breaks;

   -------------------
   -- Is_Empty_List --
   -------------------

   function Is_Empty_List (Node : Lk_Node) return Boolean is
      T : Lk_Token;
   begin
      if not Node.Is_List_Node or else Node.Children_Count > 0 then
         return False;
      end if;

      --  Node is an empty list, so it covers no token. Check for the presence
      --  of comments in the trivias that logically precede Node in the token
      --  stream: if we find at least one comment, we consider that this list
      --  node is not empty.

      T := Node.Token_Start.Previous;
      while not T.Is_Null and then T.Is_Trivia loop
         if T.Is_Comment then
            return False;
         end if;
         T := T.Previous;
      end loop;
      return True;
   end Is_Empty_List;

   ----------------------
   -- Process_Trivias_ --
   ----------------------

   procedure Process_Trivias
     (Token              : in out Lk_Token;
      Items              : in out Document_Vectors.Vector;
      Pool               : in out Document_Pool;
      Trivias            : Trivias_Info;
      Skip_Token         : Boolean;
      Skip_If_Reattached : Boolean := True)
   is
      Trace : constant Boolean := Trivias_Trace.Is_Active;

      Line_Break_Required_Before_Line_Comment : Boolean := True;
      --  Whether inserting a line break will be necessary if what comes next
      --  is a line comment.

      procedure Process (Fragment : Unparsing_Fragment);
      --  Callback for ``Iterate_On_Trivia_Fragments``, to turn unparsing
      --  fragments for trivias into internal documents in ``Items``.

      -------------
      -- Process --
      -------------

      procedure Process (Fragment : Unparsing_Fragment) is
      begin
         --  If ``Fragment`` is a line command and there was no empty line nor
         --  comment before it, make sure there is at least one line break
         --  before it, so that it does not accidentally gets unparsed as a
         --  *suffix* comment.

         if Fragment.Kind = Line_Comment
            and then Line_Break_Required_Before_Line_Comment
         then
            Items.Append (Pool.Create_Expected_Line_Breaks (1));
         end if;
         Line_Break_Required_Before_Line_Comment := False;
         Append_Trivia_Fragment (Fragment, Items, Pool);
      end Process;
   begin
      if Trace then
         declare
            What   : constant String :=
              (if Skip_Token
               then "trivias after"
               else "trivias starting at");
            Status : constant String :=
              (if Skip_Token then "excluded" else "included");
         begin
            Trivias_Trace.Trace
              ("Processing " & What & " " & Image_With_Sloc (Token)
               & " (" & Status & ")");
         end;
      end if;

      if Skip_Token then
         Token := Token.Next;
         if Trace then
            Trivias_Trace.Trace ("  ... i.e. " & Image_With_Sloc (Token));
         end if;
      end if;

      --  Skip reattached tokens if asked to

      if Skip_If_Reattached
         and then Trivias.First_Reattached_Trivias.Contains (Token)
      then
         if Trace then
            Trivias_Trace.Trace ("  ... never mind, they are reattached");
         end if;
         Token := Token.Next (Exclude_Trivia => True);
         return;
      end if;

      --  Now go through unparsing fragments for the sequence of trivias that
      --  start at ``Token`` and add the corresponding internal documents to
      --  ``Items``.

      if Token.Is_Trivia then
         Iterate_On_Trivia_Fragments (Token, Trivias, Process'Access);
         Token := Token.Next (Exclude_Trivia => True);
      end if;
   end Process_Trivias;

   ----------------------------
   -- Append_Trivia_Fragment --
   ----------------------------

   procedure Append_Trivia_Fragment
     (Fragment : Unparsing_Fragment;
      Items    : in out Document_Vectors.Vector;
      Pool     : in out Document_Pool) is
   begin
      case Trivia_Fragment_Kind (Fragment.Kind) is
         when Suffix_Comment | Line_Comment =>
            Items.Append
              (Pool.Create_Token
                 (Fragment.Comment_Token.Kind,
                  To_Unbounded_Text (Fragment.Comment_Token.Text)));

         when Whitespaces =>
            Items.Append
              (Pool.Create_Expected_Whitespaces (Fragment.Whitespaces_Count));

         when Line_Breaks =>
            Items.Append
              (Pool.Create_Expected_Line_Breaks (Fragment.Line_Breaks_Count));
      end case;
   end Append_Trivia_Fragment;

   --------------------------------
   -- Process_Reattached_Trivias --
   --------------------------------

   procedure Process_Reattached_Trivias
     (First_Trivia : Lk_Token;
      Node         : Lk_Node;
      Items        : in out Document_Vectors.Vector;
      Pool         : in out Document_Pool;
      Trivias      : in out Trivias_Info)
   is
      Current_Token : Lk_Token := First_Trivia;
   begin
      if not First_Trivia.Is_Null
         and then Reattached_Trivia_Needs_Processing
                    (First_Trivia, Node, Trivias)
         and then not Trivias
                      .Processed_Reattached_Trivias
                      .Contains (First_Trivia)
      then
         Process_Trivias
           (Current_Token,
            Items,
            Pool,
            Trivias,
            Skip_Token         => False,
            Skip_If_Reattached => False);
         Trivias.Processed_Reattached_Trivias.Include (First_Trivia);
      end if;
   end Process_Reattached_Trivias;

   --------------------------
   -- Iterate_On_Fragments --
   --------------------------

   procedure Iterate_On_Fragments
     (Node          : Lk_Node;
      Current_Token : in out Lk_Token;
      Process       : access procedure (Fragment      : Unparsing_Fragment;
                                        Current_Token : in out Lk_Token))
   is
      Id        : constant Language_Id := Node.Language;
      Desc      : constant Language_Descriptor_Access := +Id;
      Unparsers : Unparsers_Impl renames Desc.Unparsers.all;

      procedure Process_Tokens (Tokens : Token_Sequence);

      --------------------
      -- Process_Tokens --
      --------------------

      procedure Process_Tokens (Tokens : Token_Sequence) is
      begin
         for T of Tokens.all loop

            --  Let the ``Process`` procedure take care of updating
            --  ``Current_Token`` for ``T``.

            pragma Assert (To_Index (Kind (Current_Token)) = T.Kind);
            Process.all (Fragment_For (Id, T), Current_Token);
         end loop;
      end Process_Tokens;

      Node_Type     : constant Type_Ref := Type_Of (Node);
      Node_Unparser : Node_Unparser_Impl renames
        Unparsers.Node_Unparsers (To_Index (Node_Type)).all;
   begin
      case Node_Unparser.Kind is
         when Regular =>

            --  Process fragments that precede the first field

            Process_Tokens (Node_Unparser.Pre_Tokens);

            --  Then process fragments for each field and the tokens between
            --  them.

            for I in 1 .. Node_Unparser.Field_Unparsers.N loop
               declare
                  Field_Unparser : Field_Unparser_Impl renames
                    Node_Unparser.Field_Unparsers.Field_Unparsers (I);
                  Inter_Tokens   : Token_Sequence renames
                    Node_Unparser.Field_Unparsers.Inter_Tokens (I);

                  Child : constant Lk_Node := Node.Child (I);
               begin
                  --  Process fragments that appear unconditionally between
                  --  fields.

                  Process_Tokens (Inter_Tokens);

                  --  Then process fragments for the field itself, if present

                  if Is_Field_Present (Child, Field_Unparser) then
                     Process.all
                       (Fragment      =>
                          (Kind               => Field_Fragment,
                           Node               => Child,
                           Field              => From_Index
                                                   (Id, Field_Unparser.Member),
                           Field_Unparser_Ref => Field_Unparser'Access),
                        Current_Token => Current_Token);
                  end if;
               end;
            end loop;

            --  Process fragments that follow the last field

            Process_Tokens (Node_Unparser.Post_Tokens);

         when List =>
            declare
               Count        : constant Natural := Node.Children_Count;
               Sep_Fragment : Unparsing_Fragment :=
                 (if Node_Unparser.Separator = null
                  then (Kind          => List_Separator_Fragment,
                        Token_Kind    => No_Token_Kind_Ref,
                        Token_Text    => To_Unbounded_Text (""),
                        List_Sep_Kind => Sep_Template)
                  else Fragment_For
                         (Id, Node_Unparser.Separator, Sep_Template));
            begin
               --  Emit the leading separator, if present

               if Node_Unparser.Sep_Extra = Allow_Leading
                  and then Count > 0
                  and then Token_Matches
                             (Node.Token_Start, Node_Unparser.Separator)
               then
                  Sep_Fragment.List_Sep_Kind := Leading_Sep_Template;
                  Process.all (Sep_Fragment, Current_Token);
               end if;

               --  Then each list element, with each separator token in between

               for I in 1 .. Count loop
                  if I > 1 then
                     Sep_Fragment.List_Sep_Kind := Sep_Template;
                     Process.all (Sep_Fragment, Current_Token);
                  end if;

                  Process.all
                    (Fragment      =>
                       (Kind        => List_Child_Fragment,
                        Node        => Node.Child (I),
                        Child_Index => I),
                     Current_Token => Current_Token);
               end loop;

               --  And finally emit the trailing separator, if present

               if Node_Unparser.Sep_Extra = Allow_Trailing
                  and then Count > 0
                  and then Token_Matches
                             (Node.Token_End, Node_Unparser.Separator)
               then
                  Sep_Fragment.List_Sep_Kind := Trailing_Sep_Template;
                  Process.all (Sep_Fragment, Current_Token);
               end if;
            end;

         when Token =>
            Process.all
              (Fragment      =>
                 (Kind       => Token_Fragment,
                  Token_Kind => Token_Node_Kind (Node_Type),
                  Token_Text => To_Unbounded_Text (Node.Text)),
               Current_Token => Current_Token);
      end case;
   end Iterate_On_Fragments;

   ------------------------
   -- Instantiate_Symbol --
   ------------------------

   function Instantiate_Symbol
     (Self   : in out Symbol_Instantiation_Context;
      Symbol : Template_Symbol) return Template_Symbol
   is
      use Symbol_Instantiation_Maps;

      Next_Symbol : Some_Template_Symbol renames Self.Next_Symbol.all;
      Cur         : Cursor;
   begin
      if Symbol = No_Template_Symbol then
         return No_Template_Symbol;
      end if;

      Cur := Self.Map.Find (Symbol);
      if Has_Element (Cur) then
         return Element (Cur);
      else
         return Result : constant Template_Symbol := Next_Symbol do
            Next_Symbol := Next_Symbol + 1;
            Self.Map.Insert (Symbol, Result);
         end return;
      end if;
   end Instantiate_Symbol;

   -----------
   -- Image --
   -----------

   function Image (Item : Linear_Template_Item) return String is
   begin
      case Item.Kind is
         when Token_Item =>
            return "token """ & Image (To_Text (Item.Token_Text)) & """";
         when Field_Item =>
            return "field " & Image
                                (Format_Name
                                  (Member_Name (Item.Field_Ref), Lower));
         when Recurse_Item =>
            return "recurse";
         when Recurse_Left =>
            return "recurse_left";
         when Recurse_Right =>
            return "recurse_right";
      end case;
   end Image;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent (Left, Right : Linear_Template_Item) return Boolean
   is
   begin
      if Left.Kind /= Right.Kind then
         return False;
      end if;

      --  Omit Token_Kind and Field_Ref from comparisons: these fields are
      --  correctly initialized in the linear template computed from unparsing
      --  tables, but it is still uninitialized during parsing. It is precisely
      --  the comparison between what has been parse and the "expected" linear
      --  template that will allow to initialize these components.

      case Left.Kind is
         when Token_Item =>
            return Left.Token_Text = Right.Token_Text;
         when Field_Item =>
            return Left.Field_Ref = Right.Field_Ref;
         when Recurse_Item | Recurse_Left | Recurse_Right =>
            return True;
      end case;
   end Is_Equivalent;

   -----------------------------------
   -- Linear_Template_From_Unparser --
   -----------------------------------

   procedure Linear_Template_From_Unparser
     (Linear_Template : in out Linear_Template_Vectors.Vector;
      Id              : Language_Id;
      Tokens          : Token_Sequence)
   is
   begin
      for T of Tokens.all loop
         declare
            F : constant Unparsing_Fragment := Fragment_For (Id, T);
         begin
            Linear_Template.Append
              (Linear_Template_Item'
                 (Kind       => Token_Item,
                  Token_Kind => F.Token_Kind,
                  Token_Text => F.Token_Text));
         end;
      end loop;
   end Linear_Template_From_Unparser;

   ---------------------
   -- Linear_Template --
   ---------------------

   function Linear_Template
     (Node : Type_Ref) return Linear_Template_Vectors.Vector
   is
      Id        : constant Language_Id := Language (Node);
      Desc      : constant Language_Descriptor_Access := +Id;
      Unparsers : Unparsers_Impl renames Desc.Unparsers.all;

      Result : Linear_Template_Vectors.Vector;

      Node_Unparser : Node_Unparser_Impl renames
        Unparsers.Node_Unparsers (To_Index (Node)).all;
      pragma Assert (Node_Unparser.Kind = Regular);
   begin
      --  Append tokens that precede the first field

      Linear_Template_From_Unparser (Result, Id, Node_Unparser.Pre_Tokens);

      --  Then append recurse_field documents for each field and the tokens
      --  between them.

      for I in 1 .. Node_Unparser.Field_Unparsers.N loop
         declare
            Field_Unparser : Field_Unparser_Impl renames
              Node_Unparser.Field_Unparsers.Field_Unparsers (I);
            Inter_Tokens   : Token_Sequence renames
              Node_Unparser.Field_Unparsers.Inter_Tokens (I);
         begin
            --  Append tokens that appear unconditionally between fields

            Linear_Template_From_Unparser (Result, Id, Inter_Tokens);

            --  Append fields themselves

            Result.Append
              (Linear_Template_Item'
                (Kind           => Field_Item,
                 Field_Ref      => From_Index (Id, Field_Unparser.Member),
                 Field_Position => I));
         end;
      end loop;

      --  Append tokens that follow the last field

      Linear_Template_From_Unparser (Result, Id, Node_Unparser.Post_Tokens);

      return Result;
   end Linear_Template;

   ---------------------
   -- Linear_Template --
   ---------------------

   function Linear_Template
     (Node  : Type_Ref;
      Field : Struct_Member_Ref) return Linear_Template_Vectors.Vector
   is
      Id        : constant Language_Id := Language (Node);
      Desc      : constant Language_Descriptor_Access := +Id;
      Unparsers : Unparsers_Impl renames Desc.Unparsers.all;

      Node_Unparser : Node_Unparser_Impl renames
        Unparsers.Node_Unparsers (To_Index (Node)).all;
      pragma Assert (Node_Unparser.Kind = Regular);

      Result : Linear_Template_Vectors.Vector;

      --  Look for the unparser corresponding to Field
   begin
      for I in 1 .. Node_Unparser.Field_Unparsers.N loop
         declare
            Field_Unparser : Field_Unparser_Impl renames
              Node_Unparser.Field_Unparsers.Field_Unparsers (I);
         begin
            if Field_Unparser.Member = To_Index (Field) then
               Linear_Template_From_Unparser
                 (Result, Id, Field_Unparser.Pre_Tokens);
               Result.Append
                 (Linear_Template_Item'(Kind => Recurse_Item));
               Linear_Template_From_Unparser
                 (Result, Id, Field_Unparser.Post_Tokens);
               return Result;
            end if;
         end;
      end loop;

      --  Linear_Template is supposed to be called with a valid pair of
      --  Node/Field, so the loop above is supposed to return a value.

      raise Program_Error;
   end Linear_Template;

   -----------------------
   -- Table_Needs_Split --
   -----------------------

   function Table_Needs_Split
     (Inter_Trivia : Lk_Token;
      Config       : List_Config_Record;
      Trivias      : Trivias_Info) return Boolean
   is
      Result : Boolean := False;

      procedure Process (Fragment : Unparsing_Fragment);
      --  Callback for ``Iterate_On_Trivia_Fragment``. Update ``Result``
      --  for the given fragment according to the split rules in ``Config``.

      -------------
      -- Process --
      -------------

      procedure Process (Fragment : Unparsing_Fragment) is
      begin
         case Trivia_Fragment_Kind (Fragment.Kind) is
            when Whitespaces | Suffix_Comment =>
               null;

            when Line_Comment =>
               if Config.Table_Config.Split (Line_Comment) then
                  Result := True;
               end if;

            when Line_Breaks =>
               if Fragment.Line_Breaks_Count >= 2
                  and then Config.Table_Config.Split (Empty_Line)
               then
                  Result := True;
               end if;
         end case;
      end Process;
   begin
      --  The table must split if at least one trivia in the given sequence
      --  must trigger the split.

      if Inter_Trivia.Is_Trivia then
         Iterate_On_Trivia_Fragments (Inter_Trivia, Trivias, Process'Access);
      end if;
      return Result;
   end Table_Needs_Split;

   -----------------------
   -- Table_Needs_Split --
   -----------------------

   function Table_Needs_Split
     (Next_Child : Lk_Node;
      Last_Sep   : Lk_Token;
      Config     : List_Config_Record;
      Trivias    : Trivias_Info) return Boolean is
   begin
      return
        --  The table must split if at least one trivia in the sequence that
        --  preceeds ``First_Trivia`` must trigger the split.

        Table_Needs_Split
          (Next_Child.Token_Start.Previous (Exclude_Trivia => True).Next,
           Config,
           Trivias)

        --  It must also split if trivias before the previous list separator
        --  triggers the split.

        or else Table_Needs_Split (Last_Sep.Next, Config, Trivias);
   end Table_Needs_Split;

   -------------------------------------
   -- Default_Unparsing_Configuration --
   -------------------------------------

   function Default_Unparsing_Configuration
     (Language : Language_Id) return Unparsing_Configuration
   is
      Diagnostics : Diagnostics_Vectors.Vector;
      Result      : constant Unparsing_Configuration :=
        Load_Unparsing_Config_From_Buffer
          (Language,
           Language.Unparsers.Default_Config.all,
           Diagnostics,
           Check_All_Nodes => False);
   begin
      if not Diagnostics.Is_Empty then
         raise Program_Error;
      end if;
      return Result;
   end Default_Unparsing_Configuration;

   ---------------------------
   -- Load_Unparsing_Config --
   ---------------------------

   function Load_Unparsing_Config
     (Language        : Language_Id;
      Filename        : String;
      Diagnostics     : in out Diagnostics_Vectors.Vector;
      Check_All_Nodes : Boolean := False)
      return Unparsing_Configuration
   is
      use type GNAT.Strings.String_Access;

      JSON_Text : GNAT.Strings.String_Access := Create (+Filename).Read_File;
   begin
      if JSON_Text = null then
         Append
           (Diagnostics,
            No_Source_Location_Range,
            To_Text ("cannot read " & Filename));
         return No_Unparsing_Configuration;
      end if;

      return Result : constant Unparsing_Configuration :=
        Load_Unparsing_Config_From_Buffer
          (Language, JSON_Text.all, Diagnostics, Check_All_Nodes)
      do
         GNAT.Strings.Free (JSON_Text);
      end return;
   end Load_Unparsing_Config;

   ---------------------------------------
   -- Load_Unparsing_Config_From_Buffer --
   ---------------------------------------

   function Load_Unparsing_Config_From_Buffer
     (Language        : Language_Id;
      Buffer          : String;
      Diagnostics     : in out Diagnostics_Vectors.Vector;
      Check_All_Nodes : Boolean)
      return Unparsing_Configuration
   is
      Desc      : constant Language_Descriptor_Access := +Language;
      Unparsers : Unparsers_Impl renames Desc.Unparsers.all;

      ------------------
      -- JSON helpers --
      ------------------

      package Node_JSON_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Type_Index,
         Element_Type    => JSON_Value,
         Hash            => Hash,
         Equivalent_Keys => "=");

      function Node_Entries (JSON : JSON_Value) return Node_JSON_Maps.Map;
      --  Assuming that ``JSON`` is an object whose keys are node type names,
      --  compute the corresponding map where keys are converted to node types.

      package Field_JSON_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Struct_Member_Index,
         Element_Type    => JSON_Value,
         Hash            => Hash,
         Equivalent_Keys => "=");

      function Field_Entries
        (Node : Type_Ref; JSON : JSON_Value) return Field_JSON_Maps.Map;
      --  Assuming that ``JSON`` is an object whose keys are field type names,
      --  compute the corresponding map where keys are converted to struct
      --  member references.

      --  Create a map so that we can lookup nodes/fields by name

      Symbols : constant Symbol_Table := Create_Symbol_Table;
      Map     : constant Name_Map := Create_Name_Map
        (Id             => Language,
         Symbols        => Symbols,
         Enum_Types     => Camel,
         Enum_Values    => Lower,
         Struct_Types   => Camel,
         Struct_Members => Lower);

      function To_Symbol (Name : String) return Symbol_Type
      is (Find (Symbols, To_Text (Name)));
      --  Convert a name to the corresponding symbol in Symbols

      function To_Type_Index (Name : String) return Type_Index;
      --  Return the type index for the node type that has the given
      --  camel-case Name. Raise an Invalid_Input exception if there is no such
      --  node.

      function To_Struct_Member_Index
        (Name : String; Node : Type_Ref) return Struct_Member_Index;
      --  Return the member index for the field that has the given lower-case
      --  Name for the given Node. Raise an Invalid_Input exception if there is
      --  no such field.

      function To_Predicate_Ref
        (Name : String; Node : Type_Ref) return Struct_Member_Ref;
      --  Look for the member of ``Node`` that matches ``Name``.
      --
      --  Return it if it matches a predicate (property that takes no argument
      --  and returns a boolean). Raise an ``Invalid_Input`` exception
      --  otherwise.

      function Node_Type_Image (Node : Type_Ref) return String
      is (Image (Format_Name (Node_Type_Name (Node), Camel)));
      --  Return the expected name for the given Node

      function Field_Image
        (Member : Struct_Member_Ref; Node : Type_Ref) return String
      is (Node_Type_Image (Node)
          & "." & Image (Format_Name (Member_Name (Member), Lower)));
      --  Return the expected name for the given Member in the given Node

      type Template_Parsing_State_Kind is
        (Simple_Recurse,
         Recurse_Field,
         Recurse_In_Field,
         Recurse_In_Join);
      --  There are two kinds of templates we expect to find in unparsing
      --  configurations:
      --
      --  * Simple_Recurse templates, that cannot contain "text" nodes and
      --    whose linearization must yield a single "recurse" node.
      --
      --  * Recurse_Field templates, whose linearization must yield the
      --    sequence of "text"/"recurse_field" that is expected for a node.
      --
      --  * Recurse_In_Field templates, whose linearization must yield the
      --    sequence of "text"/"recurse" that is expected for a node field.
      --
      --  * Recurse_In_Join templates, whose linearization must yield the
      --    sequence: ["recurse_left", "recurse_right"].

      type Template_Parsing_State
        (Kind : Template_Parsing_State_Kind := Simple_Recurse)
      is record
         case Kind is
            when Simple_Recurse =>
               Recurse_Found : Boolean;
               --  Whether template parsing has found the "recurse" node
               --  expected for the current branch.

            when Recurse_Field | Recurse_In_Field | Recurse_In_Join =>
               Linear_Template : Linear_Template_Vectors.Vector;
               --  Sequence of tokens/fields that the parsed template is
               --  supposed to yield once instantiated/formatted.

               Linear_Position : Positive;
               --  Position in Linear_Template of the next item to be found in
               --  the template to parse.
         end case;
      end record;

      type Template_Parsing_Context_Kind is
        (Node_Template, Field_Template, Sep_Template, Join_Template);
      --  Indicate which kind of template we are parsing:
      --
      --  ``Node_Template``: a "node" template.
      --
      --  ``Field_Template``: a template in the "fields" mapping.
      --
      --  ``Sep_Template``: a "sep"/"leading_sep"/"trailing_sep" template.
      --
      --  ``Join_Template``: template for a table row join.

      type Template_Parsing_Context (Kind : Template_Parsing_Context_Kind) is
      record
         Node : Type_Ref;
         --  Node for which we parse this template

         State : Template_Parsing_State;
         --  Keep track of the parsing state for this template; used for
         --  validation.

         Symbols : Symbol_Parsing_Maps.Map;
         --  Symbols referenced/declared in this template. Initialize it
         --  with the symbols that can be referenced from the template, and
         --  read after template parsing what symbols the template
         --  declares/references.
         --
         --  TODO??? (eng/libadalang/langkit#805) For now, this is out of the
         --  parsing state, and thus it is not sensitive to control flow
         --  constructs: it is not possible to create groups with the same
         --  symbol in alternative paths. This also means that it is possible
         --  to refer to a group even in contexts where we are not guaranteed
         --  that the group will be printed. These limitations should be
         --  addressed at some point.

         case Kind is
            when Node_Template =>
               null;

            when Field_Template =>
               Field : Struct_Member_Ref;
               --  Field for which we parse this template

            when Sep_Template =>
               Sep_Kind : List_Sep_Template_Kind;
               --  Kind of separator for this template

            when Join_Template =>
               null;
         end case;
      end record;

      function Template_Kind
        (Context : Template_Parsing_Context_Kind;
         JSON    : JSON_Value) return Template_Parsing_State_Kind;
      --  Determine the plausible kind for the given JSON-encoded template in
      --  the given context. Note that this is just a heuristic: if will
      --  return the right kind for a well-formed template, but will return an
      --  approximation for an ill-formed template.

      function Initial_State_For
        (Node    : Type_Ref;
         JSON    : JSON_Value;
         Context : Template_Parsing_Context) return Template_Parsing_State;
      --  Return an inital template parsing state for the "node" template of
      --  ``Node``, to create from ``JSON``. Raise an Invalid_Input exception
      --  if the initial state found is invalid in this context.

      function Initial_State_For
        (Node    : Type_Ref;
         Field   : Struct_Member_Ref;
         JSON    : JSON_Value) return Template_Parsing_State;
      --  Return an inital template parsing state for the template ``JSON``
      --  corresponding to the node field ``Node``/``Field``.
      --  ``Node``, to create from ``JSON``.

      function Parse_Template
        (JSON    : JSON_Value;
         Context : in out Template_Parsing_Context) return Template_Type;
      --  Parse a JSON-encoded temlate document. Raise an Invalid_Input
      --  exception if the JSON encoding is invalid or if the template is
      --  ill-formed.

      function Parse_Template_Helper
        (JSON    : JSON_Value;
         Context : in out Template_Parsing_Context) return Document_Type;
      --  Helper for ``Parse_Template``. Implement the recursive part of
      --  templates parsing: ``Parse_Template`` takes care of the post-parsing
      --  validation.

      procedure Process_Recurse (Context : in out Template_Parsing_Context);
      --  Record in ``Context.State``` that a "recurse" or "recurse_flatten"
      --  template item has been found. This raises an error if one has already
      --  been found.

      procedure Process_Linear_Template_Item
        (Item    : in out Linear_Template_Item;
         Context : in out Template_Parsing_Context);
      --  Record in ``Context.State``that the given ``Item`` (token or
      --  "recurse_field" template) has been found. This raises an error if
      --  ``Item`` was not expected at that point for this template.
      --
      --  On success, update ``Item`` so that its Token_Ref/Field_Position
      --  component reflect the ones found in the linear template fo the
      --  current nod.

      procedure Check_Symbols
        (Node            : Type_Ref;
         Parent_Name     : String;
         Parent_Template : Template_Type;
         Child_Name      : String;
         Child_Template  : Template_Type);
      --  Check that symbol usage for both templates is compatible. Names are
      --  used for error message formatting.

      procedure Abort_Parsing
        (Context : Template_Parsing_Context; Message : String)
      with No_Return;
      --  Raise an Invalid_Input exception with the given message,
      --  contextualized.

      procedure Abort_Parsing_Linear
        (Context : Template_Parsing_Context; Message : String)
      with No_Return;
      --  Like ``Abort_Parsing``, but add the expected linear template at the
      --  end of the error message.

      procedure Abort_Parsing (Message : String) with No_Return;
      --  Append an item to ``Diagnostics`` and raise an Invalid_Input
      --  exception.

      Result : Unparsing_Configuration_Access :=
        new Unparsing_Configuration_Record;
      Pool   : Document_Pool renames Result.Pool;

      ------------------
      -- Node_Entries --
      ------------------

      function Node_Entries (JSON : JSON_Value) return Node_JSON_Maps.Map is
         Result : Node_JSON_Maps.Map;

         procedure Process (Name : String; Value : JSON_Value);
         --  Add Value to Result

         procedure Process (Name : String; Value : JSON_Value) is
            Key : constant Type_Index := To_Type_Index (Name);
         begin
            Result.Insert (Key, Value);
         end Process;
      begin
         JSON.Map_JSON_Object (Process'Access);
         return Result;
      end Node_Entries;

      -------------------
      -- Field_Entries --
      -------------------

      function Field_Entries
        (Node : Type_Ref; JSON : JSON_Value) return Field_JSON_Maps.Map
      is
         Result : Field_JSON_Maps.Map;

         procedure Process (Name : String; Value : JSON_Value);
         --  Add Value to Result

         procedure Process (Name : String; Value : JSON_Value) is
            Key : constant Struct_Member_Index :=
              To_Struct_Member_Index (Name, Node);
         begin
            Result.Insert (Key, Value);
         end Process;
      begin
         JSON.Map_JSON_Object (Process'Access);
         return Result;
      end Field_Entries;

      -------------------
      -- To_Type_Index --
      -------------------

      function To_Type_Index (Name : String) return Type_Index is
         T : constant Type_Ref := Map.Lookup_Type (To_Symbol (Name));
      begin
         if T = No_Type_Ref or else not Is_Node_Type (T) then
            Abort_Parsing ("invalid node name: " & Name);
         end if;
         return To_Index (T);
      end To_Type_Index;

      ----------------------------
      -- To_Struct_Member_Index --
      ----------------------------

      function To_Struct_Member_Index
        (Name : String; Node : Type_Ref) return Struct_Member_Index
      is
         M : constant Struct_Member_Ref :=
           Map.Lookup_Struct_Member (Node, To_Symbol (Name));
      begin
         if M = No_Struct_Member_Ref then
            Abort_Parsing
              ("invalid field for " & Node_Type_Image (Node) & ": " & Name);
         elsif not Is_Field (M) then
            Abort_Parsing
              (Name & " is not a syntax field for " & Node_Type_Image (Node));
         elsif Is_Null_For (M, Node) then
            Abort_Parsing
              (Name & " is a null field for " & Node_Type_Image (Node));
         else
            return To_Index (M);
         end if;
      end To_Struct_Member_Index;

      ----------------------
      -- To_Predicate_Ref --
      ----------------------

      function To_Predicate_Ref
        (Name : String; Node : Type_Ref) return Struct_Member_Ref
      is
         M : constant Struct_Member_Ref :=
           Map.Lookup_Struct_Member (Node, To_Symbol (Name));
      begin
         if M = No_Struct_Member_Ref then
            Abort_Parsing
              (Node_Type_Image (Node) & " has no " & Name & " member");
         elsif not Is_Property (M) then
            Abort_Parsing
              (Name & " is not a property for " & Node_Type_Image (Node));
         elsif Member_Type (M) /= Type_Of (From_Bool (Language, False)) then
            Abort_Parsing (Name & " must return a boolean");
         elsif Member_Last_Argument (M) /= 0 then
            Abort_Parsing (Name & " must take no argument");
         else
            return M;
         end if;
      end To_Predicate_Ref;

      -------------------
      -- Template_Kind --
      -------------------

      function Template_Kind
        (Context : Template_Parsing_Context_Kind;
         JSON    : JSON_Value) return Template_Parsing_State_Kind
      is
         Result          : Template_Parsing_State_Kind := Simple_Recurse;
         Abort_Recursion : exception;

         function Kind_Matches
           (JSON : JSON_Value; Kind : String) return Boolean;
         --  Assuming that JSON is an object, return whether it has a "kind"
         --  field equal to Kind.

         procedure Process (JSON : JSON_Value);
         procedure Process_Map_Item (Name : String; JSON : JSON_Value);

         ------------------
         -- Kind_Matches --
         ------------------

         function Kind_Matches
           (JSON : JSON_Value; Kind : String) return Boolean
         is
         begin
            return JSON.Has_Field ("kind")
                   and then JSON.Get ("kind").Kind = JSON_String_Type
                   and then String'(JSON.Get ("kind")) = Kind;
         end Kind_Matches;

         -------------
         -- Process --
         -------------

         procedure Process (JSON : JSON_Value) is
         begin
            case JSON.Kind is
               when JSON_Object_Type =>

                  --   As soon as we find a "tableSeparator"/"text" template
                  --   node, we know this is a Recurse_Field template (in
                  --   "node") or a Recurse_In_Field template (in "fields").
                  --
                  --   For a "node", we also know this is a Recurse_Field
                  --   template as soon as we find a "recurse_field" node.

                  case Context is
                     when Node_Template =>
                        if Kind_Matches (JSON, "text")
                           or else Kind_Matches (JSON, "recurse_field")
                        then
                           Result := Recurse_Field;
                           raise Abort_Recursion;
                        end if;

                     when Field_Template =>
                        if Kind_Matches (JSON, "tableSeparator")
                           or else Kind_Matches (JSON, "text")
                        then
                           Result := Recurse_In_Field;
                           raise Abort_Recursion;
                        end if;

                     when Sep_Template =>
                        null;

                     when Join_Template =>
                        null;
                  end case;

                  JSON.Map_JSON_Object (Process_Map_Item'Access);

               when JSON_Array_Type =>
                  for Item of JSON_Array'(JSON.Get) loop
                     Process (Item);
                  end loop;

               when others =>
                  null;
            end case;
         end Process;

         ----------------------
         -- Process_Map_Item --
         ----------------------

         procedure Process_Map_Item (Name : String; JSON : JSON_Value) is
            pragma Unreferenced (Name);
         begin
            Process (JSON);
         end Process_Map_Item;

      begin
         Process (JSON);
         return Result;
      exception
         when Abort_Recursion =>
            return Result;
      end Template_Kind;

      -----------------------
      -- Initial_State_For --
      -----------------------

      function Initial_State_For
        (Node    : Type_Ref;
         JSON    : JSON_Value;
         Context : Template_Parsing_Context) return Template_Parsing_State is
      begin
         return Result : Template_Parsing_State
                           (Template_Kind (Context.Kind, JSON))
         do
            case Result.Kind is
               when Recurse_In_Join =>

                  --  Knowing that we are processing a node template,
                  --  Template_Kind is not supposed to return Recurse_In_Join.

                  raise Program_Error;

               when Simple_Recurse =>
                  Result.Recurse_Found := False;

               when Recurse_Field | Recurse_In_Field =>
                  if Result.Kind = Recurse_Field then

                     --  Ensure that "recurse_field" templates are valid for
                     --  this node.

                     if Is_Abstract (Node) then
                        Abort_Parsing
                          (Context,
                           "text/recurse_field are valid for concrete nodes"
                           & " only");
                     elsif Is_Token_Node (Node) then
                        Abort_Parsing
                          (Context,
                           "text/recurse_field are not valid for token nodes");
                     elsif Is_List_Node (Node) then
                        Abort_Parsing
                          (Context,
                           "text/recurse_field are not valid for list nodes");
                     end if;
                  end if;

                  Result.Linear_Template := Linear_Template (Node);
                  Result.Linear_Position := 1;
            end case;
         end return;
      end Initial_State_For;

      -----------------------
      -- Initial_State_For --
      -----------------------

      function Initial_State_For
        (Node    : Type_Ref;
         Field   : Struct_Member_Ref;
         JSON    : JSON_Value) return Template_Parsing_State
      is
      begin
         return Result : Template_Parsing_State
                           (Template_Kind (Field_Template, JSON))
         do
            case Result.Kind is
               when Simple_Recurse =>
                  Result.Recurse_Found := False;

               when Recurse_Field | Recurse_In_Join =>

                  --  Knowing that we are processing a field template,
                  --  Template_Kind is not supposed to return
                  --  Recurse_Field/Recurse_In_Join.

                  raise Program_Error;

               when Recurse_In_Field =>
                  Result.Linear_Template := Linear_Template (Node, Field);
                  Result.Linear_Position := 1;
            end case;
         end return;
      end Initial_State_For;

      --------------------
      -- Parse_Template --
      --------------------

      function Parse_Template
        (JSON    : JSON_Value;
         Context : in out Template_Parsing_Context) return Template_Type
      is
         Initial_Symbol_Map : constant Symbol_Parsing_Maps.Map :=
           Context.Symbols;
         --  Symbol map before template parsing, i.e. only symbols defined in a
         --  "parent" template.

         Result_Symbols : Symbol_Parsing_Maps.Map;
         --  Symbol map that represent this template only (its own definitions,
         --  its own references).

         Root : Document_Type;
      begin
         --  Clear symbol reference marks (so that we compute which symbols
         --  *this* template references) and run the template parser.

         for Info of Context.Symbols loop
            pragma Assert (Info.Has_Definition);
            Info.Is_Referenced := False;
         end loop;

         Root := Parse_Template_Helper (JSON, Context);

         --  Now, sanity check symbol usage and compute symbol usage that is
         --  specific to this template (Result_Symbols).

         for Ctx_Cur in Context.Symbols.Iterate loop
            declare
               use Symbol_Parsing_Maps;

               K        : constant Symbol_Type := Key (Ctx_Cur);
               Ctx_Info : Symbol_Info renames Context.Symbols (Ctx_Cur);
               Init_Cur : constant Cursor := Initial_Symbol_Map.Find (K);
            begin
               --  Make sure that all symbols referenced in this template are
               --  also defined in this template.

               if not Ctx_Info.Has_Definition then
                  Abort_Parsing
                    (Context,
                     "undefined symbol: " & To_String (Ctx_Info.Source_Name));
               end if;

               --  Report definitions

               if not Has_Element (Init_Cur) then

                  --  The template we parsed defines this symbol

                  Result_Symbols.Insert (K, Ctx_Info);

               elsif Ctx_Info.Is_Referenced then

                  --  The template we parse does not define this symbol, but it
                  --  references it.

                  Result_Symbols.Insert
                    (K,
                     (Ctx_Info.Source_Name,
                      Ctx_Info.Template_Sym,
                      Has_Definition => False,
                      Is_Referenced  => True));
               end if;
            end;
         end loop;

         case Context.State.Kind is
            when Simple_Recurse =>
               if Context.State.Recurse_Found then
                  return
                    (Kind    => With_Recurse,
                     Root    => Root,
                     Symbols => Result_Symbols);
               else
                  Abort_Parsing (Context, "recursion is missing");
               end if;

            when Recurse_Field | Recurse_In_Field | Recurse_In_Join =>

               --  Make sure that the template covers all items in the linear
               --  template.

               if Context.State.Linear_Position
                  /= Context.State.Linear_Template.Last_Index + 1
               then
                  Abort_Parsing_Linear
                    (Context,
                     "missing "
                     & Image
                         (Context.State.Linear_Template
                            (Context.State.Linear_Position)));
               end if;

               case Context.State.Kind is
                  when Simple_Recurse =>
                     raise Program_Error;
                  when Recurse_Field =>
                     return
                       (Kind    => With_Recurse_Field,
                        Root    => Root,
                        Symbols => Result_Symbols);
                  when Recurse_In_Field =>
                     return
                       (Kind    => With_Text_Recurse,
                        Root    => Root,
                        Symbols => Result_Symbols);
                  when Recurse_In_Join =>
                     return
                       (Kind    => Join_Template,
                        Root    => Root,
                        Symbols => Result_Symbols);
               end case;
         end case;
      end Parse_Template;

      ---------------------------
      -- Parse_Template_Helper --
      ---------------------------

      function Parse_Template_Helper
        (JSON    : JSON_Value;
         Context : in out Template_Parsing_Context) return Document_Type
      is
         Symbol_Map : Symbol_Parsing_Maps.Map renames Context.Symbols;
      begin
         case JSON.Kind is
         when JSON_Array_Type =>
            declare
               Items : Document_Vectors.Vector;
            begin
               for D of JSON_Array'(JSON.Get) loop
                  Items.Append
                    (Parse_Template_Helper (D, Context));
               end loop;
               return Pool.Create_List (Items);
            end;

         when JSON_String_Type =>
            declare
               Value : constant String := JSON.Get;
            begin
               if Value = "breakParent" then
                  return Pool.Create_Break_Parent;
               elsif Value = "flushLineBreaks" then
                  return Pool.Create_Flush_Line_Breaks;
               elsif Value = "hardline" then
                  return Pool.Create_Hard_Line;
               elsif Value = "hardlineWithoutBreakParent" then
                  return Pool.Create_Hard_Line_Without_Break_Parent;
               elsif Value = "line" then
                  return Pool.Create_Line;
               elsif Value = "literalline" then
                  return Pool.Create_Literal_Line;
               elsif Value = "recurse" then
                  Process_Recurse (Context);
                  return Pool.Create_Recurse;
               elsif Value = "recurse_left" then
                  declare
                     Item : Linear_Template_Item := (Kind => Recurse_Left);
                  begin
                     Process_Linear_Template_Item (Item, Context);
                  end;
                  return Pool.Create_Recurse_Left;
               elsif Value = "recurse_right" then
                  declare
                     Item : Linear_Template_Item := (Kind => Recurse_Right);
                  begin
                     Process_Linear_Template_Item (Item, Context);
                  end;
                  return Pool.Create_Recurse_Right;
               elsif Value = "softline" then
                  return Pool.Create_Soft_Line;
               elsif Value = "trim" then
                  return Pool.Create_Trim;
               elsif Value = "whitespace" then
                  return Pool.Create_Whitespace;
               else
                  Abort_Parsing
                    (Context,
                     "invalid string template document kind: " & Value);
               end if;
            end;

         when JSON_Object_Type =>
            if not JSON.Has_Field ("kind") then
               Abort_Parsing (Context, "missing ""kind"" key");
            elsif JSON.Get ("kind").Kind /= JSON_String_Type then
               Abort_Parsing
                 (Context,
                  "invalid ""kind"": " & JSON.Get ("kind").Kind'Image);
            end if;

            declare
               Kind : constant String := JSON.Get ("kind");
            begin
               if Kind = "align" then
                  declare
                     Width : JSON_Value;
                     Data  : Prettier.Alignment_Data_Type;
                  begin
                     if not JSON.Has_Field ("width") then
                        Abort_Parsing
                          (Context, "missing ""width"" key for align");
                     end if;
                     Width := JSON.Get ("width");
                     case Width.Kind is
                        when JSON_Int_Type =>
                           Data := (Kind => Prettier.Width, N => Width.Get);
                        when JSON_String_Type =>
                           Data := (Kind => Prettier.Text, T => Width.Get);
                        when others =>
                           Abort_Parsing
                             (Context, "invalid ""width"" key for align");
                     end case;

                     if not JSON.Has_Field ("contents") then
                        Abort_Parsing
                          (Context, "missing ""contents"" key for align");
                     end if;

                     return Pool.Create_Align
                       (Data,
                        Parse_Template_Helper
                          (JSON.Get ("contents"), Context));
                  end;

               elsif Kind in
                  "dedent"
                  | "dedentToRoot"
                  | "markAsRoot"
                  | "innerRoot"
                  | "continuationLineIndent"
               then
                  if not JSON.Has_Field ("contents") then
                     Abort_Parsing
                       (Context, "missing ""contents"" key for " & Kind);
                  end if;
                  return Pool.Create_Align
                    (Data     => (if Kind = "dedent"
                                  then (Kind => Prettier.Dedent)
                                  elsif Kind = "dedentToRoot"
                                  then (Kind => Prettier.Dedent_To_Root)
                                  elsif Kind = "markAsRoot"
                                  then (Kind => Prettier.Root)
                                  elsif Kind = "innerRoot"
                                  then (Kind => Prettier.Inner_Root)
                                  elsif Kind = "continuationLineIndent"
                                  then
                                    (Kind => Prettier.Continuation_Line_Indent)
                                  else raise Program_Error),
                     Contents => Parse_Template_Helper
                                   (JSON.Get ("contents"), Context));

               elsif Kind = "fill" then
                  declare
                     Document : Document_Type;
                  begin
                     if not JSON.Has_Field ("document") then
                        Abort_Parsing
                          (Context, "missing ""document"" key for fill");
                     end if;
                     Document :=
                       Parse_Template_Helper (JSON.Get ("document"), Context);

                     return Pool.Create_Fill (Document);
                  end;

               elsif Kind = "group" then
                  declare
                     Document     : Document_Type;
                     Should_Break : Boolean := False;
                     Id           : Template_Symbol := No_Template_Symbol;
                  begin
                     if not JSON.Has_Field ("document") then
                        Abort_Parsing
                          (Context, "missing ""document"" key for group");
                     end if;
                     Document :=
                       Parse_Template_Helper (JSON.Get ("document"), Context);

                     if JSON.Has_Field ("shouldBreak") then
                        declare
                           JSON_Should_Break : constant JSON_Value :=
                             JSON.Get ("shouldBreak");
                        begin
                           if JSON_Should_Break.Kind /= JSON_Boolean_Type then
                              Abort_Parsing
                                (Context,
                                 "invalid group shouldBreak: "
                                 & JSON_Should_Break.Kind'Image);
                           end if;
                           Should_Break := JSON_Should_Break.Get;
                        end;
                     end if;

                     --  If a symbol is given to identify this group, create an
                     --  internal symbol for it.

                     if JSON.Has_Field ("id") then
                        declare
                           JSON_Id : constant JSON_Value := JSON.Get ("id");
                        begin
                           if JSON_Id.Kind /= JSON_String_Type then
                              Abort_Parsing
                                (Context,
                                 "invalid group id: "
                                 & JSON_Id.Kind'Image);
                           end if;

                           begin
                              Id := Declare_Symbol
                                (JSON_Id.Get, Symbols, Symbol_Map);
                           exception
                              when Duplicate_Symbol_Definition =>
                                 Abort_Parsing
                                   (Context,
                                    "duplicate group id: " & JSON_Id.Get);
                           end;
                        end;
                     end if;

                     return Pool.Create_Group (Document, Should_Break, Id);
                  end;

               elsif Kind = "ifBreak" then
                  declare
                     Contents      : Document_Type;
                     Flat_Contents : Document_Type;

                     Contents_Context : Template_Parsing_Context := Context;
                     Flat_Context     : Template_Parsing_Context := Context;

                     Group_Id : Template_Symbol := No_Template_Symbol;
                  begin
                     if not JSON.Has_Field ("breakContents") then
                        Abort_Parsing
                          (Context,
                           "missing ""breakContents"" key for ifBreak");
                     end if;

                     Contents :=
                       Parse_Template_Helper
                         (JSON.Get ("breakContents"), Contents_Context);

                     Flat_Contents :=
                       (if JSON.Has_Field ("flatContents")
                        then Parse_Template_Helper
                               (JSON.Get ("flatContents"), Flat_Context)
                        else null);

                     --  Unify the parsing state for both branches and update
                     --  Context accordingly.

                     if Contents_Context.State /= Flat_Context.State then
                        Abort_Parsing
                          (Context,
                           "ifBreak alternatives have inconsistent recurse"
                           & " structure");
                     end if;
                     Context.State := Contents_Context.State;

                     --  If present, get the symbol for the given group id

                     if JSON.Has_Field ("groupId") then
                        declare
                           JSON_Id : constant JSON_Value :=
                             JSON.Get ("groupId");
                        begin
                           if JSON_Id.Kind /= JSON_String_Type then
                              Abort_Parsing
                                (Context,
                                 "invalid group id: "
                                 & JSON_Id.Kind'Image);
                           end if;

                           Group_Id :=
                             Reference_Symbol
                               (JSON_Id.Get, Symbols, Symbol_Map);
                        end;
                     end if;

                     return Pool.Create_If_Break
                              (Contents, Flat_Contents, Group_Id);
                  end;

               elsif Kind = "ifEmpty" then
                  if Context.Kind /= Field_Template then
                     Abort_Parsing
                       (Context,
                        """ifEmpty"" is valid only in field templates");
                  end if;

                  declare
                     Then_Contents : Document_Type;
                     Else_Contents : Document_Type;

                     Then_Context : Template_Parsing_Context := Context;
                     Else_Context : Template_Parsing_Context := Context;
                  begin
                     if not JSON.Has_Field ("then") then
                        Abort_Parsing
                          (Context,
                           "missing ""then"" key for ifEmpty");
                     end if;
                     Then_Contents :=
                       Parse_Template_Helper (JSON.Get ("then"), Then_Context);

                     if not JSON.Has_Field ("else") then
                        Abort_Parsing
                          (Context,
                           "missing ""else"" key for ifEmpty");
                     end if;
                     Else_Contents :=
                       Parse_Template_Helper (JSON.Get ("else"), Else_Context);

                     --  Unify the parsing state for both branches and update
                     --  Context accordingly.

                     if Then_Context.State /= Else_Context.State then
                        Abort_Parsing
                          (Context,
                           "ifEmpty alternatives have inconsistent recurse"
                           & " structure");
                     end if;
                     Context.State := Else_Context.State;

                     return Pool.Create_If_Empty
                              (Then_Contents, Else_Contents);
                  end;

               elsif Kind = "ifKind" then
                  if Context.Kind not in Node_Template | Field_Template then
                     Abort_Parsing
                       (Context,
                        """ifKind"" is valid in node templates and field"
                        & " templates only");
                  end if;

                  declare
                     Field_JSON    : constant JSON_Value :=
                       JSON.Get ("field");
                     Matchers_JSON : constant JSON_Value :=
                       JSON.Get ("matchers");
                     Default_JSON  : constant JSON_Value :=
                       JSON.Get ("default");
                     Null_JSON     : constant JSON_Value :=
                       JSON.Get ("null");

                     Field_Ref : Struct_Member_Ref;
                  begin
                     --  Validate that the keys are present and have the
                     --  correct type.

                     if Context.Kind = Node_Template then
                        if Field_JSON.Kind = JSON_Null_Type then
                           Abort_Parsing
                             (Context, "missing ""field"" key for ifKind");

                        elsif Field_JSON.Kind /= JSON_String_Type then
                           Abort_Parsing
                             (Context,
                              "invalid ""field"" key kind for ifKind: found "
                              & Field_JSON.Kind'Image
                              & "; expected "
                              & JSON_String_Type'Image);
                        end if;

                        Field_Ref := From_Index
                          (Language,
                           To_Struct_Member_Index
                             (Field_JSON.Get, Context.Node));

                     elsif Field_JSON.Kind /= JSON_Null_Type then
                        Abort_Parsing
                          (Context, "invalid ""field"" key for ifKind");
                     else
                        Field_Ref := Context.Field;
                     end if;

                     if Default_JSON.Kind = JSON_Null_Type then
                        Abort_Parsing
                          (Context, "missing ""default"" key for ifKind");
                     end if;

                     if Matchers_JSON.Kind = JSON_Null_Type then
                        Abort_Parsing
                          (Context, "missing ""matchers"" key for ifKind");

                     elsif Matchers_JSON.Kind /= JSON_Array_Type then
                        Abort_Parsing
                          (Context,
                           "invalid ""matchers"" key kind for ifKind: "
                           & "found "
                           & Matchers_JSON.Kind'Image
                           & "; expected "
                           & JSON_Array_Type'Image);
                     end if;

                     declare
                        --  Before parsing the "matchers", "default" or "null"
                        --  keys, save the current context.
                        --
                        --  Start by parsing the "default" key with the current
                        --  context.
                        --
                        --  Then process each matcher with its own nested
                        --  context confirming that all end on the same state.

                        Initial_Context : constant Template_Parsing_Context :=
                          Context;

                        If_Kind_Default  : Document_Type;
                        If_Kind_Null     : Document_Type := null;
                        If_Kind_Matchers : Matcher_Vectors.Vector;

                        procedure Process_Matcher (Matcher_JSON : JSON_Value);
                        --  Process Matcher_JSON with their own nested context
                        --  context confirming that it ends in the same state
                        --  as Context.

                        function Parse_Type_Ref
                          (JSON : JSON_Value) return Type_Ref;
                        --  Return the type reference corresponding to JSON,
                        --  expected to be a string. Abort parsing if this is
                        --  not a valid type reference.

                        ---------------------
                        -- Process_Matcher --
                        ---------------------

                        procedure Process_Matcher (Matcher_JSON : JSON_Value)
                        is
                           Kind           : constant JSON_Value :=
                             Matcher_JSON.Get ("kind");
                           Document_JSON  : constant JSON_Value :=
                             Matcher_JSON.Get ("document");
                           Types          : Type_Ref_Vectors.Vector;
                           Nested_Context : Template_Parsing_Context :=
                             Initial_Context;
                        begin
                           if Kind.Kind = JSON_String_Type then
                              Types.Append (Parse_Type_Ref (Kind));
                           elsif Kind.Kind = JSON_Array_Type then
                              for K of JSON_Array'(Kind.Get) loop
                                 Types.Append (Parse_Type_Ref (K));
                              end loop;
                           else
                              Abort_Parsing
                                (Context,
                                 "invalid matcher ""kind"" field for "
                                 & """ifKind"" - found "
                                 & Kind.Kind'Image
                                 & "; expected a string or array of strings");
                           end if;

                           --  Parse the matcher and store it in the table

                           If_Kind_Matchers.Append
                             (Matcher_Record'
                               (Types,
                                Parse_Template_Helper
                                 (Document_JSON, Nested_Context)));

                           --  Confirm that the final linear position is
                           --  homogeneous between all matchers.

                           if Nested_Context.State /= Context.State then
                              Abort_Parsing
                                (Context,
                                 "ifKind matcher """ & Kind.Get & """ has an "
                                 & "inconsistent recurse structure");
                           end if;
                        end Process_Matcher;

                        --------------------
                        -- Parse_Type_Ref --
                        --------------------

                        function Parse_Type_Ref
                          (JSON : JSON_Value) return Type_Ref is
                        begin
                           if JSON.Kind /= JSON_String_Type then
                              Abort_Parsing
                                (Context,
                                 "invalid matcher ""kind"" field for "
                                 & """ifKind"" - found "
                                 & JSON.Kind'Image
                                 & "; expected "
                                 & JSON_String_Type'Image);
                           end if;
                           return From_Index
                             (Language, To_Type_Index (JSON.Get));
                        end Parse_Type_Ref;

                     begin
                        If_Kind_Default :=
                          Parse_Template_Helper (Default_JSON, Context);

                        if Null_JSON.Kind /= JSON_Null_Type then
                           declare
                              If_Kind_Null_Context :
                                Template_Parsing_Context :=
                                  Initial_Context;

                           begin
                              If_Kind_Null :=
                                Parse_Template_Helper
                                  (Null_JSON, If_Kind_Null_Context);

                              if If_Kind_Null_Context.State /= Context.State
                              then
                                 Abort_Parsing
                                   (Context,
                                    "ifKind ""null"" matcher has an "
                                    & "inconsistent recurse structure");
                              end if;
                           end;
                        end if;

                        for Matcher_JSON of
                          JSON_Array'(Get (Matchers_JSON))
                        loop
                           if Matcher_JSON.Kind /= JSON_Object_Type then
                              Abort_Parsing
                                (Context,
                                 "invalid ""matchers"" element kind for "
                                 & """ifKind"" - found "
                                 & Matcher_JSON.Kind'Image
                                 & "; expected "
                                 & JSON_Object_Type'Image);
                           end if;

                           Process_Matcher (Matcher_JSON);
                        end loop;

                        return
                          Pool.Create_If_Kind
                            (Field_Ref,
                             If_Kind_Matchers,
                             If_Kind_Default,
                             If_Kind_Null);
                     end;
                  end;

               elsif Kind = "indent" then
                  if not JSON.Has_Field ("contents") then
                     Abort_Parsing
                       (Context, "missing ""contents"" key for indent");
                  end if;
                  return Pool.Create_Indent
                    (Parse_Template_Helper (JSON.Get ("contents"), Context));

               elsif Kind = "recurse_field" then
                  declare
                     F : JSON_Value;
                  begin
                     if not JSON.Has_Field ("field") then
                        Abort_Parsing
                          (Context, "missing ""field"" key for recurse_field");
                     end if;

                     F := JSON.Get ("field");
                     if F.Kind /= JSON_String_Type then
                        Abort_Parsing
                          (Context,
                           "invalid recurse_field field: " & F.Kind'Image);
                     end if;

                     --  Validate that "recurse_field" can appear in this
                     --  template at this place. Let
                     --  Process_Linear_Template_Item give us the field
                     --  position: initialize it with Positive'Last (an
                     --  obviously invalid position) to make it clear that this
                     --  component needs an update.

                     declare
                        Item : Linear_Template_Item :=
                          (Kind           => Field_Item,
                           Field_Ref      => From_Index
                                               (Language,
                                                To_Struct_Member_Index
                                                  (F.Get, Context.Node)),
                           Field_Position => Positive'Last);
                     begin
                        Process_Linear_Template_Item (Item, Context);
                        return Pool.Create_Recurse_Field
                                 (Item.Field_Ref, Item.Field_Position);
                     end;
                  end;

               elsif Kind = "recurse_flatten" then
                  declare
                     L     : JSON_Value;
                     T     : Type_Ref;
                     Types : Type_Vectors.Vector;
                  begin
                     --  Use the given type guard for the flattening, or use
                     --  the root node to flatten for all nodes.

                     if JSON.Has_Field ("if") then
                        L := JSON.Get ("if");
                        if L.Kind /= JSON_Array_Type then
                           Abort_Parsing
                             (Context,
                              "invalid recurse_flatten if: " & L.Kind'Image);
                        end if;
                        for Name of JSON_Array'(L.Get) loop
                           if Name.Kind /= JSON_String_Type then
                              Abort_Parsing
                                (Context,
                                 "invalid item in recurse_flatten if: "
                                 & Name.Kind'Image);
                           end if;

                           T := Map.Lookup_Type (To_Symbol (Name.Get));
                           if T = No_Type_Ref or else not Is_Node_Type (T) then
                              Abort_Parsing
                                (Context,
                                 "invalid node type in recurse_flatten if: "
                                 & Name.Get);
                           end if;

                           Types.Append (T);
                        end loop;
                     else
                        Types.Append (Root_Node_Type (Language));
                     end if;
                     Process_Recurse (Context);
                     return Pool.Create_Recurse_Flatten (Types);
                  end;

               elsif Kind in "tableSeparator" | "text" then
                  declare
                     T : JSON_Value;
                  begin
                     if not JSON.Has_Field ("text") then
                        Abort_Parsing
                          (Context, "missing ""text"" key for " & Kind);
                     end if;

                     T := JSON.Get ("text");
                     if T.Kind /= JSON_String_Type then
                        Abort_Parsing
                          (Context, "invalid text field: " & T.Kind'Image);
                     end if;

                     --  Validate that this text template can appear in this
                     --  template at this place. Let
                     --  Process_Linear_Template_Item give us the token type:
                     --  initialize it with No_Token_Kind_Ref (an obviously
                     --  invalid position) to make it clear that this component
                     --  needs an update.
                     --
                     --  There is one special case: we allow empty table
                     --  separators, so that they can be put in table join
                     --  templates.

                     if Kind = "tableSeparator"
                        and then Length (Unbounded_String'(T.Get)) = 0
                     then
                        return Pool.Create_Empty_Table_Separator;
                     end if;

                     declare
                        Item : Linear_Template_Item :=
                          (Kind       => Token_Item,
                           Token_Kind => No_Token_Kind_Ref,
                           Token_Text => To_Unbounded_Text
                                           (From_UTF8 (T.Get)));
                     begin
                        Process_Linear_Template_Item (Item, Context);

                        if Kind = "text" then
                           return Pool.Create_Token
                                    (Item.Token_Kind, Item.Token_Text);
                        else
                           return Pool.Create_Table_Separator
                                    (Item.Token_Kind, Item.Token_Text);
                        end if;
                     end;
                  end;

               elsif Kind = "whitespace" then
                  if not JSON.Has_Field ("length") then
                     Abort_Parsing (Context, "missing ""length"" key");
                  end if;
                  declare
                     Length : constant JSON_Value := JSON.Get ("length");
                  begin
                     if Length.Kind /= JSON_Int_Type then
                        Abort_Parsing
                          (Context,
                           "invalid whitespace length: " & Length.Kind'Image);
                     end if;
                     return Pool.Create_Whitespace (Length.Get);
                  end;

               else
                  Abort_Parsing
                    (Context, "invalid template document kind: " & Kind);
               end if;
            end;

         when others =>
            Abort_Parsing
              (Context, "invalid template JSON node: " & JSON.Kind'Image);
         end case;
      end Parse_Template_Helper;

      ---------------------
      -- Process_Recurse --
      ---------------------

      procedure Process_Recurse (Context : in out Template_Parsing_Context) is
      begin
         case Context.State.Kind is
            when Simple_Recurse =>
               if Context.State.Recurse_Found then
                  Abort_Parsing (Context, "too many recursions");
               else
                  Context.State.Recurse_Found := True;
               end if;

            when Recurse_Field =>
               Abort_Parsing
                 (Context,
                  "using ""recurse""/""recurse_flatten"" in the same template"
                  & " as ""recurse_field""/""text"" is invalid");

            when Recurse_In_Field =>
               declare
                  Item : Linear_Template_Item := (Kind => Recurse_Item);
               begin
                  Process_Linear_Template_Item (Item, Context);
               end;

            when Recurse_In_Join =>
               Abort_Parsing
                 (Context,
                  "only ""recurse_left"" or ""recurse_right"" are valid in"
                  & " table join templates");
         end case;
      end Process_Recurse;

      ----------------------------------
      -- Process_Linear_Template_Item --
      ----------------------------------

      procedure Process_Linear_Template_Item
        (Item    : in out Linear_Template_Item;
         Context : in out Template_Parsing_Context)
      is
         function What return String
         is (case Item.Kind is
             when Token_Item    => "text",
             when Field_Item    => "recurse_field",
             when Recurse_Item  => "recurse",
             when Recurse_Left  => "recurse_left",
             when Recurse_Right => "recurse_right");
      begin
         --  Ensure that it is valid to have a "recurse_field" node in this
         --  template.

         if Context.State.Kind
            not in Recurse_Field | Recurse_In_Field | Recurse_In_Join
         then
            Abort_Parsing
              (Context, What & " cannot appear in a ""recurse"" template");
         end if;

         --  Now validate this new item: it must match what the linear template
         --  expects next.

         if Context.State.Linear_Position
            > Context.State.Linear_Template.Last_Index
         then
            Abort_Parsing_Linear
              (Context,
               "unexpected extra template item: " & Image (Item));
         end if;

         declare
            Expected : constant Linear_Template_Item :=
              Context.State.Linear_Template (Context.State.Linear_Position);
         begin
            if not Is_Equivalent (Item, Expected) then
               Abort_Parsing_Linear
                 (Context,
                  Image (Expected) & " expected, got " & Image (Item));
            end if;

            --  As per this procedure's contract, update Item to given the
            --  expected value to all of its components.

            Item := Expected;
         end;

         --  If we reach this point, we know that Item was expected: update the
         --  linear template position for the next expected item.

         Context.State.Linear_Position := Context.State.Linear_Position + 1;
      end Process_Linear_Template_Item;

      -------------------
      -- Check_Symbols --
      -------------------

      procedure Check_Symbols
        (Node            : Type_Ref;
         Parent_Name     : String;
         Parent_Template : Template_Type;
         Child_Name      : String;
         Child_Template  : Template_Type)
      is
         use Symbol_Parsing_Maps;
      begin
         for Child_Cur in Child_Template.Symbols.Iterate loop
            declare
               Child_Info        : Symbol_Info renames
                 Child_Template.Symbols (Child_Cur);
               Defined_In_Parent : constant Boolean :=
                 Parent_Template.Symbols.Contains (Key (Child_Cur));
            begin
               if Child_Info.Has_Definition then
                  if Defined_In_Parent then
                     Abort_Parsing
                       (Debug_Name (Node) & ": group id "
                        & To_String (Child_Info.Source_Name)
                        & " in " & Parent_Name
                        & " is duplicated in " & Child_Name);
                  end if;
               else
                  if not Defined_In_Parent then
                     Abort_Parsing
                       (Debug_Name (Node) & ": group id "
                        & To_String (Child_Info.Source_Name)
                        & " is referenced in " & Child_Name
                        & " but not defined in " & Parent_Name);
                  end if;
               end if;
            end;
         end loop;
      end Check_Symbols;

      -------------------
      -- Abort_Parsing --
      -------------------

      procedure Abort_Parsing
        (Context : Template_Parsing_Context; Message : String)
      is
         Prefix : constant String :=
           (case Context.Kind is
            when Node_Template =>
              """node"" template for " & Node_Type_Image (Context.Node),
            when Field_Template =>
              "template for " & Field_Image (Context.Field, Context.Node),
            when Sep_Template =>
              """" & JSON_Key_For (Context.Sep_Kind) & """ template for "
              & Node_Type_Image (Context.Node),
            when Join_Template =>
              """table""/""join""/""template"" template for "
              & Node_Type_Image (Context.Node));
      begin
         Abort_Parsing (Prefix & ": " & Message);
      end Abort_Parsing;

      --------------------------
      -- Abort_Parsing_Linear --
      --------------------------

      procedure Abort_Parsing_Linear
        (Context : Template_Parsing_Context; Message : String)
      is
         Suffix : Unbounded_String;
      begin
         Append (Suffix, ASCII.LF & "Expected:");
         for Item of Context.State.Linear_Template loop
            Append (Suffix, ASCII.LF & "* " & Image (Item));
         end loop;
         Abort_Parsing (Context, Message & To_String (Suffix));
      end Abort_Parsing_Linear;

      -------------------
      -- Abort_Parsing --
      -------------------

      procedure Abort_Parsing (Message : String) is
      begin
         Append (Diagnostics, No_Source_Location_Range, To_Text (Message));
         raise Invalid_Input;
      end Abort_Parsing;

      Node_JSON_Map : Node_JSON_Maps.Map;
      --  For each node type described in the unparsing configuration,
      --  reference to the corresponding node configuration.

      --  First, parse the JSON document

      JSON_Result : constant Read_Result := Read (Buffer);
      JSON        : JSON_Value;
   begin
      Result.Ref_Count := 1;
      Result.Language := Language;
      Result.Symbols := Symbols;

      if JSON_Result.Success then
         JSON := JSON_Result.Value;
      else
         declare
            Sloc       : constant Source_Location :=
              (Line_Number (JSON_Result.Error.Line),
               Column_Number (JSON_Result.Error.Column));
            Sloc_Range : constant Source_Location_Range :=
              Make_Range (Sloc, Sloc);
         begin
            Append
              (Diagnostics,
               Sloc_Range,
               To_Text (To_String (JSON_Result.Error.Message)));
            raise Invalid_Input;
         end;
      end if;

      --  Then load the unparsing configuration from it. Require a
      --  "node_configs" key.

      if not JSON.Has_Field ("node_configs") then
         Abort_Parsing ("missing ""node_configs"" key");
      end if;

      --  Register all node configurations in Node_JSON_Map

      Node_JSON_Map := Node_Entries (JSON.Get ("node_configs"));

      --  Now, go through all node types: parse JSON configurations for nodes
      --  that have one, and implement configuration inheritance in general.

      for Node of All_Node_Types (Language) loop
         declare
            Key     : constant Type_Index := To_Index (Node);
            Present : constant Boolean := Node_JSON_Map.Contains (Key);
            JSON    : constant JSON_Value :=
              (if Present
               then Node_JSON_Map.Element (Key)
               else JSON_Null);

            --  Create the configuration for this node, and look for the
            --  configuration of the node it derives.

            Base_Config : constant Node_Config_Access :=
              (if Node = Root_Node_Type (Language)
               then null
               else Result.Node_Configs.Element (To_Index (Base_Type (Node))));
            Node_Config : constant Node_Config_Access :=
              new Node_Config_Record'
                (Is_Automatic  =>
                   not Present
                   and then (Base_Config = null
                             or else Base_Config.Is_Automatic),
                 Node_Template => No_Template,
                 Field_Configs => <>,
                 List_Config   => No_List_Config);
         begin
            if Check_All_Nodes then

               --  Emit an error if this is a concrete parse node for which no
               --  explicit config from the JSON matches.

               if Is_Concrete (Node)
                  and then not Is_Synthetic (Node)
                  and then Node_Config.Is_Automatic
               then
                  Append
                    (Diagnostics,
                     No_Source_Location_Range,
                     To_Text ("missing node config for " & Debug_Name (Node)));
               end if;
            end if;

            Result.Node_Configs.Insert (Key, Node_Config);

            --  Decode the JSON configuration:

            if JSON.Kind not in JSON_Null_Type | JSON_Object_Type then
               Abort_Parsing
                 ("invalid JSON configuration for " & Debug_Name (Node));
            end if;

            --  (1) the "node" entry (if present). If not present, inherit the
            --  config from the base field. As the last resort, use the default
            --  template.

            if JSON.Kind = JSON_Object_Type and then JSON.Has_Field ("node")
            then
               declare
                  JSON_Template : constant JSON_Value := JSON.Get ("node");
                  Context       : Template_Parsing_Context :=
                    (Kind    => Node_Template,
                     Node    => Node,
                     State   => <>,
                     Symbols => <>);
               begin
                  Context.State :=
                    Initial_State_For (Node, JSON_Template, Context);
                  Node_Config.Node_Template :=
                    Parse_Template (JSON_Template, Context);
               end;
            elsif Base_Config /= null then
               Node_Config.Node_Template := Base_Config.Node_Template;
            else
               Node_Config.Node_Template := Pool.Create_Recurse;
            end if;

            --  (2) the "fields" entry (if present)

            declare
               Field_JSON_Map : constant Field_JSON_Maps.Map :=
                 (if JSON.Kind = JSON_Object_Type
                     and then JSON.Has_Field ("fields")
                  then Field_Entries (Node, JSON.Get ("fields"))
                  else Field_JSON_Maps.Empty_Map);
            begin
               for Member of Members (Node) loop
                  if Is_Field (Member) and then not Is_Null_For (Member, Node)
                  then
                     declare
                        Key : constant Struct_Member_Index :=
                          To_Index (Member);
                        T   : Template_Type;
                     begin
                        --  If we have a JSON template for this field, just use
                        --  it.

                        if Field_JSON_Map.Contains (Key) then
                           declare
                              JSON    : constant JSON_Value :=
                                Field_JSON_Map.Element (Key);
                              Context : Template_Parsing_Context :=
                                (Kind    => Field_Template,
                                 State   => Initial_State_For
                                              (Node, Member, JSON),
                                 Symbols => Node_Config.Node_Template.Symbols,
                                 Node    => Node,
                                 Field   => Member);
                           begin
                              T := Parse_Template (JSON, Context);
                           end;

                        --  Otherwise, if the base node has this field, inherit
                        --  its configuration.

                        elsif Base_Config /= null
                              and then Base_Config.Field_Configs.Contains (Key)
                        then
                           T := Base_Config.Field_Configs.Element (Key);

                        --  If none of the above work, just provide the default
                        --  template.

                        else
                           T := Pool.Create_Recurse;
                        end if;

                        Node_Config.Field_Configs.Insert (Key, T);
                     end;
                  end if;
               end loop;
            end;

            --  (3) the "sep"/"leading_sep"/"trailing_sep" entries (if
            --  present). "sep" follows the usual inheritance rules, but for
            --  the two others:
            --
            --  * copy the "sep" template if provided for the same node
            --  * otherwise inherit the corresponding template from the base
            --    node.

            for Kind in List_Sep_Template_Kind'Range loop
               declare
                  JSON_Key : constant String := JSON_Key_For (Kind);

                  function From_Base return Template_Type
                  is (if Base_Config /= null
                      then Base_Config.List_Config.Seps (Kind)
                      else Pool.Create_Recurse);
               begin
                  if JSON.Kind = JSON_Object_Type
                     and then JSON.Has_Field (JSON_Key)
                  then
                     --  Make sure this is for a list node

                     if not Is_List_Node (Node) then
                        Abort_Parsing
                          (Debug_Name (Node)
                           & " is not a list node, invalid """ & JSON_Key
                           & """ configuration");
                     end if;

                     --  Make sure that the list node accepts leading/trailing
                     --  separators if we have a template for them.

                     declare
                        Sep_Extra : constant List_Sep_Extra :=
                          Unparsers.Node_Unparsers.all (Key).Sep_Extra;
                     begin
                        if Kind = Leading_Sep_Template
                           and then Sep_Extra /= Allow_Leading
                        then
                           Abort_Parsing
                             (Debug_Name (Node)
                              & " does not allow leading separators: invalid"
                              & " """ & JSON_Key & """ configuration");

                        elsif Kind = Trailing_Sep_Template
                           and then Sep_Extra /= Allow_Trailing
                        then
                           Abort_Parsing
                             (Debug_Name (Node) & " does not allow trailing"
                              & " separators: invalid """ & JSON_Key & """"
                              & " configuration");
                        end if;
                     end;

                     declare
                        Context : Template_Parsing_Context :=
                          (Kind     => Sep_Template,
                           Node     => Node,
                           Sep_Kind => Kind,
                           State    => (Kind          => Simple_Recurse,
                                        Recurse_Found => False),
                           Symbols  => Node_Config.Node_Template.Symbols);
                     begin
                        Node_Config.List_Config.Seps (Kind) :=
                          Parse_Template (JSON.Get (JSON_Key), Context);
                     end;

                  elsif Kind = Sep_Template then
                     Node_Config.List_Config.Seps (Kind) := From_Base;

                  elsif JSON.Kind = JSON_Object_Type
                        and then JSON.Has_Field (JSON_Key_For (Sep_Template))
                  then
                     Node_Config.List_Config.Seps (Kind) :=
                       Node_Config.List_Config.Seps (Sep_Template);
                  else
                     Node_Config.List_Config.Seps (Kind) := From_Base;
                  end if;
               end;
            end loop;

            --  (4) the "table" entry (if present). Inherit if possible/needed

            if JSON.Kind = JSON_Object_Type and then JSON.Has_Field ("table")
            then
               if not Is_List_Node (Node) then
                  Abort_Parsing
                    (Debug_Name (Node) & " is not a list node, invalid"
                    & " ""table"" configuration");
               end if;
               declare
                  Cfg             : Table_Config_Record renames
                    Node_Config.List_Config.Table_Config;
                  Table_JSON      : constant JSON_Value := JSON.Get ("table");
                  Sep_Before_JSON : JSON_Value;
                  Split_JSON      : JSON_Value;
                  Must_Break_JSON : JSON_Value;
                  Join_JSON       : JSON_Value;
               begin
                  if Table_JSON.Kind = JSON_Null_Type then
                     Cfg := (Enabled => False);
                  elsif Table_JSON.Kind /= JSON_Object_Type then
                     Abort_Parsing
                       ("invalid ""table"" entry for " & Debug_Name (Node)
                        & ": object expected");
                  else
                     Cfg :=
                       (Enabled        => True,
                        Sep_Before     => True,
                        Split          => (others => False),
                        Must_Break     => False,
                        Join_Predicate => No_Struct_Member_Ref,
                        Join_Template  => No_Template);

                     --  Process the optional "sep_before" entry

                     if Table_JSON.Has_Field ("sep_before") then
                        Sep_Before_JSON := Table_JSON.Get ("sep_before");
                        if Sep_Before_JSON.Kind /= JSON_Boolean_Type then
                           Abort_Parsing
                             ("invalid ""table""/""sep_before"" entry for "
                              & Debug_Name (Node) & ": boolean expected");
                        end if;
                        Cfg.Sep_Before := Sep_Before_JSON.Get;
                     end if;

                     --  Process the optional "split" entry

                     if Table_JSON.Has_Field ("split") then
                        Split_JSON := Table_JSON.Get ("split");
                        if Split_JSON.Kind /= JSON_Array_Type then
                           Abort_Parsing
                             ("invalid ""table""/""split"" entry for "
                              & Debug_Name (Node) & ": array expected");
                        end if;
                        for Item of JSON_Array'(Split_JSON.Get) loop
                           if Item.Kind /= JSON_String_Type then
                              Abort_Parsing
                                ("invalid ""table""/""split"" entry for "
                                 & Debug_Name (Node));
                           end if;

                           declare
                              Split : constant String := Item.Get;
                              Kind  : List_Table_Split_Kind;
                           begin
                              if Split = "empty_line" then
                                 Kind := Empty_Line;
                              elsif Split = "line_comment" then
                                 Kind := Line_Comment;
                              else
                                 Abort_Parsing
                                   ("invalid ""table""/""split"" entry for "
                                    & Debug_Name (Node));
                              end if;
                              Cfg.Split (Kind) := True;
                           end;
                        end loop;
                     end if;

                     --  Process the optional "must_break" entry

                     if Table_JSON.Has_Field ("must_break") then
                        Must_Break_JSON := Table_JSON.Get ("must_break");
                        if Must_Break_JSON.Kind /= JSON_Boolean_Type then
                           Abort_Parsing
                             ("invalid ""table""/""must_break"" entry for "
                              & Debug_Name (Node) & ": boolean expected");
                        end if;
                        Cfg.Must_Break := Must_Break_JSON.Get;
                     end if;

                     --  Process the optional "join" entry

                     if Table_JSON.Has_Field ("join") then
                        Join_JSON := Table_JSON.Get ("join");
                        if Join_JSON.Kind /= JSON_Object_Type then
                           Abort_Parsing
                             ("invalid ""table""/""join"" entry for "
                              & Debug_Name (Node) & ": object expected");
                        end if;

                        --  Validate the mandatory "predicate" property
                        --  reference.

                        if not Join_JSON.Has_Field ("predicate")
                           or else Join_JSON.Get ("predicate").Kind
                                   /= JSON_String_Type
                        then
                           Abort_Parsing
                             (Debug_Name (Node) & """table""/""join"" entries"
                              & " must have a ""predicate"" string entry");
                        end if;
                        Cfg.Join_Predicate :=
                          To_Predicate_Ref
                            (Join_JSON.Get ("predicate"),
                             List_Element_Type (Node));

                        --  Parse the join template if present. Provide a
                        --  default one that just concatenates the two rows
                        --  otherwise.

                        if Join_JSON.Has_Field ("template") then
                           declare
                              Context : Template_Parsing_Context :=
                                (Kind     => Join_Template,
                                 Node     => Node,
                                 State    =>
                                   (Kind            => Recurse_In_Join,
                                    Linear_Template =>
                                      Linear_Template_For_Join,
                                    Linear_Position => 1),
                                 Symbols  =>
                                   Node_Config.Node_Template.Symbols);
                           begin
                              Cfg.Join_Template :=
                                Parse_Template
                                  (Join_JSON.Get ("template"), Context);
                           end;
                        else
                           declare
                              Items : Document_Vectors.Vector;
                           begin
                              Items.Append (Pool.Create_Recurse_Left);
                              Items.Append (Pool.Create_Recurse_Right);
                              Cfg.Join_Template :=
                                (Kind    => Join_Template,
                                 Root    => Pool.Create_List (Items),
                                 Symbols => <>);
                           end;
                        end if;
                     end if;
                  end if;
               end;

            elsif Base_Config /= null then
               Node_Config.List_Config.Table_Config :=
                 Base_Config.List_Config.Table_Config;
            else
               Node_Config.List_Config.Table_Config := (Enabled => False);
            end if;

            --  Since inheritance may mix templates from different node
            --  configurations, we need to double check that there are no
            --  duplicate symbol definitions, and that all referenced symbols
            --  are defined.

            for Cur in Node_Config.Field_Configs.Iterate loop
               declare
                  Field : constant Struct_Member_Ref :=
                    From_Index (Language, Field_Config_Maps.Key (Cur));
               begin
                  Check_Symbols
                    (Node,
                     "the ""node"" template",
                     Node_Config.Node_Template,
                     "the """ & Debug_Name (Field) & """ template",
                     Field_Config_Maps.Element (Cur));
               end;
            end loop;
            for Kind in List_Sep_Template_Kind'Range loop
               declare
                  JSON_Key : constant String := JSON_Key_For (Kind);
               begin
                  Check_Symbols
                    (Node,
                     """node"" template",
                     Node_Config.Node_Template,
                     """" & JSON_Key & """ template",
                     Node_Config.List_Config.Seps (Kind));
               end;
            end loop;

            --  (5) the "flush_before_children" entry (if present). If not
            --  present, inherit the setting from the base node.

            if JSON.Kind = JSON_Object_Type
               and then JSON.Has_Field ("flush_before_children")
            then
               if not Is_List_Node (Node) then
                  Abort_Parsing
                    (Debug_Name (Node) & " is not a list node, invalid"
                     & " ""flush_before_children"" configuration");
               end if;

               declare
                  Value : constant JSON_Value :=
                    JSON.Get ("flush_before_children");
               begin
                  if Value.Kind /= JSON_Boolean_Type then
                     Abort_Parsing
                       ("invalid ""flush_before_children"" entry for "
                        & Debug_Name (Node) & ": boolean expected");
                  end if;

                  Node_Config.List_Config.Flush_Before_Children := Value.Get;
               end;
            elsif Base_Config /= null then
               Node_Config.List_Config.Flush_Before_Children :=
                 Base_Config.List_Config.Flush_Before_Children;
            else
               Node_Config.List_Config.Flush_Before_Children := True;
            end if;
         end;
      end loop;

      --  Process the optional "max_empty_lines" entry

      declare
         Max_Empty_Lines : Integer := -1;
      begin
         if JSON.Has_Field ("max_empty_lines") then
            declare
               Value : constant JSON_Value := JSON.Get ("max_empty_lines");
            begin
               if Value.Kind /= JSON_Int_Type or else Integer'(Value.Get) < 0
               then
                  Abort_Parsing
                    ("invalid ""max_empty lines"" entry: natural integer"
                     & " expected");
               end if;
               Max_Empty_Lines := Value.Get;
            end;
         end if;
         Result.Max_Empty_Lines := Max_Empty_Lines;
      end;

      --  If non-fatal errors were emitted, still fail to load the config

      if not Diagnostics.Is_Empty then
         Release (Result);
         return No_Unparsing_Configuration;
      end if;

      return (Ada.Finalization.Controlled with Value => Result);

   exception
      when Invalid_Input =>
         pragma Assert (not Diagnostics.Is_Empty);
         Release (Result);
         return No_Unparsing_Configuration;
   end Load_Unparsing_Config_From_Buffer;

   --------------------------
   -- Instantiate_Template --
   --------------------------

   function Instantiate_Template
     (Pool          : in out Document_Pool;
      Symbols       : in out Symbol_Instantiation_Context;
      Node          : Lk_Node;
      Current_Token : in out Lk_Token;
      Trivias       : Trivias_Info;
      Template      : Template_Type;
      Arguments     : Template_Instantiation_Args) return Document_Type
   is
      State : Instantiation_State :=
        (Node,
         Current_Token,
         Symbols'Unrestricted_Access,
         Arguments'Unrestricted_Access,
         Trivias'Unrestricted_Access);
   begin
      return Result : constant Document_Type :=
        Instantiate_Template_Helper (Pool, State, Template.Root)
      do
         Current_Token := State.Current_Token;
      end return;
   end Instantiate_Template;

   ---------------------------------
   -- Instantiate_Template_Helper --
   ---------------------------------

   function Instantiate_Template_Helper
     (Pool     : in out Document_Pool;
      State    : in out Instantiation_State;
      Template : Document_Type) return Document_Type is
   begin
      case Template_Document_Kind (Template.Kind) is
         when Align =>
            return Pool.Create_Align
              (Template.Align_Data,
               Instantiate_Template_Helper
                 (Pool, State, Template.Align_Contents));

         when Break_Parent =>
            return Pool.Create_Break_Parent;

         when Empty_Table_Separator =>
            return Pool.Create_Empty_Table_Separator;

         when Fill =>
            return Pool.Create_Fill
              (Instantiate_Template_Helper
                 (Pool, State, Template.Fill_Document));

         when Flush_Line_Breaks =>
            return Pool.Create_Flush_Line_Breaks;

         when Group =>
            return Pool.Create_Group
              (Instantiate_Template_Helper
                 (Pool, State, Template.Group_Document),
               Template.Group_Should_Break,
               Instantiate_Symbol (State.Symbols.all, Template.Group_Id));

         when Hard_Line =>
            return Pool.Create_Hard_Line;

         when Hard_Line_Without_Break_Parent =>
            return Pool.Create_Hard_Line_Without_Break_Parent;

         when If_Break =>
            declare
               Break_State : Instantiation_State := State;
               Flat_State  : Instantiation_State := State;

               --  Instantiate both alternative templates

               Break_Doc : constant Document_Type :=
                 Instantiate_Template_Helper
                   (Pool, Break_State, Template.If_Break_Contents);
               Flat_Doc  : constant Document_Type :=
                 Instantiate_Template_Helper
                   (Pool, Flat_State, Template.If_Break_Flat_Contents);
            begin
               --  Thanks to our validation process, both alternatives are
               --  supposed to process the same number of tokens.

               pragma Assert
                 (Break_State.Current_Token = Flat_State.Current_Token);
               State := Break_State;

               return Pool.Create_If_Break
                 (Break_Doc,
                  Flat_Doc,
                  Instantiate_Symbol
                    (State.Symbols.all, Template.If_Break_Group_Id));
            end;

         when If_Empty =>
            declare
               --  Consider that a list node with no child but with attached
               --  comments is *not* empty. This makes more sense for
               --  formatting concerns, as we unparse these comments as list
               --  children.

               Child       : constant Lk_Node :=
                 State.Arguments.With_Recurse_Doc.Node;
               Subtemplate : constant Document_Type :=
                 (if Is_Empty_List (Child)
                  then Template.If_Empty_Then
                  else Template.If_Empty_Else);
            begin
               return Instantiate_Template_Helper
                        (Pool, State, Subtemplate);
            end;

         when If_Kind =>
            declare
               Field_Node       : constant Lk_Node :=
                 Eval_Syntax_Field (State.Node, Template.If_Kind_Field);
               Matched_Template : Document_Type := Template.If_Kind_Default;

            begin
               --  If the field is not null, pick the document for the
               --  first matcher that accepts it.

               if not Field_Node.Is_Null then
                  for I in
                    Template.If_Kind_Matchers.First_Index
                    .. Template.If_Kind_Matchers.Last_Index
                  loop
                     if Matches
                          (Field_Node, Template.If_Kind_Matchers.Reference (I))
                     then
                        Matched_Template :=
                          Template.If_Kind_Matchers (I).Document;
                        exit;
                     end if;
                  end loop;

               --  Otherwise, use the null template, if present. For all
               --  other cases, use the default template.

               elsif Template.If_Kind_Null /= null then
                  Matched_Template := Template.If_Kind_Null;
               end if;

               return Instantiate_Template_Helper
                        (Pool, State, Matched_Template);
            end;

         when Indent =>
            return Pool.Create_Indent
              (Instantiate_Template_Helper
                 (Pool, State, Template.Indent_Document));

         when Line =>
            return Pool.Create_Line;

         when List =>
            declare
               Items : Document_Vectors.Vector;
            begin
               for I in 1 .. Template.List_Documents.Last_Index loop
                  Items.Append
                    (Instantiate_Template_Helper
                       (Pool,
                        State,
                        Template.List_Documents.Element (I)));
               end loop;
               return Pool.Create_List (Items);
            end;

         when Literal_Line =>
            return Pool.Create_Literal_Line;

         --  For all "recurse" nodes, the knowledge of how to update
         --  ``State.Current_Token`` is encoded in the ``Next_Token`` member of
         --  the corresponding ``Single_Template_Instantiation_Argument``
         --  record.

         when Recurse =>
            declare
               Arg : constant Single_Template_Instantiation_Argument :=
                 State.Arguments.With_Recurse_Doc;
            begin
               State.Current_Token := Arg.Next_Token;
               return Deep_Copy (Pool, Arg.Document);
            end;

         when Recurse_Field =>
            declare
               Arg : constant Single_Template_Instantiation_Argument :=
                 State.Arguments.Field_Docs (Template.Recurse_Field_Position);
            begin
               State.Current_Token := Arg.Next_Token;
               return Deep_Copy (Pool, Arg.Document);
            end;

         when Recurse_Flatten =>
            declare
               Arg : constant Single_Template_Instantiation_Argument :=
                 State.Arguments.With_Recurse_Doc;
            begin
               return Result : Document_Type := Arg.Document do
                  State.Current_Token := Arg.Next_Token;

                  --  As long as Result is a document we can flatten and that
                  --  was created by a node that passes the flattening guard,
                  --  unwrap it.

                  while not Arg.Node.Is_Null
                        and then Node_Matches
                                   (Arg.Node, Template.Recurse_Flatten_Types)
                  loop
                     case Result.Kind is
                        when Align =>
                           Result := Result.Align_Contents;

                        when Fill =>
                           Result := Result.Fill_Document;

                        when Group =>
                           Result := Result.Group_Document;

                        when Indent =>
                           Result := Result.Indent_Document;

                        when List =>
                           exit when Result.List_Documents.Length /= 1;
                           Result := Result.List_Documents.First_Element;

                        when others =>
                           exit;
                     end case;
                  end loop;

                  Result := Deep_Copy (Pool, Result);
               end return;
            end;

         when Recurse_Left =>
            return Deep_Copy (Pool, State.Arguments.Join_Left);

         when Recurse_Right =>
            return Deep_Copy (Pool, State.Arguments.Join_Right);

         when Soft_Line =>
            return Pool.Create_Soft_Line;

         when Table_Separator | Token =>
            declare
               Items : Document_Vectors.Vector;
               Inner : constant Document_Type :=
                 (case Template.Kind is
                  when Table_Separator => Pool.Create_Table_Separator
                                            (Template.Token_Kind,
                                             Template.Token_Text),
                  when Token           => Pool.Create_Token
                                            (Template.Token_Kind,
                                             Template.Token_Text),
                  when others          => raise Program_Error);
            begin
               Items.Append (Inner);
               Process_Trivias
                 (State.Current_Token,
                  Items,
                  Pool,
                  State.Trivias.all,
                  Skip_Token => True);
               return Pool.Create_List (Items);
            end;

         when Trim =>
            return Pool.Create_Trim;

         when Whitespace =>
            return Pool.Create_Whitespace (Template.Whitespace_Length);
      end case;
   end Instantiate_Template_Helper;

   -------------------------
   -- Unparse_To_Prettier --
   -------------------------

   function Unparse_To_Prettier
     (Node          : Lk_Node;
      Config        : Unparsing_Configuration;
      Process_Error : access procedure
                        (Node : Lk_Node; Message : String) := null)
      return Prettier_Ada.Documents.Document_Type
   is
      procedure Emit_Error (Node : Lk_Node; Message : String);
      --  Call ``Process_Error`` if it is not null. Print the error otherwise.

      Trivias     : Trivias_Info;
      Pool        : Document_Pool;
      Next_Symbol : Some_Template_Symbol := 1;

      procedure Skip_Tokens
        (Current_Token : in out Lk_Token;
         Tokens        : Token_Sequence;
         Label         : String);
      --  Update ``Current_Token`` as if we unparsed the given token sequence.
      --
      --  ``Label`` is used to track which token sequence is processed in debug
      --  traces.

      procedure Unparse_Tokens
        (Current_Token : in out Lk_Token;
         Tokens        : Token_Sequence;
         Items         : in out Document_Vectors.Vector;
         Label         : String);
      --  Create template nodes for each element in ``Tokens`` and append them
      --  to ``Items``. Update ``Current_Token`` accordingly.
      --
      --  ``Label`` is used to track which token sequence is processed in debug
      --  traces.

      procedure Append_To_Row
        (Rows    : in out Document_Vectors.Vector;
         Items   : in out Document_Vectors.Vector;
         New_Row : Boolean);
      --  Assuming that ``Rows`` is a vector of list nodes:
      --
      --  * If ``New_Row`` is true, append a new list node to ``Rows`` and
      --    transfer elements of ``Items`` to it.
      --
      --  * Otherwise, transfer elements of ``Items`` to the last list in
      --    ``Rows``.

      procedure Create_Table
        (Tables     : in out Document_Vectors.Vector;
         Rows       : in out Document_Vectors.Vector;
         Must_Break : Boolean);
      --  If ``Rows`` is not empty, create a table document out of it and
      --  append it to ``Tables``.

      function Unparse_Node
        (N : Lk_Node; Current_Token : in out Lk_Token) return Document_Type;
      --  Using the unparsing configuration for N, unparse it to a Prettier
      --  document. Update ``Current_Token`` accordingly.

      procedure Unparse_Field
        (Node          : Lk_Node;
         Node_Config   : Node_Config_Record;
         Child         : Lk_Node;
         Field_Ref     : Struct_Member_Index;
         Unparser      : Field_Unparser_Impl;
         Items         : in out Document_Vectors.Vector;
         Current_Token : in out Lk_Token;
         Symbols       : in out Symbol_Instantiation_Context);
      --  Unparse ``Child``, which is the ``Field_Ref`` field of ``Node``. The
      --  Resulting items are appended to ``Items``. ``Node_Config`` must be
      --  the node unparsing configuration for ``Node``, and ``Unparser`` must
      --  be the unparser for this field. Update ``Current_Token`` accordingly.

      ----------------
      -- Emit_Error --
      ----------------

      procedure Emit_Error (Node : Lk_Node; Message : String) is
      begin
         if Process_Error = null then
            Put_Line
              (Simple_Name (Node.Unit.Filename)
               & ":" & Image (Start_Sloc (Node.Sloc_Range))
               & ": " & Message);
         else
            Process_Error.all (Node, Message);
         end if;
      end Emit_Error;

      -----------------
      -- Skip_Tokens --
      -----------------

      procedure Skip_Tokens
        (Current_Token : in out Lk_Token;
         Tokens        : Token_Sequence;
         Label         : String) is
      begin
         for T of Tokens.all loop
            pragma Assert (To_Index (Kind (Current_Token)) = T.Kind);
            Current_Token := Current_Token.Next (Exclude_Trivia => True);
         end loop;

         if Tokens.all'Length > 0 and then Current_Token_Trace.Is_Active then
            Current_Token_Trace.Trace
              (Tokens.all'Length'Image & " token(s) skipped (" & Label
               & "), current token: " & Image_With_Sloc (Current_Token));
         end if;
      end Skip_Tokens;

      --------------------
      -- Unparse_Tokens --
      --------------------

      procedure Unparse_Tokens
        (Current_Token : in out Lk_Token;
         Tokens        : Token_Sequence;
         Items         : in out Document_Vectors.Vector;
         Label         : String) is
      begin
         for T of Tokens.all loop
            declare
               Fragment : constant Unparsing_Fragment :=
                 Fragment_For (Config.Value.Language, T);
               pragma Assert (Fragment.Kind = Token_Fragment);
            begin
               Items.Append
                 (Pool.Create_Token
                    (Fragment.Token_Kind, Fragment.Token_Text));
               Process_Trivias
                 (Current_Token, Items, Pool, Trivias, Skip_Token => True);
            end;
         end loop;

         if Tokens.all'Length > 0 and then Current_Token_Trace.Is_Active then
            Current_Token_Trace.Trace
              (Tokens.all'Length'Image & " token(s) unparsed (" & Label
               & "), current token: " & Image_With_Sloc (Current_Token));
         end if;
      end Unparse_Tokens;

      -------------------
      -- Append_To_Row --
      -------------------

      procedure Append_To_Row
        (Rows    : in out Document_Vectors.Vector;
         Items   : in out Document_Vectors.Vector;
         New_Row : Boolean)
      is
         Row : Document_Type;
      begin
         if New_Row or else Rows.Is_Empty then
            Row := Pool.Create_Empty_List;
            Rows.Append (Row);
         else
            Row := Rows.Last_Element;
         end if;
         Row.List_Documents.Append (Items);
         Items.Clear;
      end Append_To_Row;

      ------------------
      -- Create_Table --
      ------------------

      procedure Create_Table
        (Tables     : in out Document_Vectors.Vector;
         Rows       : in out Document_Vectors.Vector;
         Must_Break : Boolean)
      is
      begin
         if Rows.Is_Empty then
            return;
         end if;

         --  Prettier expect tables to start at the same columns as the rows it
         --  contains, so flush line breaks before inserting the table, so that
         --  the table starts at the same indentation level as all of its rows.

         Tables.Append (Pool.Create_Flush_Line_Breaks);
         Tables.Append (Pool.Create_Table (Rows, Must_Break));
      end Create_Table;

      ------------------
      -- Unparse_Node --
      ------------------

      function Unparse_Node
        (N : Lk_Node; Current_Token : in out Lk_Token) return Document_Type
      is
         Symbols     : Symbol_Instantiation_Context :=
           (Map         => Symbol_Instantiation_Maps.Empty_Map,
            Next_Symbol => Next_Symbol'Unrestricted_Access);
         Node_Config : Node_Config_Record renames
           Config.Value.Node_Configs.Element (To_Index (Type_Of (N))).all;
         Items       : Document_Vectors.Vector;

         --  Table management (for list nodes with this behavior enabled)

         Is_List              : constant Boolean := N.Is_List_Node;
         With_Table           : constant Boolean :=
           Node_Config.List_Config.Table_Config.Enabled;
         Table_Sep_Before     : constant Boolean :=
           (if With_Table
            then Node_Config.List_Config.Table_Config.Sep_Before
            else False);
         Table_Must_Break     : constant Boolean :=
           (if With_Table
            then Node_Config.List_Config.Table_Config.Must_Break
            else False);
         Table_Join_Predicate : constant Struct_Member_Ref :=
           (if With_Table
            then Node_Config.List_Config.Table_Config.Join_Predicate
            else No_Struct_Member_Ref);
         Table_Join_Template  : constant Template_Type :=
           (if With_Table
            then Node_Config.List_Config.Table_Config.Join_Template
            else No_Template);
         pragma Assert (not With_Table or else Is_List);

         Tables : Document_Vectors.Vector;
         --  List of tables we are creating for this list node (thus unused
         --  unless ``With_Table`` is true).

         Table_Rows : Document_Vectors.Vector;
         --  For the current table we are creating for this list node (thus
         --  unused unless ``With_Table`` is true): vector of list documents:
         --  one for each table row.

         Last_Sep : Lk_Token := No_Lk_Token;
         --  If we just processed a list separator, reference to it (unused
         --  unless ``With_Table`` is true), ``No_Lk_Token`` if there is none.

         procedure Process_Fragment
           (F             : Unparsing_Fragment;
            Current_Token : in out Lk_Token);
         --  Append the documents to ``Items`` to represent the given unparsing
         --  fragment.

         ----------------------
         -- Process_Fragment --
         ----------------------

         procedure Process_Fragment
           (F             : Unparsing_Fragment;
            Current_Token : in out Lk_Token) is
         begin
            if Current_Token_Trace.Is_Active then
               Current_Token_Trace.Trace
                 ("About to unparse " & F.Kind'Image
                  & ", current token: " & Image_With_Sloc (Current_Token));
            end if;

            --  For list nodes, there are only two possible fragments kinds:
            --  list separators and children. Since ``With_Table`` can be true
            --  only for list nodes, this means that we can get only these two
            --  fragment kinds when generating a table, and thus that ``Items``
            --  is not used to collect fragments in that case.

            pragma Assert
              (not Is_List
               or else F.Kind in List_Separator_Fragment
                               | List_Child_Fragment);

            case Non_Trivia_Fragment_Kind (F.Kind) is
               when Token_Fragment | List_Separator_Fragment =>
                  declare
                     Is_Fake_Token : constant Boolean :=
                       (F.Kind = List_Separator_Fragment
                        and then F.Token_Kind = No_Token_Kind_Ref);

                     --  Make sure Current_Token is synchronized with the given
                     --  unparsing fragment. There is one exception: the
                     --  fragment is a fake empty token, for the case of
                     --  processing the separator of a list with no separator
                     --  token. Doing so is necessary to apply the "sep"
                     --  template of such a list node.

                     pragma Assert
                       (F.Token_Kind = Current_Token.Kind
                        or else Is_Fake_Token);

                     Token : Document_Type :=
                       (if F.Token_Kind = No_Token_Kind_Ref
                        then Pool.Create_Empty_List
                        else Pool.Create_Token (F.Token_Kind, F.Token_Text));
                  begin
                     --  If we have a list separator, instantiate the
                     --  corresponding template to wrap ``Token``.

                     if F.Kind = List_Separator_Fragment then
                        declare
                           Sep_Template : constant Template_Type :=
                             Node_Config.List_Config.Seps (F.List_Sep_Kind);

                           pragma Assert (Sep_Template.Kind = With_Recurse);

                           Sep_Items : Document_Vectors.Vector;
                           --  Token and trivia used to unparse the list
                           --  separator.

                           Next_Token : Lk_Token := Current_Token;
                           --  Value for ``Current_Token`` after the list
                           --  separator has been processed.

                           Args : Template_Instantiation_Args (With_Recurse);
                        begin
                           if not Is_Fake_Token then
                              Last_Sep := Current_Token;
                              Sep_Items.Append (Token);
                              Process_Trivias
                                (Next_Token,
                                 Sep_Items,
                                 Pool,
                                 Trivias,
                                 Skip_Token => True);
                           end if;
                           Args.With_Recurse_Doc :=
                             (Document   => Pool.Create_List (Sep_Items),
                              Node       => N,
                              Next_Token => Next_Token);
                           Token := Instantiate_Template
                             (Pool          => Pool,
                              Symbols       => Symbols,
                              Node          => N,
                              Current_Token => Current_Token,
                              Trivias       => Trivias,
                              Template      => Sep_Template,
                              Arguments     => Args);
                           pragma Assert (Current_Token = Next_Token);
                           Items.Append (Token);
                        end;

                        --  If we are generating a table for this list node,
                        --  append the list separator unparsing (in ``Items``)
                        --  to the last row, or create a new row for it
                        --  (depending on the unparsing configuration for this
                        --  list node).

                        if With_Table then
                           Append_To_Row
                             (Table_Rows,
                              Items,
                              New_Row => not Table_Sep_Before);

                        elsif Node_Config.List_Config.Flush_Before_Children
                        then
                           --  If requested, flush line breaks before unparsing
                           --  the next list child, so that line breaks do not
                           --  get inserted inside groups/indents/... that
                           --  belong to the next child.
                           --
                           --  Note that we avoid this when generating a table,
                           --  since the end of the table row may insert a new
                           --  line: the flush must come after so that this
                           --  implicit line break is accounted for during the
                           --  flush.

                           Items.Append (Pool.Create_Flush_Line_Breaks);
                        end if;

                     else
                        --  This is an actual token (non-trivia) fragment:
                        --  unparse it and process its trivias.

                        Items.Append (Token);
                        Process_Trivias
                          (Current_Token,
                           Items,
                           Pool,
                           Trivias,
                           Skip_Token => True);
                     end if;
                  end;
                  if Current_Token_Trace.Is_Active then
                     Current_Token_Trace.Trace
                       ("Token fragment unparsed, current token: "
                        & Image_With_Sloc (Current_Token));
                  end if;

               when Field_Fragment =>
                  Unparse_Field
                    (Node          => N,
                     Node_Config   => Node_Config,
                     Child         => F.Node,
                     Field_Ref     => To_Index (F.Field),
                     Unparser      => F.Field_Unparser_Ref.all,
                     Items         => Items,
                     Current_Token => Current_Token,
                     Symbols       => Symbols);
                  if Current_Token_Trace.Is_Active then
                     Current_Token_Trace.Trace
                       ("Field fragment " & Debug_Name (F.Field)
                        & " unparsed, current token: "
                        & Image_With_Sloc (Current_Token));
                  end if;

               when List_Child_Fragment =>

                  --  If the current table has a least one row and trivia that
                  --  preceed this list child match the unparsing
                  --  configuration's criteria, split tables.

                  if not Table_Rows.Is_Empty
                     and then Table_Needs_Split
                                (F.Node,
                                 Last_Sep,
                                 Node_Config.List_Config,
                                 Trivias)
                  then
                     Create_Table (Tables, Table_Rows, Table_Must_Break);
                  end if;

                  --  If reattached tokens precede this child, process them now
                  --  so that they are unparsed as part of this list node
                  --  instead of another (possibly deeply nested) list node.

                  Process_Reattached_Trivias
                    (First_Trivia_Before (F.Node),
                     N,
                     Items,
                     Pool,
                     Trivias);

                  --  If requested, flush line breaks before unparsing the list
                  --  child, so that line breaks do not get inserted inside
                  --  groups/indents/... that would belong to it.

                  if Node_Config.List_Config.Flush_Before_Children then
                     Items.Append (Pool.Create_Flush_Line_Breaks);
                  end if;

                  --  Unparse the list child itself

                  Items.Append (Unparse_Node (F.Node, Current_Token));
                  if Current_Token_Trace.Is_Active then
                     Current_Token_Trace.Trace
                       ("List child fragment unparsed, current token: "
                        & Image_With_Sloc (Current_Token));
                  end if;

                  --  Likewise, if reattached tokens come after this child,
                  --  process them now.

                  Process_Reattached_Trivias
                    (F.Node.Token_End.Next,
                     N,
                     Items,
                     Pool,
                     Trivias);

                  --  If we are generating tables for this list node, transfer
                  --  the result of the unparsing for this child node (in
                  --  ``Items``) to a dedicated row in the current table.
                  --
                  --  This must create a new row if 1) there was no row so far
                  --  for the current table or if 2) the table separator just
                  --  got inserted in the last row.

                  if With_Table then
                     Append_To_Row
                       (Table_Rows,
                        Items,
                        New_Row => Table_Sep_Before);

                     --  If row join is enabled, query the join predicate and
                     --  do the join if needed. Note that we consider that we
                     --  must not join if the predicate crashes.

                     if Table_Rows.Length >= 2
                        and then Table_Join_Predicate /= No_Struct_Member_Ref
                     then
                        declare
                           Left_Row   : Document_Type;
                           Right_Row  : Document_Type;
                           Joined_Row : Document_Type;
                           Result     : constant Value_Or_Error :=
                             Eval_Node_Member (F.Node, Table_Join_Predicate);
                           Must_Join  : Boolean;
                        begin
                           if Result.Is_Error then
                              Emit_Error
                                (Node    => F.Node,
                                 Message =>
                                   "predicate "
                                   & Debug_Name (Table_Join_Predicate)
                                   & " raised " & Exception_Name (Result.Error)
                                   & ": " & Exception_Message (Result.Error));
                              Must_Join := False;
                           else
                              Must_Join := As_Bool (Result.Value);
                           end if;

                           if Must_Join then

                              --  Pop the last two rows and instantiate the
                              --  join template to be the new replacement row.

                              Right_Row := Table_Rows.Last_Element;
                              Table_Rows.Delete_Last;
                              Left_Row := Table_Rows.Last_Element;
                              Table_Rows.Delete_Last;

                              Joined_Row :=
                                Instantiate_Template
                                  (Pool          => Pool,
                                   Symbols       => Symbols,
                                   Node          => N,
                                   Current_Token => Current_Token,
                                   Trivias       => Trivias,
                                   Template      => Table_Join_Template,
                                   Arguments     =>
                                     (Kind       => Join_Template,
                                      Join_Left  => Left_Row,
                                      Join_Right => Right_Row));
                              Table_Rows.Append (Joined_Row);
                           end if;
                        end;
                     end if;
                  end if;
                  Last_Sep := No_Lk_Token;
            end case;
         end Process_Fragment;

         Template : Template_Type renames Node_Config.Node_Template;
         Result   : Document_Type;
      begin
         if Current_Token_Trace.Is_Active then
            Current_Token_Trace.Increase_Indent
              ("Unparsing " & N.Image
               & ", current token: " & Image_With_Sloc (Current_Token));
         end if;

         case Some_Template_Kind (Template.Kind) is
            when Join_Template =>

               --  We are supposed to create these templates for table join
               --  separators only, never for nodes.

               raise Program_Error;

            when With_Recurse =>

               --  First gather documents for all the fragments in this node,
               --  then group them in a list document, and use that list to
               --  instantiate the template for the node itself.

               Iterate_On_Fragments
                 (N, Current_Token, Process_Fragment'Access);

               --  If we unparsed a list document for which we must create
               --  tables, finalize the last table (if not empty), then create
               --  the list of tables.

               if With_Table then
                  pragma Assert (Items.Is_Empty);
                  Create_Table (Tables, Table_Rows, Table_Must_Break);
                  Items.Append (Pool.Create_List (Tables));
               end if;

               Result := Instantiate_Template
                 (Pool          => Pool,
                  Symbols       => Symbols,
                  Node          => N,
                  Trivias       => Trivias,
                  Current_Token => Current_Token,
                  Template      => Template,
                  Arguments     =>
                    (Kind             => With_Recurse,
                     With_Recurse_Doc =>
                       (Document   => Pool.Create_List (Items),
                        Node       => N,
                        Next_Token => Current_Token)));

            when With_Recurse_Field =>
               if Current_Token_Trace.Is_Active then
                  Current_Token_Trace.Increase_Indent
                    ("About to process a ""recurse_field"" template");
               end if;

               --  Compute sub-documents for all fields (do not forget the
               --  field's own pre/post tokens) and let the template do its
               --  magic.

               declare
                  Id            : constant Language_Id := N.Language;
                  Desc          : constant Language_Descriptor_Access := +Id;
                  Unparsers     : Unparsers_Impl renames Desc.Unparsers.all;
                  Node_Type     : constant Type_Ref := Type_Of (N);
                  Node_Unparser : Node_Unparser_Impl renames
                    Unparsers.Node_Unparsers (To_Index (Node_Type)).all;

                  Arguments : Template_Instantiation_Args (With_Recurse_Field);

                  Field_Token : Lk_Token := Current_Token;
                  --  Copy of Current_Token, maintained just for the
                  --  instantiation of template arguments (one for each field):
                  --  we get back to Current_Token to instantiate the node-wide
                  --  template.

               begin
                  --  Prepare arguments to instantiate ``Template``. Use
                  --  ``Field_Token`` to achieve this.

                  Skip_Tokens
                    (Field_Token, Node_Unparser.Pre_Tokens, "pre tokens");

                  for I in 1 .. N.Children_Count loop
                     declare
                        Child          : constant Lk_Node := N.Child (I);
                        Field_Unparser : Field_Unparser_Impl renames
                          Node_Unparser.Field_Unparsers.Field_Unparsers (I);
                        Child_Doc      : Document_Type;
                     begin
                        Skip_Tokens
                          (Field_Token,
                           Node_Unparser.Field_Unparsers.Inter_Tokens (I),
                           "inter tokens");

                        if Current_Token_Trace.Is_Active then
                           Current_Token_Trace.Increase_Indent
                             ("Processing field " & Child.Image
                              & ", field token: "
                              & Image_With_Sloc (Field_Token));
                        end if;

                        if Is_Field_Present (Child, Field_Unparser) then
                           Items.Clear;
                           Unparse_Field
                             (Node          => N,
                              Node_Config   => Node_Config,
                              Child         => Child,
                              Field_Ref     => Field_Unparser.Member,
                              Unparser      => Field_Unparser,
                              Items         => Items,
                              Current_Token => Field_Token,
                              Symbols       => Symbols);
                           Child_Doc := Pool.Create_List (Items);
                        else
                           Child_Doc := Pool.Create_Empty_List;
                        end if;
                        Arguments.Field_Docs.Append
                          (Single_Template_Instantiation_Argument'
                             (Document   => Child_Doc,
                              Node       => Child,
                              Next_Token => Field_Token));

                        if Current_Token_Trace.Is_Active then
                           Current_Token_Trace.Decrease_Indent
                             ("Done processing field " & Child.Image
                              & ", field token: "
                              & Image_With_Sloc (Field_Token));
                        end if;
                     end;
                  end loop;

                  Skip_Tokens
                    (Field_Token, Node_Unparser.Post_Tokens, "post tokens");

                  if Current_Token_Trace.Is_Active then
                     Current_Token_Trace.Trace
                       ("Template arguments for fields are ready, now"
                        & " instantiating the main template");
                  end if;

                  --  Now run the template instantiation, getting back to
                  --  ``Current_Token``. In the end, ``Current_Token`` and
                  --  ``Field_Token`` should be in sync.

                  Result := Instantiate_Template
                    (Pool          => Pool,
                     Symbols       => Symbols,
                     Node          => N,
                     Current_Token => Current_Token,
                     Trivias       => Trivias,
                     Template      => Template,
                     Arguments     => Arguments);
                  pragma Assert (Current_Token = Field_Token);

                  if Current_Token_Trace.Is_Active then
                     Current_Token_Trace.Decrease_Indent;
                  end if;
               end;

            when With_Text_Recurse =>

               --  We are supposed to create these templates only for fields,
               --  never for nodes.

               raise Program_Error;
         end case;

         --  If reattached trivias precede or come after this node, this is our
         --  last chance to process them and integrate them to the returned
         --  document.

         declare
            Items  : Document_Vectors.Vector;
            T      : Lk_Token;
         begin
            Process_Reattached_Trivias
              (First_Trivia_Before (N), N, Items, Pool, Trivias);

            Items.Append (Result);

            --  If this is a ghost node, leading and trailing trivias are the
            --  same. In addition, ``N.Token_End`` points at the first token
            --  that appears logically after the ghost node, so what comes next
            --  has nothing to do with the ghost node.

            if not N.Is_Ghost then
               T := N.Token_End.Next;
               if T.Is_Trivia then
                  Process_Reattached_Trivias (T, N, Items, Pool, Trivias);
               end if;
            end if;

            Result := Pool.Create_List (Items);
         end;

         if Current_Token_Trace.Is_Active then
            Current_Token_Trace.Decrease_Indent
              ("Done with " & N.Image
               & ", current token: " & Image_With_Sloc (Current_Token));
         end if;

         return Result;
      end Unparse_Node;

      -------------------
      -- Unparse_Field --
      -------------------

      procedure Unparse_Field
        (Node          : Lk_Node;
         Node_Config   : Node_Config_Record;
         Child         : Lk_Node;
         Field_Ref     : Struct_Member_Index;
         Unparser      : Field_Unparser_Impl;
         Items         : in out Document_Vectors.Vector;
         Current_Token : in out Lk_Token;
         Symbols       : in out Symbol_Instantiation_Context)
      is
         Field_Template : constant Template_Type :=
           Node_Config.Field_Configs.Element (Field_Ref);
         pragma Assert
           (Field_Template.Kind in With_Recurse | With_Text_Recurse);

         Handle_Tokens : constant Boolean :=
           Field_Template.Kind = With_Recurse;
         --  Whether we have to take care of pre/post tokens for this field.
         --  For With_Text_Recurse templates, it is Field_Template that takes
         --  care of them.

         Field_Template_Args : Template_Instantiation_Args
                                 (Field_Template.Kind);

         Next_Token : Lk_Token;
         --  Token that follows ``Child``, i.e. token to assign to
         --  ``Current_Token`` after the child has been processed.
         --
         --  Before we compute the template argument, it must be set just
         --  passed the pre-tokens.
      begin
         if Handle_Tokens then

            --  ``Field_Template`` does *not* take care of pre/post tokens for
            --  this field: do it now.

            Unparse_Tokens
              (Current_Token, Unparser.Pre_Tokens, Items, "pre tokens");
            Next_Token := Current_Token;

         else
            --  ``Field_Template`` does not take care of pre/post tokens for
            --  this field: just make sure ``Next_Token`` goes past pre-tokens.

            Next_Token := Current_Token;
            Skip_Tokens (Next_Token, Unparser.Pre_Tokens, "pre tokens");
         end if;

         declare
            Field_Doc : constant Document_Type :=
              Unparse_Node (Child, Next_Token);
         begin
            Field_Template_Args.With_Recurse_Doc :=
              (Document   => Field_Doc,
               Node       => Child,
               Next_Token => Next_Token);
         end;

         --  Now that the argument for this field template is ready,
         --  instantiate the template.

         Items.Append
           (Instantiate_Template
              (Pool          => Pool,
               Symbols       => Symbols,
               Node          => Node,
               Current_Token => Current_Token,
               Trivias       => Trivias,
               Template      => Field_Template,
               Arguments     => Field_Template_Args));

         if Handle_Tokens then
            Unparse_Tokens
              (Current_Token, Unparser.Post_Tokens, Items, "post tokens");
         end if;
      end Unparse_Field;

   begin
      if Config.Value = null then
         raise Precondition_Failure with "null unparsing configuration";
      elsif Config.Value.Language /= Node.Language then
         raise Precondition_Failure with "inconsistent languages";
      end if;

      --  Before running the unparser itself, determine the set of reattached
      --  trivias.

      Compute_Trivias_Info (Node, Trivias);

      declare
         Items : Document_Vectors.Vector;

         --  If ``Node`` is the root of its analysis unit, also unparse trivias
         --  that come before the root node.

         Current_Token   : Lk_Token := Node.Unit.First_Token;
         Internal_Result : Document_Type;
      begin
         if Current_Token.Is_Trivia
            and then not Node.Is_List_Node
            and then Node = Node.Unit.Root
         then
            Process_Trivias
              (Current_Token,
               Items,
               Pool,
               Trivias,
               Skip_Token => False);
         else
            Current_Token := Node.Token_Start;
         end if;

         --  Now run the unparser on the given node

         Items.Append (Unparse_Node (Node, Current_Token));
         Internal_Result := Pool.Create_List (Items);
         Dump (Internal_Result, Expanded_Trace);

         Bubble_Up_Trivias (Pool, Internal_Result);
         Dump (Internal_Result, Bubble_Up_Trace);

         Insert_Required_Spacing
           (Pool, Internal_Result, Config.Value.Max_Empty_Lines);
         Dump (Internal_Result, Final_Trace);

         Free (Trivias);

         --  Produce the Prettier document from our internal document tree

         return Result : constant Prettier.Document_Type :=
           To_Prettier_Document (Internal_Result)
         do
            Pool.Release;
         end return;
      end;
   end Unparse_To_Prettier;

   -----------------------
   -- Pretty_Print_Main --
   -----------------------

   procedure Pretty_Print_Main (Language : Language_Id) is
      use Ada.Command_Line;
      use GNATCOLL.Opt_Parse;

      function Convert (Arg : String) return Grammar_Rule_Ref;
      --  Convert a rule name to the corresponding grammar rule for
      --  ``Language``. Raise a ``Constraint_Error`` if ``Arg`` is not a valid
      --  rule name.

      type Sloc_Specifier is record
         Sloc         : Source_Location;
         Parent_Level : Natural;
      end record;

      No_Sloc_Specifier     : constant Sloc_Specifier :=
        (No_Source_Location, 0);
      Sloc_Specifier_Regexp : constant GNAT.Regpat.Pattern_Matcher :=
        GNAT.Regpat.Compile
          ("^([0-9]+)"
           & ":([0-9]+)"
           & "(\^([0-9]+))?$");

      function Convert (Arg : String) return Sloc_Specifier;

      -------------
      -- Convert --
      -------------

      function Convert (Arg : String) return Grammar_Rule_Ref is
         A : constant Text_Type := To_Text (Arg);
         R : Grammar_Rule_Ref;
      begin
         for I in Grammar_Rule_Index'First .. Last_Grammar_Rule (Language) loop
            R := From_Index (Language, I);
            if A = Format_Name (Grammar_Rule_Name (R), Lower) then
               return R;
            end if;
         end loop;
         raise Opt_Parse_Error with "invalid grammar rule name";
      end Convert;

      -------------
      -- Convert --
      -------------

      function Convert (Arg : String) return Sloc_Specifier is
         use GNAT.Regpat;
         Matches : Match_Array (0 .. 4);
      begin
         Match (Sloc_Specifier_Regexp, Arg, Matches);
         if Matches (0) = No_Match then
            raise Opt_Parse_Error with "invalid sloc specifier";
         end if;

         return
           (Sloc         => Value
                              (Arg (Matches (1).First .. Matches (2).Last)),
            Parent_Level => (if Matches (4) = No_Match
                             then 0
                             else Natural'Value
                                    (Arg (Matches (4).First
                                          ..  Matches (4).Last))));
      end Convert;

      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Pretty-print a source file");

      package Autochecks is new Parse_Flag
        (Parser      => Parser,
         Short       => "-A",
         Long        => "--auto-checks",
         Help        =>
           "Perform various checks on the reformatted sources: they must"
           & " contain the same sequence of tokens as the original source, "
           & " and re-running reformatting should be a no-op.");

      package Output_Filename is new Parse_Option
        (Parser      => Parser,
         Short       => "-o",
         Long        => "--output",
         Arg_Type    => Unbounded_String,
         Help        => "File to write",
         Default_Val => Null_Unbounded_String);

      package Width is new Parse_Option
        (Parser      => Parser,
         Short       => "-w",
         Long        => "--width",
         Arg_Type    => Natural,
         Help        => "Line width",
         Default_Val => 79);

      package Indentation_Kind is new Parse_Enum_Option
        (Parser      => Parser,
         Short       => "-k",
         Long        => "--indentation-kind",
         Arg_Type    => Prettier.Indentation_Kind,
         Help        => "Indentation kind: spaces or tabs",
         Default_Val => Prettier.Spaces);

      package Indentation_Width is new Parse_Option
        (Parser      => Parser,
         Short       => "-i",
         Long        => "--indentation-width",
         Arg_Type    => Natural,
         Help        => "Indentation width",
         Default_Val => 4);

      package Indentation_Continuation is new Parse_Option
        (Parser      => Parser,
         Short       => "-I",
         Long        => "--indentation-continuation",
         Arg_Type    => Natural,
         Help        => "Continuation line indentation",
         Default_Val => 2);

      package End_Of_Line is new Parse_Enum_Option
        (Parser      => Parser,
         Short       => "-e",
         Long        => "--end-of-line",
         Arg_Type    => Prettier.End_Of_Line_Kind,
         Help        => "End of line: LF, CR, CRLF",
         Default_Val => Prettier.LF);

      package Rule is new Parse_Option
        (Parser      => Parser,
         Short       => "-r",
         Long        => "--rule-name",
         Arg_Type    => Grammar_Rule_Ref,
         Help        =>
           "Grammar rule name to parse the source file to pretty-print",
         Default_Val => Default_Grammar_Rule (Language));

      package Traces is new Parse_Option_List
        (Parser      => Parser,
         Short       => "-t",
         Long        => "--trace",
         Arg_Type    => Unbounded_String,
         Accumulate  => True,
         Help        =>
           "LANGKIT.UNPARSING.*. sub-trace name to activate");

      package Sloc is new Parse_Option
        (Parser      => Parser,
         Short       => "-s",
         Long        => "--sloc",
         Arg_Type    => Sloc_Specifier,
         Help        => "Location of the node to unparse. Format: L:C (L ="
                        & " line number, C = column number) or L:C^N (N = "
                        & " number of times we get the parent of the node"
                        & " looked up by sloc).",
         Default_Val => No_Sloc_Specifier);

      package Config_Filename is new Parse_Option
        (Parser      => Parser,
         Short       => "-c",
         Long        => "--config",
         Arg_Type    => Unbounded_String,
         Help        => "Name of the JSON pretty-printer configuration file",
         Default_Val => Null_Unbounded_String);

      package Check_All_Nodes is new Parse_Flag
        (Parser      => Parser,
         Short       => "-C",
         Long        => "--check-all-nodes",
         Help        => "Treat a missing node configuration as an error.");

      package Source_Filename is new Parse_Positional_Arg
        (Parser   => Parser,
         Name     => "src-file",
         Help     => "Name of the source file to pretty-print",
         Arg_Type => Unbounded_String);

      package Dump_Document is new Parse_Flag
        (Parser      => Parser,
         Short       => "-d",
         Long        => "--dump-document",
         Help        => "Dump the Prettier document in ""doc.json""");

      Config  : Unparsing_Configuration;
      Context : Lk_Context;
      Unit    : Lk_Unit;
      Node    : Lk_Node;
   begin
      GNATCOLL.Traces.Parse_Config_File;
      if not Parser.Parse then
         return;
      end if;

      --  Enable all requested traces

      for N of Traces.Get loop
         Traces_To_Enable.Include
           (To_Unbounded_String ("LANGKIT.UNPARSING.")
            & To_Unbounded_String
                (Ada.Characters.Handling.To_Upper (To_String (N))));
      end loop;
      GNATCOLL.Traces.For_Each_Handle (Process_Enable_Traces'Access);

      --  Parse the configuration file if provided (and abort if there is an
      --  error), use the default unparsing configuration otherwise.

      declare
         Diagnostics : Diagnostics_Vectors.Vector;
         Filename    : constant String := To_String (Config_Filename.Get);
      begin
         if Filename = "" then
            Config := Default_Unparsing_Configuration (Language);
         else
            Config := Load_Unparsing_Config
              (Language, Filename, Diagnostics, Check_All_Nodes.Get);
            if Config = No_Unparsing_Configuration then
               Put_Line ("Error when loading the unparsing configuration:");
               Print (Diagnostics);
               Set_Exit_Status (Failure);
               return;
            end if;
         end if;
      end;

      --  Parse the source file to pretty-print. Abort if there is a parsing
      --  failure.

      Context := Create_Context (Language);
      Unit := Context.Get_From_File
        (Filename => To_String (Source_Filename.Get),
         Rule     => Rule.Get);
      if Unit.Has_Diagnostics then
         Put_Line ("Cannot parse source file: aborting...");
         for D of Unit.Diagnostics loop
            Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;
         Set_Exit_Status (Failure);
         return;
      end if;

      --  Look for the node to unparse

      Node := Unit.Root;
      declare
         SS : constant Sloc_Specifier := Sloc.Get;
      begin
         if SS /= No_Sloc_Specifier then
            if Autochecks.Get then
               Put_Line ("-A/--auto-checks cannot be used with -s/--sloc");
               Set_Exit_Status (Failure);
               return;
            end if;

            Node := Node.Lookup (SS.Sloc);
            for I in 1 .. SS.Parent_Level loop
               exit when Node.Is_Null;
               Node := Node.Parent;
            end loop;

            if Node.Is_Null then
               Put_Line ("No node found at the given location");
               Set_Exit_Status (Failure);
               return;
            end if;
         end if;
      end;

      --  Unparse the tree to a Prettier document

      declare
         Options : constant Prettier.Format_Options_Type :=
           (Width       => Width.Get,
            Indentation =>
              (Kind         => Indentation_Kind.Get,
               Width        => Indentation_Width.Get,
               Continuation => Indentation_Continuation.Get,
               Offset       => (Tabs => 0, Spaces => 0)),
            End_Of_Line => End_Of_Line.Get);

         F         : File_Type;
         Doc       : constant Prettier.Document_Type :=
           Unparse_To_Prettier (Node, Config);
         Formatted : constant Unbounded_String :=
           Prettier.Format (Doc, Options);
      begin
         --  If requested, dump it as a JSON file

         if Dump_Document.Get then
            Create (F, Name => "doc.json");
            Put_Line (F, Prettier.Json.Serialize (Doc));
            Close (F);
         end if;

         --  If requested, perform auto-check on the reformatted sources

         if Autochecks.Get then

            --  First check that the reformatted source parses correctly and
            --  yields the same sequence of tokens + comments as the original
            --  unit.

            Context := Create_Context (Language);
            declare
               use Ada.Strings.Unbounded.Aux;

               Buffer      : Big_String_Access;
               Buffer_Last : Natural;
               U           : Lk_Unit;
            begin
               --  To avoid overflowing the secondary stack with big sources,
               --  use the internal
               --  Ada.Strings.Wide_Wide_Unbounded.Aux.Get_String API to access
               --  the reformatted source string.

               Get_String (Formatted, Buffer, Buffer_Last);
               U := Context.Get_From_Buffer
                 (Filename => To_String (Source_Filename.Get),
                  Buffer   => Buffer.all (1 .. Buffer_Last),
                  Rule     => Rule.Get);
               if U.Has_Diagnostics then
                  Put_Line ("Reformatted source has parsing errors:");
                  for D of U.Diagnostics loop
                     Put_Line (U.Format_GNU_Diagnostic (D));
                  end loop;
                  Set_Exit_Status (Failure);
                  return;
               end if;
               Check_Same_Tokens (Unit, U);

               --  Run the formatter a second time and check that the output is
               --  stable.

               declare
                  D : constant Prettier.Document_Type :=
                    Unparse_To_Prettier (U.Root, Config);
                  F : constant Unbounded_String :=
                    Prettier.Format (D, Options);
               begin
                  if F /= Formatted then
                     Put_Line ("Reformatting is not stable");
                     Set_Exit_Status (Failure);
                     return;
                  end if;
               end;
            end;
         end if;

         --  Finally, write the formatted source code on the standard output

         if Length (Output_Filename.Get) > 0 then
            Create (F, Name => To_String (Output_Filename.Get));
            Put_Line (F, Formatted);
            Close (F);
         else
            Put_Line (Formatted);
         end if;
      end;
   end Pretty_Print_Main;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Unparsing_Configuration) is
   begin
      if Self.Value /= null then
         Self.Value.Ref_Count := Self.Value.Ref_Count + 1;
      end if;
   end Adjust;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Unparsing_Configuration_Access) is
   begin
      for Cur in Self.Node_Configs.Iterate loop
         declare
            Node_Config : Node_Config_Access renames
              Self.Node_Configs.Reference (Cur);
         begin
            Free (Node_Config);
         end;
      end loop;
      Self.Pool.Release;
      if Self.Symbols /= null then
         Destroy (Self.Symbols);
      end if;
      Free (Self);
   end Release;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Unparsing_Configuration) is
   begin
      if Self.Value /= null then
         Self.Value.Ref_Count := Self.Value.Ref_Count - 1;
         if Self.Value.Ref_Count = 0 then
            Release (Self.Value);
         end if;
      end if;
   end Finalize;

end Langkit_Support.Generic_API.Unparsing;
