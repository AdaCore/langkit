--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Unchecked_Deallocation;

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

   function Load_Unparsing_Config_From_Buffer
     (Language    : Language_Id;
      Buffer      : String;
      Diagnostics : in out Diagnostics_Vectors.Vector)
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
      Empty_Lines);
   subtype Non_Trivia_Fragment_Kind is
     Unparsing_Fragment_Kind range Token_Fragment .. List_Child_Fragment;
   subtype Trivia_Fragment_Kind is
     Unparsing_Fragment_Kind range Suffix_Comment .. Empty_Lines;
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

         when Empty_Lines =>

            --  Fragment to record the presence of empty lines in the original
            --  source code.

            Empty_Lines_Count : Positive;
            --  Number of empty lines found
      end case;
   end record;
   --  Source code unparsing fragment used to unparse source code. Fragments
   --  are either tokens or nodes (that must be decomposed themselves into
   --  fragments).

   package Token_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Lk_Token,
      Hash                => Hash,
      Equivalent_Elements => "=");

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
      First_Reattached_Trivias : Token_Sets.Set;
      --  Set of first trivia for all sequences of reattached trivias

      Processed_Reattached_Trivias : Token_Sets.Set;
      --  Subset of ``First_Reattached_Trivias`` for the trivias that were
      --  already processed (included in the unparsing).

      Fragments : Trivias_Fragments;
      --  Unparsing fragments for all trivias
   end record;

   procedure Free (Self : in out Trivias_Info);
   --  Release resources allocated for ``Self``

   function Fragment_For
     (Id          : Language_Id;
      Token       : Token_Unparser;
      Is_List_Sep : Boolean := False) return Unparsing_Fragment;
   --  Return the unparsing fragment corresponding to the given token.
   --
   --  ``Is_List_Sep`` designates whether this token is used as a list
   --  separator.

   procedure Compute_Trivia_Fragments
     (Node : Lk_Node; Fragments : out Trivias_Fragments);
   --  Compute unparsing fragments for all trivias in ``Node`` and store them
   --  in ``Fragments``.

   procedure Compute_Trivias_Info (Node : Lk_Node; Info : out Trivias_Info);
   --  Preparatory pass for the unparsing of trivias (empty lines, comments):
   --  compute information about trivias found in the ``Node`` subtree and fill
   --  ``Info``'s data structures accordingly.

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
      Items        : in out Document_Vectors.Vector;
      Pool         : in out Document_Pool;
      Trivias      : in out Trivias_Info);
   --  If unprocessed reattached trivias precede ``Node``, process them and add
   --  the corresponding internal document nodes to ``Items``.

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

   -----------------------
   --  Template symbols --
   -----------------------

   --  The following map type is used during templates parsing to validate the
   --  names used as symbols in JSON templates, and to turn them into their
   --  internal representation: ``Template_Symbol``.

   type Symbol_Info is record
      Source_Name : Unbounded_String;
      --  Name for this symbol as found in the unparsing configuration

      Template_Sym : Template_Symbol;
      --  Unique identifier for this symbol

      Has_Definition : Boolean;
      --  Whether we have found one definition for this symbol
   end record;

   package Symbol_Parsing_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Symbol_Info,
      Hash            => Hash,
      Equivalent_Keys => "=");

   function Lookup
     (Source_Name : Unbounded_String;
      Symbols     : Symbol_Table;
      Symbol_Map  : in out Symbol_Parsing_Maps.Map)
      return Symbol_Parsing_Maps.Reference_Type;
   --  Return a reference to the entry in ``Symbol_Map`` corresponding to the
   --  ```Source_Name`` symbol (converted to a ``Symbol_Type`` using
   --  ``Symbols``). Create a map entry if it does not exist yet.

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
   --  They are used to process templates that embed text.

   type Linear_Template_Item_Kind is (Token_Item, Field_Item, Recurse_Item);
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

         when Recurse_Item =>
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

   function Hash (Self : Struct_Member_Index) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type'Mod (Self));

   function Hash (Self : Type_Index) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type'Mod (Self));

   package Field_Config_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Struct_Member_Index,
      Element_Type    => Template_Type,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Node_Config_Record is limited record
      Node_Template : Template_Type;
      --  Template to decorate the unparsing of the whole node

      Field_Configs : Field_Config_Maps.Map;
      --  For each non-null syntax field in this node, template to decorate the
      --  unparsing of the field.

      List_Sep : Template_Type;
      --  For list nodes only: template to decorate the unparsing of the list
      --  separator.
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
      end case;
   end record;

   function Instantiate_Template
     (Pool          : in out Document_Pool;
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
     (Id          : Language_Id;
      Token       : Token_Unparser;
      Is_List_Sep : Boolean := False) return Unparsing_Fragment
   is
      Kind : constant Token_Kind_Ref := From_Index (Id, Token.Kind);
      Text : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Token.Text.all);
   begin
      return
        (if Is_List_Sep
         then (List_Separator_Fragment, Kind, Text)
         else (Token_Fragment, Kind, Text));
   end Fragment_For;

   ------------------------------
   -- Compute_Trivia_Fragments --
   ------------------------------

   procedure Compute_Trivia_Fragments
     (Node : Lk_Node; Fragments : out Trivias_Fragments)
   is
      Trace : constant Boolean := Trivias_Trace.Is_Active;

      T, Exit_Token : Lk_Token;
      --  Token/trivia cursor, and token to stop trivia fragments computation

      First_Trivia : Token_Index := No_Token_Index;
      --  When we are processing a sequence of trivia, this contains the index
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
               Trivias_Trace.Trace ("Found " & T.Image);
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

                  Line_Breaks : constant Natural := Count_Line_Breaks (T);
               begin
                  if Trace then
                     Trivias_Trace.Trace
                       ("   ... it has" & Line_Breaks'Image & " line breaks");
                  end if;

                  --  If we are still processing the first line in this
                  --  sequence of trivias, compute the number of spaces that
                  --  come before the potential suffix comment that comes next.

                  if First_Line and then Line_Breaks = 0 then
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
                  --  empty lines to account for.

                  if Line_Breaks >= 2 then
                     Append
                       ((Kind              => Empty_Lines,
                         Empty_Lines_Count => Line_Breaks - 1));
                  end if;

                  --  If this trivia contained at least one line break, the
                  --  next trivia to process will not start on the first line.

                  if Line_Breaks > 0 then
                     First_Line := False;
                  end if;
               end;

            elsif First_Line then
               if Trace then
                  Trivias_Trace.Trace ("   ... it is a suffix comment");
               end if;

               --  Propagate spaces that appear before this suffix comment, if
               --  any.

               if Columns > 0 then
                  Append ((Whitespaces, Whitespaces_Count => Columns));
               end if;

               Append ((Suffix_Comment, T));

            else
               if Trace then
                  Trivias_Trace.Trace ("   ... it is a line comment");
               end if;

               Append ((Line_Comment, T));
            end if;

         else
            --  ``T`` is not a trivia: update our state to represent that we
            --  are not in a sequence of trivias.

            First_Trivia := No_Token_Index;
         end if;
         T := T.Next;
      end loop;
   end Compute_Trivia_Fragments;

   --------------------------
   -- Compute_Trivias_Info --
   --------------------------

   procedure Compute_Trivias_Info (Node : Lk_Node; Info : out Trivias_Info)
   is
      Trace : constant Boolean := Trivias_Trace.Is_Active;

      function Process (Node : Lk_Node) return Visit_Status;
      --  Callback for ``Langkit_Support.Analysis.Traverse``

      procedure Reattach (T : Lk_Token; What : String);
      --  Include ``T`` to reattached trivias. ``What`` is used to qualify this
      --  trivia in debug logs.

      -------------
      -- Process --
      -------------

      function Process (Node : Lk_Node) return Visit_Status is
      begin
         --  Register reattached trivias that come before/after list nodes

         if Node.Is_List_Node then
            declare
               First_Trivia : constant Lk_Token := First_Trivia_Before (Node);
            begin
               if not First_Trivia.Is_Null then
                  Reattach (First_Trivia, "leading trivias before list node");
               end if;
            end;

            if Node.Children_Count > 0 then
               declare
                  T : constant Lk_Token := Node.Token_End.Next;
               begin
                  if T.Is_Trivia then
                     Reattach (T, "trailing trivias after list node");
                  end if;
               end;
            end if;
         end if;

         --  Also register reattached trivias that come after list children:
         --  they must be processed during the unparsing of the parent list.

         if not Node.Parent.Is_Null and then Node.Parent.Is_List_Node then
            declare
               T : constant Lk_Token := Node.Token_End.Next;
            begin
               if T.Is_Trivia then
                  Reattach (T, "trailing trivias after list child");
               end if;
            end;
         end if;

         return Into;
      end Process;

      --------------
      -- Reattach --
      --------------

      procedure Reattach (T : Lk_Token; What : String) is
      begin
         Info.First_Reattached_Trivias.Include (T);
         if Trace then
            Trivias_Trace.Trace ("Reattaching " & What & ":" & T.Image);
         end if;
      end Reattach;
   begin
      --  Determine which tokens have reattached trivias

      Info.First_Reattached_Trivias.Clear;
      Node.Traverse (Process'Access);

      --  Scan all tokens and create the corresponding trivias

      Compute_Trivia_Fragments (Node, Info.Fragments);
   end Compute_Trivias_Info;

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
         --  If ``Fragment`` is a line commend and there was no empty line nor
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
         Trivias_Trace.Trace
           ("Processing trivias after " & Token.Image
            & " (" & (if Skip_Token then "excluded" else "included") & ")");
      end if;

      if Skip_Token then
         Token := Token.Next;
      end if;

      --  Skip reattached tokens if asked to

      if Skip_If_Reattached
         and then Trivias.First_Reattached_Trivias.Contains (Token)
      then
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
                 (Kind (Fragment.Comment_Token),
                  To_Unbounded_Text (Analysis.Text (Fragment.Comment_Token))));

         when Whitespaces =>
            Items.Append
              (Pool.Create_Expected_Whitespaces (Fragment.Whitespaces_Count));

         when Empty_Lines =>

            --  It takes N+1 line consecutive breaks to make N empty lines

            Items.Append
              (Pool.Create_Expected_Line_Breaks
                 (Fragment.Empty_Lines_Count + 1));
      end case;
   end Append_Trivia_Fragment;

   --------------------------------
   -- Process_Reattached_Trivias --
   --------------------------------

   procedure Process_Reattached_Trivias
     (First_Trivia : Lk_Token;
      Items        : in out Document_Vectors.Vector;
      Pool         : in out Document_Pool;
      Trivias      : in out Trivias_Info)
   is
      Current_Token : Lk_Token := First_Trivia;
   begin
      if not First_Trivia.Is_Null
         and then Trivias.First_Reattached_Trivias.Contains (First_Trivia)
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
            for I in 1 .. Node.Children_Count loop
               if I > 1 then
                  if Node_Unparser.Separator = null then
                     Process.all
                       (Fragment      =>
                          (Kind       => List_Separator_Fragment,
                           Token_Kind => No_Token_Kind_Ref,
                           Token_Text => To_Unbounded_Text ("")),
                        Current_Token => Current_Token);
                  else
                     Process.all
                       (Fragment      =>
                          Fragment_For
                            (Id, Node_Unparser.Separator, Is_List_Sep => True),
                        Current_Token => Current_Token);
                  end if;
               end if;

               Process.all
                 (Fragment      =>
                    (Kind        => List_Child_Fragment,
                     Node        => Node.Child (I),
                     Child_Index => I),
                  Current_Token => Current_Token);
            end loop;

         when Token =>
            Process.all
              (Fragment      =>
                 (Kind       => Token_Fragment,
                  Token_Kind => Token_Node_Kind (Node_Type),
                  Token_Text => To_Unbounded_Text (Node.Text)),
               Current_Token => Current_Token);
      end case;
   end Iterate_On_Fragments;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Source_Name : Unbounded_String;
      Symbols     : Symbol_Table;
      Symbol_Map  : in out Symbol_Parsing_Maps.Map)
      return Symbol_Parsing_Maps.Reference_Type
   is
      Symbol   : constant Symbol_Type :=
        Find (Symbols, To_Text (To_String (Source_Name)));
      Position : Symbol_Parsing_Maps.Cursor := Symbol_Map.Find (Symbol);
      Inserted : Boolean;
   begin
      if not Symbol_Parsing_Maps.Has_Element (Position) then

         --  This is the first time we see this symbol in the current template:
         --  create a new internal symbol for it. All internal symbols are
         --  tracked as entries in ``Symbol_Map``, so we can use its length to
         --  compute internal symbols that are unique for the current template.

         declare
            Info : constant Symbol_Info :=
              (Source_Name    => Source_Name,
               Template_Sym   => Template_Symbol (Symbol_Map.Length + 1),
               Has_Definition => False);
         begin
            Symbol_Map.Insert (Symbol, Info, Position, Inserted);
            pragma Assert (Inserted);
         end;
      end if;

      return Symbol_Map.Reference (Position);
   end Lookup;

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
         when Recurse_Item =>
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

   -------------------------------------
   -- Default_Unparsing_Configuration --
   -------------------------------------

   function Default_Unparsing_Configuration
     (Language : Language_Id) return Unparsing_Configuration
   is
      Diagnostics : Diagnostics_Vectors.Vector;
      Result      : constant Unparsing_Configuration :=
        Load_Unparsing_Config_From_Buffer
          (Language, Language.Unparsers.Default_Config.all, Diagnostics);
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
     (Language    : Language_Id;
      Filename    : String;
      Diagnostics : in out Diagnostics_Vectors.Vector)
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
          (Language, JSON_Text.all, Diagnostics)
      do
         GNAT.Strings.Free (JSON_Text);
      end return;
   end Load_Unparsing_Config;

   ---------------------------------------
   -- Load_Unparsing_Config_From_Buffer --
   ---------------------------------------

   function Load_Unparsing_Config_From_Buffer
     (Language    : Language_Id;
      Buffer      : String;
      Diagnostics : in out Diagnostics_Vectors.Vector)
      return Unparsing_Configuration
   is
      --  Create a map so that we can lookup nodes/fields by name

      Symbols : Symbol_Table := Create_Symbol_Table;
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

      function Node_Type_Image (Node : Type_Ref) return String
      is (Image (Format_Name (Node_Type_Name (Node), Camel)));
      --  Return the expected name for the given Node

      function Field_Image
        (Member : Struct_Member_Ref; Node : Type_Ref) return String
      is (Node_Type_Image (Node)
          & "." & Image (Format_Name (Member_Name (Member), Lower)));
      --  Return the expected name for the given Member in the given Node

      type Template_Parsing_State_Kind is
        (Simple_Recurse, Recurse_Field, Recurse_In_Field);
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

      type Template_Parsing_State
        (Kind : Template_Parsing_State_Kind := Simple_Recurse)
      is record
         case Kind is
            when Simple_Recurse =>
               Recurse_Found : Boolean;
               --  Whether template parsing has found the "recurse" node
               --  expected for the current branch.

            when Recurse_Field | Recurse_In_Field =>
               Linear_Template : Linear_Template_Vectors.Vector;
               --  Sequence of tokens/fields that the parsed template is
               --  supposed to yield once instantiated/formatted.

               Linear_Position : Positive;
               --  Position in Linear_Template of the next item to be found in
               --  the template to parse.
         end case;
      end record;

      type Template_Parsing_Context_Kind is
        (Node_Template, Field_Template, Sep_Template);
      --  Indicate which kind of template we are parsing:
      --
      --  ``Node_Template``: a "node" template.
      --
      --  ``Field_Template``: a template in the "fields" mapping.
      --
      --  ``Sep_Template``: a "sep" template.

      type Template_Parsing_Context (Kind : Template_Parsing_Context_Kind) is
      record
         Node : Type_Ref;
         --  Node for which we parse this template

         State : Template_Parsing_State;
         --  Keep track of the parsing state for this template; used for
         --  validation.

         case Kind is
            when Node_Template | Sep_Template =>
               null;

            when Field_Template =>
               Field : Struct_Member_Ref;
               --  Field for which we parse this template
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
        (JSON       : JSON_Value;
         Context    : in out Template_Parsing_Context;
         Symbol_Map : in out Symbol_Parsing_Maps.Map) return Document_Type;
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

      procedure Load_Field_Configs
        (Node    : Type_Ref;
         JSON    : JSON_Value;
         Configs : out Field_Config_Maps.Map);
      --  Assuming that JSON is the "fields" configuration for Node, parse its
      --  field configurations and set Configs accordingly.

      procedure Abort_Parsing (Message : String) with No_Return;
      --  Append an item to ``Diagnostics`` and raise an Invalid_Input
      --  exception.

      Result : Unparsing_Configuration_Access :=
        new Unparsing_Configuration_Record;
      Pool   : Document_Pool renames Result.Pool;

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

                  --   As soon as we find a "text" template node, we know this
                  --   is a Recurse_Field template (in "node") or a
                  --   Recurse_In_Field template (in "fields").
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
                        if Kind_Matches (JSON, "text") then
                           Result := Recurse_In_Field;
                           raise Abort_Recursion;
                        end if;

                     when Sep_Template =>
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

               when Recurse_Field =>

                  --  Knowing that we are processing a field template,
                  --  Template_Kind is not supposed to return Recurse_Field.

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
         Symbol_Map : Symbol_Parsing_Maps.Map;
         --  Mapping from name symbols found in the JSON (Symbol_Type) and
         --  "ids" (Template_Symbol).

         Root : constant Document_Type :=
           Parse_Template_Helper (JSON, Context, Symbol_Map);
      begin
         --  Make sure that all symbols referenced in this template are also
         --  defined in this template.

         for Info of Symbol_Map loop
            if not Info.Has_Definition then
               Abort_Parsing
                 (Context,
                  "undefined symbol: " & To_String (Info.Source_Name));
            end if;
         end loop;

         case Context.State.Kind is
            when Simple_Recurse =>
               if Context.State.Recurse_Found then
                  return (Kind => With_Recurse, Root => Root);
               else
                  Abort_Parsing (Context, "recursion is missing");
               end if;

            when Recurse_Field | Recurse_In_Field =>

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
                     return (Kind => With_Recurse_Field, Root => Root);
                  when Recurse_In_Field =>
                     return (Kind => With_Text_Recurse, Root => Root);
               end case;
         end case;
      end Parse_Template;

      ---------------------------
      -- Parse_Template_Helper --
      ---------------------------

      function Parse_Template_Helper
        (JSON       : JSON_Value;
         Context    : in out Template_Parsing_Context;
         Symbol_Map : in out Symbol_Parsing_Maps.Map) return Document_Type is
      begin
         case JSON.Kind is
         when JSON_Array_Type =>
            declare
               Items : Document_Vectors.Vector;
            begin
               for D of JSON_Array'(JSON.Get) loop
                  Items.Append
                    (Parse_Template_Helper (D, Context, Symbol_Map));
               end loop;
               return Pool.Create_List (Items);
            end;

         when JSON_String_Type =>
            declare
               Value : constant String := JSON.Get;
            begin
               if Value = "breakParent" then
                  return Pool.Create_Break_Parent;
               elsif Value = "hardline" then
                  return Pool.Create_Hard_Line;
               elsif Value = "hardlineWithoutBreakParent" then
                  return Pool.Create_Hard_Line_Without_Break_Parent;
               elsif Value = "line" then
                  return Pool.Create_Line;
               elsif Value = "recurse" then
                  Process_Recurse (Context);
                  return Pool.Create_Recurse;
               elsif Value = "softline" then
                  return Pool.Create_Soft_Line;
               elsif Value = "literalline" then
                  return Pool.Create_Literal_Line;
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
                          (JSON.Get ("contents"), Context, Symbol_Map));
                  end;

               elsif Kind in
                  "dedent" | "dedentToRoot" | "markAsRoot" | "innerRoot"
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
                                  else raise Program_Error),
                     Contents => Parse_Template_Helper
                                   (JSON.Get ("contents"),
                                    Context,
                                    Symbol_Map));

               elsif Kind = "fill" then
                  declare
                     Document : Document_Type;
                  begin
                     if not JSON.Has_Field ("document") then
                        Abort_Parsing
                          (Context, "missing ""document"" key for fill");
                     end if;
                     Document :=
                       Parse_Template_Helper
                         (JSON.Get ("document"), Context, Symbol_Map);

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
                       Parse_Template_Helper
                         (JSON.Get ("document"), Context, Symbol_Map);

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

                           declare
                              Info : Symbol_Info renames
                                Lookup (JSON_Id.Get, Symbols, Symbol_Map);
                           begin
                              --  Ensure that there is no conflicting symbol
                              --  definition in this template.

                              if Info.Has_Definition then
                                 Abort_Parsing
                                   (Context,
                                    "duplicate group id: " & JSON_Id.Get);
                              else
                                 Info.Has_Definition := True;
                              end if;
                              Id := Info.Template_Sym;
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
                         (JSON.Get ("breakContents"),
                          Contents_Context,
                          Symbol_Map);

                     Flat_Contents :=
                       (if JSON.Has_Field ("flatContents")
                        then Parse_Template_Helper
                               (JSON.Get ("flatContents"),
                                Flat_Context,
                                Symbol_Map)
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
                             Lookup (JSON_Id.Get, Symbols, Symbol_Map)
                             .Template_Sym;
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
                       Parse_Template_Helper
                         (JSON.Get ("then"), Then_Context, Symbol_Map);

                     if not JSON.Has_Field ("else") then
                        Abort_Parsing
                          (Context,
                           "missing ""else"" key for ifEmpty");
                     end if;
                     Else_Contents :=
                       Parse_Template_Helper
                         (JSON.Get ("else"), Else_Context, Symbol_Map);

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
                  if Context.Kind /= Node_Template then
                     Abort_Parsing
                       (Context,
                        """ifKind"" is valid only in node templates");
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

                  begin
                     --  Validate that the keys are present and have the
                     --  correct type.

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

                        ---------------------
                        -- Process_Matcher --
                        ---------------------

                        procedure Process_Matcher (Matcher_JSON : JSON_Value)
                        is
                           Kind          : constant JSON_Value :=
                             Matcher_JSON.Get ("kind");
                           Document_JSON : constant JSON_Value :=
                             Matcher_JSON.Get ("document");

                           Nested_Context : Template_Parsing_Context :=
                             Initial_Context;

                        begin
                           if Kind.Kind /= JSON_String_Type then
                              Abort_Parsing
                                (Context,
                                 "invalid matcher ""kind"" field for "
                                 & """ifKind"" - found "
                                 & Kind.Kind'Image
                                 & "; expected "
                                 & JSON_String_Type'Image);
                           end if;

                           --  Parse the matcher and store it in the table

                           If_Kind_Matchers.Append
                             ((From_Index
                                 (Language, To_Type_Index (Kind.Get)),
                               Parse_Template_Helper
                                 (Document_JSON, Nested_Context, Symbol_Map)));

                           --  Confirm that the final linear position is
                           --  homogeneous between all matchers.

                           if Nested_Context.State /= Context.State then
                              Abort_Parsing
                                (Context,
                                 "ifKind matcher """ & Kind.Get & """ has an "
                                 & "inconsistent recurse structure");
                           end if;
                        end Process_Matcher;

                     begin
                        If_Kind_Default :=
                          Parse_Template_Helper
                            (Default_JSON, Context, Symbol_Map);

                        if Null_JSON.Kind /= JSON_Null_Type then
                           declare
                              If_Kind_Null_Context :
                                Template_Parsing_Context :=
                                  Initial_Context;

                           begin
                              If_Kind_Null :=
                                Parse_Template_Helper
                                  (Null_JSON,
                                   If_Kind_Null_Context,
                                   Symbol_Map);

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
                            (From_Index
                               (Language,
                                To_Struct_Member_Index
                                  (Field_JSON.Get, Context.Node)),
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
                    (Parse_Template_Helper
                       (JSON.Get ("contents"), Context, Symbol_Map));

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

               elsif Kind = "text" then
                  declare
                     T : JSON_Value;
                  begin
                     if not JSON.Has_Field ("text") then
                        Abort_Parsing
                          (Context, "missing ""text"" key for text");
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

                     declare
                        Item : Linear_Template_Item :=
                          (Kind       => Token_Item,
                           Token_Kind => No_Token_Kind_Ref,
                           Token_Text => To_Unbounded_Text
                                           (From_UTF8 (T.Get)));
                     begin
                        Process_Linear_Template_Item (Item, Context);
                        return Pool.Create_Token
                                 (Item.Token_Kind, Item.Token_Text);
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
             when Token_Item   => "text",
             when Field_Item   => "recurse_field",
             when Recurse_Item => "recurse");
      begin
         --  Ensure that it is valid to have a "recurse_field" node in this
         --  template.

         if Context.State.Kind not in Recurse_Field | Recurse_In_Field then
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
      -- Abort_Parsing --
      -------------------

      procedure Abort_Parsing
        (Context : Template_Parsing_Context; Message : String)
      is
         Prefix : constant String :=
           (case Context.Kind is
            when Node_Template =>
              """node"" template for " & Node_Type_Image (Context.Node),
            when Sep_Template =>
              """sep"" template for " & Node_Type_Image (Context.Node),
            when Field_Template =>
              "template for " & Field_Image (Context.Field, Context.Node));
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

      ------------------------
      -- Load_Field_Configs --
      ------------------------

      procedure Load_Field_Configs
        (Node    : Type_Ref;
         JSON    : JSON_Value;
         Configs : out Field_Config_Maps.Map)
      is
         procedure Process (Name : String; Value : JSON_Value);
         --  Load the Value template for the Node field called Name

         -------------
         -- Process --
         -------------

         procedure Process (Name : String; Value : JSON_Value) is
            Member  : constant Struct_Member_Index :=
              To_Struct_Member_Index (Name, Node);
            Context : Template_Parsing_Context :=
              (Kind  => Field_Template,
               State => Initial_State_For
                          (Node  => Node,
                           Field => From_Index (Language, Member),
                           JSON  => Value),
               Node  => Node,
               Field => From_Index (Language, Member));
         begin
            Configs.Insert (Member, Parse_Template (Value, Context));
         end Process;

      begin
         Configs.Clear;
         JSON.Map_JSON_Object (Process'Access);
      end Load_Field_Configs;

      -------------------
      -- Abort_Parsing --
      -------------------

      procedure Abort_Parsing (Message : String) is
      begin
         Append (Diagnostics, No_Source_Location_Range, To_Text (Message));
         raise Invalid_Input;
      end Abort_Parsing;

      --  First, parse the JSON document

      JSON_Result : constant Read_Result := Read (Buffer);
      JSON        : JSON_Value;
   begin
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

      Result.Ref_Count := 1;
      Result.Language := Language;

      if not JSON.Has_Field ("node_configs") then
         Abort_Parsing ("missing ""node_configs"" key");
      end if;

      declare
         Node_Configs : constant JSON_Value := JSON.Get ("node_configs");

         procedure Process (Name : String; Value : JSON_Value);
         --  Load the unparsing configuration from Value for the node called
         --  Name.

         -------------
         -- Process --
         -------------

         procedure Process (Name : String; Value : JSON_Value) is
            Key     : constant Type_Index := To_Type_Index (Name);
            Node    : constant Type_Ref := From_Index (Language, Key);
            Config  : Node_Config_Access := new Node_Config_Record'
              (Node_Template => No_Template,
               Field_Configs => <>,
               List_Sep      => No_Template);
         begin
            if Value.Has_Field ("node") then
               declare
                  JSON_Template : constant JSON_Value := Value.Get ("node");
                  Context       : Template_Parsing_Context :=
                    (Kind  => Node_Template,
                     Node  => Node,
                     State => <>);
               begin
                  Context.State :=
                    Initial_State_For (Node, JSON_Template, Context);
                  Config.Node_Template :=
                    Parse_Template (JSON_Template, Context);
               end;
            else
               Config.Node_Template := Pool.Create_Recurse;
            end if;

            if Value.Has_Field ("fields") then
               Load_Field_Configs
                 (Node, Value.Get ("fields"), Config.Field_Configs);
            end if;

            if Value.Has_Field ("sep") then
               if not Is_List_Node (Node) then
                  Abort_Parsing
                    (Name & " is not a list node, invalid ""sep"""
                     & " configuration");
               end if;
               declare
                  Context : Template_Parsing_Context :=
                    (Kind  => Sep_Template,
                     Node  => Node,
                     State => (Kind          => Simple_Recurse,
                               Recurse_Found => False));
               begin
                  Config.List_Sep :=
                    Parse_Template (Value.Get ("sep"), Context);
               end;
            end if;

            --  It is only now that this node config has been successfully
            --  imported that we can add it to the unparsing configuration.
            --  This is necessary so that potential premature release of the
            --  unparsing configuration can be released in case of input
            --  validation error.

            Result.Node_Configs.Insert (Key, Config);

         exception
            when others =>
               Free (Config);
               raise;
         end Process;

      begin
         Node_Configs.Map_JSON_Object (Process'Access);
      end;

      --  Now that we all node customizations, compute the unparsing
      --  configuration for all nodes.

      for Node of All_Node_Types (Language) loop
         declare
            Key         : constant Type_Index := To_Index (Node);
            Cur         : constant Node_Config_Maps.Cursor :=
              Result.Node_Configs.Find (Key);
            Node_Config : Node_Config_Access;
            Base_Config : constant Node_Config_Access :=
              (if Node = Root_Node_Type (Language)
               then null
               else Result.Node_Configs.Element (To_Index (Base_Type (Node))));
         begin
            --  Create a configuration for this node if there isn't one yet

            if Node_Config_Maps.Has_Element (Cur) then
               Node_Config := Node_Config_Maps.Element (Cur);
            else
               Node_Config := new Node_Config_Record;
               Result.Node_Configs.Insert (Key, Node_Config);
            end if;

            --  Inherit configuration details from the base node, or provide
            --  the default one for the root node.

            if Node_Config.Node_Template = No_Template then
               Node_Config.Node_Template :=
                 (if Base_Config = null
                  then Pool.Create_Recurse
                  else Base_Config.Node_Template);
            end if;

            for Member of Members (Node) loop
               if Is_Field (Member) and then not Is_Null_For (Member, Node)
               then
                  declare
                     Key            : constant Struct_Member_Index :=
                       To_Index (Member);
                     Cur            : constant Field_Config_Maps.Cursor :=
                       Node_Config.Field_Configs.Find (Key);
                     Present        : constant Boolean :=
                        Field_Config_Maps.Has_Element (Cur);
                     Field_Template : Template_Type;
                  begin
                     if Present then
                        Field_Template := Field_Config_Maps.Element (Cur);
                     elsif Base_Config /= null then
                        declare
                           Cur : constant Field_Config_Maps.Cursor :=
                             Base_Config.Field_Configs.Find (Key);
                        begin
                           Field_Template :=
                             (if Field_Config_Maps.Has_Element (Cur)
                              then Field_Config_Maps.Element (Cur)
                              else Pool.Create_Recurse);
                        end;
                     end if;

                     if Field_Template = No_Template then
                        Field_Template := Pool.Create_Recurse;
                     end if;

                     if Present then
                        Node_Config.Field_Configs.Replace_Element
                          (Cur, Field_Template);
                     else
                        Node_Config.Field_Configs.Insert (Key, Field_Template);
                     end if;
                  end;
               end if;
            end loop;

            if Node_Config.List_Sep = No_Template then
               Node_Config.List_Sep :=
                 (if Base_Config = null
                  then Pool.Create_Recurse
                  else Base_Config.List_Sep);
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

      Destroy (Symbols);
      return (Ada.Finalization.Controlled with Value => Result);

   exception
      when Invalid_Input =>
         pragma Assert (not Diagnostics.Is_Empty);
         Destroy (Symbols);
         Release (Result);
         return No_Unparsing_Configuration;
   end Load_Unparsing_Config_From_Buffer;

   --------------------------
   -- Instantiate_Template --
   --------------------------

   function Instantiate_Template
     (Pool          : in out Document_Pool;
      Node          : Lk_Node;
      Current_Token : in out Lk_Token;
      Trivias       : Trivias_Info;
      Template      : Template_Type;
      Arguments     : Template_Instantiation_Args) return Document_Type
   is
      State : Instantiation_State :=
        (Node,
         Current_Token,
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

         when Fill =>
            return Pool.Create_Fill
              (Instantiate_Template_Helper
                 (Pool, State, Template.Fill_Document));

         when Group =>
            return Pool.Create_Group
              (Instantiate_Template_Helper
                 (Pool, State, Template.Group_Document),
               Template.Group_Should_Break,
               Template.Group_Id);

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
                 (Break_Doc, Flat_Doc, Template.If_Break_Group_Id);
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
                  for J in
                    Template.If_Kind_Matchers.First_Index
                    .. Template.If_Kind_Matchers.Last_Index
                  loop
                     if Type_Matches
                          (Field_Node,
                           Template
                             .If_Kind_Matchers
                             .Reference (J)
                             .Matched_Type)
                     then
                        Matched_Template :=
                          Template.If_Kind_Matchers (J).Document;
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
               return Arg.Document;
            end;

         when Recurse_Field =>
            declare
               Arg : constant Single_Template_Instantiation_Argument :=
                 State.Arguments.Field_Docs (Template.Recurse_Field_Position);
            begin
               State.Current_Token := Arg.Next_Token;
               return Arg.Document;
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
               end return;
            end;

         when Soft_Line =>
            return Pool.Create_Soft_Line;

         when Token =>
            declare
               Items : Document_Vectors.Vector;
            begin
               Items.Append
                 (Pool.Create_Token
                    (Template.Token_Kind, Template.Token_Text));
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
     (Node   : Lk_Node;
      Config : Unparsing_Configuration)
      return Prettier_Ada.Documents.Document_Type
   is
      Trivias : Trivias_Info;
      Pool    : Document_Pool;

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
         Current_Token : in out Lk_Token);
      --  Unparse ``Child``, which is the ``Field_Ref`` field of ``Node``. The
      --  Resulting items are appended to ``Items``. ``Node_Config`` must be
      --  the node unparsing configuration for ``Node``, and ``Unparser`` must
      --  be the unparser for this field. Update ``Current_Token`` accordingly.

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
               & "), current token: " & Current_Token.Image);
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
               & "), current token: " & Current_Token.Image);
         end if;
      end Unparse_Tokens;

      ------------------
      -- Unparse_Node --
      ------------------

      function Unparse_Node
        (N : Lk_Node; Current_Token : in out Lk_Token) return Document_Type
      is
         Node_Config : Node_Config_Record renames
           Config.Value.Node_Configs.Element (To_Index (Type_Of (N))).all;
         Items       : Document_Vectors.Vector;

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
                  & ", current token: " & Current_Token.Image);
            end if;

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
                        pragma Assert
                          (Node_Config.List_Sep.Kind = With_Recurse);
                        declare
                           Sep_Items : Document_Vectors.Vector;
                           --  Token and trivia used to unparse the list
                           --  separator.

                           Next_Token : Lk_Token := Current_Token;
                           --  Value for ``Current_Token`` after the list
                           --  separator has been processed.

                           Args : Template_Instantiation_Args (With_Recurse);
                        begin
                           if not Is_Fake_Token then
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
                              Node          => N,
                              Current_Token => Current_Token,
                              Trivias       => Trivias,
                              Template      => Node_Config.List_Sep,
                              Arguments     => Args);
                           pragma Assert (Current_Token = Next_Token);
                           Items.Append (Token);
                        end;

                        --  Flush line breaks before unparsing the next list
                        --  child, so that line breaks do not get inserted
                        --  inside groups/indents/... that belong to the next
                        --  child.

                        Items.Append (Pool.Create_Flush_Line_Breaks);

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
                        & Current_Token.Image);
                  end if;

               when Field_Fragment =>
                  Unparse_Field
                    (Node          => N,
                     Node_Config   => Node_Config,
                     Child         => F.Node,
                     Field_Ref     => To_Index (F.Field),
                     Unparser      => F.Field_Unparser_Ref.all,
                     Items         => Items,
                     Current_Token => Current_Token);
                  if Current_Token_Trace.Is_Active then
                     Current_Token_Trace.Trace
                       ("Field fragment " & Debug_Name (F.Field)
                        & " unparsed, current token: " & Current_Token.Image);
                  end if;

               when List_Child_Fragment =>

                  --  If reattached tokens precede this child, process them now
                  --  so that they are unparsed as part of this list node
                  --  instead of another (possibly deeply nested) list node.

                  Process_Reattached_Trivias
                    (First_Trivia_Before (F.Node), Items, Pool, Trivias);

                  --  Flush line breaks before unparsing the list child, so
                  --  that line breaks do not get inserted inside
                  --  groups/indents/... that would belong to it.

                  Items.Append (Pool.Create_Flush_Line_Breaks);

                  --  Finally unparse the list child

                  Items.Append (Unparse_Node (F.Node, Current_Token));
                  if Current_Token_Trace.Is_Active then
                     Current_Token_Trace.Trace
                       ("List child fragment unparsed, current token: "
                        & Current_Token.Image);
                  end if;
            end case;
         end Process_Fragment;

         Template : Template_Type renames Node_Config.Node_Template;
         Result   : Document_Type;
      begin
         if Current_Token_Trace.Is_Active then
            Current_Token_Trace.Increase_Indent
              ("Unparsing " & N.Image
               & ", current token: " & Current_Token.Image);
         end if;

         case Some_Template_Kind (Template.Kind) is
            when With_Recurse =>

               --  First gather documents for all the fragments in this node,
               --  then group them in a list document, and use that list to
               --  instantiate the template for the node itself.

               Iterate_On_Fragments
                 (N, Current_Token, Process_Fragment'Access);
               Result := Instantiate_Template
                 (Pool          => Pool,
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
                              & ", field token: " & Field_Token.Image);
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
                              Current_Token => Field_Token);
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
                              & ", field token: " & Field_Token.Image);
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
              (First_Trivia_Before (N), Items, Pool, Trivias);

            Items.Append (Result);

            --  If this is a ghost node, leading and trailing trivias are the
            --  same. In addition, ``N.Token_End`` points at the first token
            --  that appears logically after the ghost node, so what comes next
            --  has nothing to do with the ghost node.

            if not N.Is_Ghost then
               T := N.Token_End.Next;
               if T.Is_Trivia then
                  Process_Reattached_Trivias (T, Items, Pool, Trivias);
               end if;
            end if;

            Result := Pool.Create_List (Items);
         end;

         if Current_Token_Trace.Is_Active then
            Current_Token_Trace.Decrease_Indent
              ("Done with " & N.Image
               & ", current token: " & Current_Token.Image);
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
         Current_Token : in out Lk_Token)
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

         Dump (Internal_Result, Before_Spacing_Trace);
         Insert_Required_Spacing
           (Pool, Internal_Result, Config.Value.Max_Empty_Lines);
         Dump (Internal_Result, Final_Doc_Trace);

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

      package Config_Filename is new Parse_Positional_Arg
        (Parser   => Parser,
         Name     => "config-file",
         Help     => "Name of the JSON pretty-printer configuration file",
         Arg_Type => Unbounded_String);

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

      --  Parse the configuration file and the source file to pretty-print.
      --  Abort if there is a parsing failure.

      declare
         Diagnostics : Diagnostics_Vectors.Vector;
         Filename    : constant String := To_String (Config_Filename.Get);
      begin
         Config := Load_Unparsing_Config (Language, Filename, Diagnostics);
         if Config = No_Unparsing_Configuration then
            Put_Line ("Error when loading the unparsing configuration:");
            Print (Diagnostics);
            return;
         end if;
      end;
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
         F         : File_Type;
         Doc       : constant Prettier.Document_Type :=
           Unparse_To_Prettier (Node, Config);
         Formatted : constant Unbounded_String :=
           Prettier.Format
             (Document => Doc,
              Options  =>
                (Width       => Width.Get,
                 Indentation =>
                   (Indentation_Kind.Get,
                    Indentation_Width.Get,
                    Offset => (Tabs => 0, Spaces => 0)),
                 End_Of_Line => End_Of_Line.Get));
      begin
         --  If requested, dump it as a JSON file

         if Dump_Document.Get then
            Create (F, Name => "doc.json");
            Put_Line (F, Prettier.Json.Serialize (Doc));
            Close (F);
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
