--
--  Copyright (C) 2014-2025, AdaCore
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

with GNATCOLL.Iconv;
with GNATCOLL.Opt_Parse;
with GNATCOLL.VFS; use GNATCOLL.VFS;
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
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

package body Langkit_Support.Generic_API.Unparsing is

   use type Ada.Containers.Count_Type;

   function Image_With_Sloc (T : Lk_Token) return String
   is (T.Image & " (" & Image (Start_Sloc (T.Sloc_Range)) & ")");

   function Unwrap_Unparsing_Configuration
     (Config : Unparsing_Configuration)
      return Unparsing_Configuration_Access
   is (Config.Value)
   with
     Export,
     External_Name => External_Name_Prefix & "unwrap_unparsing_config";

   function Load_Unparsing_Config_From_Buffer
     (Language        : Language_Id;
      Buffer          : String;
      Diagnostics     : in out Diagnostics_Vectors.Vector;
      Check_All_Nodes : Boolean)
      return Unparsing_Configuration
   is (Ada.Finalization.Controlled with
       Value => Load_Unparsing_Config_From_Buffer
                  (Language, Buffer, Diagnostics, Check_All_Nodes));
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

   function Is_Field_Present
     (Field       : Lk_Node;
      Field_Index : Positive) return Boolean;
   --  Return whether ``Field`` must be considered as present for unparsing.
   --
   --  ``Field_Index`` must be the index of ``Field`` in its parent's list of
   --  children.

   function Token_Matches
     (Token : Lk_Token; Unparser : Token_Unparser) return Boolean
   is (To_Index (Token.Kind) = Unparser.Kind);
   --  Return whether the given ``Token`` has a kind that matches the one found
   --  in ``Unparser``.

   procedure Check_Same_Tokens (Original, Reformatted : Lk_Unit);
   --  Check that both units have the same sequence of tokens + comments. Print
   --  an error message and set the exit status to Failure if this not the
   --  case.

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

   function Comment_Stripped_Text (Self : Lk_Token) return Unbounded_Text_Type
   with Pre => Self.Is_Comment;
   --  Return the text associated to the given comment token, with the trailing
   --  spaces and CR/LF codepoints stripped.
   --
   --  Post processing in Prettier will strip these trailing characters:
   --  keeping them would make width computations imprecise, and thus have
   --  unintended effects on formatting. To avoid this problem, we must remove
   --  them before handing comments to Prettier.

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

   type Shared_Document is record
      Document : Document_Type;
      --  Document that may be use multiple times

      Used : Boolean;
      --  Whether ``Document`` was already used during the instantiation. When
      --  true, further uses must do deep copies to avoid sharing, and have the
      --  instantiated document stay a tree.
   end record;

   function Create_Shared_Document
     (Self : Document_Type) return Shared_Document
   is (Document => Self, Used => False);

   function Use_Shared_Document
     (Pool : in out Document_Pool;
      Self : in out Shared_Document) return Document_Type;
   --  Return the document in ``Self``, or a deep copy of it if the original
   --  document is already used.

   type Single_Template_Instantiation_Argument is record
      Document : Shared_Document;
      --  Document to substitute to "recurse*" nodes when instantiating a
      --  template.

      Node : Lk_Node;
      --  Node from which ``Document`` was generated. Keeping track of this is
      --  necessary in order to implement instantiation for "recurse_flatten".

      Next_Token : Lk_Token;
      --  Token that follows ``Node``, i.e. the token to assign to
      --  ``Current_Token`` after this template argument has been processed.
   end record;

   function Use_Template_Argument
     (Pool       : in out Document_Pool;
      Argument   : in out Single_Template_Instantiation_Argument;
      Next_Token : out Lk_Token) return Document_Type;
   --  Return the document in ``Argument``, or a deep copy of it if the
   --  original document is already used. Also set ``Next_Token` to
   --  ``Argument.Next_Token``.

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
            Join_Left, Join_Right : Shared_Document;
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

      Arguments : not null access Template_Instantiation_Args;
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

      function Is_Equivalent_Wrapper (Left, Right : Lk_Token) return Boolean;
      --  Wrapper around ``Is_Equivalent`` to determine if we can consider that
      --  ``Left`` and ``Right`` must be considered equivalent tokens.

      ---------------------
      -- Skip_Formatting --
      ---------------------

      procedure Skip_Formatting (T : in out Lk_Token) is
      begin
         while not T.Is_Null and then T.Is_Trivia and then not T.Is_Comment
         loop
            T := T.Next;
         end loop;
      end Skip_Formatting;

      ---------------------------
      -- Is_Equivalent_Wrapper --
      ---------------------------

      function Is_Equivalent_Wrapper (Left, Right : Lk_Token) return Boolean is
      begin
         return
           (if Left.Is_Comment and then Right.Is_Comment
            then Comment_Stripped_Text (Left) = Comment_Stripped_Text (Right)
            else Is_Equivalent (Left, Right));
      end Is_Equivalent_Wrapper;

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

         if not Is_Equivalent_Wrapper (T1, T2) then
            Put_Line
              ("Unexpected change for " & T1.Image & " ("
               & Image (Start_Sloc (Sloc_Range (T1))) & "):");
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

   ----------------------
   -- Is_Field_Present --
   ----------------------

   function Is_Field_Present
     (Field       : Lk_Node;
      Field_Index : Positive) return Boolean
   is
   begin
      if Field.Is_Null then

         --  Null nodes are always considered as absent

         return False;

      elsif Field.Parent.Is_Null or else Field.Parent.Is_List_Node then

         --  Root nodes and list children are always considered as present

         return True;

      else
         return Is_Field_Present
            (Field          => Field,
             Field_Unparser => Field_Unparser_For
                                 (Type_Of (Field.Parent), Field_Index).all);
      end if;
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

      procedure Process (Node : Lk_Node; Index : Positive);
      --  Reattach relevant trivias to ``Node``. This processes ``Node``'s
      --  children recursively.
      --
      --  ``Index`` must be the index of ``Node`` in its parent's list of
      --  children.

      procedure Reattach (T : Lk_Token; To : Lk_Node; What : String);
      --  Reattach ``T`` to the ``To`` node. ``What`` is used to qualify this
      --  trivia in debug logs.

      -------------
      -- Process --
      -------------

      procedure Process (Node : Lk_Node; Index : Positive) is

         --  Consider that empty lists that materialize absent nodes do not
         --  exist: do not reattach trivias to them.

         Is_Present_List : constant Boolean :=
           not Node.Is_Null
           and then Node.Is_List_Node
           and then (Node.Parent.Is_Null
                     or else Is_Field_Present (Node, Index));
      begin
         --  Register reattached trivias that come before list nodes

         if Is_Present_List then
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

         declare
            C_Index : Positive := 1;
         begin
            for C of Node.Children loop
               if not C.Is_Null then
                  Process (C, C_Index);
               end if;
               C_Index := C_Index + 1;
            end loop;
         end;

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

         if Is_Present_List then
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
         Position : Token_To_Node_Maps.Cursor;
         Inserted : Boolean;
      begin
         Info.First_Reattached_Trivias.Insert
           (T, To, Position, Inserted);
         if Trace then
            declare
               T_Img  : constant String := What & ": " & Image_With_Sloc (T);
               To_Img : constant String :=
                 Token_To_Node_Maps.Element (Position).Image;
            begin
               if Inserted then
                  Trivias_Trace.Trace
                    ("Reattaching " & T_Img & " to " & To_Img);
               else
                  Trivias_Trace.Trace
                    ("Not reattaching " & T_Img & " to " & To.Image
                     & " (already reattached to " & To_Img & ")");
               end if;
            end;
         end if;
      end Reattach;
   begin
      --  Determine which tokens have reattached trivias

      Info.First_Reattached_Trivias.Clear;
      Process (Node, 1);

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

   ---------------------------
   -- Comment_Stripped_Text --
   ---------------------------

   function Comment_Stripped_Text (Self : Lk_Token) return Unbounded_Text_Type
   is
      Result : constant Text_Type := Self.Text;
      Last   : Natural := Result'Last;
   begin
      while
         Last in Result'Range
         and then Result (Last) in ' ' | Chars.HT | Chars.CR
      loop
         Last := Last - 1;
      end loop;
      return To_Unbounded_Text (Result (Result'First .. Last));
   end Comment_Stripped_Text;

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
                  Comment_Stripped_Text (Fragment.Comment_Token)));

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
      Id : constant Language_Id := Node.Language;

      Node_Type     : constant Type_Ref := Type_Of (Node);
      Node_Unparser : Node_Unparser_Impl renames
        Node_Unparser_For (Node_Type).all;
   begin
      case Node_Unparser.Kind is
         when Regular =>

            --  "recurse" templates for regular nodes are supposed to be
            --  expanded into "recurse_field" ones during configuration
            --  loading, so this should never happen.

            raise Program_Error;

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

   -------------------------
   -- Use_Shared_Document --
   -------------------------

   function Use_Shared_Document
     (Pool : in out Document_Pool;
      Self : in out Shared_Document) return Document_Type
   is
   begin
      return Result : Document_Type := Self.Document do
         if Self.Used then
            Result := Deep_Copy (Pool, Result);
         else
            Self.Used := True;
         end if;
      end return;
   end Use_Shared_Document;

   ---------------------------
   -- Use_Template_Argument --
   ---------------------------

   function Use_Template_Argument
     (Pool       : in out Document_Pool;
      Argument   : in out Single_Template_Instantiation_Argument;
      Next_Token : out Lk_Token) return Document_Type is
   begin
      return Result : constant Document_Type :=
        Use_Shared_Document (Pool, Argument.Document)
      do
         Next_Token := Argument.Next_Token;
      end return;
   end Use_Template_Argument;

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
                 (Pool, State, Template.Align_Contents),
               Template.Align_Bubble_Up);

         when Break_Parent | Empty_Table_Separator =>
            return Template;

         when Fill =>
            return Pool.Create_Fill
              (Instantiate_Template_Helper
                 (Pool, State, Template.Fill_Document),
               Template.Fill_Bubble_Up);

         when Flush_Line_Breaks =>
            return Template;

         when Group =>
            return Pool.Create_Group
              (Instantiate_Template_Helper
                 (Pool, State, Template.Group_Document),
               Template.Group_Should_Break,
               Instantiate_Symbol (State.Symbols.all, Template.Group_Id),
               Template.Group_Bubble_Up);

         when Hard_Line | Hard_Line_Without_Break_Parent =>
            return Template;

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
               --  If the field is present, pick the document for the first
               --  matcher that accepts it.

               if Is_Field_Present
                    (Field_Node,
                     Syntax_Field_Index
                       (Template.If_Kind_Field, Type_Of (State.Node)))
               then
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

               elsif Template.If_Kind_Absent /= null then
                  Matched_Template := Template.If_Kind_Absent;
               end if;

               return Instantiate_Template_Helper
                        (Pool, State, Matched_Template);
            end;

         when Indent =>
            return Pool.Create_Indent
              (Instantiate_Template_Helper
                 (Pool, State, Template.Indent_Document),
               Template.Indent_Bubble_Up);

         when Line =>
            return Template;

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
            return Template;

         --  For all "recurse" nodes, the knowledge of how to update
         --  ``State.Current_Token`` is encoded in the ``Next_Token`` member of
         --  the corresponding ``Single_Template_Instantiation_Argument``
         --  record.

         when Recurse =>
            return Use_Template_Argument
              (Pool       => Pool,
               Argument   => State.Arguments.With_Recurse_Doc,
               Next_Token => State.Current_Token);

         when Recurse_Field =>
            return Use_Template_Argument
              (Pool       => Pool,
               Argument   => State.Arguments.Field_Docs
                               (Template.Recurse_Field_Position),
               Next_Token => State.Current_Token);

         when Recurse_Flatten =>
            declare
               Arg : constant Single_Template_Instantiation_Argument :=
                 State.Arguments.With_Recurse_Doc;
            begin
               return Result : Document_Type :=
                  Use_Template_Argument
                    (Pool       => Pool,
                     Argument   => State.Arguments.With_Recurse_Doc,
                     Next_Token => State.Current_Token)
               do
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

         when Recurse_Left =>
            return Use_Shared_Document (Pool, State.Arguments.Join_Left);

         when Recurse_Right =>
            return Use_Shared_Document (Pool, State.Arguments.Join_Right);

         when Soft_Line =>
            return Template;

         when Table_Separator | Token =>
            declare
               Items : Document_Vectors.Vector;
            begin
               Items.Append (Template);
               Process_Trivias
                 (State.Current_Token,
                  Items,
                  Pool,
                  State.Trivias.all,
                  Skip_Token => True);
               return Pool.Create_List (Items);
            end;

         when Trim | Whitespace =>
            return Template;
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
                             (Document   => Create_Shared_Document
                                              (Pool.Create_List (Sep_Items)),
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
                                      Join_Left  => Create_Shared_Document
                                                      (Left_Row),
                                      Join_Right => Create_Shared_Document
                                                      (Right_Row)));
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
                       (Document   => Create_Shared_Document
                                        (Pool.Create_List (Items)),
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
                  Node_Type     : constant Type_Ref := Type_Of (N);
                  Node_Unparser : Node_Unparser_Impl renames
                    Node_Unparser_For (Node_Type).all;

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
                             (Document   => Create_Shared_Document (Child_Doc),
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
              (Document   => Create_Shared_Document (Field_Doc),
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
      elsif Node.Is_Null then
         raise Precondition_Failure with "null node";
      elsif Node.Unit.Has_Diagnostics then
         raise Precondition_Failure with "node's unit has parsing errors";
      end if;

      --  Refresh memoized Prettier documents stored in the unparsing
      --  configuration, since they use Prettier's document IDs that may be
      --  obsolete (i.e. used in a previous tree unparsing session).

      Refresh_Prettier_Documents (Config.Value.Pool);

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

      procedure Write_Encoded
        (Content : Unbounded_String; Output : File_Type; Charset : String);
      --  Transcode the UTF-8-encoded Content to Charset and write the result
      --  to Output.

      function Process_One_File (Filename : String) return Lk_Unit;
      --  Run the unparser on the given source file and return the anlysis unit
      --  used to reformat it.

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

      -------------------
      -- Write_Encoded --
      -------------------

      procedure Write_Encoded
        (Content : Unbounded_String; Output : File_Type; Charset : String)
      is
         use Ada.Strings.Unbounded.Aux;

         Content_Buffer      : Big_String_Access;
         Content_Buffer_Last : Natural;
      begin
         --  GNATCOLL.Iconv raises a Constraint_Error for empty strings: handle
         --  them here.

         if Content = Null_Unbounded_String then
            return;
         end if;

         --  To avoid overflowing the secondary stack with big sources, use the
         --  internal Ada.Strings.Wide_Wide_Unbounded.Aux.Get_String API to
         --  access the reformatted source string.

         Get_String (Content, Content_Buffer, Content_Buffer_Last);

         --  Transcode Content_Buffer from UTF-8 (the encoding for Prettier's
         --  output) to the same charset as the original source file. Use
         --  GNATCOLL.Iconv to achieve that and a 4096-bytes long transcoding
         --  buffer (work in chunks).

         declare
            use GNATCOLL.Iconv;

            State  : Iconv_T;
            Status : Iconv_Result := Full_Buffer;

            Inbuf       : String renames
              Content_Buffer.all (1 ..  Content_Buffer_Last);
            Input_Index : Positive := Inbuf'First;

            Outbuf       : String (1 .. 4096);
            Output_Index : Positive := Outbuf'First;
         begin
            --  This should never raise an Unsupported_Conversion exception
            --  since, if we got here, we managed to successfully parse a
            --  source file using Charset.Get.

            State := Iconv_Open (To_Code => Charset, From_Code => "utf-8");

            while Status /= GNATCOLL.Iconv.Success loop
               Iconv (State, Inbuf, Input_Index, Outbuf, Output_Index, Status);
               case Status is
                  when Invalid_Multibyte_Sequence
                     | Incomplete_Multibyte_Sequence
                  =>

                     --  All codepoints in Content are supposed to come from
                     --  the source file, which was decoded using Charset, so
                     --  we should never have any trouble re-encoding these
                     --  codepoints.

                     raise Program_Error;

                  when GNATCOLL.Iconv.Success | Full_Buffer =>
                     Put (Output, Outbuf (Outbuf'First .. Output_Index - 1));
                     Output_Index := Outbuf'First;
               end case;
            end loop;

            Iconv_Close (State);
         end;
      end Write_Encoded;

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

      package Charset is new Parse_Option
        (Parser      => Parser,
         Long        => "--charset",
         Arg_Type    => Unbounded_String,
         Help        =>
           "Charset to use to decode the source file and encode the"
           & " reformatted source.",
         Default_Val => Null_Unbounded_String);

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

      package Batch_Mode is new Parse_Flag
        (Parser      => Parser,
         Short       => "-b",
         Long        => "--batch",
         Help        =>
           "Batch mode: the input file is treated as a text file that contains"
           & " the list of source files to reformat (one per line).");

      Config  : Unparsing_Configuration;
      Context : Lk_Context;

      ----------------------
      -- Process_One_File --
      ----------------------

      function Process_One_File (Filename : String) return Lk_Unit is
         Unit : Lk_Unit;
         Node : Lk_Node;
      begin
         --  Parse the source file to pretty-print. Abort if there is a parsing
         --  failure.

         Unit := Context.Get_From_File
           (Filename => Filename,
            Rule     => Rule.Get);
         if Unit.Has_Diagnostics then
            Put_Line ("Cannot parse source file: aborting...");
            for D of Unit.Diagnostics loop
               Put_Line (Unit.Format_GNU_Diagnostic (D));
            end loop;
            Set_Exit_Status (Failure);
            return Unit;
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
                  return Unit;
               end if;

               Node := Node.Lookup (SS.Sloc);
               for I in 1 .. SS.Parent_Level loop
                  exit when Node.Is_Null;
                  Node := Node.Parent;
               end loop;

               if Node.Is_Null then
                  Put_Line ("No node found at the given location");
                  Set_Exit_Status (Failure);
                  return Unit;
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
               --  yields the same sequence of tokens + comments as the
               --  original unit.

               Context := Create_Context (Language);
               declare
                  use Ada.Strings.Unbounded.Aux;

                  Buffer      : Big_String_Access;
                  Buffer_Last : Natural;
                  U           : Lk_Unit;
               begin
                  --  To avoid overflowing the secondary stack with big
                  --  sources, use the internal
                  --  Ada.Strings.Wide_Wide_Unbounded.Aux.Get_String API to
                  --  access the reformatted source string.
                  --
                  --  Formatted contains the result of Prettier unparsing, and
                  --  is thus encoded in UTF-8: parse it as an UTF-8 source
                  --  regardless of the original encoding (not relevant here).

                  Get_String (Formatted, Buffer, Buffer_Last);
                  U := Context.Get_From_Buffer
                    (Filename => Filename,
                     Buffer   => Buffer.all (1 .. Buffer_Last),
                     Charset  => "utf-8",
                     Rule     => Rule.Get);
                  if U.Has_Diagnostics then
                     Put_Line ("Reformatted source has parsing errors:");
                     for D of U.Diagnostics loop
                        Put_Line (U.Format_GNU_Diagnostic (D));
                     end loop;
                     Set_Exit_Status (Failure);
                     return Unit;
                  end if;
                  Check_Same_Tokens (Unit, U);

                  --  Run the formatter a second time and check that the output
                  --  is stable.

                  declare
                     D : constant Prettier.Document_Type :=
                       Unparse_To_Prettier (U.Root, Config);
                     F : constant Unbounded_String :=
                       Prettier.Format (D, Options);
                  begin
                     if F /= Formatted then
                        Put_Line ("Reformatting is not stable");
                        Set_Exit_Status (Failure);
                        return Unit;
                     end if;
                  end;
               end;
            end if;

            --  Finally, write the formatted source code on the standard output
            --  or on the requested output file.

            if Length (Output_Filename.Get) > 0 then
               Create (F, Name => To_String (Output_Filename.Get));
               Write_Encoded (Formatted, F, Unit.Charset);
               Close (F);
            else
               Write_Encoded (Formatted, Standard_Output, Unit.Charset);
            end if;
            return Unit;
         end;
      end Process_One_File;

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

      Context := Create_Context (Language, Charset => To_String (Charset.Get));

      --  Run through all the source files to process

      declare
         Filename : constant String := To_String (Source_Filename.Get);
         Unit     : Lk_Unit;
      begin
         if Batch_Mode.Get then
            declare
               F : File_Type;
            begin
               Open (F, In_File, Filename);
               loop
                  declare
                     Line : constant String := Get_Line (F);
                  begin
                     Put_Line ("# " & Line);
                     Unit := Process_One_File (Line);

                     --  Reparse the unit with an empty buffer to reduce memory
                     --  footprint.

                     Unit.Reparse_From_Buffer (Buffer => "");
                  end;
               end loop;
            exception
               when End_Error =>
                  Close (F);
            end;
         else
            Unit := Process_One_File (Filename);
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
