--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Prettier_Ada.Document_Vectors;   use Prettier_Ada.Document_Vectors;
with Prettier_Ada.Documents.Builders; use Prettier_Ada.Documents.Builders;

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Generic_API.Unparsing;
use Langkit_Support.Generic_API.Unparsing;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;
with Langkit_Support.Names;  use Langkit_Support.Names;

package body Langkit_Support.Prettier_Utils is

   use type Ada.Containers.Count_Type;

   package Symbol_Maps is new Ada.Containers.Vectors
     (Some_Template_Symbol, Prettier.Symbol_Type);
   --  Mapping between our internal Template_Symbol and Prettier's actual
   --  symbol type.
   --
   --  Note that even though this is a logical map from one symbol type to the
   --  other, we implement it as a vector for efficiency since our symbols are
   --  contiguous integers.

   function Lookup
     (Source_Name : Unbounded_String;
      Symbols     : Symbol_Table;
      Symbol_Map  : in out Symbol_Parsing_Maps.Map)
      return Symbol_Parsing_Maps.Reference_Type;
   --  Return a reference to the entry in ``Symbol_Map`` corresponding to the
   --  ```Source_Name`` symbol (converted to a ``Symbol_Type`` using
   --  ``Symbols``). Create a map entry if it does not exist yet.

   function To_Prettier_Symbol
     (Symbol_Map : in out Symbol_Maps.Vector;
      Symbol     : Template_Symbol) return Prettier.Symbol_Type;
   --  Return a Prettier symbol that correspond to ``Symbol`` according to
   --  ``Symbol_Map``. This creates the Prettier symbol first if the requested
   --  one does not --  exist yet.

   type Spacing_State is record
      In_Broken_Group : Boolean;
      --  Whether the current group is known to be broken, i.e. whether Line
      --  and Soft_Line can be assumed to yield line breaks.

      Expected : Spacing_Type;
      --  Spacing that is required at the current point between the content
      --  already processed and what comes next.

      Actual : Spacing_Type;
      --  Spacing that is already present up to the current point

      Last_Token : Token_Kind_Ref;
      --  Kind for the last token/comment that was emitted at the current
      --  point.
   end record;
   --  Track spacing information while traversing a document tree to compute
   --  spacing information.

   Initial_Spacing_State : constant Spacing_State :=
     (False, No_Spacing, No_Spacing, No_Token_Kind_Ref);

   function Join (Left, Right : Spacing_State) return Spacing_State;
   --  Merge the ``Left`` and ``Right`` states: keep the strongest spacing
   --  requirement and the weakest actual spacing. This is to keep guesses
   --  conservative when handling disjunction commands (like ``If_Break``).

   generic
      with procedure Preprocess
        (Self : in out Document_Type; State : Spacing_State) is null;
      --  Callback invoked on a node whenever the traversal is about to process
      --  that node. It can use the current spacing state to transform the
      --  given ``Self`` document in place.

      with procedure Postprocess (Self : in out Document_Type) is null;
      --  Callback invoked on a node whenever the traversal is finished
      --  processing a node and its children. It can refine the given ``Self``
      --  document in place.

      with procedure Insert_Spacing
        (Self  : in out Document_Type;
         Kind  : Some_Spacing_Kind;
         Count : Positive) is null;
      --  Callback used to implement the modification of a document to insert
      --  spacing corresponding to ``Kind``/``Count`` before ``Self`` (or
      --  instead of ``Self`` it is an internal node whose purpose is to be
      --  expanded to spacing.

   procedure Process_Required_Spacing
     (Self : in out Document_Type; Max_Empty_Lines : Integer);
   --  Common implementation for the pass to detect broken groups, and the pass
   --  that inserts required spacing.
   --
   --  Both passes need to know where spacing (whitespaces, line breaks) is in
   --  the internal tree, and where to insert it when it is missing. Both
   --  passes also need to make the same decisions, so it makes sense to
   --  maintain a single implementation.
   --
   --  This procedure traverses the tree rooted as ``Self`` using depth-first
   --  search order.

   ------------------------
   -- To_Prettier_Symbol --
   ------------------------

   function To_Prettier_Symbol
     (Symbol_Map : in out Symbol_Maps.Vector;
      Symbol     : Template_Symbol) return Prettier.Symbol_Type is
   begin
      --  ``Symbol`` maps never track the special "no symbol" value: handle it
      --  manually here.

      if Symbol = No_Template_Symbol then
         return Prettier.No_Symbol;
      end if;

      --  Ensure that we have created a Prettier symbol for the requested
      --  ``Template_Symbol``.

      for S in Symbol_Map.Last_Index + 1 .. Symbol loop
         Symbol_Map.Append (Prettier.New_Symbol);
      end loop;

      return Symbol_Map (Symbol);
   end To_Prettier_Symbol;

   ----------
   -- Join --
   ----------

   function Join (Left, Right : Spacing_State) return Spacing_State is
   begin
      if Left.Last_Token /= Right.Last_Token then
         raise Program_Error;
      end if;
      return
        (In_Broken_Group => Left.In_Broken_Group
                            and then Right.In_Broken_Group,
         Expected        => Max_Spacing (Left.Expected, Right.Expected),
         Actual          => Min_Spacing (Left.Actual, Right.Actual),
         Last_Token      => Left.Last_Token);
   end Join;

   ------------------------------
   -- Process_Required_Spacing --
   ------------------------------

   procedure Process_Required_Spacing
     (Self : in out Document_Type; Max_Empty_Lines : Integer)
   is
      procedure Process
        (Self   : in out Document_Type;
         State  : in out Spacing_State;
         Breaks : out Boolean);
      --  Process ``Self`` and its children, potentially modifying it in place.
      --
      --  Upon return, ``State`` is updated to reflect the effect of ``Self``
      --  on the spacing state, and ``Break`` is set to whether this document
      --  breaks the parent group.

      -------------
      -- Process --
      -------------

      procedure Process
        (Self   : in out Document_Type;
         State  : in out Spacing_State;
         Breaks : out Boolean)
      is
         procedure Do_Break;
         --  Helper to break the parent group

         --------------
         -- Do_Break --
         --------------

         procedure Do_Break is
         begin
            Breaks := True;
            State.In_Broken_Group := True;
         end Do_Break;
      begin
         Breaks := False;

         Preprocess (Self, State);

         case Instantiated_Template_Document_Kind (Self.Kind) is
            when Align =>
               Process (Self.Align_Contents, State, Breaks);

            when Break_Parent =>
               Do_Break;

            when Expected_Line_Breaks =>
               Extend_Spacing
                 (State.Expected,
                  (Line_Breaks, Self.Expected_Line_Breaks_Count));

            when Expected_Whitespaces =>
               Extend_Spacing
                 (State.Expected,
                  (Whitespaces, Self.Expected_Whitespaces_Count));

            when Fill =>
               Process (Self.Fill_Document, State, Breaks);

            when Flush_Line_Breaks =>
               declare
                  Expected_Lines : constant Natural :=
                    Required_Line_Breaks (State.Expected, Max_Empty_Lines);
                  Actual_Lines   : constant Natural :=
                    Required_Line_Breaks (State.Actual, -1);
               begin
                  if Expected_Lines > Actual_Lines then
                     Insert_Spacing
                       (Self, Line_Breaks, Expected_Lines - Actual_Lines);
                     State.Actual := State.Expected;
                     Do_Break;
                  end if;
               end;

            when Group =>
               declare
                  Parent_Broken : constant Boolean := State.In_Broken_Group;
                  Inner_Breaks  : Boolean;
               begin
                  --  The fact that the parent group is broken does not imply
                  --  that this child group is broken: just pick the
                  --  information we have about it currently.

                  State.In_Broken_Group := Self.Group_Should_Break;
                  Process (Self.Group_Document, State, Inner_Breaks);

                  --  Restore broken state for the parent group. In addition,
                  --  the fact that the child group is broken does imply that
                  --  the parent is broken.

                  State.In_Broken_Group := Parent_Broken;
                  if Inner_Breaks then
                     Do_Break;
                     Self.Group_Should_Break := True;
                  end if;
               end;

            when Hard_Line =>
               Extend_Spacing (State.Actual, One_Line_Break_Spacing);
               Do_Break;

            when Hard_Line_Without_Break_Parent =>
               Extend_Spacing (State.Actual, One_Line_Break_Spacing);

            when If_Break =>
               declare
                  BS     : Spacing_State := State;
                  FS     : Spacing_State := State;
                  BB, FB : Boolean;
               begin
                  --  If this If_Break node is conditionned on its own
                  --  parent group, then the If_Break_Content part is known
                  --  to operate in a broken group.

                  if Self.If_Break_Group_Id = No_Template_Symbol then
                     BS.In_Broken_Group := True;
                  end if;

                  Process (Self.If_Break_Contents, BS, BB);
                  Process (Self.If_Break_Flat_Contents, FS, FB);

                  State := Join (BS, FS);
                  if BB and then FB then
                     Do_Break;
                  end if;
               end;

            when Indent =>
               Process (Self.Indent_Document, State, Breaks);

            when Line =>

               --  A Line command can be replaced by a line break or a space:
               --  unless we could prove that it would return into an actual
               --  line break, be conservative and consider its weakest form: a
               --  space.

               if State.In_Broken_Group then
                  Extend_Spacing (State.Actual, One_Line_Break_Spacing);
                  Do_Break;
               else
                  Extend_Spacing (State.Actual, One_Whitespace_Spacing);
               end if;

            when List =>
               for I in 1 .. Self.List_Documents.Last_Index loop
                  declare
                     Inner_Breaks : Boolean;
                     D            : Document_Type :=
                       Self.List_Documents.Element (I);
                  begin
                     Process (D, State, Inner_Breaks);
                     if Inner_Breaks then
                        Do_Break;
                     end if;
                     Self.List_Documents.Replace_Element (I, D);
                  end;
               end loop;

               --  Changes may allow to simplify the tree: remove empty list
               --  children, and simplify further if there is only a single
               --  element left.

               for I in reverse 1 .. Self.List_Documents.Last_Index loop
                  declare
                     D : constant Document_Type :=
                       Self.List_Documents.Element (I);
                  begin
                     if D.Kind = List and then D.List_Documents.Is_Empty then
                        Self.List_Documents.Delete (I);
                     end if;
                  end;
               end loop;

               if Self.List_Documents.Length = 1 then
                  Self := Self.List_Documents.First_Element;
               end if;

            when Literal_Line =>
               Extend_Spacing (State.Actual, One_Line_Break_Spacing);
               Do_Break;

            when Soft_Line =>

               --  A Soft_Line command can be replaced by line break or a
               --  space: unless we could prove that it would return into an
               --  actual line break, be conservative and consider its weakest
               --  form: nothing.

               if State.In_Broken_Group then
                  Extend_Spacing (State.Actual, One_Line_Break_Spacing);
                  Do_Break;
               end if;

            when Table =>
               for I in 1 .. Self.Table_Rows.Last_Index loop
                  declare
                     Inner_Breaks : Boolean;
                     Last         : constant Natural :=
                       Self.Table_Rows.Last_Index;
                     D            : Document_Type :=
                       Self.Table_Rows.Element (I);
                  begin
                     Process (D, State, Inner_Breaks);
                     Self.Table_Rows.Replace_Element (I, D);
                     if Inner_Breaks then
                        Do_Break;
                        Self.Table_Must_Break := True;
                     end if;

                     --  For breaking tables, all but the last row imply a
                     --  hard line break.

                     if Self.Table_Must_Break and then I < Last then
                        Extend_Spacing (State.Actual, One_Line_Break_Spacing);
                        Do_Break;
                     end if;
                  end;
               end loop;

            when Table_Separator | Token =>
               declare
                  Saved_Actual : constant Spacing_Type := State.Actual;
                  Required     : constant Spacing_Type :=
                    Max_Spacing
                       (Required_Spacing (State.Last_Token, Self.Token_Kind),
                        State.Expected);
               begin
                  State.Expected := No_Spacing;
                  State.Actual := No_Spacing;
                  State.Last_Token := Self.Token_Kind;

                  --  If we notice that there is no guarantee that this
                  --  token has the required spacing with the previous token
                  --  unparsed: insert the spacing that is missing.

                  if Saved_Actual < Required then
                     case Required.Kind is
                        when None =>
                           raise Program_Error;

                        when Whitespaces =>
                           declare
                              Last_Whitespaces : constant Natural :=
                                (case Saved_Actual.Kind is
                                 when None        => 0,
                                 when Whitespaces => Saved_Actual.Count,
                                 when Line_Breaks => raise Program_Error);
                           begin
                              Insert_Spacing
                                (Self,
                                 Whitespaces,
                                 Required.Count - Last_Whitespaces);
                           end;

                        when Line_Breaks =>
                           Insert_Spacing
                             (Self,
                              Line_Breaks,
                              Required.Count
                                - Required_Line_Breaks (Saved_Actual, -1));
                           Do_Break;
                     end case;
                  end if;
               end;

            when Trim =>
               null;

            when Whitespace =>
               Extend_Spacing (State.Actual, One_Whitespace_Spacing);
         end case;

         Postprocess (Self);
      end Process;

      State  : Spacing_State := Initial_Spacing_State;
      Breaks : Boolean;
   begin
      Process (Self, State, Breaks);
   end Process_Required_Spacing;

   ------------------
   -- Node_Matches --
   ------------------

   function Node_Matches
     (Node : Lk_Node; Types : Type_Vectors.Vector) return Boolean
   is
      N : constant Value_Ref := From_Node (Node.Language, Node);
   begin
      for T of Types loop
         if Type_Matches (N, T) then
            return True;
         end if;
      end loop;
      return False;
   end Node_Matches;

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
               Has_Definition => False,
               Is_Referenced  => False);
         begin
            Symbol_Map.Insert (Symbol, Info, Position, Inserted);
            pragma Assert (Inserted);
         end;
      end if;

      return Symbol_Map.Reference (Position);
   end Lookup;

   --------------------
   -- Declare_Symbol --
   --------------------

   function Declare_Symbol
     (Source_Name : Unbounded_String;
      Symbols     : Symbol_Table;
      Symbol_Map  : in out Symbol_Parsing_Maps.Map)
      return Some_Template_Symbol
   is
      Info : Symbol_Info renames Lookup (Source_Name, Symbols, Symbol_Map);
   begin
      --  Ensure that there is no conflicting symbol definition in this
      --  template.

      if Info.Has_Definition then
         raise Duplicate_Symbol_Definition;
      else
         Info.Has_Definition := True;
      end if;
      return Info.Template_Sym;
   end Declare_Symbol;

   ----------------------
   -- Reference_Symbol --
   ----------------------

   function Reference_Symbol
     (Source_Name : Unbounded_String;
      Symbols     : Symbol_Table;
      Symbol_Map  : in out Symbol_Parsing_Maps.Map)
      return Some_Template_Symbol
   is
      Info : Symbol_Info renames Lookup (Source_Name, Symbols, Symbol_Map);
   begin
      Info.Is_Referenced := True;
      return Info.Template_Sym;
   end Reference_Symbol;

   -------------------------
   -- Extract_Definitions --
   -------------------------

   function Extract_Definitions
     (Source : Symbol_Parsing_Maps.Map) return Symbol_Parsing_Maps.Map
   is
      use Symbol_Parsing_Maps;
   begin
      return Result : Map do
         for Cur in Source.Iterate loop
            declare
               K : constant Symbol_Type := Key (Cur);
               V : Symbol_Info := Element (Cur);
            begin
               if V.Has_Definition then
                  V.Is_Referenced := False;
                  Result.Insert (K, V);
               end if;
            end;
         end loop;
      end return;
   end Extract_Definitions;

   --------------------------
   -- To_Prettier_Document --
   --------------------------

   function To_Prettier_Document
     (Document : Document_Type) return Prettier.Document_Type
   is
      Symbol_Map : Symbol_Maps.Vector;
      --  Mapping between the symbols found in ``Document`` and the ones given
      --  to Prettier.

      function Recurse
        (Document : Document_Type) return Prettier.Document_Type;
      --  Actual transformation function to Prettier document, to be called
      --  recursively. ``To_Prettier_Document`` is just a wrapper.

      function "+" (Text : Unbounded_Text_Type) return Unbounded_String
      is (To_Unbounded_String (To_UTF8 (To_Text (Text))));

      function Text_For (Document : Document_Type) return Unbounded_String
      is (case Document.Kind is
          when Table_Separator | Token => +Document.Token_Text,
          when Whitespace              => Document.Whitespace_Length * ' ',
          when others                  => raise Program_Error);

      procedure Append_Flattened_List
        (Self : Document_Type; Dest : in out Document_Vector);
      --  If ``Self`` is not a list, just append its Prettier conversion to
      --  ``Dest``. Otherwise, recurse on each list item.

      -------------
      -- Recurse --
      -------------

      function Recurse
        (Document : Document_Type) return Prettier.Document_Type
      is
      begin
         case Final_Document_Kind (Document.Kind) is
            when Align =>
               return Align
                 (Data     => Document.Align_Data,
                  Contents => Recurse (Document.Align_Contents));

            when Break_Parent =>
               return Break_Parent;

            when Fill =>
               return Fill (Recurse (Document.Fill_Document));

            when Group =>
               return Group
                 (Documents    => Recurse (Document.Group_Document),
                  Should_Break => Document.Group_Should_Break,
                  Id           => To_Prettier_Symbol
                                    (Symbol_Map,
                                     Document.Group_Id));

            when Hard_Line =>
               return Hard_Line;

            when Hard_Line_Without_Break_Parent =>
               return Hard_Line_Without_Break_Parent;

            when If_Break =>
               return If_Break
                 (Recurse (Document.If_Break_Contents),
                  Recurse (Document.If_Break_Flat_Contents),
                  (Group_Id => To_Prettier_Symbol
                                 (Symbol_Map, Document.If_Break_Group_Id)));

            when Indent =>
               return Indent (Recurse (Document.Indent_Document));

            when Line =>
               return Line;

            when List =>

               --  Flatten nested lists, to avoid document bloat, and merge
               --  consecutive tokens.

               declare
                  Items : Document_Vector;
                  Text  : Unbounded_String;

                  procedure Process_List (Document : Document_Type);
                  procedure Flush_Text;

                  ------------------
                  -- Process_List --
                  ------------------

                  procedure Process_List (Document : Document_Type) is
                     D : Document_Type;
                  begin
                     for I in 1 .. Document.List_Documents.Last_Index loop
                        D := Document.List_Documents.Element (I);
                        if D.Kind = List then
                           Process_List (D);
                        elsif D.Kind in Token | Whitespace then
                           Append (Text, Text_For (D));
                        else
                           Flush_Text;
                           Items.Append (Recurse (D));
                        end if;
                     end loop;
                  end Process_List;

                  ----------------
                  -- Flush_Text --
                  ----------------

                  procedure Flush_Text is
                  begin
                     if Length (Text) > 0 then
                        Items.Append (Prettier.Builders.Text (Text));
                        Text := Null_Unbounded_String;
                     end if;
                  end Flush_Text;

               begin
                  Process_List (Document);
                  Flush_Text;
                  return List (Items);
               end;

            when Literal_Line =>
               return Literal_Line;

            when Soft_Line =>
               return Soft_Line;

            when Table =>
               declare
                  Rows : Document_Table;
               begin
                  for I in 1 .. Document.Table_Rows.Last_Index loop
                     declare
                        Row : Document_Vector;
                     begin
                        Append_Flattened_List (Document.Table_Rows (I), Row);
                        Rows.Append (Row);
                     end;
                  end loop;
                  return Alignment_Table (Rows, Document.Table_Must_Break);
               end;

            when Table_Separator =>
               return Alignment_Table_Separator (Text_For (Document));

            when Token | Whitespace =>
               return Prettier.Builders.Text (Text_For (Document));

            when Trim =>
               return Trim;
         end case;
      end Recurse;

      ---------------------------
      -- Append_Flattened_List --
      ---------------------------

      procedure Append_Flattened_List
        (Self : Document_Type; Dest : in out Document_Vector) is
      begin
         if Self.Kind = List then
            for I in 1 .. Self.List_Documents.Last_Index loop
               Append_Flattened_List (Self.List_Documents (I), Dest);
            end loop;
         else
            Dest.Append (Recurse (Self));
         end if;
      end Append_Flattened_List;

   begin
      return Recurse (Document);
   end To_Prettier_Document;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Document_Pool) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Document_Record, Document_Type);
   begin
      for Document of Self loop
         Free (Document);
      end loop;
      Self.Clear;
   end Release;

   --------------
   -- Register --
   --------------

   procedure Register (Self : in out Document_Pool; Document : Document_Type)
   is
   begin
      Self.Append (Document);
   end Register;

   ------------------
   -- Create_Align --
   ------------------

   function Create_Align
     (Self     : in out Document_Pool;
      Data     : Prettier.Alignment_Data_Type;
      Contents : Document_Type) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind           => Align,
           Align_Data     => Data,
           Align_Contents => Contents)
      do
         Self.Register (Result);
      end return;
   end Create_Align;

   -------------------------
   -- Create_Break_Parent --
   -------------------------

   function Create_Break_Parent
     (Self : in out Document_Pool) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => Break_Parent)
      do
         Self.Register (Result);
      end return;
   end Create_Break_Parent;

   ---------------------------------
   -- Create_Expected_Line_Breaks --
   ---------------------------------

   function Create_Expected_Line_Breaks
     (Self : in out Document_Pool; Count : Positive) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind => Expected_Line_Breaks, Expected_Line_Breaks_Count => Count)
      do
         Self.Register (Result);
      end return;
   end Create_Expected_Line_Breaks;

   ---------------------------------
   -- Create_Expected_Whitespaces --
   ---------------------------------

   function Create_Expected_Whitespaces
     (Self : in out Document_Pool; Count : Positive) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind => Expected_Whitespaces, Expected_Whitespaces_Count => Count)
      do
         Self.Register (Result);
      end return;
   end Create_Expected_Whitespaces;

   -----------------
   -- Create_Fill --
   -----------------

   function Create_Fill
     (Self     : in out Document_Pool;
      Document : Document_Type) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind          => Fill,
           Fill_Document => Document)
      do
         Self.Register (Result);
      end return;
   end Create_Fill;

   ------------------------------
   -- Create_Flush_Line_Breaks --
   ------------------------------

   function Create_Flush_Line_Breaks
     (Self : in out Document_Pool) return Document_Type
   is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => Flush_Line_Breaks)
      do
         Self.Register (Result);
      end return;
   end Create_Flush_Line_Breaks;

   ------------------
   -- Create_Group --
   ------------------

   function Create_Group
     (Self         : in out Document_Pool;
      Document     : Document_Type;
      Should_Break : Boolean;
      Id           : Template_Symbol) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind               => Group,
           Group_Document     => Document,
           Group_Should_Break => Should_Break,
           Group_Id           => Id)
      do
         Self.Register (Result);
      end return;
   end Create_Group;

   ----------------------
   -- Create_Hard_Line --
   ----------------------

   function Create_Hard_Line (Self : in out Document_Pool) return Document_Type
   is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => Hard_Line)
      do
         Self.Register (Result);
      end return;
   end Create_Hard_Line;

   -------------------------------------------
   -- Create_Hard_Line_Without_Break_Parent --
   -------------------------------------------

   function Create_Hard_Line_Without_Break_Parent
     (Self : in out Document_Pool) return Document_Type
   is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => Hard_Line_Without_Break_Parent)
      do
         Self.Register (Result);
      end return;
   end Create_Hard_Line_Without_Break_Parent;

   ---------------------
   -- Create_If_Break --
   ---------------------

   function Create_If_Break
     (Self          : in out Document_Pool;
      Contents      : Document_Type;
      Flat_Contents : Document_Type := null;
      Group_Id      : Template_Symbol := No_Template_Symbol)
      return Document_Type
   is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind                   => If_Break,
           If_Break_Contents      => Contents,
           If_Break_Flat_Contents =>
             (if Flat_Contents = null
              then Self.Create_Empty_List
              else Flat_Contents),
           If_Break_Group_Id      => Group_Id)
      do
         Self.Register (Result);
      end return;
   end Create_If_Break;

   ---------------------
   -- Create_If_Empty --
   ---------------------

   function Create_If_Empty
     (Self          : in out Document_Pool;
      Then_Contents : Document_Type;
      Else_Contents : Document_Type) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind          => If_Empty,
           If_Empty_Then => Then_Contents,
           If_Empty_Else => Else_Contents)
      do
         Self.Register (Result);
      end return;
   end Create_If_Empty;

   --------------------
   -- Create_If_Kind --
   --------------------

   function Create_If_Kind
     (Self             : in out Document_Pool;
      If_Kind_Field    : Struct_Member_Ref;
      If_Kind_Matchers : in out Matcher_Vectors.Vector;
      If_Kind_Default  : Document_Type;
      If_Kind_Null     : Document_Type) return Document_Type
   is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind             => If_Kind,
           If_Kind_Field    => If_Kind_Field,
           If_Kind_Matchers => Matcher_Vectors.Empty_Vector,
           If_Kind_Default  => If_Kind_Default,
           If_Kind_Null     => If_Kind_Null)
      do
         Result.If_Kind_Matchers.Move (If_Kind_Matchers);
         Self.Register (Result);
      end return;
   end Create_If_Kind;

   -------------------
   -- Create_Indent --
   -------------------

   function Create_Indent
     (Self     : in out Document_Pool;
      Document : Document_Type) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind            => Indent,
           Indent_Document => Document)
      do
         Self.Register (Result);
      end return;
   end Create_Indent;

   -----------------
   -- Create_Line --
   -----------------

   function Create_Line (Self : in out Document_Pool) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => Line)
      do
         Self.Register (Result);
      end return;
   end Create_Line;

   -----------------
   -- Create_List --
   -----------------

   function Create_List
     (Self      : in out Document_Pool;
      Documents : in out Document_Vectors.Vector) return Document_Type is
   begin
      if Documents.Length = 1 then
         return Documents.Element (1);
      end if;

      return Result : constant Document_Type :=
        new Document_Record (Kind => List)
      do
         Result.List_Documents.Move (Documents);
         Self.Register (Result);
      end return;
   end Create_List;

   -------------------------
   -- Create_Literal_Line --
   -------------------------

   function Create_Literal_Line
     (Self : in out Document_Pool) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => Literal_Line)
      do
         Self.Register (Result);
      end return;
   end Create_Literal_Line;

   -----------------------
   -- Create_Empty_List --
   -----------------------

   function Create_Empty_List
     (Self : in out Document_Pool) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => List)
      do
         Self.Register (Result);
      end return;
   end Create_Empty_List;

   --------------------
   -- Create_Recurse --
   --------------------

   function Create_Recurse (Self : in out Document_Pool) return Document_Type
   is
   begin
      return Result : constant Document_Type := new Document_Record (Recurse)
      do
         Self.Register (Result);
      end return;
   end Create_Recurse;

   --------------------
   -- Create_Recurse --
   --------------------

   function Create_Recurse (Self : in out Document_Pool) return Template_Type
   is
   begin
      return
        (Kind    => With_Recurse,
         Root    => Self.Create_Recurse,
         Symbols => Symbol_Parsing_Maps.Empty_Map);
   end Create_Recurse;

   --------------------------
   -- Create_Recurse_Field --
   --------------------------

   function Create_Recurse_Field
     (Self     : in out Document_Pool;
      Field    : Struct_Member_Ref;
      Position : Positive) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind                   => Recurse_Field,
           Recurse_Field_Ref      => Field,
           Recurse_Field_Position => Position)
      do
         Self.Register (Result);
      end return;
   end Create_Recurse_Field;

   ----------------------------
   -- Create_Recurse_Flatten --
   ----------------------------

   function Create_Recurse_Flatten
     (Self  : in out Document_Pool;
      Types : in out Type_Vectors.Vector) return Document_Type
   is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Recurse_Flatten)
      do
         Result.Recurse_Flatten_Types.Move (Types);
         Self.Register (Result);
      end return;
   end Create_Recurse_Flatten;

   ----------------------
   -- Create_Soft_Line --
   ----------------------

   function Create_Soft_Line (Self : in out Document_Pool) return Document_Type
   is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => Soft_Line)
      do
         Self.Register (Result);
      end return;
   end Create_Soft_Line;

   ------------------
   -- Create_Table --
   ------------------

   function Create_Table
     (Self       : in out Document_Pool;
      Rows       : in out Document_Vectors.Vector;
      Must_Break : Boolean) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => Table)
      do
         Result.Table_Rows.Move (Rows);
         Result.Table_Must_Break := Must_Break;
         Self.Register (Result);
      end return;
   end Create_Table;

   ----------------------------
   -- Create_Table_Separator --
   ----------------------------

   function Create_Table_Separator
     (Self : in out Document_Pool;
      Kind : Token_Kind_Ref;
      Text : Unbounded_Text_Type) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind       => Table_Separator,
           Token_Kind => Kind,
           Token_Text => Text)
      do
         Self.Register (Result);
      end return;
   end Create_Table_Separator;

   ------------------
   -- Create_Token --
   ------------------

   function Create_Token
     (Self : in out Document_Pool;
      Kind : Token_Kind_Ref;
      Text : Unbounded_Text_Type) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind       => Token,
           Token_Kind => Kind,
           Token_Text => Text)
      do
         Self.Register (Result);
      end return;
   end Create_Token;

   -----------------
   -- Create_Trim --
   -----------------

   function Create_Trim (Self : in out Document_Pool) return Document_Type is
   begin
      return Result : constant Document_Type :=
        new Document_Record (Kind => Trim)
      do
         Self.Register (Result);
      end return;
   end Create_Trim;

   -----------------------
   -- Create_Whitespace --
   -----------------------

   function Create_Whitespace
     (Self : in out Document_Pool; Length : Positive := 1) return Document_Type
   is
   begin
      return Result : constant Document_Type :=
        new Document_Record'
          (Kind              => Whitespace,
           Whitespace_Length => Length)
      do
         Self.Register (Result);
      end return;
   end Create_Whitespace;

   --------------------------
   -- Detect_Broken_Groups --
   --------------------------

   procedure Detect_Broken_Groups
     (Self : in out Document_Type; Max_Empty_Lines : Integer)
   is
      procedure Process is new Process_Required_Spacing;
   begin
      Process (Self, Max_Empty_Lines);
      Dump (Self, Broken_Groups_Trace);
   end Detect_Broken_Groups;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Document : Document_Type; Trace : GNATCOLL.Traces.Trace_Handle := null)
   is
      use type GNATCOLL.Traces.Trace_Handle;

      Simple_Indent : constant Unbounded_String := To_Unbounded_String ("  ");
      List_Indent   : constant Unbounded_String := To_Unbounded_String ("| ");

      procedure Write (S : Unbounded_String);
      procedure Process (Document : Document_Type; Prefix : Unbounded_String);

      -----------
      -- Write --
      -----------

      procedure Write (S : Unbounded_String) is
      begin
         if Trace = null then
            Put_Line (To_String (S));
         else
            Trace.Trace (To_String (S));
         end if;
      end Write;

      -------------
      -- Process --
      -------------

      procedure Process (Document : Document_Type; Prefix : Unbounded_String)
      is
      begin
         if Document = null then
            Write (Prefix & "<none>");
            return;
         end if;
         case Document.Kind is
            when Align =>
               Write (Prefix & "align:");
               declare
                  Line : Unbounded_String;
               begin
                  Line := Prefix & Simple_Indent & "data: ";
                  case Document.Align_Data.Kind is
                     when Prettier.Width =>
                        Append
                          (Line,
                           "width (number):" & Document.Align_Data.N'Image);
                     when Prettier.Text =>
                        Append
                          (Line,
                           "width (text):"
                           & To_String (Document.Align_Data.T));
                     when Prettier.Dedent =>
                        Append (Line, "dedent");
                     when Prettier.Dedent_To_Root =>
                        Append (Line, "dedentToRoot");
                     when Prettier.Root =>
                        Append (Line, "markAsRoot");
                     when Prettier.Inner_Root =>
                        Append (Line, "innerRoot");
                     when Prettier.Continuation_Line_Indent =>
                        Append (Line, "continuationLineIndent");
                     when Prettier.None =>
                        Append (Line, "none");
                  end case;
                  Write (Line);
               end;
               Process (Document.Align_Contents, Prefix & Simple_Indent);

            when Break_Parent =>
               Write (Prefix & "breakParent");

            when Expected_Line_Breaks =>
               Write
                 (Prefix & "expectedLineBreaks:"
                  & Document.Expected_Line_Breaks_Count'Image);

            when Expected_Whitespaces =>
               Write
                 (Prefix & "expectedWhitespaces:"
                  & Document.Expected_Whitespaces_Count'Image);

            when Flush_Line_Breaks =>
               Write (Prefix & "flushLineBreaks");

            when Fill =>
               Write (Prefix & "fill:");
               Process (Document.Fill_Document, Prefix & Simple_Indent);

            when Group =>
               Write (Prefix & "group:");
               Write
                 (Prefix & Simple_Indent & "shouldBreak: "
                  & Document.Group_Should_Break'Image);
               Write
                 (Prefix & Simple_Indent & "id:" & Document.Group_Id'Image);
               Process (Document.Group_Document, Prefix & Simple_Indent);

            when Hard_Line =>
               Write (Prefix & "hardline");

            when Hard_Line_Without_Break_Parent =>
               Write (Prefix & "hardlineWithoutBreakParent");

            when If_Break =>
               Write (Prefix & "ifBreak:");
               if Document.If_Break_Group_Id /= No_Template_Symbol then
                  Write
                    (Prefix & Simple_Indent & "groupId: "
                     & Document.If_Break_Group_Id'Image);
               end if;
               Process (Document.If_Break_Contents, Prefix & List_Indent);
               Process (Document.If_Break_Flat_Contents, Prefix & List_Indent);

            when If_Empty =>
               Write (Prefix & "ifEmpty:");
               Process (Document.If_Empty_Then, Prefix & List_Indent);
               Process (Document.If_Empty_Else, Prefix & List_Indent);

            when If_Kind =>
               Write (Prefix & "ifKind:");
               Write (Prefix & Simple_Indent & "default:");
               Process
                 (Document.If_Kind_Default,
                  Prefix & Simple_Indent & Simple_Indent);
               Write (Prefix & Simple_Indent & "null:");
               Process
                 (Document.If_Kind_Null,
                  Prefix & Simple_Indent & Simple_Indent);
               Write (Prefix & Simple_Indent & "matchers:");
               declare
                  Matcher_Kind_Indent     : constant Unbounded_String :=
                    Prefix & Simple_Indent & Simple_Indent;
                  Matcher_Document_Indent : constant Unbounded_String :=
                    Prefix & Simple_Indent & Simple_Indent & Simple_Indent;

               begin
                  for J in
                    Document.If_Kind_Matchers.First_Index
                    .. Document.If_Kind_Matchers.Last_Index
                  loop
                     Write
                       (Matcher_Kind_Indent
                        & Debug_Name
                            (Document
                               .If_Kind_Matchers
                               .Element (J)
                               .Matched_Type));
                     Process
                       (Document.If_Kind_Matchers.Element (J).Document,
                        Matcher_Document_Indent);
                  end loop;
               end;

            when Indent =>
               Write (Prefix & "indent:");
               Process (Document.Indent_Document, Prefix & Simple_Indent);

            when Line =>
               Write (Prefix & "line");

            when List =>
               Write (Prefix & "list:");
               for I in 1 .. Document.List_Documents.Last_Index loop
                  Process
                    (Document.List_Documents.Element (I),
                     Prefix & List_Indent);
               end loop;

            when Literal_Line =>
               Write (Prefix & "literalline");

            when Recurse =>
               Write (Prefix & "recurse");

            when Recurse_Field =>
               Write
                 (Prefix & "recurse_field: "
                  & Debug_Name (Document.Recurse_Field_Ref));

            when Recurse_Flatten =>
               Write (Prefix & "recurse_flatten:");
               for I in 1 .. Document.Recurse_Flatten_Types.Last_Index loop
                  declare
                     T : constant Type_Ref :=
                       Document.Recurse_Flatten_Types.Element (I);
                  begin
                     Write (Prefix & Simple_Indent & Debug_Name (T));
                  end;
               end loop;

            when Soft_Line =>
               Write (Prefix & "softline");

            when Table =>
               Write (Prefix & "table:");
               Write
                 (Prefix & Simple_Indent & "must_break: "
                  & Document.Table_Must_Break'Image);
               for I in 1 .. Document.Table_Rows.Last_Index loop
                  Write (Prefix & Simple_Indent & "row:");
                  Process
                    (Document.Table_Rows.Element (I),
                     Prefix & Simple_Indent & List_Indent);
               end loop;

            when Table_Separator | Token =>
               declare
                  Label      : constant String :=
                    (case Document.Kind is
                     when Table_Separator => "tableSeparator",
                     when Token           => "token",
                     when others          => raise Program_Error);
                  Token_Name : constant Name_Type :=
                    Token_Kind_Name (Document.Token_Kind);
               begin
                  Write
                    (Prefix & Label & "["
                     & Image (Format_Name (Token_Name, Camel)) & "]: "
                     & Image (To_Text (Document.Token_Text)));
               end;

            when Trim =>
               Write (Prefix & "trim");

            when Whitespace =>
               Write
                 (Prefix & "whitespace(" & Document.Whitespace_Length'Image
                  & ")");
         end case;
      end Process;
   begin
      if Trace /= null and then not Trace.Is_Active then
         return;
      end if;
      Process (Document, Null_Unbounded_String);
   end Dump;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Spacing_Type) return Boolean is
   begin
      if Left.Kind < Right.Kind then
         return True;
      elsif Left.Kind > Right.Kind then
         return False;
      end if;

      case Left.Kind is
         when None =>
            return False;
         when Whitespaces | Line_Breaks =>
            return Left.Count < Right.Count;
      end case;
   end "<";

   ----------------------
   -- Required_Spacing --
   ----------------------

   function Required_Spacing (Left, Right : Token_Kind_Ref) return Spacing_Type
   is
   begin
      if Left = No_Token_Kind_Ref then
         return No_Spacing;
      elsif Language (Left) /= Language (Right) then
         raise Precondition_Failure with
           "inconsistent languages for requested token kinds";
      end if;

      declare
         Id : constant Language_Descriptor_Access := +Language (Left);
         LK : constant Token_Kind_Index := To_Index (Left);

         function Family (Kind : Token_Kind_Index) return Token_Family_Index
         is (Id.Token_Kinds (Kind).Family);
      begin
         --  If a line break is required after Left, we do not even need to
         --  check what Right is.

         if Id.Unparsers.Token_Newlines (LK) then
            return One_Line_Break_Spacing;

         --  Otherwise, check if at least a space is required between Left and
         --  Right.

         elsif Id.Unparsers.Token_Spacings
                 (Family (LK), Family (To_Index (Right)))
         then
            return One_Whitespace_Spacing;

         else
            return No_Spacing;
         end if;
      end;
   end Required_Spacing;

   --------------------
   -- Extend_Spacing --
   --------------------

   procedure Extend_Spacing
     (Self : in out Spacing_Type; Requirement : Spacing_Type) is
   begin
      if Self.Kind < Requirement.Kind then
         Self := Requirement;

      elsif Self.Kind = Requirement.Kind then
         case Self.Kind is
            when None =>
               null;

            when Whitespaces | Line_Breaks =>
               Self.Count := Self.Count + Requirement.Count;
         end case;
      end if;
   end Extend_Spacing;

   --------------------------
   -- Required_Line_Breaks --
   --------------------------

   function Required_Line_Breaks
     (Self : Spacing_Type; Max_Empty_Lines : Integer) return Natural
   is
      Count : constant Natural :=
        (case Self.Kind is
         when None | Whitespaces => 0,
         when Line_Breaks        => Self.Count);
   begin
      return
        (if Max_Empty_Lines < 0
         then Count
         else Natural'Min (Max_Empty_Lines + 1, Count));
   end Required_Line_Breaks;

   -----------------------------
   -- Insert_Required_Spacing --
   -----------------------------

   procedure Insert_Required_Spacing
     (Pool            : in out Document_Pool;
      Document        : in out Document_Type;
      Max_Empty_Lines : Integer)
   is
      procedure Preprocess
        (Self : in out Document_Type; State : Spacing_State);
      --  Callback for ``Process_Required_Spacing``

      procedure Postprocess (Self : in out Document_Type);
      --  Callback for ``Process_Required_Spacing``

      procedure Insert_Spacing
        (Self  : in out Document_Type;
         Kind  : Some_Spacing_Kind;
         Count : Positive);
      --  Callback for ``Process_Required_Spacing``

      ----------------
      -- Preprocess --
      ----------------

      procedure Preprocess
        (Self : in out Document_Type; State : Spacing_State) is
      begin
         case Instantiated_Template_Document_Kind (Self.Kind) is
            when If_Break =>

               --  If this If_Break node is conditionned on its own parent
               --  group and if we know that this parent group is broken,
               --  simplify the tree: just keep the "broken group" branch.

               if Self.If_Break_Group_Id = No_Template_Symbol
                  and then State.In_Broken_Group
               then
                  Self := Self.If_Break_Contents;
               end if;

            when others =>
               null;
         end case;
      end Preprocess;

      -----------------
      -- Postprocess --
      -----------------

      procedure Postprocess (Self : in out Document_Type) is
      begin
         if Self.Kind not in Final_Document_Kind then
            Self := Pool.Create_Empty_List;
         end if;
      end Postprocess;

      --------------------
      -- Insert_Spacing --
      --------------------

      procedure Insert_Spacing
        (Self  : in out Document_Type;
         Kind  : Some_Spacing_Kind;
         Count : Positive)
      is
         Items : Document_Vectors.Vector;
      begin
         case Kind is
            when Whitespaces =>
               Items.Append (Pool.Create_Whitespace (Count));

            when Line_Breaks =>
               for Dummy in 1 .. Count loop
                  Items.Append (Pool.Create_Hard_Line);
               end loop;
         end case;

         --  If the spacing is meant to be inserted as the expansion of an
         --  intermediate node (not an actual Prettier document), do not
         --  preserve that node.

         if Self.Kind in Final_Document_Kind then
            Items.Append (Self);
         end if;

         Self := Pool.Create_List (Items);
      end Insert_Spacing;

      procedure Process is new Process_Required_Spacing
        (Preprocess, Postprocess, Insert_Spacing);

   begin
      Detect_Broken_Groups (Document, Max_Empty_Lines);
      Process (Document, Max_Empty_Lines);
   end Insert_Required_Spacing;

end Langkit_Support.Prettier_Utils;
