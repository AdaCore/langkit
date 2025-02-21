with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
pragma Warnings (Off, "internal");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "internal");

with GNATCOLL.Iconv;

with Liblktlang_Support.Errors;   use Liblktlang_Support.Errors;
use Liblktlang_Support.Errors.Unparsing;
with Liblktlang_Support.Internal; use Liblktlang_Support.Internal;
with Liblktlang_Support.Internal.Analysis;
use Liblktlang_Support.Internal.Analysis;
with Liblktlang_Support.Internal.Conversions;
use Liblktlang_Support.Internal.Conversions;
with Liblktlang_Support.Internal.Descriptor;
use Liblktlang_Support.Internal.Descriptor;
with Liblktlang_Support.Internal.Unparsing;
use Liblktlang_Support.Internal.Unparsing;
with Liblktlang_Support.Token_Data_Handlers;
use Liblktlang_Support.Token_Data_Handlers;

package body Liblktlang_Support.Rewriting.Unparsing is

   type Unparsing_Tables is record
      Token_Kinds         : Token_Kind_Descriptor_Array_Access;
      Token_Spacings      : Token_Spacing_Table;
      Token_Newlines      : Token_Newline_Table;
      Node_Unparsers      : Node_Unparser_Map;
   end record;

   function Unparsing_Tables_From_Node
     (Node : Abstract_Node) return Unparsing_Tables;
   --  Return unparsing tables corresponding to the given node

   procedure Apply_Spacing_Rules
     (Tables     : Unparsing_Tables;
      Buffer     : in out Unparsing_Buffer;
      Next_Token : Token_Kind_Index);
   --  Add a whitespace or a newline to buffer if mandated by spacing rules
   --  given the next token to emit.

   --  The "template" data structures below are helpers for the original
   --  source code formatting preservation logic. A template can be thought as
   --  the instantiation of an unparser from an original AST node. It captures
   --  actual sequences of tokens.

   type Token_Sequence_Template (Present : Boolean := False) is record
      case Present is
         when False => null;
         when True =>
            First, Last : Lk_Token;
      end case;
   end record;
   --  Captured sequence of tokens

   subtype Present_Token_Sequence_Template is Token_Sequence_Template (True);

   Empty_Token_Sequence_Template : constant Present_Token_Sequence_Template :=
     (Present => True, First => No_Lk_Token, Last => No_Lk_Token);

   function Create_Token_Sequence
     (Unparser    : Token_Sequence;
      First_Token : in out Lk_Token) return Present_Token_Sequence_Template
      with Pre => not First_Token.Is_Null;
   --  Create a sequence of tokens starting from First_Token and containing the
   --  same number of tokens as indicated in Unparser. Before returning, this
   --  updates First_Token to point at the first token that appears after the
   --  sequence.

   type Token_Sequence_Template_Array is
      array (Positive range <>) of Present_Token_Sequence_Template;

   type Field_Template (Present : Boolean := False) is record
      case Present is
         when False => null;
         when True =>
            Pre_Tokens, Post_Tokens : Present_Token_Sequence_Template;
      end case;
   end record;
   --  Captured sequences of tokens before and after a node field. This is the
   --  instantiation of Field_Unparser.

   type Field_Template_Array is array (Positive range <>) of Field_Template;

   type Regular_Node_Template (Present : Boolean; Count : Natural) is record
      case Present is
         when False => null;
         when True =>
            Pre_Tokens   : Present_Token_Sequence_Template;
            Fields       : Field_Template_Array (1 .. Count);
            Inter_Tokens : Token_Sequence_Template_Array (1 .. Count);
            Post_Tokens  : Present_Token_Sequence_Template;
      end case;
   end record;
   --  Captured sequences of tokens corresponding to a regular node. This is
   --  the instantiation of Regular_Node_Unparser.

   function Extract_Regular_Node_Template
     (Unparser       : Regular_Node_Unparser;
      Rewritten_Node : Lk_Node) return Regular_Node_Template;
   --  Return the regular node template corresponding to the instatiation of
   --  Rewritten_Node according to Unparser.
   --
   --  This is an absent template if Rewritten_Node is null. Likewise, returned
   --  field templates are absent if the corresponding Rewritten_Node children
   --  are absent.

   function Field_Present
     (Node : Abstract_Node; Unparser : Field_Unparser_Impl) return Boolean;
   --  Return whether the given field is to be considered present according to
   --  the given field unparser.

   procedure Unparse_Node
     (Tables              : Unparsing_Tables;
      Node                : Abstract_Node;
      Preserve_Formatting : Boolean;
      Result              : in out Unparsing_Buffer);
   --  Using the node unparsing tables, unparse the given Node

   procedure Unparse_Regular_Node
     (Tables              : Unparsing_Tables;
      Node                : Abstract_Node;
      Unparser            : Regular_Node_Unparser;
      Rewritten_Node      : Lk_Node;
      Preserve_Formatting : Boolean;
      Result              : in out Unparsing_Buffer);
   --  Helper for Unparse_Node, focuses on regular nodes

   procedure Unparse_List_Node
     (Tables              : Unparsing_Tables;
      Node                : Abstract_Node;
      Unparser            : List_Node_Unparser;
      Rewritten_Node      : Lk_Node;
      Preserve_Formatting : Boolean;
      Result              : in out Unparsing_Buffer);
   --  Helper for Unparse_Node, focuses on list nodes

   procedure Unparse_Token
     (Tables   : Unparsing_Tables;
      Unparser : Token_Unparser_Impl;
      Result   : in out Unparsing_Buffer);
   --  Using the Unparser unparsing table, unparse a token

   procedure Unparse_Token_Sequence
     (Tables   : Unparsing_Tables;
      Unparser : Token_Sequence_Impl;
      Result   : in out Unparsing_Buffer);
   --  Using the Unparser unparsing table, unparse a sequence of tokens

   function Last_Token_Index (Token : Lk_Token) return Token_Index;
   --  Return the index of the last token in ``Token``'s TDH

   function Relative_Token
     (Token : Lk_Token; Offset : Integer) return Lk_Token with
      Pre => not Token.Is_Null
             and then not Token.Is_Trivia
             and then Token_Index (Integer (Token.Index) + Offset)
                      in First_Token_Index .. Last_Token_Index (Token),
      Post => not Relative_Token'Result.Is_Null;
   --  Considering only tokens that are not trivia and assuming Token is at
   --  index I, return the token that is at index I + Offset.

   function Last_Trivia (Token : Lk_Token) return Lk_Token
      with Pre => not Token.Is_Null;
   --  If Token (which can be a token or a trivia) is followed by a sequence of
   --  trivias, return the last of them. Otherwise, return Token itself.

   procedure Append_Tokens
     (Tables                  : Unparsing_Tables;
      Result                  : in out Unparsing_Buffer;
      First_Token, Last_Token : Lk_Token;
      With_Trailing_Trivia    : Boolean := True);
   --  Emit to Result the sequence of tokens from First_Token to Last_Token.
   --  Trivias that appear between tokens in the sequence to emit are emitted
   --  as well. If With_Trailing_Trivia is true, also emit the sequence of
   --  trivia that follows Last_Token.

   procedure Append_Tokens
     (Tables   : Unparsing_Tables;
      Result   : in out Unparsing_Buffer;
      Template : Token_Sequence_Template);
   --  Emit to Result the sequence of tokens in Template, or do nothing if the
   --  template is absent.

   --------------------------------
   -- Unparsing_Tables_From_Node --
   --------------------------------

   function Unparsing_Tables_From_Node
     (Node : Abstract_Node) return Unparsing_Tables
   is
      Lang : constant Language_Id :=
        (case Node.Kind is
         when From_Parsing   => Node.Parsing_Node.Language,
         when From_Rewriting =>
           Node.Rewriting_Node.Context_Handle.Context.Language);
      Desc : Language_Descriptor renames "+" (Lang).all;
   begin
      return (Desc.Token_Kinds,
              Desc.Unparsers.Token_Spacings,
              Desc.Unparsers.Token_Newlines,
              Desc.Unparsers.Node_Unparsers);
   end Unparsing_Tables_From_Node;

   ----------------------
   -- Last_Token_Index --
   ----------------------

   function Last_Token_Index (Token : Lk_Token) return Token_Index is
      Unused_Id : Any_Language_Id;
      T         : Internal_Token;
      Unused_SN : Token_Safety_Net;
   begin
      Unwrap_Token (Token, Unused_Id, T, Unused_SN);
      return Last_Token (T.TDH.all);
   end Last_Token_Index;

   --------------------------------
   -- Abstract_Node_From_Parsing --
   --------------------------------

   function Abstract_Node_From_Parsing
     (Parsing_Node : Lk_Node) return Abstract_Node is
   begin
      return (From_Parsing, Parsing_Node);
   end Abstract_Node_From_Parsing;

   ----------------------------------
   -- Abstract_Node_From_Rewriting --
   ----------------------------------

   function Abstract_Node_From_Rewriting
     (Rewriting_Node : Node_Rewriting_Handle_Access) return Abstract_Node is
   begin
      return (From_Rewriting, Rewriting_Node);
   end Abstract_Node_From_Rewriting;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Node : Abstract_Node) return Boolean is
   begin
      case Node.Kind is
         when From_Parsing =>
            return Node.Parsing_Node.Is_Null;
         when From_Rewriting =>
            return Node.Rewriting_Node = null;
      end case;
   end Is_Null;

   -------------
   -- Type_Of --
   -------------

   function Type_Of (Node : Abstract_Node) return Type_Ref is
   begin
      case Node.Kind is
         when From_Parsing =>
            return Type_Of (Node.Parsing_Node);
         when From_Rewriting =>
            return Node.Rewriting_Node.Kind;
      end case;
   end Type_Of;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Node : Abstract_Node) return Natural is
   begin
      case Node.Kind is
         when From_Parsing =>
            return Node.Parsing_Node.Children_Count;
         when From_Rewriting =>
            return Node.Rewriting_Node.Children_Count;
      end case;
   end Children_Count;

   -----------
   -- Child --
   -----------

   function Child (Node : Abstract_Node; Index : Positive) return Abstract_Node
   is
   begin
      case Node.Kind is
         when From_Parsing =>
            return Abstract_Node_From_Parsing
                     (Node.Parsing_Node.Child (Index));

         when From_Rewriting =>

            --  If this node is unexpanded, do not bother expanding it just for
            --  the sake of fetching one of its children: just fetch the
            --  original node's child.

            declare
               RN : constant Node_Rewriting_Handle_Access :=
                 Node.Rewriting_Node;
            begin
               if RN.Children.Kind = Unexpanded then
                  return Abstract_Node_From_Parsing (RN.Node.Child (Index));
               else
                  return Abstract_Node_From_Rewriting
                           (RN.Children.Vector.Element (Index));
               end if;
            end;
      end case;
   end Child;

   ------------------
   -- Iterate_List --
   ------------------

   function Iterate_List (Node : Abstract_Node) return Abstract_Cursor is
   begin
      case Node.Kind is
         when From_Parsing =>
            return
              (Kind             => From_Parsing,
               Parsing_List     => Node.Parsing_Node,
               Next_Child_Index => 1);

         when From_Rewriting =>

            --  If this node is unexpanded, do not bother expanding it just for
            --  the sake of iterating on it: just iterate on the original node.

            declare
               RN : constant Node_Rewriting_Handle_Access :=
                 Node.Rewriting_Node;
            begin
               if RN.Children.Kind = Unexpanded then
                  return Abstract_Node_From_Parsing (RN.Node).Iterate_List;
               else
                  return
                    (Kind            => From_Rewriting,
                     Rewriting_Child => RN.Children.First);
               end if;
            end;
      end case;
   end Iterate_List;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Cursor : Abstract_Cursor) return Boolean is
   begin
      case Cursor.Kind is
         when From_Parsing =>
            return Cursor.Next_Child_Index
                   <= Cursor.Parsing_List.Children_Count;

         when From_Rewriting =>
            return Cursor.Rewriting_Child /= null;
      end case;
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element (Cursor : Abstract_Cursor) return Abstract_Node is
   begin
      case Cursor.Kind is
         when From_Parsing =>
            return
              Abstract_Node_From_Parsing
                (Cursor.Parsing_List.Child (Cursor.Next_Child_Index));

         when From_Rewriting =>
            return Abstract_Node_From_Rewriting (Cursor.Rewriting_Child);
      end case;
   end Element;

   ----------
   -- Next --
   ----------

   function Next (Cursor : Abstract_Cursor) return Abstract_Cursor is
   begin
      case Cursor.Kind is
         when From_Parsing =>
            return (Kind             => From_Parsing,
                    Parsing_List     => Cursor.Parsing_List,
                    Next_Child_Index => Cursor.Next_Child_Index + 1);

         when From_Rewriting =>
            return (Kind            => From_Rewriting,
                    Rewriting_Child => Cursor.Rewriting_Child.Next);
      end case;
   end Next;

   ----------
   -- Text --
   ----------

   function Text (Node : Abstract_Node) return Text_Type is
   begin
      case Node.Kind is
         when From_Parsing =>
            return Text (Node.Parsing_Node);
         when From_Rewriting =>
            return Text (Node.Rewriting_Node);
      end case;
   end Text;

   --------------------
   -- Rewritten_Node --
   --------------------

   function Rewritten_Node (Node : Abstract_Node) return Lk_Node is
   begin
      case Node.Kind is
         when From_Parsing =>
            return Node.Parsing_Node;
         when From_Rewriting =>
            return Node.Rewriting_Node.Node;
      end case;
   end Rewritten_Node;

   ---------------------------
   -- Create_Token_Sequence --
   ---------------------------

   function Create_Token_Sequence
     (Unparser    : Token_Sequence;
      First_Token : in out Lk_Token)
      return Present_Token_Sequence_Template
   is
      Result : Present_Token_Sequence_Template;
   begin
      if Unparser'Length = 0 then
         return (Present => True, First => No_Lk_Token, Last => No_Lk_Token);
      else
         Result.First := First_Token;
         Result.Last := Relative_Token (First_Token, Unparser'Length - 1);
         First_Token := Relative_Token (Result.Last, 1);
         return Result;
      end if;
   end Create_Token_Sequence;

   -----------------------------------
   -- Extract_Regular_Node_Template --
   -----------------------------------

   function Extract_Regular_Node_Template
     (Unparser       : Regular_Node_Unparser;
      Rewritten_Node : Lk_Node) return Regular_Node_Template
   is
      Result     : Regular_Node_Template (True, Unparser.Field_Unparsers.N);
      Next_Token : Lk_Token;
   begin
      if Rewritten_Node.Is_Null then
         return (Present => False, Count => 0);
      end if;

      Next_Token := Rewritten_Node.Token_Start;

      --  Recover tokens that precede the first field from the rewritten node

      Result.Pre_Tokens :=
        Create_Token_Sequence (Unparser.Pre_Tokens, Next_Token);

      --  For each field, recover the tokens that surround the field itself,
      --  but only if both the original node and the one to unparse are
      --  present.

      for I in 1 .. Rewritten_Node.Children_Count loop
         declare
            U  : Field_Unparser_List_Impl renames Unparser.Field_Unparsers.all;
            F  : Field_Unparser_Impl renames U.Field_Unparsers (I);
            T  : Token_Sequence renames U.Inter_Tokens (I);
            FT : Field_Template renames Result.Fields (I);

            Rewritten_Child : constant Lk_Node := Rewritten_Node.Child (I);
            R_Child         : constant Abstract_Node :=
              Abstract_Node_From_Parsing (Rewritten_Child);
         begin
            Result.Inter_Tokens (I) :=
              (if I = 1
               then Empty_Token_Sequence_Template
               else Create_Token_Sequence (T, Next_Token));

            if Field_Present (R_Child, F) then
               FT := (Present => True, others => <>);

               --  Pre_Tokens is the sequence that starts at Next_Token and
               --  whose length is the one the unparser gives.

               FT.Pre_Tokens :=
                  Create_Token_Sequence (F.Pre_Tokens, Next_Token);

               --  Post_Tokens is the sequence that starts right after the last
               --  token of the node field, also sized from the unparser.
               --  Beware of ghost nodes, which own no token.

               Next_Token :=
                 (if Rewritten_Child.Is_Ghost
                  then Rewritten_Child.Token_Start
                  else Relative_Token (Rewritten_Child.Token_End, 1));
               FT.Post_Tokens :=
                  Create_Token_Sequence (F.Post_Tokens, Next_Token);

            else
               FT := (Present => False);
            end if;
         end;
      end loop;

      --  Recover tokens that succeed to the first field from the rewritten
      --  node.

      Result.Post_Tokens :=
         Create_Token_Sequence (Unparser.Post_Tokens, Next_Token);

      return Result;
   end Extract_Regular_Node_Template;

   -------------------
   -- Field_Present --
   -------------------

   function Field_Present
     (Node : Abstract_Node; Unparser : Field_Unparser_Impl) return Boolean is
   begin
      return not Node.Is_Null
             and then (not Unparser.Empty_List_Is_Absent
                       or else Node.Children_Count > 0);
   end Field_Present;

   ------------
   -- Append --
   ------------

   procedure Append (Buffer : in out Unparsing_Buffer; Char : Character_Type)
   is
   begin
      Append (Buffer.Content, Char);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Buffer : in out Unparsing_Buffer;
      Kind   : Token_Kind_Index;
      Text   : Text_Type) is
   begin
      Append (Buffer.Content, Text);
      Buffer.Last_Token := Kind;
   end Append;

   -------------------------
   -- Apply_Spacing_Rules --
   -------------------------

   procedure Apply_Spacing_Rules
     (Tables     : Unparsing_Tables;
      Buffer     : in out Unparsing_Buffer;
      Next_Token : Token_Kind_Index) is
   begin
      if Length (Buffer.Content) = 0 then
         null;

      elsif Tables.Token_Newlines (+Buffer.Last_Token) then
         Append (Buffer, Chars.LF);

      elsif Tables.Token_Spacings
        (Tables.Token_Kinds (Buffer.Last_Token).Family,
         Tables.Token_Kinds (Next_Token).Family)
      then
         Append (Buffer, ' ');
      end if;
   end Apply_Spacing_Rules;

   -------------
   -- Unparse --
   -------------

   procedure Unparse
     (Node                : Abstract_Node;
      Unit                : Lk_Unit;
      Preserve_Formatting : Boolean;
      As_Unit             : Boolean;
      Result              : out Unparsing_Buffer)
   is
      Tables : constant Unparsing_Tables := Unparsing_Tables_From_Node (Node);
   begin
      --  Unparse Node, and the leading trivia if we are unparsing the unit as
      --  a whole.

      if As_Unit then
         declare
            First : constant Lk_Token := Unit.First_Token;
         begin
            if First.Is_Trivia then
               Append_Tokens
                 (Tables,
                  Result,
                  First,
                  Last_Trivia (First),
                  With_Trailing_Trivia => False);
            end if;
         end;
      end if;
      Unparse_Node (Tables, Node, Preserve_Formatting, Result);
   end Unparse;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Node                : Abstract_Node;
      Unit                : Lk_Unit;
      Preserve_Formatting : Boolean;
      As_Unit             : Boolean) return String
   is
      Result : String_Access :=
         Unparse (Node, Unit, Preserve_Formatting, As_Unit);
      R      : constant String := Result.all;
   begin
      Free (Result);
      return R;
   end Unparse;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Node                : Abstract_Node;
      Unit                : Lk_Unit;
      Preserve_Formatting : Boolean;
      As_Unit             : Boolean) return String_Access
   is
      use Ada.Strings.Wide_Wide_Unbounded.Aux;

      Buffer        : Unparsing_Buffer;
      --  Buffer to store the result of unparsing as text

      Buffer_Access : Big_Wide_Wide_String_Access;
      Length        : Natural;
      --  Buffer internals, to avoid costly buffer copies
   begin
      Unparse (Node, Unit, Preserve_Formatting, As_Unit, Buffer);
      Get_Wide_Wide_String (Buffer.Content, Buffer_Access, Length);

      --  GNATCOLL.Iconv raises a Constraint_Error for empty strings: handle
      --  them here.

      if Length = 0 then
         return new String'("");
      end if;

      declare
         use GNATCOLL.Iconv;

         State  : constant Iconv_T := Iconv_Open
           (To_Code   => Unit.Charset,
            From_Code => Text_Charset);
         Status : Iconv_Result;

         To_Convert_String : constant String (1 .. 4 * Length)
            with Import     => True,
                 Convention => Ada,
                 Address    => Buffer_Access.all'Address;

         Output_Buffer : String_Access :=
            new String (1 .. 4 * To_Convert_String'Length);
         --  Encodings should not take more than 4 bytes per code point, so
         --  this should be enough to hold the conversion.

         Input_Index  : Positive := To_Convert_String'First;
         Output_Index : Positive := Output_Buffer'First;
      begin
         Iconv
           (State,
            To_Convert_String,
            Input_Index,
            Output_Buffer.all,
            Output_Index,
            Status);
         Iconv_Close (State);
         case Status is
            when Success => null;
            when others => raise Program_Error with "cannot encode result";
         end case;

         declare
            Result_Slice : String renames
               Output_Buffer (Output_Buffer'First ..  Output_Index - 1);
            Result       : constant String_Access :=
               new String (Result_Slice'Range);
         begin
            Result.all := Result_Slice;
            Free (Output_Buffer);
            return Result;
         end;
      end;
   end Unparse;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Node                : Abstract_Node;
      Unit                : Lk_Unit;
      Preserve_Formatting : Boolean;
      As_Unit             : Boolean) return Unbounded_Text_Type
   is
      Buffer : Unparsing_Buffer;
   begin
      if Node.Is_Null then
         raise Program_Error with "cannot unparse null node";
      elsif As_Unit and then Unit = No_Lk_Unit then
         raise Program_Error with "cannot unparse node as unit without a unit";
      end if;

      Unparse (Node, Unit, Preserve_Formatting, As_Unit, Buffer);
      return Buffer.Content;
   end Unparse;

   ------------------
   -- Unparse_Node --
   ------------------

   procedure Unparse_Node
     (Tables              : Unparsing_Tables;
      Node                : Abstract_Node;
      Preserve_Formatting : Boolean;
      Result              : in out Unparsing_Buffer)
   is
      Kind     : constant Type_Ref := Node.Type_Of;
      Unparser : Node_Unparser_Impl renames
        Tables.Node_Unparsers (Kind.To_Index).all;

      RN : constant Lk_Node :=
        (if Preserve_Formatting
         then Node.Rewritten_Node
         else No_Lk_Node);
   begin
      case Unparser.Kind is
         when Regular =>
            Unparse_Regular_Node
              (Tables,
               Node,
               Unparser,
               RN,
               Preserve_Formatting,
               Result);

         when List =>
            Unparse_List_Node
              (Tables,
               Node,
               Unparser,
               RN,
               Preserve_Formatting,
               Result);

         when Token =>
            declare
               Tok_Kind : constant Token_Kind_Index :=
                 Kind.Token_Node_Kind.To_Index;
            begin
               --  Add the single token that materialize Node itself

               Apply_Spacing_Rules (Tables, Result, Tok_Kind);
               Append (Result, Tok_Kind, Text (Node));

               --  If Node comes from an original node, also append the trivia
               --  that comes after.

               if not RN.Is_Null then
                  declare
                     Token     : constant Lk_Token := RN.Token_End;
                     Last_Triv : constant Lk_Token := Last_Trivia (Token);
                  begin
                     Append_Tokens
                       (Tables,
                        Result,
                        Token.Next,
                        Last_Triv,
                        With_Trailing_Trivia => False);
                  end;
               end if;
            end;
      end case;
   end Unparse_Node;

   --------------------------
   -- Unparse_Regular_Node --
   --------------------------

   procedure Unparse_Regular_Node
     (Tables              : Unparsing_Tables;
      Node                : Abstract_Node;
      Unparser            : Regular_Node_Unparser;
      Rewritten_Node      : Lk_Node;
      Preserve_Formatting : Boolean;
      Result              : in out Unparsing_Buffer)
   is
      Template : constant Regular_Node_Template :=
         Extract_Regular_Node_Template (Unparser, Rewritten_Node);
   begin
      --  Unparse tokens that precede the first field. Re-use original ones if
      --  available.

      if Template.Present then
         Append_Tokens (Tables, Result, Template.Pre_Tokens);
      else
         Unparse_Token_Sequence (Tables, Unparser.Pre_Tokens.all, Result);
      end if;

      --  Unparse Node's fields, and the tokens between them

      declare
         U : Field_Unparser_List_Impl renames Unparser.Field_Unparsers.all;
      begin
         for I in 1 .. U.N loop
            declare
               F     : Field_Unparser_Impl renames U.Field_Unparsers (I);
               Child : constant Abstract_Node := Node.Child (I);
            begin
               --  First unparse tokens that appear unconditionally between
               --  fields.

               if Template.Present then
                  Append_Tokens (Tables, Result, Template.Inter_Tokens (I));
               else
                  Unparse_Token_Sequence
                    (Tables, U.Inter_Tokens (I).all, Result);
               end if;

               --  Then unparse the field itself

               if Field_Present (Child, F) then
                  if Template.Present and then Template.Fields (I).Present then
                     Append_Tokens
                       (Tables, Result, Template.Fields (I).Pre_Tokens);
                     Unparse_Node (Tables, Child, Preserve_Formatting, Result);
                     Append_Tokens
                       (Tables, Result, Template.Fields (I).Post_Tokens);

                  else
                     Unparse_Token_Sequence (Tables, F.Pre_Tokens.all, Result);
                     Unparse_Node (Tables, Child, Preserve_Formatting, Result);
                     Unparse_Token_Sequence
                       (Tables, F.Post_Tokens.all, Result);
                  end if;
               end if;
            end;
         end loop;
      end;

      --  Unparse tokens that suceed to the last field. Re-use original ones if
      --  available.

      if Template.Present then
         Append_Tokens (Tables, Result, Template.Post_Tokens);
      else
         Unparse_Token_Sequence (Tables, Unparser.Post_Tokens.all, Result);
      end if;
   end Unparse_Regular_Node;

   -----------------------
   -- Unparse_List_Node --
   -----------------------

   procedure Unparse_List_Node
     (Tables              : Unparsing_Tables;
      Node                : Abstract_Node;
      Unparser            : List_Node_Unparser;
      Rewritten_Node      : Lk_Node;
      Preserve_Formatting : Boolean;
      Result              : in out Unparsing_Buffer)
   is
      Cursor   : Abstract_Cursor := Node.Iterate_List;
      I        : Positive := 1;
      AN_Child : Abstract_Node;
   begin
      while Cursor.Has_Element loop
         AN_Child := Cursor.Element;
         if AN_Child.Is_Null then
            raise Malformed_Tree_Error with "null node found in a list";
         end if;

         --  For all elements but the first one, emit the separator. If
         --  possible, preserve original formatting for the corresponding
         --  separator in the original source.

         if I > 1 and then Unparser.Separator /= null then
            if not Rewritten_Node.Is_Null
               and then Rewritten_Node.Children_Count >= I
            then
               declare
                  BN_Child : constant Lk_Node := Rewritten_Node.Child (I);
                  Tok      : constant Lk_Token :=
                    Relative_Token (BN_Child.Token_Start, -1);
               begin
                  Append_Tokens (Tables, Result, Tok, Tok);
               end;
            elsif Unparser.Separator /= null then
               Unparse_Token (Tables, Unparser.Separator.all, Result);
            end if;
         end if;

         Unparse_Node (Tables, AN_Child, Preserve_Formatting, Result);

         Cursor := Cursor.Next;
         I := I + 1;
      end loop;
   end Unparse_List_Node;

   -------------------
   -- Unparse_Token --
   -------------------

   procedure Unparse_Token
     (Tables   : Unparsing_Tables;
      Unparser : Token_Unparser_Impl;
      Result   : in out Unparsing_Buffer) is
   begin
      Apply_Spacing_Rules (Tables, Result, Unparser.Kind);
      Append (Result, Unparser.Kind, Unparser.Text.all);
   end Unparse_Token;

   ----------------------------
   -- Unparse_Token_Sequence --
   ----------------------------

   procedure Unparse_Token_Sequence
     (Tables   : Unparsing_Tables;
      Unparser : Token_Sequence_Impl;
      Result   : in out Unparsing_Buffer) is
   begin
      for U of Unparser loop
         Unparse_Token (Tables, U.all, Result);
      end loop;
   end Unparse_Token_Sequence;

   --------------------
   -- Relative_Token --
   --------------------

   function Relative_Token
     (Token : Lk_Token; Offset : Integer) return Lk_Token
   is
      Current_Token  : Lk_Token := Token;
      Current_Offset : Integer := 0;
   begin
      if Offset < 0 then
         while Current_Offset > Offset loop
            Current_Token := Current_Token.Previous;
            if not Current_Token.Is_Trivia then
               Current_Offset := Current_Offset - 1;
            end if;
         end loop;

      else
         while Current_Offset < Offset loop
            Current_Token := Current_Token.Next;
            if not Current_Token.Is_Trivia then
               Current_Offset := Current_Offset + 1;
            end if;
         end loop;
      end if;

      return Current_Token;
   end Relative_Token;

   -----------------
   -- Last_Trivia --
   -----------------

   function Last_Trivia (Token : Lk_Token) return Lk_Token is
      Result : Lk_Token := Token;
      Cur    : Lk_Token := Token.Next;
   begin
      --  Move Last to the last trivia that comes before the next token

      while not Cur.Is_Null and then Cur.Is_Trivia loop
         Result := Cur;
         Cur := Cur.Next;
      end loop;
      return Result;
   end Last_Trivia;

   -------------------
   -- Append_Tokens --
   -------------------

   procedure Append_Tokens
     (Tables                  : Unparsing_Tables;
      Result                  : in out Unparsing_Buffer;
      First_Token, Last_Token : Lk_Token;
      With_Trailing_Trivia    : Boolean := True) is
   begin
      if (First_Token.Is_Null and then Last_Token.Is_Null)
         or else Last_Token < First_Token
      then
         return;
      end if;
      pragma Assert (not First_Token.Is_Null and then not Last_Token.Is_Null);
      Apply_Spacing_Rules (Tables, Result, First_Token.Kind.To_Index);

      declare
         Last : constant Lk_Token := (if With_Trailing_Trivia
                                      then Last_Trivia (Last_Token)
                                      else Last_Token);
      begin
         Append (Result, Last.Kind.To_Index, Text (First_Token, Last));
      end;
   end Append_Tokens;

   -------------------
   -- Append_Tokens --
   -------------------

   procedure Append_Tokens
     (Tables   : Unparsing_Tables;
      Result   : in out Unparsing_Buffer;
      Template : Token_Sequence_Template)
   is
   begin
      if Template.Present then
         Append_Tokens (Tables, Result, Template.First, Template.Last);
      end if;
   end Append_Tokens;

end Liblktlang_Support.Rewriting.Unparsing;
