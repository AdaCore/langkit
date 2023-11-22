## vim: filetype=makoada

<%namespace name="unparsers"    file="unparsers_ada.mako" />

<% concrete_astnodes = [astnode for astnode in ctx.astnode_types
                        if not astnode.abstract] %>

pragma Warnings (Off, "internal");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "internal");

with GNATCOLL.Iconv;

with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Internal; use Langkit_Support.Internal;
with Langkit_Support.Internal.Descriptor;
use Langkit_Support.Internal.Descriptor;
with Langkit_Support.Internal.Unparsing;
use Langkit_Support.Internal.Unparsing;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

with ${ada_lib_name}.Common;         use ${ada_lib_name}.Common;
with ${ada_lib_name}.Generic_API;    use ${ada_lib_name}.Generic_API;
with ${ada_lib_name}.Generic_Introspection;
use ${ada_lib_name}.Generic_Introspection;
with ${ada_lib_name}.Implementation; use ${ada_lib_name}.Implementation;
with ${ada_lib_name}.Private_Converters;
use ${ada_lib_name}.Private_Converters;

package body ${ada_lib_name}.Unparsing_Implementation is

   subtype String_Access is Ada.Strings.Unbounded.String_Access;

   Id                  : Language_Descriptor_Access := +Self_Id;
   Token_Kinds         : Token_Kind_Descriptor_Array renames
     Id.Token_Kinds.all;
   Token_Spacing_Table : Token_Spacing_Table_Impl renames
     Id.Unparsers.Token_Spacings.all;
   Token_Newline_Table : Token_Newline_Table_Impl renames
     Id.Unparsers.Token_Newlines.all;
   Node_Unparsers      : Node_Unparser_Map_Impl renames
     Id.Unparsers.Node_Unparsers.all;

   --  Token families and have 1-based indexes in the generic API, and the 'Pos
   --  of their language-specific type is 0-based.

   function "+" (Kind : Token_Kind) return Token_Kind_Index
   is (Token_Kind'Pos (Kind) + 1);
   function "+" (Kind : Token_Kind_Index) return Token_Kind
   is (Token_Kind'Val (Kind - 1));

   --  The "template" data structures below are helpers for the original
   --  source code formatting preservation algorithms. A template can be
   --  thought as the instantiation of an unparser from an original AST node.
   --  It captures actual sequences of tokens.

   type Token_Sequence_Template (Present : Boolean := False) is record
      case Present is
         when False => null;
         when True =>
            First, Last : Token_Reference;
      end case;
   end record;
   --  Captured sequence of tokens

   subtype Present_Token_Sequence_Template is Token_Sequence_Template (True);

   Empty_Token_Sequence_Template : constant Present_Token_Sequence_Template :=
     (Present => True, First => No_Token, Last => No_Token);

   function Create_Token_Sequence
     (Unparser    : Token_Sequence;
      First_Token : in out Token_Reference)
      return Present_Token_Sequence_Template
      with Pre => First_Token /= No_Token;
   --  Create a present sequence of tokens starting from First_Token and
   --  containing the same number of tokens as indicated in Unparser. Before
   --  returning, this updates First_Token to point at the first token that
   --  appear after the sequence.

   type Token_Sequence_Template_Array is
      array (Positive range <>) of Present_Token_Sequence_Template;

   type Field_Template (Present : Boolean := False) is record
      case Present is
         when False => null;
         when True =>
            Pre_Tokens, Post_Tokens : Token_Sequence_Template (Present);
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
      Rewritten_Node : ${T.root_node.name}) return Regular_Node_Template;
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

   procedure Update_Sloc
     (Sloc : in out Source_Location; Char : Wide_Wide_Character);
   --  Update Sloc as if it represented a cursor that move right-wards after
   --  inserting Char to a buffer.

   procedure Unparse_Node
     (Node                : Abstract_Node;
      Preserve_Formatting : Boolean;
      Result              : in out Unparsing_Buffer);
   --  Using the Node_Unparsers unparsing tables, unparse the given Node

   procedure Unparse_Regular_Node
     (Node                : Abstract_Node;
      Unparser            : Regular_Node_Unparser;
      Rewritten_Node      : ${T.root_node.name};
      Preserve_Formatting : Boolean;
      Result              : in out Unparsing_Buffer);
   --  Helper for Unparse_Node, focuses on regular nodes

   procedure Unparse_List_Node
     (Node                : Abstract_Node;
      Unparser            : List_Node_Unparser;
      Rewritten_Node      : ${T.root_node.name};
      Preserve_Formatting : Boolean;
      Result              : in out Unparsing_Buffer);
   --  Helper for Unparse_Node, focuses on list nodes

   procedure Unparse_Token
     (Unparser : Token_Unparser_Impl;
      Result   : in out Unparsing_Buffer);
   --  Using the Unparser unparsing table, unparse a token

   procedure Unparse_Token_Sequence
     (Unparser : Token_Sequence_Impl; Result : in out Unparsing_Buffer);
   --  Using the Unparser unparsing table, unparse a sequence of tokens

   function Relative_Token
     (Token : Token_Reference; Offset : Integer) return Token_Reference with
      Pre => Token /= No_Token
             and then not Is_Trivia (Token)
             and then
               Token_Index (Integer (Get_Token_Index (Token).Token) + Offset)
               in First_Token_Index .. Last_Token (Get_Token_TDH (Token).all),
      Post => Relative_Token'Result /= No_Token;
   --  Considering only tokens that are not trivia and assuming Token is at
   --  index I, return the token that is at index I + Offset.

   function Last_Trivia (Token : Token_Reference) return Token_Reference
      with Pre => Token /= No_Token;
   --  If Token (which can be a token or a trivia) is followed by a sequence of
   --  trivias, return the last of them. Otherwise, return Token itself.

   procedure Append_Tokens
     (Result                  : in out Unparsing_Buffer;
      First_Token, Last_Token : Token_Reference;
      With_Trailing_Trivia    : Boolean := True);
   --  Emit to Result the sequence of tokens from First_Token to Last_Token.
   --  Trivias that appear between tokens in the sequence to emit are emitted
   --  as well. If With_Trailing_Trivia is true, also emit the sequence of
   --  trivia that follows Last_Token.

   procedure Append_Tokens
     (Result   : in out Unparsing_Buffer;
      Template : Token_Sequence_Template);
   --  Emit to Result the sequence of tokens in Template, or do nothing if the
   --  template is absent.

   --------------------------
   -- Create_Abstract_Node --
   --------------------------

   function Create_Abstract_Node
     (Parsing_Node : ${T.root_node.name}) return Abstract_Node is
   begin
      return (From_Parsing, Parsing_Node);
   end Create_Abstract_Node;

   --------------------------
   -- Create_Abstract_Node --
   --------------------------

   function Create_Abstract_Node
     (Rewriting_Node : Node_Rewriting_Handle) return Abstract_Node is
   begin
      return (From_Rewriting, Rewriting_Node);
   end Create_Abstract_Node;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Node : Abstract_Node) return Boolean is
   begin
      case Node.Kind is
         when From_Parsing =>
            return Node.Parsing_Node = null;
         when From_Rewriting =>
            return Node.Rewriting_Node = null;
      end case;
   end Is_Null;

   ----------
   -- Kind --
   ----------

   function Kind (Node : Abstract_Node) return ${T.node_kind} is
   begin
      case Node.Kind is
         when From_Parsing =>
            return Node.Parsing_Node.Kind;
         when From_Rewriting =>
            return Node.Rewriting_Node.Kind;
      end case;
   end Kind;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Node : Abstract_Node) return Natural is
   begin
      case Node.Kind is
         when From_Parsing =>
            return Children_Count (Node.Parsing_Node);
         when From_Rewriting =>
            return Children_Count (Node.Rewriting_Node);
      end case;
   end Children_Count;

   -----------
   -- Child --
   -----------

   function Child
     (Node : Abstract_Node; Index : Positive) return Abstract_Node is
   begin
      case Node.Kind is
         when From_Parsing =>
            return Create_Abstract_Node (Child (Node.Parsing_Node, Index));

         when From_Rewriting =>
            --  In the context of unparsing, it is pointless to expand the
            --  rewritting tree (which is what Rewriting_Implementation.Child
            --  does). If the node is not expanded, switch to the original
            --  node instead.

            declare
               RN : constant Node_Rewriting_Handle := Node.Rewriting_Node;
            begin
               if RN.Children.Kind = Unexpanded then
                  return Create_Abstract_Node (Child (RN.Node, Index));
               else
                  return Create_Abstract_Node
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
            return
              (Kind            => From_Rewriting,
               Rewriting_Child => First_Child (Node.Rewriting_Node));
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
                   <= Children_Count (Cursor.Parsing_List);

         when From_Rewriting =>
            return Cursor.Rewriting_Child /= No_Node_Rewriting_Handle;
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
              Create_Abstract_Node
                (Child (Cursor.Parsing_List, Cursor.Next_Child_Index));

         when From_Rewriting =>
            return
              Create_Abstract_Node (Cursor.Rewriting_Child);
      end case;
   end Element;

   ----------
   -- Next --
   ----------

   function Next (Cursor : Abstract_Cursor) return Abstract_Cursor is
   begin
      case Cursor.Kind is
         when From_Parsing =>
            return
              (Kind             => From_Parsing,
               Parsing_List     => Cursor.Parsing_List,
               Next_Child_Index => Cursor.Next_Child_Index + 1);

         when From_Rewriting =>
            return
              (Kind            => From_Rewriting,
               Rewriting_Child => Next_Child (Cursor.Rewriting_Child));
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

   function Rewritten_Node
     (Node : Abstract_Node) return ${T.root_node.name} is
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
      First_Token : in out Token_Reference)
      return Present_Token_Sequence_Template
   is
      Result : Present_Token_Sequence_Template;
   begin
      if Unparser'Length = 0 then
         return (Present => True, First => No_Token, Last => No_Token);
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
      Rewritten_Node : ${T.root_node.name}) return Regular_Node_Template
   is
      Result     : Regular_Node_Template (True, Unparser.Field_Unparsers.N);
      Next_Token : Token_Reference;
   begin
      if Rewritten_Node = null then
         return (Present => False, Count => 0);
      end if;

      Next_Token := Token_Start (Rewritten_Node);

      --  Recover tokens that precede the first field from the rewritten node
      Result.Pre_Tokens := Create_Token_Sequence
        (Unparser.Pre_Tokens, Next_Token);

      --  For each field, recover the tokens that surround the field itself,
      --  but only if both the original node and the one to unparse are
      --  present.
      for I in 1 .. Children_Count (Rewritten_Node) loop
         declare
            U  : Field_Unparser_List_Impl renames Unparser.Field_Unparsers.all;
            F  : Field_Unparser_Impl renames U.Field_Unparsers (I);
            T  : Token_Sequence renames U.Inter_Tokens (I);
            FT : Field_Template renames Result.Fields (I);

            Rewritten_Child : constant ${T.root_node.name} :=
               Child (Rewritten_Node, I);
            R_Child         : constant Abstract_Node :=
               Create_Abstract_Node (Rewritten_Child);
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
                 (if Is_Ghost (Rewritten_Child)
                  then Token_Start (Rewritten_Child)
                  else Relative_Token (Token_End (Rewritten_Child), 1));
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
      return (not Is_Null (Node)
              and then (not Unparser.Empty_List_Is_Absent
                        or else Children_Count (Node) > 0));
   end Field_Present;

   -----------------
   -- Update_Sloc --
   -----------------

   procedure Update_Sloc
     (Sloc : in out Source_Location; Char : Wide_Wide_Character) is
   begin
      --  TODO??? Handle tabs

      if Wide_Wide_Character'Pos (Char) = Character'Pos (ASCII.LF) then
         Sloc.Line := Sloc.Line + 1;
         Sloc.Column := 1;
      else
         Sloc.Column := Sloc.Column + 1;
      end if;
   end Update_Sloc;

   ------------
   -- Append --
   ------------

   procedure Append
     (Buffer : in out Unparsing_Buffer; Char : Wide_Wide_Character) is
   begin
      Update_Sloc (Buffer.Last_Sloc, Char);
      Append (Buffer.Content, Char);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Buffer : in out Unparsing_Buffer;
      Kind   : Token_Kind;
      Text   : Text_Type) is
   begin
      for C of Text loop
         Update_Sloc (Buffer.Last_Sloc, C);
      end loop;
      Append (Buffer.Content, Text);
      Buffer.Last_Token := Kind;
   end Append;

   -------------------------
   -- Apply_Spacing_Rules --
   -------------------------

   procedure Apply_Spacing_Rules
     (Buffer     : in out Unparsing_Buffer;
      Next_Token : Token_Kind) is
   begin
      if Length (Buffer.Content) = 0 then
         null;

      elsif Token_Newline_Table (+Buffer.Last_Token) then
         Append (Buffer, Chars.LF);

      elsif Token_Spacing_Table
        (Token_Kinds (+Buffer.Last_Token).Family,
         Token_Kinds (+Next_Token).Family)
      then
         Append (Buffer, ' ');
      end if;
   end Apply_Spacing_Rules;

   -------------
   -- Unparse --
   -------------

   procedure Unparse
     (Node                : Abstract_Node;
      Unit                : Internal_Unit;
      Preserve_Formatting : Boolean;
      As_Unit             : Boolean;
      Result              : out Unparsing_Buffer) is
   begin
      --  Unparse Node, and the leading trivia if we are unparsing the unit as
      --  a whole.
      if As_Unit then
         declare
            First : constant Token_Reference := First_Token (Unit);
         begin
            if Is_Trivia (First) then
               Append_Tokens (Result, First, Last_Trivia (First),
                              With_Trailing_Trivia => False);
            end if;
         end;
      end if;
      Unparse_Node (Node, Preserve_Formatting, Result);
   end Unparse;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Node                : Abstract_Node;
      Unit                : Internal_Unit;
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
      Unit                : Internal_Unit;
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

         State  : Iconv_T := Iconv_Open
           (To_Code   => Get_Charset (Unit),
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
         --  TODO??? Use GNATCOLL.Iconv to properly encode this wide wide
         --  string into a mere string using this unit's charset.
         Iconv
           (State, To_Convert_String, Input_Index, Output_Buffer.all,
            Output_Index, Status);
         Iconv_Close (State);
         case Status is
            when Success => null;
            when others => raise Program_Error with "cannot encode result";
         end case;

         declare
            Result_Slice : String renames
               Output_Buffer (Output_Buffer'First ..  Output_Index - 1);
            Result : constant String_Access :=
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
      Unit                : Internal_Unit;
      Preserve_Formatting : Boolean;
      As_Unit             : Boolean) return Unbounded_Text_Type
   is
      Buffer : Unparsing_Buffer;
   begin
      % if ctx.generate_unparser:
         if Is_Null (Node) then
            return (raise Program_Error with "cannot unparse null node");
         elsif As_Unit and then Unit = null then
            return (raise Program_Error
                    with "cannot unparse node as unit without a unit");
         end if;

         Unparse (Node, Unit, Preserve_Formatting, As_Unit, Buffer);
         return Buffer.Content;
      % else:
         pragma Unreferenced (Buffer);
         return (raise Program_Error with "Unparser not generated");
      % endif
   end Unparse;

   ------------------
   -- Unparse_Node --
   ------------------

   procedure Unparse_Node
     (Node                : Abstract_Node;
      Preserve_Formatting : Boolean;
      Result              : in out Unparsing_Buffer)
   is
      Kind     : constant ${T.node_kind} :=
        Unparsing_Implementation.Kind (Node);
      Unparser : Node_Unparser_Impl renames
        Node_Unparsers (Node_Kinds (Kind)).all;

      Rewritten_Node : constant ${T.root_node.name} :=
        (if Preserve_Formatting
         then Unparsing_Implementation.Rewritten_Node (Node)
         else null);
   begin
      case Unparser.Kind is
         when Regular =>
            Unparse_Regular_Node
              (Node, Unparser, Rewritten_Node, Preserve_Formatting, Result);

         when List =>
            Unparse_List_Node
              (Node, Unparser, Rewritten_Node, Preserve_Formatting, Result);

         when Token =>
            declare
               Tok_Kind : constant Token_Kind := Token_Node_Kind (Kind);
            begin
               --  Add the single token that materialize Node itself
               Apply_Spacing_Rules (Result, Tok_Kind);
               Append (Result, Tok_Kind, Text (Node));

               --  If Node comes from an original node, also append the trivia
               --  that comes after.
               if Rewritten_Node /= null then
                  declare
                     Token     : constant Token_Reference :=
                        Token_End (Rewritten_Node);
                     Last_Triv : constant Token_Reference :=
                        Last_Trivia (Token);
                  begin
                     Append_Tokens (Result, Next (Token), Last_Triv,
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
     (Node                : Abstract_Node;
      Unparser            : Regular_Node_Unparser;
      Rewritten_Node      : ${T.root_node.name};
      Preserve_Formatting : Boolean;
      Result              : in out Unparsing_Buffer)
   is
      Template : constant Regular_Node_Template :=
         Extract_Regular_Node_Template (Unparser, Rewritten_Node);
   begin
      --  Unparse tokens that precede the first field. Re-use original ones if
      --  available.
      if Template.Present then
         Append_Tokens (Result, Template.Pre_Tokens);
      else
         Unparse_Token_Sequence (Unparser.Pre_Tokens.all, Result);
      end if;

      --  Unparse Node's fields, and the tokens between them
      declare
         U : Field_Unparser_List_Impl renames Unparser.Field_Unparsers.all;
      begin
         for I in 1 .. U.N loop
            declare
               F     : Field_Unparser_Impl renames U.Field_Unparsers (I);
               Child : constant Abstract_Node :=
                  Unparsing_Implementation.Child (Node, I);
            begin
               --  First unparse tokens that appear unconditionally between
               --  fields.
               if Template.Present then
                  Append_Tokens (Result, Template.Inter_Tokens (I));
               else
                  Unparse_Token_Sequence (U.Inter_Tokens (I).all, Result);
               end if;

               --  Then unparse the field itself
               if Field_Present (Child, F) then
                  if Template.Present and then Template.Fields (I).Present then
                     Append_Tokens (Result, Template.Fields (I).Pre_Tokens);
                     Unparse_Node (Child, Preserve_Formatting, Result);
                     Append_Tokens (Result, Template.Fields (I).Post_Tokens);

                  else
                     Unparse_Token_Sequence (F.Pre_Tokens.all, Result);
                     Unparse_Node (Child, Preserve_Formatting, Result);
                     Unparse_Token_Sequence (F.Post_Tokens.all, Result);
                  end if;
               end if;
            end;
         end loop;
      end;

      --  Unparse tokens that suceed to the last field. Re-use original ones if
      --  available.
      if Template.Present then
         Append_Tokens (Result, Template.Post_Tokens);
      else
         Unparse_Token_Sequence (Unparser.Post_Tokens.all, Result);
      end if;
   end Unparse_Regular_Node;

   -----------------------
   -- Unparse_List_Node --
   -----------------------

   procedure Unparse_List_Node
     (Node                : Abstract_Node;
      Unparser            : List_Node_Unparser;
      Rewritten_Node      : ${T.root_node.name};
      Preserve_Formatting : Boolean;
      Result              : in out Unparsing_Buffer)
   is
      Cursor   : Abstract_Cursor := Iterate_List (Node);
      I        : Positive := 1;
      AN_Child : Abstract_Node;
   begin
      while Has_Element (Cursor) loop
         AN_Child := Element (Cursor);
         if Is_Null (AN_Child) then
            raise Malformed_Tree_Error with "null node found in a list";
         end if;

         --  For all elements but the first one, emit the separator. If
         --  possible, preserve original formatting for the corresponding
         --  separator in the original source.

         if I > 1 and then Unparser.Has_Separator then
            if Rewritten_Node /= null
               and then Children_Count (Rewritten_Node) >= I
            then
               declare
                  BN_Child : constant ${T.root_node.name} :=
                     Child (Rewritten_Node, I);
                  Tok : constant Token_Reference :=
                     Relative_Token (Token_Start (BN_Child), -1);
               begin
                  Append_Tokens (Result, Tok, Tok);
               end;
            else
               Unparse_Token (Unparser.Separator, Result);
            end if;
         end if;

         Unparse_Node (AN_Child, Preserve_Formatting, Result);

         Cursor := Next (Cursor);
         I := I + 1;
      end loop;
   end Unparse_List_Node;

   -------------------
   -- Unparse_Token --
   -------------------

   procedure Unparse_Token
     (Unparser : Token_Unparser_Impl;
      Result   : in out Unparsing_Buffer)
   is
      Kind : constant Token_Kind := +Unparser.Kind;
   begin
      Apply_Spacing_Rules (Result, Kind);
      if Unparser.Text /= null then
         Append (Result, Kind, Unparser.Text.all);
      else
         declare
            Literal : constant Text_Type := Token_Kind_Literal (Kind);
         begin
            pragma Assert (Literal'Length > 0);
            Append (Result, Kind, Literal);
         end;
      end if;
   end Unparse_Token;

   ----------------------------
   -- Unparse_Token_Sequence --
   ----------------------------

   procedure Unparse_Token_Sequence
     (Unparser : Token_Sequence_Impl; Result : in out Unparsing_Buffer) is
   begin
      for U of Unparser loop
         Unparse_Token (U.all, Result);
      end loop;
   end Unparse_Token_Sequence;

   --------------------
   -- Relative_Token --
   --------------------

   function Relative_Token
     (Token : Token_Reference; Offset : Integer) return Token_Reference
   is
      Current_Token  : Token_Reference := Token;
      Current_Offset : Integer := 0;
   begin
      if Offset < 0 then
         while Current_Offset > Offset loop
            Current_Token := Previous (Current_Token);
            if not Is_Trivia (Current_Token) then
               Current_Offset := Current_Offset - 1;
            end if;
         end loop;

      else
         while Current_Offset < Offset loop
            Current_Token := Next (Current_Token);
            if not Is_Trivia (Current_Token) then
               Current_Offset := Current_Offset + 1;
            end if;
         end loop;
      end if;

      return Current_Token;
   end Relative_Token;

   -----------------
   -- Last_Trivia --
   -----------------

   function Last_Trivia (Token : Token_Reference) return Token_Reference is
      Result : Token_Reference := Token;
      Cur    : Token_Reference := Next (Token);
   begin
      --  Move Last to the last trivia that comes before the next token
      while Cur /= No_Token and then Is_Trivia (Cur) loop
         Result := Cur;
         Cur := Next (Cur);
      end loop;
      return Result;
   end Last_Trivia;

   -------------------
   -- Append_Tokens --
   -------------------

   procedure Append_Tokens
     (Result                  : in out Unparsing_Buffer;
      First_Token, Last_Token : Token_Reference;
      With_Trailing_Trivia    : Boolean := True) is
   begin
      if (First_Token = No_Token and then Last_Token = No_Token)
         or else Last_Token < First_Token
      then
         return;
      end if;
      pragma Assert (First_Token /= No_Token and then Last_Token /= No_Token);
      Apply_Spacing_Rules (Result, Kind (Data (First_Token)));

      declare
         Last : constant Token_Reference := (if With_Trailing_Trivia
                                            then Last_Trivia (Last_Token)
                                            else Last_Token);
      begin
         Append (Result, Kind (Data (Last)), Text (First_Token, Last));
      end;
   end Append_Tokens;

   -------------------
   -- Append_Tokens --
   -------------------

   procedure Append_Tokens
     (Result   : in out Unparsing_Buffer;
      Template : Token_Sequence_Template)
   is
   begin
      if Template.Present then
         Append_Tokens (Result, Template.First, Template.Last);
      end if;
   end Append_Tokens;

end ${ada_lib_name}.Unparsing_Implementation;
