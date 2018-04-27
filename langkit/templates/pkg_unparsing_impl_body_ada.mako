## vim: filetype=makoada

<%namespace name="unparsers"    file="unparsers_ada.mako" />

<% concrete_astnodes = [astnode for astnode in ctx.astnode_types
                        if not astnode.abstract] %>

pragma Warnings (Off, "internal");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "internal");

with GNATCOLL.Iconv;

with ${ada_lib_name}.Analysis.Implementation;
use ${ada_lib_name}.Analysis.Implementation;
with ${ada_lib_name}.Introspection; use ${ada_lib_name}.Introspection;
with ${ada_lib_name}.Lexer;         use ${ada_lib_name}.Lexer;
use ${ada_lib_name}.Lexer.Token_Data_Handlers;

package body ${ada_lib_name}.Unparsing.Implementation is

   subtype Abstract_Node is Analysis.Implementation.Abstract_Node;
   --  Subtype to avoid visibility conflict with an Abstract_Node type coming
   --  from the Analysis package.

   --  The "template" data structures below are helpers for the original
   --  source code formatting preservation algorithms. A template can be
   --  thought as the instantiation of an unparser from an original AST node.
   --  It captures actual sequences of tokens.

   type Token_Sequence_Template (Present : Boolean := False) is record
      case Present is
         when False => null;
         when True =>
            First, Last : Token_Type;
      end case;
   end record;
   --  Captured sequence of tokens

   subtype Present_Token_Sequence_Template is Token_Sequence_Template (True);

   function Create_Token_Sequence
     (Unparser    : Token_Sequence_Access;
      First_Token : in out Token_Type) return Present_Token_Sequence_Template
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
      Rewritten_Node : ${root_node_type_name}) return Regular_Node_Template;
   --  Return the regular node template corresponding to the instatiation of
   --  Rewritten_Node according to Unparser.
   --
   --  This is an absent template if Rewritten_Node is null. Likewise, returned
   --  field templates are absent if the corresponding Rewritten_Node children
   --  are absent.

   function Field_Present
     (Node     : access Abstract_Node_Type'Class;
      Unparser : Field_Unparser) return Boolean;
   --  Return whether the given field is to be considered present according to
   --  the given field unparser.

   procedure Update_Sloc
     (Sloc : in out Source_Location; Char : Wide_Wide_Character);
   --  Update Sloc as if it represented a cursor that move right-wards after
   --  inserting Char to a buffer.

   procedure Unparse_Node
     (Node                : access Abstract_Node_Type'Class;
      Preserve_Formatting : Boolean;
      Result              : in out Unparsing_Buffer);
   --  Using the Node_Unparsers unparsing tables, unparse the given Node

   procedure Unparse_Regular_Node
     (Node           : access Abstract_Node_Type'Class;
      Unparser       : Regular_Node_Unparser;
      Rewritten_Node : ${root_node_type_name};
      Result         : in out Unparsing_Buffer);
   --  Helper for Unparse_Node, focuses on regular nodes

   procedure Unparse_Token
     (Unparser : Token_Unparser;
      Result   : in out Unparsing_Buffer);
   --  Using the Unparser unparsing table, unparse a token

   procedure Unparse_Token_Sequence
     (Unparser : Token_Sequence_Access;
      Result   : in out Unparsing_Buffer);
   --  Using the Unparser unparsing table, unparse a sequence of tokens

   function Relative_Token
     (Token : Token_Type; Offset : Integer) return Token_Type with
      Pre => Token /= No_Token
             and then not Is_Trivia (Token)
             and then
               Token_Index (Integer (Token_Indexes (Token).Token) + Offset)
               in First_Token_Index .. Last_Token (Token_Data (Token).all),
      Post => Relative_Token'Result /= No_Token;
   --  Considering only tokens that are not trivia and assuming Token is at
   --  index I, return the token that is at index I + Offset.

   function Last_Trivia (Token : Token_Type) return Token_Type
      with Pre => Token /= No_Token;
   --  If Token (which can be a token or a trivia) is followed by a sequence of
   --  trivias, return the last of them. Otherwise, return Token itself.

   procedure Append_Tokens
     (Result                  : in out Unparsing_Buffer;
      First_Token, Last_Token : Token_Type;
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

   ## Emit the table to indicate spacing rules between tokens. Use "Lexer."
   ## qualified names for token kinds and token families to avoid conflicts
   ## with node names from "Analysis.".

   Token_Spacing_Table : array (Token_Family, Token_Family) of Boolean :=
      <%
         token_families = ctx.lexer.tokens.token_families
         spacing_table = ctx.lexer.spacing_table
      %>
      (${', '.join(
         'Lexer.{} => ({})'.format(tf1.ada_name, ', '.join(
            'Lexer.{} => {}'.format(tf2.ada_name, spacing_table[tf1][tf2])
            for tf2 in token_families
         ))
         for tf1 in token_families)});
   --  A space must be inserted between two consecutive tokens T1 and T2 iff
   --  given their respective families TF1 and TF2, the following is true:
   --  Token_Spacing_Table (TF1, TF2).

   Token_Newline_Table : array (Token_Kind) of Boolean :=
     (${', '.join('Lexer.{} => {}'.format(t.ada_name,
                                          t in ctx.lexer.newline_after)
                  for t in ctx.lexer.sorted_tokens)});
   --  A line break must be append during unparsing after a token T iff
   --  Token_Newline_Table (T) is true.

   ---------------------------
   -- Create_Token_Sequence --
   ---------------------------

   function Create_Token_Sequence
     (Unparser    : Token_Sequence_Access;
      First_Token : in out Token_Type) return Present_Token_Sequence_Template
   is
      Result : Present_Token_Sequence_Template;
   begin
      Result.First := First_Token;
      Result.Last := Relative_Token (First_Token, Unparser'Length - 1);
      First_Token := Relative_Token (Result.Last, 1);
      return Result;
   end Create_Token_Sequence;

   -----------------------------------
   -- Extract_Regular_Node_Template --
   -----------------------------------

   function Extract_Regular_Node_Template
     (Unparser       : Regular_Node_Unparser;
      Rewritten_Node : ${root_node_type_name}) return Regular_Node_Template
   is
      Result     : Regular_Node_Template (True, Unparser.Field_Unparsers.N);
      Next_Token : Token_Type;
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
      for I in 1 .. Rewritten_Node.Abstract_Children_Count loop
         declare
            U     : Field_Unparser_List renames Unparser.Field_Unparsers.all;
            F     : Field_Unparser renames U.Field_Unparsers (I);
            T     : Token_Sequence_Access renames U.Inter_Tokens (I);
            FT    : Field_Template renames Result.Fields (I);

            Rewritten_Child : constant ${root_node_type_name} :=
               Rewritten_Node.Child (I);
            R_Child         : constant Abstract_Node :=
               Abstract_Node (Rewritten_Child);
         begin
            Result.Inter_Tokens (I) :=
              (if I = 1
               then (Present => False)
               else Create_Token_Sequence (T, Next_Token));

            if Field_Present (R_Child, F) then
               FT := (Present => True, others => <>);

               --  Pre_Tokens is the sequence that starts at Next_Token and
               --  whose length is the one the unparser gives.
               FT.Pre_Tokens :=
                  Create_Token_Sequence (F.Pre_Tokens, Next_Token);

               --  Post_Tokens is the sequence that starts right after the last
               --  token of the node field, also sized from the unparser.
               --  Beware of ghost nodes, which own to token.
               Next_Token :=
                 (if Rewritten_Child.Is_Ghost
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
     (Node     : access Abstract_Node_Type'Class;
      Unparser : Field_Unparser) return Boolean is
   begin
      return (Node /= null
              and then (not Unparser.Empty_List_Is_Absent
                        or else Node.Abstract_Children_Count > 0));
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

      elsif Token_Newline_Table (Buffer.Last_Token) then
         Append (Buffer, Chars.LF);

      elsif Token_Spacing_Table
        (Token_Kind_To_Family (Buffer.Last_Token),
         Token_Kind_To_Family (Next_Token))
      then
         Append (Buffer, ' ');
      end if;
   end Apply_Spacing_Rules;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Node                : access Abstract_Node_Type'Class;
      Unit                : Analysis_Unit;
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
     (Node                : access Abstract_Node_Type'Class;
      Unit                : Analysis_Unit;
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

      --  Unparse Node, and the leading trivia if we are unparsing the unit as
      --  a whole.
      if As_Unit then
         declare
            First : constant Token_Type := First_Token (Unit);
         begin
            if Is_Trivia (First) then
               Append_Tokens (Buffer, First, Last_Trivia (First),
                              With_Trailing_Trivia => False);
            end if;
         end;
      end if;
      Unparse_Node (Node, Preserve_Formatting, Buffer);

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
            From_Code => Internal_Charset);
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
     (Node                : access Abstract_Node_Type'Class;
      Preserve_Formatting : Boolean) return Text_Type
   is
      Buffer : Unparsing_Buffer;
   begin
      % if ctx.generate_unparser:
         if Node = null then
            return (raise Program_Error with "cannot unparse null node");
         end if;

         Unparse_Node (Node, Preserve_Formatting, Buffer);
         return To_Wide_Wide_String (Buffer.Content);
      % else:
         pragma Unreferenced (Buffer);
         return (raise Program_Error with "Unparser not generated");
      % endif
   end Unparse;

   ------------------
   -- Unparse_Node --
   ------------------

   procedure Unparse_Node
     (Node                : access Abstract_Node_Type'Class;
      Preserve_Formatting : Boolean;
      Result              : in out Unparsing_Buffer)
   is
      Kind     : constant ${root_node_kind_name} := Node.Abstract_Kind;
      Unparser : Node_Unparser renames Node_Unparsers (Kind);

      Rewritten_Node : constant ${root_node_type_name} :=
        (if Preserve_Formatting
         then Node.Abstract_Rewritten_Node
         else null);
   begin
      case Unparser.Kind is
         when Regular =>
            Unparse_Regular_Node (Node, Unparser, Rewritten_Node, Result);

         when List =>
            declare
               Count : constant Natural := Node.Abstract_Children_Count;
            begin
               for I in 1 .. Count loop
                  Unparse_Node (Node.Abstract_Child (I),
                                Preserve_Formatting,
                                Result);

                  if I < Count and then Unparser.Has_Separator then
                     Unparse_Token (Unparser.Separator, Result);
                  end if;
               end loop;
            end;

         when Token =>
            declare
               Tok_Kind : constant Token_Kind := Token_Node_Kind (Kind);
            begin
               --  Add the single token that materialize Node itself
               Apply_Spacing_Rules (Result, Tok_Kind);
               Append (Result, Tok_Kind, Node.Abstract_Text);

               --  If Node comes from an original node, also append the trivia
               --  that comes after.
               if Rewritten_Node /= null then
                  declare
                     Token     : constant Token_Type :=
                        Rewritten_Node.Token_End;
                     Last_Triv : constant Token_Type := Last_Trivia (Token);
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
     (Node           : access Abstract_Node_Type'Class;
      Unparser       : Regular_Node_Unparser;
      Rewritten_Node : ${root_node_type_name};
      Result         : in out Unparsing_Buffer)
   is
      Template : constant Regular_Node_Template :=
         Extract_Regular_Node_Template (Unparser, Rewritten_Node);
   begin
      --  Unparse tokens that precede the first field. Re-use original ones if
      --  available.
      if Template.Present then
         Append_Tokens (Result, Template.Pre_Tokens);
      else
         Unparse_Token_Sequence (Unparser.Pre_Tokens, Result);
      end if;

      --  Unparse Node's fields, and the tokens between them
      declare
         U : Field_Unparser_List renames Unparser.Field_Unparsers.all;
      begin
         for I in 1 .. U.N loop
            declare
               F     : Field_Unparser renames U.Field_Unparsers (I);
               Child : constant Abstract_Node := Node.Abstract_Child (I);
            begin
               --  First unparse tokens that appear unconditionally between
               --  fields.
               if Template.Present then
                  Append_Tokens (Result, Template.Inter_Tokens (I));
               else
                  Unparse_Token_Sequence (U.Inter_Tokens (I), Result);
               end if;

               --  Then unparse the field itself
               if Field_Present (Child, F) then
                  if Template.Present and then Template.Fields (I).Present then
                     Append_Tokens (Result, Template.Fields (I).Pre_Tokens);
                     Unparse_Node (Child, Rewritten_Node /= null, Result);
                     Append_Tokens (Result, Template.Fields (I).Post_Tokens);

                  else
                     Unparse_Token_Sequence (F.Pre_Tokens, Result);
                     Unparse_Node (Child, Rewritten_Node /= null, Result);
                     Unparse_Token_Sequence (F.Post_Tokens, Result);
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
         Unparse_Token_Sequence (Unparser.Post_Tokens, Result);
      end if;
   end Unparse_Regular_Node;

   -------------------
   -- Unparse_Token --
   -------------------

   procedure Unparse_Token
     (Unparser : Token_Unparser;
      Result   : in out Unparsing_Buffer) is
   begin
      Apply_Spacing_Rules (Result, Unparser.Kind);
      if Unparser.Text /= null then
         Append (Result, Unparser.Kind, Unparser.Text.all);
      else
         declare
            Literal : constant Text_Type := Token_Kind_Literal (Unparser.Kind);
         begin
            pragma Assert (Literal'Length > 0);
            Append (Result, Unparser.Kind, Literal);
         end;
      end if;
   end Unparse_Token;

   ----------------------------
   -- Unparse_Token_Sequence --
   ----------------------------

   procedure Unparse_Token_Sequence
     (Unparser : Token_Sequence_Access;
      Result   : in out Unparsing_Buffer) is
   begin
      for U of Unparser.all loop
         Unparse_Token (U, Result);
      end loop;
   end Unparse_Token_Sequence;

   --------------------
   -- Relative_Token --
   --------------------

   function Relative_Token
     (Token : Token_Type; Offset : Integer) return Token_Type
   is
      Current_Token  : Token_Type := Token;
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

   function Last_Trivia (Token : Token_Type) return Token_Type is
      Result : Token_Type := Token;
      Cur    : Token_Type := Next (Token);
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
      First_Token, Last_Token : Token_Type;
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
         Last : constant Token_Type := (if With_Trailing_Trivia
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

   % if ctx.generate_unparser:

      ## Emit constants for token unparsers and token sequence unparsers

      % for tok in ctx.unparsers.sorted_token_unparsers:
         ${tok.var_name} : aliased constant Token_Unparser := \
           (${tok.token.ada_name}, \
            ${("new Text_Type'({})".format(tok.string_repr)
               if tok.match_text else 'null')});
      % endfor

      % for tok_seq in ctx.unparsers.token_sequence_unparsers:
         % if tok_seq:
            ${tok_seq.var_name} : aliased constant Token_Sequence := \
              (${', '.join(
                  '{} => {}'.format(i, tok.var_name)
                  for i, tok in enumerate(tok_seq.tokens, 1))});
         % endif
      % endfor

      ## Emit constants for lists of field unparsers

      % for node in ctx.astnode_types:
         % if is_regular_node_unparser(node.unparser) \
              and node.unparser.field_unparsers:

            <%
               unparser_list = node.unparser.zip_fields
               field_unparsers = [
                  ('{} => Empty_Field_Unparser'.format(i)
                   if not f.pre_tokens and not f.post_tokens else
                   "{} => ({}'Access, {}'Access, {})".format(
                        i, f.pre_tokens.var_name, f.post_tokens.var_name,
                        f.empty_list_is_absent))
                  for i, (f, _) in enumerate(unparser_list, 1)
               ]
               inter_tokens = [
                  "{} => {}'Access".format(i, tok_seq.var_name)
                  for i, (_, tok_seq) in enumerate(unparser_list, 1)
               ]
            %>

            ${node.unparser.fields_unparser_var_name} \
               : aliased constant Field_Unparser_List \
               := (N => ${len(unparser_list)},
                   Field_Unparsers => (${', '.join(field_unparsers)}),
                   Inter_Tokens => (${', '.join(inter_tokens)}));
         % endif
      % endfor

      ## Finally, emit the unparsing table for nodes themselves

      Node_Unparsers_Array : aliased constant Node_Unparser_Map := (
         % for i, node in enumerate(ctx.astnode_types, 1):
            % if not node.abstract:
               <% unparser = node.unparser %>

               ${node.ada_kind_name} => ( \
                  % if is_regular_node_unparser(unparser):
                     Kind            => Regular,
                     Pre_Tokens      => \
                        ${unparser.pre_tokens.var_name}'Access,
                     Field_Unparsers => \
                        ${unparser.fields_unparser_var_name}'Access,
                     Post_Tokens     => \
                        ${unparser.post_tokens.var_name}'Access

                  % elif is_list_node_unparser(unparser):
                     Kind          => List, \
                     Has_Separator => ${unparser.separator is not None}, \
                     Separator     => ${('<>' if unparser.separator is None else
                                         unparser.separator.var_name)}

                  % elif is_token_node_unparser(unparser):
                     Kind => Token

                  % else:
                     ## This node is synthetic, so it cannot be unparsed:
                     ## provide a dummy entry.
                     <% assert ((node.abstract or node.synthetic) and
                                unparser is None), (
                        'Unexpected unparser for {}: {}'.format(
                           node.dsl_name, unparser
                        )
                     ) %>
                     Kind => Token
                  % endif
               )${',' if i < len(ctx.astnode_types) else ''}
            % endif
         % endfor
      );
   % endif

begin
   Node_Unparsers := ${("Node_Unparsers_Array'Access"
                        if ctx.generate_unparser else 'null')};
end ${ada_lib_name}.Unparsing.Implementation;
