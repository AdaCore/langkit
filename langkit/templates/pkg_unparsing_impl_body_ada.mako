## vim: filetype=makoada

<%namespace name="unparsers"    file="unparsers_ada.mako" />

<% concrete_astnodes = [astnode for astnode in ctx.astnode_types
                        if not astnode.abstract] %>

with GNATCOLL.Iconv;

with ${ada_lib_name}.Analysis.Implementation;
use ${ada_lib_name}.Analysis.Implementation;

with ${ada_lib_name}.Lexer; use ${ada_lib_name}.Lexer;

package body ${ada_lib_name}.Unparsing.Implementation is

   subtype Abstract_Node is Analysis.Implementation.Abstract_Node;
   --  Subtype to avoid visibility conflict with an Abstract_Node type coming
   --  from the Analysis package.

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
      Unparser       : Node_Unparser;
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

   procedure Append (Buffer : in out Unparsing_Buffer; Text : Text_Type) is
   begin
      for C of Text loop
         Update_Sloc (Buffer.Last_Sloc, C);
      end loop;
      Append (Buffer.Content, Text);
   end Append;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Node                : access Abstract_Node_Type'Class;
      Unit                : Analysis_Unit;
      Preserve_Formatting : Boolean) return String
   is
      Result : String_Access := Unparse (Node, Unit, Preserve_Formatting);
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
      Preserve_Formatting : Boolean) return String_Access
   is
      Buffer : constant Text_Type := Unparse (Node, Preserve_Formatting);
   begin
      --  GNATCOLL.Iconv raises a Constraint_Error for empty strings: handle
      --  them here.
      if Buffer'Length = 0 then
         return new String'("");
      end if;

      declare
         use GNATCOLL.Iconv;

         State  : Iconv_T := Iconv_Open
           (To_Code   => Get_Charset (Unit),
            From_Code => Internal_Charset);
         Status : Iconv_Result;

         To_Convert_String : constant String (1 .. 4 * Buffer'Length)
            with Import     => True,
                 Convention => Ada,
                 Address    => Buffer'Address;

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
      Unparser : Node_Unparser renames Node_Unparsers (Node.Abstract_Kind);

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
            Append (Result, Node.Abstract_Text);
            Append (Result, " ");
      end case;
   end Unparse_Node;

   --------------------------
   -- Unparse_Regular_Node --
   --------------------------

   procedure Unparse_Regular_Node
     (Node           : access Abstract_Node_Type'Class;
      Unparser       : Node_Unparser;
      Rewritten_Node : ${root_node_type_name};
      Result         : in out Unparsing_Buffer)
   is
      Token_Cursor : Token_Type := No_Token;
   begin
      --  Unparse tokens that precede the first field
      Unparse_Token_Sequence (Unparser.Pre_Tokens, Result);

      --  Unparse Node's fields, and the tokens between them
      declare
         U : Field_Unparser_List renames Unparser.Field_Unparsers.all;
      begin
         for I in 1 .. U.N loop
            declare
               F     : Field_Unparser renames U.Field_Unparsers (I);
               T     : Token_Sequence_Access renames U.Inter_Tokens (I);
               Child : constant Abstract_Node := Node.Abstract_Child (I);
            begin
               Unparse_Token_Sequence (T, Result);
               if Field_Present (Child, F) then
                  Unparse_Token_Sequence (F.Pre_Tokens, Result);
                  Unparse_Node (Child, Rewritten_Node /= null, Result);
                  Unparse_Token_Sequence (F.Post_Tokens, Result);
               end if;
            end;
         end loop;
      end;

      --  Unparse tokens that suceed to the last field
      Unparse_Token_Sequence (Unparser.Post_Tokens, Result);
   end Unparse_Regular_Node;

   -------------------
   -- Unparse_Token --
   -------------------

   procedure Unparse_Token
     (Unparser : Token_Unparser;
      Result   : in out Unparsing_Buffer)
   is
   begin
      if Unparser.Text /= null then
         Append (Result, Unparser.Text.all);
      else
         declare
            Literal : constant Text_Type := Token_Kind_Literal (Unparser.Kind);
         begin
            pragma Assert (Literal'Length > 0);
            Append (Result, Literal);
         end;
      end if;
      Append (Result, " ");
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
