## vim: filetype=makoada

<%
   lexer = ctx.lexer
   termination = lexer.Termination.ada_name

   def token_actions(action_class):
       return sorted(t.ada_name for t in lexer.token_actions[action_class])

   with_symbol_actions = token_actions('WithSymbol')
   with_trivia_actions = token_actions('WithTrivia')
%>

with GNATCOLL.VFS;

with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;
with Langkit_Support.Slocs;        use Langkit_Support.Slocs;
with Langkit_Support.Text;         use Langkit_Support.Text;

with Langkit_Support.Symbols;
use Langkit_Support.Symbols;

with ${ada_lib_name}.Common;
with ${ada_lib_name}.Implementation; use ${ada_lib_name}.Implementation;
use ${ada_lib_name}.Implementation.Precomputed_Symbols;
with ${ada_lib_name}.Lexer_State_Machine;
use ${ada_lib_name}.Lexer_State_Machine;

% if ctx.symbol_canonicalizer:
with ${ctx.symbol_canonicalizer.unit_fqn};
% endif

package body ${ada_lib_name}.Lexer_Implementation is

   use Token_Vectors, Trivia_Vectors, Integer_Vectors;

   procedure Extract_Tokens_From_Text_Buffer
     (Contents    : in out Decoded_File_Contents;
      With_Trivia : Boolean;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   --  Helper for the Extract_Tokens procedure

   generic
      With_Trivia : Boolean;
   procedure Process_All_Tokens
     (Contents    : Decoded_File_Contents;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector);

   function Force_Symbol
     (TDH : Token_Data_Handler;
      T   : in out Stored_Token_Data) return Symbol_Type;
   --  If T has a symbol, return it. Otherwise, force its symbolization and
   --  return the symbol.

   ------------------------
   -- Process_All_Tokens --
   ------------------------

   procedure Process_All_Tokens
     (Contents    : Decoded_File_Contents;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is

      Token    : Lexed_Token;
      Token_Id : Token_Kind := ${termination};
      % if lexer.track_indent:
      Prev_Id  : Token_Kind := ${termination};
      % endif
      Symbol   : Thin_Symbol;

      Last_Token_Last : Natural := Contents.Buffer'First - 1;
      --  Index in TDH.Source_Buffer for the last character of the previous
      --  token. Used to process chunks of ignored text.

      ## Variables specific to indentation tracking
      % if lexer.track_indent:

      --  Stack of indentation levels. Used to emit the proper number of dedent
      --  tokens on dedent.
      Columns_Stack     : array (1 .. 128) of Column_Number := (others => 0);
      Columns_Stack_Len : Natural := 0;

      Ign_Layout_Level  : Integer := 0;
      --  Whether to ignore layout tokens or not. If 0, Ignore is off, if >0,
      --  ignore is on.

      function Get_Col return Column_Number
      is (if Columns_Stack_Len > 0
          then Columns_Stack (Columns_Stack_Len)
          else 1)
      with Inline;
      --  Get the current indent column in the stack

      % endif

      Last_Token_Was_Trivia : Boolean := False;
      --  Whether the last item we added to TDH was a trivia

      procedure Append_Token (Data : Stored_Token_Data) with Inline;
      --  Append a token to TDH and update Last_Token_Was_Trivia accordingly

      procedure Append_Trivia (Data : Stored_Token_Data) with Inline;
      --  If trivia are disabled, do nothing. Otherwise, append a trivia to TDH
      --  and update Last_Token_Was_Trivia and the token/trivia mapping in TDH
      --  accordingly.

      function Source_First return Positive is (Token.Text_First);
      --  Index in TDH.Source_Buffer for the first character corresponding to
      --  the current token.

      function Source_Last return Natural is (Token.Text_Last);
      --  Likewise, for the last character

      ------------------
      -- Append_Token --
      ------------------

      procedure Append_Token (Data : Stored_Token_Data) is
      begin
         --  By default, the current token will have no trivia
         Append (TDH.Tokens_To_Trivias, Integer (No_Token_Index));

         TDH.Tokens.Append (Data);
         Last_Token_Was_Trivia := False;
      end Append_Token;

      -------------------
      -- Append_Trivia --
      -------------------

      procedure Append_Trivia (Data : Stored_Token_Data) is
      begin
         if not With_Trivia then
            return;
         end if;

         --  If the last item added to TDH was a trivia, extend the current
         --  trivia chain. Otherwise, update the Tokens_To_Trivias map to state
         --  that the trivia we are about to add is the first trivia that comes
         --  after the last token.

         if Last_Token_Was_Trivia then
            TDH.Trivias.Last_Element.all.Has_Next := True;
         else
            TDH.Tokens_To_Trivias.Last_Element.all :=
               TDH.Trivias.Last_Index + 1;
         end if;
         TDH.Trivias.Append ((Has_Next => False,
                              T        => Data));
         Last_Token_Was_Trivia := True;
      end Append_Trivia;

      State : Lexer_State;

   begin
      Initialize (State, Contents.Buffer, Contents.First, Contents.Last);
      Token := Last_Token (State);

      --  The first entry in the Tokens_To_Trivias map is for leading trivias
      TDH.Tokens_To_Trivias.Append (Integer (No_Token_Index));

      while Has_Next (State) loop
         Next_Token (State, Token);

         % if lexer.track_indent:
         --  Update the previous token id variable
         Prev_Id := Token_Id;
         % endif

         Token_Id := Token.Kind;
         Symbol := No_Thin_Symbol;

         case Token_Id is

         % if with_symbol_actions:
            ## Token id is part of the class of token types for which we want
            ## to internalize the text.
            when ${' | '.join(with_symbol_actions)} =>
               if TDH.Symbols /= No_Symbol_Table then
                  declare
                     Bounded_Text : Text_Type renames
                        Contents.Buffer (Token.Text_First .. Token.Text_Last);

                     Symbol_Res : constant Symbolization_Result :=
                        % if ctx.symbol_canonicalizer:
                           ${ctx.symbol_canonicalizer.fqn} (Bounded_Text);
                        % else:
                           Create_Symbol (Bounded_Text);
                        % endif
                  begin
                     if Symbol_Res.Success then
                        Symbol := Find (TDH.Symbols, Symbol_Res.Symbol);
                     else
                        Append
                          (Diagnostics,
                           Make_Range
                             (Get_Sloc (TDH, Token.Text_First),
                              Get_Sloc (TDH, Token.Text_Last)),
                           Symbol_Res.Error_Message);
                     end if;
                  end;
               end if;
         % endif

         % if with_trivia_actions:
            when ${' | '.join(with_trivia_actions)} =>
               Append_Trivia ((Kind         => From_Token_Kind (Token_Id),
                               Source_First => Source_First,
                               Source_Last  => Source_Last,
                               Symbol       => No_Thin_Symbol));

               if Token_Id = ${lexer.LexingFailure.ada_name} then
                  Append
                    (Diagnostics,
                     Make_Range
                       (Get_Sloc (TDH, Token.Text_First),
                        Get_Sloc (TDH, Token.Text_Last)),
                     "Invalid token, ignored");
               end if;

               goto Dont_Append;
         % endif

            ## Else, don't keep the text at all
            when others =>
               null;

         end case;

         % if not lexer.track_indent:
         Append_Token
           ((Kind         => From_Token_Kind (Token_Id),
             Source_First => Source_First,
             Source_Last  => Source_Last,
             Symbol       => Symbol));

         ##  This whole section is only emitted if the user chose to track
         ##  indentation in the lexer. It has complex machinery to emit
         ##  Indent/Dedent tokens, but only when in a zone of the code where
         ##  layout is not ignored.
         % else:

         --  If the token is termination, emit the missing dedent tokens
         if Token_Id = ${termination} then
            while Get_Col > 1 loop
               Append_Token
                 ((Kind         => From_Token_Kind (${lexer.Dedent.ada_name}),
                   Source_First => TDH.Source_Last + 1,
                   Source_Last  => TDH.Source_Last,
                   Symbol       => No_Thin_Symbol));
               Columns_Stack_Len := Columns_Stack_Len - 1;
            end loop;
         end if;

         <%
            end_ilayout_toks = (
               " | ".join(t.ada_name
               for t in lexer.tokens if t.end_ignore_layout)
            )
            start_ilayout_toks = (
               " | ".join(t.ada_name
               for t in lexer.tokens if t.start_ignore_layout)
            )
         %>

         --  If we're reading a token that triggers the end of layout ignore...
         % if end_ilayout_toks:
         if Token_Id in ${end_ilayout_toks} then
            --  Decrement the ignore stack...
            Ign_Layout_Level := Ign_Layout_Level - 1;
         end if;
         % endif

         --  If we don't ignore layout, and the token is the first on a new
         --  line, and it is not a newline token, then:
         if Ign_Layout_Level <= 0
            and then Prev_Id = ${lexer.Newline.ada_name}
            and then Token_Id /= ${lexer.Newline.ada_name}
         then
            declare
               T : Stored_Token_Data :=
                 (Kind         => <>,
                  Source_First => Source_First,
                  Source_Last  => Source_First - 1,
                  Symbol       => No_Thin_Symbol);

               --  Compute the column number for the current line: In this case
               --  we know that the previous token was a newline, so we can
               --  just compute from the prev token's last position to the
               --  current token start position.
               Col : Column_Number
                 := Column_Number (Source_First - Last_Token_Last);
            begin
               if Col < Get_Col then
                  --  Emit every necessary dedent token if the line is
                  --  dedented, and pop values from the stack.
                  T.Kind := From_Token_Kind (${lexer.Dedent.ada_name});
                  while Col < Get_Col loop
                     Append_Token (T);
                     Columns_Stack_Len := Columns_Stack_Len - 1;
                  end loop;

               elsif Col > Get_Col then
                  --  Emit a single indent token, and put the new value on the
                  --  indent stack.
                  T.Kind := From_Token_Kind (${lexer.Indent.ada_name});
                  Append_Token (T);
                  Columns_Stack_Len := Columns_Stack_Len + 1;
                  Columns_Stack (Columns_Stack_Len) := Col;
               end if;
            end;

         end if;

         % if start_ilayout_toks:
         --  If we're reading a token that triggers the start of layout ignore,
         --  increment the ignore level.
         if Token_Id in ${start_ilayout_toks} then
            Ign_Layout_Level := Ign_Layout_Level + 1;
         end if;
         % endif

         --  If we're in ignore layout mode, we don't want to emit newline
         --  tokens either.
         if Token_Id /= ${lexer.Newline.ada_name}
            or else Ign_Layout_Level <= 0
         then
            Append_Token
              ((Kind         => From_Token_Kind (Token_Id),
                Source_First => Source_First,
                Source_Last  => Source_Last,
                Symbol       => Symbol));
         end if;
         % endif

         Last_Token_Last := Source_Last;

      % if lexer.token_actions['WithTrivia']:
         <<Dont_Append>>
      % endif
      end loop;

   end Process_All_Tokens;

   procedure Process_All_Tokens_With_Trivia is new Process_All_Tokens (True);
   procedure Process_All_Tokens_No_Trivia is new Process_All_Tokens (False);

   -------------------------------------
   -- Extract_Tokens_From_Text_Buffer --
   -------------------------------------

   procedure Extract_Tokens_From_Text_Buffer
     (Contents    : in out Decoded_File_Contents;
      With_Trivia : Boolean;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector) is
   begin
      --  Make sure CLRF line endings get canonicalized to LF

      Canonicalize_Line_Endings (Contents);

      --  In the case we are reparsing an analysis unit, we want to get rid of
      --  the tokens from the old one.

      Reset
        (TDH, Contents.Buffer, Contents.First, Contents.Last);

      if With_Trivia then
         Process_All_Tokens_With_Trivia (Contents, TDH, Diagnostics);
      else
         Process_All_Tokens_No_Trivia (Contents, TDH, Diagnostics);
      end if;
   end Extract_Tokens_From_Text_Buffer;

   --------------------
   -- Extract_Tokens --
   --------------------

   procedure Extract_Tokens
     (Input         : Internal_Lexer_Input;
      With_Trivia   : Boolean;
      File_Reader   : access Implementation.Internal_File_Reader'Class;
      TDH           : in out Token_Data_Handler;
      Diagnostics   : in out Diagnostics_Vectors.Vector;
      Old_TDH       : access constant Token_Data_Handler;
      Same_Contents : out Boolean)
   is
      use type GNATCOLL.VFS.Filesystem_String;

      Contents : Decoded_File_Contents;
      Partial  : Boolean := False;

      function Is_Same_Contents (Buffer : Text_Type) return Boolean
      is (Old_TDH /= null
          and then Old_TDH.Source_Buffer /= null
          and then not Partial
          and then not Old_TDH.Source_Is_Partial
          and then Buffer = Old_TDH.Source_Buffer
                              (Old_TDH.Source_First .. Old_TDH.Source_Last));
      --  Return whether Old_TDH was passed and its source buffer is identical
      --  to ``Buffer``. If that is the case, we can set ``Same_Contents`` to
      --  true and return without running the lexer.
      --
      --  If the old source buffer or the new one are partial, we never
      --  consider that they are really the same.
   begin
      Same_Contents := False;

      --  Note that it is possible to end up here with both a file reader and a
      --  non-file input: when applying a rewriting session for a context that
      --  has a file reader, we need to parse in-memory buffers that come from
      --  the unparsing of rewritten trees.

      case Input.Kind is
         when File =>
            declare
               Filename : constant String := +Input.Filename.Full_Name.all;
               Charset  : constant String := To_String (Input.Charset);
            begin
               --  Use the file reader if there is one, otherwise just read the
               --  source file on the filesystem.
               if File_Reader = null then
                  Direct_Read
                    (Filename, Charset, Input.Read_BOM, Contents, Diagnostics);
               else
                  File_Reader.Read
                    (Filename, Charset, Input.Read_BOM, Contents, Diagnostics);
               end if;
               Partial := not Diagnostics.Is_Empty;
            end;

            --  If we realize we got the same contents as in Old_TDH, do not
            --  extract tokens and return early.

            if Is_Same_Contents
                 (Contents.Buffer (Contents.First ..  Contents.Last))
            then
               Same_Contents := True;
               Free (Contents.Buffer);
               return;
            end if;

            Extract_Tokens_From_Text_Buffer
              (Contents, With_Trivia, TDH, Diagnostics);
            TDH.Filename := Input.Filename;
            TDH.Charset := Input.Charset;

         when Bytes_Buffer =>
            declare
               Bytes : String (1 .. Input.Bytes_Count)
                  with Import, Address => Input.Bytes;
            begin
               Decode_Buffer
                 (Bytes, To_String (Input.Charset), Input.Read_BOM, Contents,
                  Diagnostics);
               Partial := not Diagnostics.Is_Empty;
            end;

            --  If we realize we got the same contents as in Old_TDH, do not
            --  extract tokens and return early.

            if Is_Same_Contents
                 (Contents.Buffer (Contents.First ..  Contents.Last))
            then
               Same_Contents := True;
               Free (Contents.Buffer);
               return;
            end if;

            Extract_Tokens_From_Text_Buffer
              (Contents, With_Trivia, TDH, Diagnostics);
            TDH.Filename := GNATCOLL.VFS.No_File;
            TDH.Charset := Input.Charset;

         when Text_Buffer =>
            declare
               Text_View : Text_Type (1 .. Input.Text_Count)
                  with Import, Address => Input.Text;
            begin
               --  If we realize we got the same contents as in Old_TDH, do not
               --  extract tokens and return early.

               if Is_Same_Contents (Text_View) then
                  Same_Contents := True;
                  return;
               end if;

               Contents.Buffer := new Text_Type (1 .. Input.Text_Count);
               Contents.First := Contents.Buffer'First;
               Contents.Last := Contents.Buffer'Last;
               Contents.Buffer.all := Text_View;

               Extract_Tokens_From_Text_Buffer
                 (Contents, With_Trivia, TDH, Diagnostics);
               TDH.Filename := GNATCOLL.VFS.No_File;
               TDH.Charset := Null_Unbounded_String;
            end;
      end case;
      TDH.Source_Is_Partial := Partial;
   end Extract_Tokens;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Token : Token_Or_Trivia_Index;
      TDH   : Token_Data_Handler) return Symbol_Type
   is
      subtype Token_Data_Reference is Token_Vectors.Element_Access;

      Token_Data : constant Token_Data_Reference :=
        (if Token.Trivia = No_Token_Index
         then Token_Data_Reference
           (TDH.Tokens.Get_Access (Natural (Token.Token)))
         else Token_Data_Reference'
           (TDH.Trivias.Get_Access (Natural (Token.Trivia) - 1).T'Access));
   begin
      return Force_Symbol (TDH, Token_Data.all);
   end Get_Symbol;

   ------------------
   -- Force_Symbol --
   ------------------

   function Force_Symbol
     (TDH : Token_Data_Handler;
      T   : in out Stored_Token_Data) return Symbol_Type is
   begin
      if T.Symbol = No_Thin_Symbol then
         declare
            Text   : Text_Type renames
               TDH.Source_Buffer (T.Source_First ..  T.Source_Last);
            Symbol : constant Symbolization_Result :=
               % if ctx.symbol_canonicalizer:
                  ${ctx.symbol_canonicalizer.fqn} (Text)
               % else:
                  Create_Symbol (Text)
               % endif
            ;
         begin
            --  This function is run as part of semantic analysis: there is
            --  currently no way to report errors from here, so just discard
            --  canonicalization issues here.
            if Symbol.Success then
               T.Symbol := Find (TDH.Symbols, Symbol.Symbol);
            end if;
         end;
      end if;
      return To_Symbol (TDH.Symbols, T.Symbol);
   end Force_Symbol;

end ${ada_lib_name}.Lexer_Implementation;
