## vim: filetype=makoada

<%
   lexer = ctx.lexer
   termination = lexer.Termination.ada_name

   def token_actions(action_class):
       return sorted(t.ada_name for t in lexer.token_actions[action_class])

   with_symbol_actions = token_actions('WithSymbol')
   with_trivia_actions = token_actions('WithTrivia')
%>

with Ada.Unchecked_Conversion;

with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;

with System;

with GNAT.Byte_Order_Mark;

with GNATCOLL.Iconv;
with GNATCOLL.Mmap;    use GNATCOLL.Mmap;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with ${ada_lib_name}.Common;
use ${ada_lib_name}.Common.Symbols;
with ${ada_lib_name}.Lexer; use ${ada_lib_name}.Lexer;

% if ctx.symbol_canonicalizer:
with ${ctx.symbol_canonicalizer.unit_fqn};
% endif

package body ${ada_lib_name}.Lexer_Implementation is

   Quex_Leading_Characters  : constant := 2;
   Quex_Trailing_Characters : constant := 1;
   Quex_Extra_Characters    : constant :=
      Quex_Leading_Characters + Quex_Trailing_Characters;
   --  Quex requires its input buffer to have two leading reserved bytes and
   --  one trailing. These are not part of the true payload, but must be
   --  available anyway.

   use Token_Vectors, Trivia_Vectors, Integer_Vectors;

   type Quex_Token_Type is record
      Id          : Unsigned_16;
      Text        : System.Address;
      Text_Length : size_t;
      Offset      : Unsigned_32;
   end record
      with Convention => C;
   type Interface_Token_Access is access all Quex_Token_Type;

   type Lexer_Type is new System.Address;

   procedure Decode_Buffer
     (Buffer, Charset : String;
      Read_BOM        : Boolean;
      Decoded_Buffer  : out Text_Access;
      Source_First    : out Positive;
      Source_Last     : out Natural);
   --  Allocate a Text_Type buffer, set it to Decoded_Buffer, decode Buffer
   --  into it using Charset and Source_First/Source_Last to the actual slice
   --  in Decoded_Buffer that hold the input source text. It is up to the
   --  caller to deallocate Decoded_Buffer when done with it.
   --
   --  Raise an Unknown_Charset exception if Charset is... unknown. Raise
   --  Invalid_Input if Buffer contains invalid byte sequences according to
   --  Charset.
   --
   --  Quex quirk: this actually allocates more than the actual buffer to keep
   --  Quex happy. The two first characters are set to null and there is an
   --  extra null character at the end of the buffer. See the Quex_*_Characters
   --  constants above.

   procedure Extract_Tokens_From_Text_Buffer
     (Decoded_Buffer : Text_Access;
      Source_First   : Positive;
      Source_Last    : Natural;
      Tab_Stop       : Positive;
      With_Trivia    : Boolean;
      TDH            : in out Token_Data_Handler;
      Diagnostics    : in out Diagnostics_Vectors.Vector);
   --  Helper for the Extract_Tokens* procedures. Buffer must be a buffer
   --  allocated to fullfil Quex's needs: see the Quex_*_Characters constants
   --  above.

   procedure Extract_Tokens_From_Bytes_Buffer
     (Buffer, Charset : String;
      Read_BOM        : Boolean;
      Tab_Stop        : Positive;
      With_Trivia     : Boolean;
      TDH             : in out Token_Data_Handler;
      Diagnostics     : in out Diagnostics_Vectors.Vector);
   --  Helper for the Extract_Token procedure

   function Lexer_From_Buffer (Buffer  : System.Address;
                               Length  : size_t)
                               return Lexer_Type
      with Import        => True,
           Convention    => C,
           External_Name => "${capi.get_name("lexer_from_buffer")}";

   procedure Free_Lexer (Lexer : Lexer_Type)
      with Import        => True,
           Convention    => C,
           External_Name => "${capi.get_name("free_lexer")}";

   function Next_Token (Lexer : Lexer_Type;
                        Token : Interface_Token_Access) return int
      with Import        => True,
           Convention    => C,
           External_Name => "${capi.get_name("next_token")}";

   generic
      With_Trivia : Boolean;
   procedure Process_All_Tokens
     (Lexer       : Lexer_Type;
      Tab_Stop    : Positive;
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
     (Lexer       : Lexer_Type;
      Tab_Stop    : Positive;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is

      Token                 : aliased Quex_Token_Type;
      Token_Id              : Token_Kind := ${termination};
      % if lexer.track_indent:
      Prev_Id               : Token_Kind := ${termination};
      % endif
      Symbol                : Symbol_Type;
      Continue              : Boolean := True;
      Last_Token_Was_Trivia : Boolean := False;

      Current_Sloc : Source_Location := (1, 1);
      --  Source location before scanning the current token

      Next_Sloc : Source_Location;
      --  Source location after scanning the current token

      Last_Token_Last : Natural;
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

      function Source_First return Positive is
        (Natural (Token.Offset) + TDH.Source_First - 1);
      --  Index in TDH.Source_Buffer for the first character corresponding to
      --  the current token.

      function Source_Last return Natural is
        (Source_First + Natural (Token.Text_Length) - 1);
      --  Likewise, for the last character

      function Sloc_Range return Source_Location_Range is
        (Make_Range (Current_Sloc, Next_Sloc));
      --  Create a sloc range value corresponding to Token

      procedure Prepare_For_Trivia
        with Inline;
      --  Append an entry for the current token in the Tokens_To_Trivias
      --  correspondence vector.

      function Sloc_After
        (Base_Sloc : Source_Location; Text : Text_Type) return Source_Location;
      --  Return Base_Sloc updated as if Text was appended

      ------------------------
      -- Prepare_For_Trivia --
      ------------------------

      procedure Prepare_For_Trivia is
      begin
         if With_Trivia then
            --  By default, the current token will have no trivia
            Append (TDH.Tokens_To_Trivias, Integer (No_Token_Index));

            --  Reset Last_Token_Was_Trivia so that new trivia is added to the
            --  current token.
            Last_Token_Was_Trivia := False;
         end if;
      end Prepare_For_Trivia;

      ----------------
      -- Sloc_After --
      ----------------

      function Sloc_After
        (Base_Sloc : Source_Location; Text : Text_Type) return Source_Location
      is
      begin
         return Result : Source_Location := Base_Sloc do
            --  TODO: use the Unicode algorithm to account for grapheme
            --  clusters.
            for T of Text loop
               case T is
                  when Chars.LF =>
                     Result := (Result.Line + 1, 1);

                  when Chars.HT =>
                     --  Make horizontal tabulations move by stride of 8
                     --  columns, as usually implemented in code editors.
                     declare
                        Zero_Based : constant Natural :=
                           Natural (Result.Column - 1);
                        Aligned    : constant Natural :=
                           (Zero_Based + Tab_Stop) / Tab_Stop * Tab_Stop;
                     begin
                        Result.Column := Column_Number (Aligned + 1);
                     end;

                  when others =>
                     Result.Column := Result.Column + 1;
               end case;
            end loop;
         end return;
      end Sloc_After;

   begin
      --  The first entry in the Tokens_To_Trivias map is for leading trivias
      Prepare_For_Trivia;

      Token.Offset := 0;
      Last_Token_Last := Source_First;

      while Continue loop

         --  Next_Token returns 0 for the last token, which will be our "null"
         --  token.

         Continue := Next_Token (Lexer, Token'Unrestricted_Access) /= 0;

         % if lexer.track_indent:
         --  Update the previous token id variable
         Prev_Id := Token_Id;
         % endif

         Token_Id := Token_Kind'Enum_Val (Token.Id);
         Symbol := null;

         --  Initialize the first sloc for the token to come. For this, process
         --  the text that was ignored since the last token.
         declare
            Ignored_Text : Text_Type renames
               TDH.Source_Buffer (Last_Token_Last + 1 .. Source_First - 1);
         begin
            Current_Sloc := Sloc_After (Current_Sloc, Ignored_Text);
            Last_Token_Last := Source_Last;
         end;

         --  Then update Next_Sloc according to Token's text
         if Token_Id /= ${termination} then
            declare
               Text : Text_Type (1 .. Natural (Token.Text_Length))
                  with Import, Address => Token.Text;
            begin
               Next_Sloc := Sloc_After (Current_Sloc, Text);
            end;
         end if;

         case Token_Id is

         % if with_symbol_actions:
            ## Token id is part of the class of token types for which we want to
            ## internalize the text.
            when ${' | '.join(with_symbol_actions)} =>
               if TDH.Symbols /= No_Symbol_Table then
                  declare
                     Bounded_Text : Text_Type
                       (1 .. Natural (Token.Text_Length))
                        with Address => Token.Text;

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
                        Append (Diagnostics, Sloc_Range,
                                Symbol_Res.Error_Message);
                     end if;
                  end;
               end if;
         % endif

         % if with_trivia_actions:
            when ${' | '.join(with_trivia_actions)} =>
               if With_Trivia then
                  if Last_Token_Was_Trivia then
                     Last_Element (TDH.Trivias).all.Has_Next := True;
                  else
                     Last_Element (TDH.Tokens_To_Trivias).all :=
                        Last_Index (TDH.Trivias) + 1;
                  end if;

                  Append
                    (TDH.Trivias,
                     (Has_Next => False,
                      T        => (Kind         => From_Token_Kind (Token_Id),
                                   Source_First => Source_First,
                                   Source_Last  => Source_Last,
                                   Symbol       => null,
                                   Sloc_Range   => Sloc_Range)));

                  Last_Token_Was_Trivia := True;
               end if;

               --  Whether or nottrivia is disabled, emit a diagnostic for
               --  lexing failures.

               if Token_Id = ${lexer.LexingFailure.ada_name} then
                  Append (Diagnostics, Sloc_Range,
                          "Invalid token, ignored");
               end if;

               goto Dont_Append;
         % endif

            ## Else, don't keep the text at all
            when others =>
               null;

         end case;

         % if not lexer.track_indent:
         --  Special case for the termination token: Quex yields inconsistent
         --  offsets/sizes. Make sure we get the end of the buffer so that the
         --  rest of our machinery (in particular source slices) works well
         --  with it.

         TDH.Tokens.Append
           ((Kind         => From_Token_Kind (Token_Id),
             Source_First => (if Token_Id = ${termination}
                              then TDH.Source_Last + 1
                              else Source_First),
             Source_Last  => (if Token_Id = ${termination}
                              then TDH.Source_Last
                              else Source_Last),
             Symbol       => Symbol,
             Sloc_Range   => Sloc_Range));

         ##  This whole section is only emitted if the user chose to track
         ##  indentation in the lexer. It has complex machinery to emit
         ##  Indent/Dedent tokens, but only when in a zone of the code where
         ##  layout is not ignored.
         % else:

         --  If the token is termination, emit the missing dedent tokens
         if Token_Id = ${termination} then
            while Get_Col > 1 loop
               TDH.Tokens.Append
                 ((Kind         => From_Token_Kind (${lexer.Dedent.ada_name}),
                   Source_First => TDH.Source_Last + 1,
                   Source_Last  => TDH.Source_Last,
                   Symbol       => null,
                   Sloc_Range   => Sloc_Range));
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

         --  If we're reading a token that triggers the end of layout ignore ..
         % if end_ilayout_toks:
         if Token_Id in ${end_ilayout_toks} then
            --  Decrement the ignore stack ..
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
                  Source_First => Source_First + 1,
                  Source_Last  => Source_First,
                  Symbol       => null,
                  Sloc_Range   =>
                    (Sloc_Range.Start_Line, Sloc_Range.Start_Line,
                     Sloc_Range.Start_Column, Sloc_Range.Start_Column));
            begin
               if Sloc_Range.Start_Column < Get_Col then
                  --  Emit every necessary dedent token if the line is
                  -- dedented, and pop values from the stack.
                  while Sloc_Range.Start_Column < Get_Col loop
                     T.Kind := From_Token_Kind (${lexer.Dedent.ada_name});
                     TDH.Tokens.Append (T);
                     Columns_Stack_Len := Columns_Stack_Len - 1;
                  end loop;
               elsif Sloc_Range.Start_Column > Get_Col then
                  --  Emit a single indent token, and put the new value on the
                  --  indent stack.
                  T.Kind := From_Token_Kind (${lexer.Indent.ada_name});
                  TDH.Tokens.Append (T);
                  Columns_Stack_Len := Columns_Stack_Len + 1;
                  Columns_Stack (Columns_Stack_Len) := Sloc_Range.Start_Column;
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
            TDH.Tokens.Append
              ((Kind         => From_Token_Kind (Token_Id),
                Source_First => (if Token_Id = ${termination}
                                 then TDH.Source_Last + 1
                                 else Source_First),
                Source_Last  => (if Token_Id = ${termination}
                                 then TDH.Source_Last
                                 else Source_Last),
                Symbol       => Symbol,
                Sloc_Range   => Sloc_Range));
         end if;
         % endif

         Prepare_For_Trivia;

      % if lexer.token_actions['WithTrivia']:
         <<Dont_Append>>
      % endif
         Current_Sloc := Next_Sloc;
      end loop;

   end Process_All_Tokens;

   procedure Process_All_Tokens_With_Trivia is new Process_All_Tokens (True);
   procedure Process_All_Tokens_No_Trivia is new Process_All_Tokens (False);

   ----------------------------
   -- Lex_From_Buffer_Helper --
   ----------------------------

   procedure Extract_Tokens_From_Text_Buffer
     (Decoded_Buffer : Text_Access;
      Source_First   : Positive;
      Source_Last    : Natural;
      Tab_Stop       : Positive;
      With_Trivia    : Boolean;
      TDH            : in out Token_Data_Handler;
      Diagnostics    : in out Diagnostics_Vectors.Vector)
   is
      Lexer : Lexer_Type := Lexer_From_Buffer
        (Decoded_Buffer.all'Address,
         size_t (Source_Last - Source_First + 1));
   begin

      --  In the case we are reparsing an analysis unit, we want to get rid of
      --  the tokens from the old one.

      Reset (TDH, Decoded_Buffer, Source_First, Source_Last);

      if With_Trivia then
         Process_All_Tokens_With_Trivia (Lexer, Tab_Stop, TDH, Diagnostics);
      else
         Process_All_Tokens_No_Trivia (Lexer, Tab_Stop, TDH, Diagnostics);
      end if;
      Free_Lexer (Lexer);
   end Extract_Tokens_From_Text_Buffer;

   --------------------------------------
   -- Extract_Tokens_From_Bytes_Buffer --
   --------------------------------------

   procedure Extract_Tokens_From_Bytes_Buffer
     (Buffer, Charset : String;
      Read_BOM        : Boolean;
      Tab_Stop        : Positive;
      With_Trivia     : Boolean;
      TDH             : in out Token_Data_Handler;
      Diagnostics     : in out Diagnostics_Vectors.Vector)
   is
      Decoded_Buffer : Text_Access;
      Source_First   : Positive;
      Source_Last    : Natural;
   begin
      Decode_Buffer (Buffer, Charset, Read_BOM, Decoded_Buffer, Source_First,
                     Source_Last);
      Extract_Tokens_From_Text_Buffer
        (Decoded_Buffer, Source_First, Source_Last, Tab_Stop, With_Trivia, TDH,
         Diagnostics);
   end Extract_Tokens_From_Bytes_Buffer;

   --------------------
   -- Extract_Tokens --
   --------------------

   procedure Extract_Tokens
     (Input       : Internal_Lexer_Input;
      Tab_Stop    : Positive;
      With_Trivia : Boolean;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector) is
   begin
      case Input.Kind is
         when File =>
            declare
               use GNATCOLL.VFS;

               --  The following call to Open_Read may fail with a Name_Error
               --  exception: just let it propagate to the caller as there is
               --  no resource to release yet here.

               File : Mapped_File := Open_Read (+Input.Filename.Full_Name.all);

               Region      : Mapped_Region := Read (File);
               Buffer_Addr : constant System.Address :=
                  Data (Region).all'Address;

               Buffer : String (1 .. Last (Region))
                  with Import  => True,
                       Address => Buffer_Addr;
            begin
               Extract_Tokens_From_Bytes_Buffer
                 (Buffer, To_String (Input.Charset), Input.Read_BOM, Tab_Stop,
                  With_Trivia, TDH, Diagnostics);
               Free (Region);
               Close (File);
            exception
               when Unknown_Charset | Invalid_Input =>
                  Free (Region);
                  Close (File);
                  raise;
            end;

         when Bytes_Buffer =>
            declare
               Bytes : String (1 .. Input.Bytes_Count)
                  with Import, Address => Input.Bytes;
            begin
               Extract_Tokens_From_Bytes_Buffer
                 (Bytes, To_String (Input.Charset), Input.Read_BOM, Tab_Stop,
                  With_Trivia, TDH, Diagnostics);
            end;

         when Text_Buffer =>
            declare
               Decoded_Buffer : Text_Access := new Text_Type
                 (1 .. Input.Text_Count + Quex_Extra_Characters);
               Source_First  : constant Positive :=
                  1 + Quex_Leading_Characters;
               Source_Last   : constant Positive :=
                  Source_First + Input.Text_Count - 1;

               Text_View : Text_Type (1 .. Input.Text_Count)
                  with Import, Address => Input.Text;
            begin
               Decoded_Buffer.all (Source_First .. Source_Last) := Text_View;
               Extract_Tokens_From_Text_Buffer
                 (Decoded_Buffer, Source_First, Source_Last, Tab_Stop,
                  With_Trivia, TDH, Diagnostics);
            end;
      end case;
   end Extract_Tokens;

   -------------------
   -- Decode_Buffer --
   -------------------

   procedure Decode_Buffer
     (Buffer, Charset : String;
      Read_BOM        : Boolean;
      Decoded_Buffer  : out Text_Access;
      Source_First    : out Positive;
      Source_Last     : out Natural)
   is
      use GNAT.Byte_Order_Mark;
      use GNATCOLL.Iconv;

      --  In the worst case, we have one character per input byte, so the
      --  following is supposed to be big enough.

      Result : Text_Access :=
         new Text_Type (1 .. Buffer'Length + Quex_Extra_Characters);
      State  : Iconv_T;
      Status : Iconv_Result;
      BOM    : BOM_Kind := Unknown;

      Input_Index, Output_Index : Positive;

      First_Output_Index : constant Positive :=
         1 + Quex_Leading_Characters * 4;
      --  Index of the first byte in Result at which Iconv must decode Buffer

      Output : Byte_Sequence (1 .. 4 * Buffer'Size);
      for Output'Address use Result.all'Address;
      --  Iconv works on mere strings, so this is a kind of a view conversion

   begin
      Decoded_Buffer := Result;
      Source_First := Result'First + Quex_Leading_Characters;

      --  If we have a byte order mark, it overrides the requested Charset

      Input_Index := Buffer'First;
      if Read_BOM then
         declare
            Len : Natural;
         begin
            GNAT.Byte_Order_Mark.Read_BOM (Buffer, Len, BOM);
            Input_Index := Input_Index + Len;
         end;
      end if;

      --  GNATCOLL.Iconv raises a Constraint_Error for empty strings: handle
      --  them here.

      if Input_Index > Buffer'Last then
         Source_Last := Source_First - 1;
         return;
      end if;

      --  Create the Iconv converter. We will notice unknown charsets here

      declare
         BOM_Kind_To_Charset : constant
            array (UTF8_All .. UTF32_BE) of String_Access :=
           (UTF8_All => UTF8'Unrestricted_Access,
            UTF16_LE => UTF16LE'Unrestricted_Access,
            UTF16_BE => UTF16BE'Unrestricted_Access,
            UTF32_LE => UTF32LE'Unrestricted_Access,
            UTF32_BE => UTF32BE'Unrestricted_Access);

         Actual_Charset : constant String :=
           (if BOM in UTF8_All .. UTF32_BE
            then BOM_Kind_To_Charset (BOM).all
            else Charset);
      begin
         State := Iconv_Open (Text_Charset, Actual_Charset);
      exception
         when Unsupported_Conversion =>
            Free (Result);
            raise Unknown_Charset;
      end;

      --  Perform the conversion itself

      Output_Index := First_Output_Index;
      Iconv (State,
             Buffer, Input_Index,
             Output (Output_Index .. Output'Last), Output_Index,
             Status);
      Source_Last := (Output_Index - 1 - Output'First) / 4 + Result'First;

      --  Raise an error if the input was invalid

      case Status is
         when Invalid_Multibyte_Sequence | Incomplete_Multibyte_Sequence =>
            --  TODO??? It may be more helpful to actually perform lexing on an
            --  incomplete buffer. The user would get both a diagnostic for the
            --  charset error and a best-effort list of tokens.

            Free (Result);
            Iconv_Close (State);
            raise Invalid_Input;

         when Full_Buffer =>

            --  This is not supposed to happen: we allocated Result to be big
            --  enough in all cases.

            raise Program_Error;

         when Success =>
            null;
      end case;

      --  Clear the bytes we left for Quex

      declare
         Nul : constant Wide_Wide_Character := Wide_Wide_Character'Val (0);
      begin
         Result (1) := Nul;
         Result (2) := Nul;
         Result (Buffer'Length + 3) := Nul;
      end;

      Iconv_Close (State);
   end Decode_Buffer;

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
      T   : in out Stored_Token_Data) return Symbols.Symbol_Type is
   begin
      if T.Symbol = null then
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
      return T.Symbol;
   end Force_Symbol;

end ${ada_lib_name}.Lexer_Implementation;
