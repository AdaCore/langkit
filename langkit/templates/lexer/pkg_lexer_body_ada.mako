## vim: filetype=makoada

<% lexer = ctx.lexer %>

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;

with System;

with GNAT.Byte_Order_Mark;

with GNATCOLL.Iconv;
with GNATCOLL.Mmap;    use GNATCOLL.Mmap;

with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;

package body ${_self.ada_api_settings.lib_name}.Lexer is

   Quex_Leading_Characters : constant := 2;
   Quex_Trailing_Characters : constant := 1;
   Quex_Extra_Characters : constant :=
      Quex_Leading_Characters + Quex_Trailing_Characters;
   --  Quex requires its input buffer to have two leading reserved bytes and
   --  one trailing. These are not part of the true payload, but must be
   --  available anyway.

   use Token_Vectors, Trivia_Vectors, Integer_Vectors;

   type Quex_Token_Type is record
      Id                       : Unsigned_16;
      Text                     : System.Address;
      Text_Length              : size_t;
      Start_Line, End_Line     : Unsigned_32;
      Start_Column, End_Column : Unsigned_16;
      Offset                   : Unsigned_32;
   end record
      with Convention => C;
   type Interface_Token_Access is access all Quex_Token_Type;

   type Lexer_Type is new System.Address;

   procedure Decode_Buffer
     (Buffer, Charset : String;
      Read_BOM        : Boolean;
      Decoded_Buffer  : out Text_Access;
      Length          : out Natural);
   --  Allocate a Text_Type buffer, set it to Decoded_Buffer, decode Buffer
   --  into it using Charset and set Length to the number of decoded characters
   --  in Decoded_Buffer. It is up to the caller to deallocate Decoded_Buffer
   --  when done with it.
   --
   --  Quex quirk: this actually allocates more than the actual buffer to keep
   --  Quex happy. The two first characters are set to null and there is an
   --  extra null character at the end of the buffer.

   function Lexer_From_Buffer (Buffer  : System.Address;
                               Length  : Size_T)
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
   procedure Process_All_Tokens (Lexer : Lexer_Type;
                                 TDH   : in out Token_Data_Handler);

   ------------------------
   -- Process_All_Tokens --
   ------------------------

   procedure Process_All_Tokens (Lexer : Lexer_Type;
                                 TDH   : in out Token_Data_Handler)
   is

      Token                 : aliased Quex_Token_Type;
      Token_Id              : Token_Kind;
      Text                  : Text_Access;
      Continue              : Boolean := True;
      Last_Token_Was_Trivia : Boolean := False;

      function Bounded_Text return Text_Type;
      --  Return a copy of the text in Token.  Do not call this if the token
      --  has no text associated.

      function Sloc_Range return Source_Location_Range is
        ((Token.Start_Line,
          Token.End_Line,
          Token.Start_Column,
          Token.End_Column));
      --  Create a sloc range value corresponding to Token

      procedure Prepare_For_Trivia
        with Inline;
      --  Append an entry for the current token in the Tokens_To_Trivias
      --  correspondence vector.

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

      ------------------
      -- Bounded_Text --
      ------------------

      function Bounded_Text return Text_Type
      is
         Length : constant Natural := Natural (Token.Text_Length);
         Buffer : Text_Type (1 .. Length);
         for Buffer'Address use Token.Text;
      begin
         return Buffer;
      end Bounded_Text;

   begin
      --  The first entry in the Tokens_To_Trivias map is for leading trivias
      Prepare_For_Trivia;

      while Continue loop

         --  Next_Token returns 0 for the last token, which will be our "null"
         --  token.

         Continue := Next_Token (Lexer, Token'Unrestricted_Access) /= 0;
         Token_Id := Token_Kind'Enum_Val (Token.Id);

         case Token_Id is

         % if lexer.token_actions['WithText']:
            ## Token id is part of the class of token types for which we want to
            ## keep the text, but without internalization of the text.
            when ${" | ".join(
               lexer.ada_token_name(tok)
               for tok in lexer.token_actions['WithText']
            )} =>
               Text := Add_String (TDH, Bounded_Text);
         % endif

         % if lexer.token_actions['WithSymbol']:
            ## Token id is part of the class of token types for which we want to
            ## internalize the text.
            when ${" | ".join(
               lexer.ada_token_name(tok)
               for tok in lexer.token_actions['WithSymbol']
            )} =>
               Text := Text_Access (Find (TDH.Symbols, Bounded_Text));
         % endif

         % if lexer.token_actions['WithTrivia']:
            when ${" | ".join(
               lexer.ada_token_name(tok)
               for tok in lexer.token_actions['WithTrivia']
            )} =>
               if With_Trivia then
                  Text := Add_String (TDH, Bounded_Text);

                  if Last_Token_Was_Trivia then
                     Last_Element (TDH.Trivias).all.Has_Next := True;
                  else
                     Last_Element (TDH.Tokens_To_Trivias).all :=
                        Last_Index (TDH.Trivias) + 1;
                  end if;

                  Append
                    (TDH.Trivias,
                     (Has_Next => False,
                      T        => (Kind       =>
                                      Token_Kind'Enum_Val (Token.Id),
                                   Text       => Text,
                                   Sloc_Range => Sloc_Range,
                                   Offset     => Token.Offset)));

                  Last_Token_Was_Trivia := True;
               end if;

               goto Dont_Append;
         % endif

            ## Else, don't keep the text at all
            when others =>
               Text := null;

         end case;

         Append
           (TDH.Tokens,
            (Kind       => Token_Kind'Enum_Val (Token.Id),
             Text       => Text,
             Sloc_Range => Sloc_Range,
             Offset     => Token.Offset));
         Prepare_For_Trivia;

      % if lexer.token_actions['WithTrivia']:
         <<Dont_Append>>
      % endif
      end loop;

   end Process_All_Tokens;

   procedure Process_All_Tokens_With_Trivia is new Process_All_Tokens (True);
   procedure Process_All_Tokens_No_Trivia is new Process_All_Tokens (False);

   -----------------------
   -- Lex_From_Filename --
   -----------------------

   procedure Lex_From_Filename
     (Filename, Charset : String;
      Read_BOM          : Boolean;
      TDH               : in out Token_Data_Handler;
      With_Trivia       : Boolean)
   is
      --  The following call to Open_Read may fail with a Name_Error exception:
      --  just let it propagate to the caller as there is no resource to
      --  release yet here.

      File        : Mapped_File := Open_Read (Filename);

      Region      : Mapped_Region := Read (File);
      Buffer_Addr : constant System.Address := Data (Region).all'Address;

      Buffer      : String (1 .. Last (Region));
      for Buffer'Address use Buffer_Addr;

   begin
      begin
         Lex_From_Buffer (Buffer, Charset, Read_BOM, TDH, With_Trivia);
      exception
         when Unknown_Charset | Invalid_Input =>
            Free (Region);
            Close (File);
            raise;
      end;
      Free (Region);
      Close (File);
   end Lex_From_Filename;

   ---------------------
   -- Lex_From_Buffer --
   ---------------------

   procedure Lex_From_Buffer
     (Buffer, Charset : String;
      Read_BOM        : Boolean;
      TDH             : in out Token_Data_Handler;
      With_Trivia     : Boolean)
   is
      Decoded_Buffer : Text_Access;
      Length         : Natural;
      Lexer          : Lexer_Type;
   begin
      Decode_Buffer (Buffer, Charset, Read_BOM, Decoded_Buffer, Length);
      Lexer := Lexer_From_Buffer (Decoded_Buffer.all'Address, size_t (Length));

      --  Preserve a copy of the text buffer in TDH. In the case we are
      --  reparsing an analysis unit, we want to get rid of the tokens from the
      --  old one.

      declare
         Actual_Decoded_Buffer : Text_Type renames Decoded_Buffer
           (Decoded_Buffer.all'First + Quex_Leading_Characters
            .. Decoded_Buffer.all'Last - Quex_Trailing_Characters);
         Rebounded_Buffer : Text_Type (1 .. Actual_Decoded_Buffer'Length)
            with Address => Actual_Decoded_Buffer'Address;
      begin
         Reset (TDH, new Text_Type'(Rebounded_Buffer));
      end;

      if With_Trivia then
         Process_All_Tokens_With_Trivia (Lexer, TDH);
      else
         Process_All_Tokens_No_Trivia (Lexer, TDH);
      end if;
      Free_Lexer (Lexer);
      Free (Decoded_Buffer);
   end Lex_From_Buffer;

   -------------------
   -- Decode_Buffer --
   -------------------

   procedure Decode_Buffer
     (Buffer, Charset : String;
      Read_BOM        : Boolean;
      Decoded_Buffer  : out Text_Access;
      Length          : out Natural)
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

      --  GNATCOLL.Iconv raises a Constraint_Error for empty strings: handle
      --  them here.

      if Buffer'Length = 0 then
         Length := 0;
         return;
      end if;

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

      --  Create the Iconv converter. We will notice unknown charsets here

      declare
         use System;

         To_Code : constant String :=
           (if Default_Bit_Order = Low_Order_First
            then UTF32LE
            else UTF32BE);

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
         State := Iconv_Open (To_Code, Actual_Charset);
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
      Length := (Output_Index - First_Output_Index) / 4;
   end Decode_Buffer;

   Token_Kind_Names : constant array (Token_Kind) of String_Access := (
      % for tok in ctx.lexer.tokens_class:
          ${ctx.lexer.ada_token_name(tok)} =>
             new String'("${tok.name}")
          % if (not loop.last):
              ,
          % endif
      % endfor
   );

   ---------------------
   -- Token_Kind_Name --
   ---------------------

   function Token_Kind_Name (Token_Id : Token_Kind) return String is
     (Token_Kind_Names (Token_Id).all);

end ${_self.ada_api_settings.lib_name}.Lexer;
