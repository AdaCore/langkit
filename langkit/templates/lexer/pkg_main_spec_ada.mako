## vim: filetype=makoada

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Slocs;       use Langkit_Support.Slocs;
with Langkit_Support.Symbols;     use Langkit_Support.Symbols;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Langkit_Support.Token_Data_Handlers;

--  This package provides types and primitives to split text streams into lists
--  of tokens.

package ${ada_lib_name}.Lexer is

   <%
      lexer = ctx.lexer
      tokens = lexer.sorted_tokens
   %>

   type Token_Kind is (
      ${',\n'.join(t.ada_name for t in tokens)}
   );

   type Token_Family is
     (${', '.join(tf.ada_name for tf in lexer.tokens.token_families)});

   % if lexer.track_indent:
   type Indent_Kind is (Indent, Dedent, Nodent, None);
   % endif

   type Token_Data_Type is record
      Kind : Token_Kind;
      --  Kind for this token

      Source_First : Positive;
      Source_Last  : Natural;
      --  Bounds in the source buffer corresponding to this token

      Symbol : Symbol_Type;
      --  Depending on the token kind (according to the lexer specification),
      --  this is either null or the symbolization of the token text.
      --
      --  For instance: null for keywords but actual text for identifiers.

      Sloc_Range : Source_Location_Range;
      --  Source location range for this token. Note that the end bound is
      --  exclusive.
   end record;

   function Sloc_Range (Token : Token_Data_Type) return Source_Location_Range;

   package Token_Data_Handlers is new Langkit_Support.Token_Data_Handlers
     (Token_Data_Type);
   use Token_Data_Handlers;

   Unknown_Charset : exception;
   --  Raised by Lex_From_* functions when the input charset is not supported

   Invalid_Input : exception;
   --  Raised by Lex_From_* functions when the input contains an invalid byte
   --  sequence.

   procedure Lex_From_Filename
     (Filename, Charset : String;
      Read_BOM          : Boolean;
      TDH               : in out Token_Data_Handler;
      Diagnostics       : in out Diagnostics_Vectors.Vector;
      With_Trivia       : Boolean);
   --  Extract tokens out of Filename and store them into TDH.
   --
   --  Raise a Name_Error exception if the file could not be open. Raise an
   --  Unknown_Charset exception if the requested charset is unknown. Raise an
   --  Invalid_Input exception if Filename's content cannot be decoded using
   --  the given Charset.

   procedure Lex_From_Buffer
     (Buffer, Charset : String;
      Read_BOM        : Boolean;
      TDH             : in out Token_Data_Handler;
      Diagnostics     : in out Diagnostics_Vectors.Vector;
      With_Trivia     : Boolean);
   --  Likewise, but extract tokens from an undecoded in-memory buffer.
   --
   --  Raise an Unknown_Charset exception if the requested charset is unknown.
   --  Raise an Invalid_Input exception if Filename's content cannot be decoded
   --  using the given Charset.

   procedure Lex_From_Buffer
     (Buffer      : Text_Type;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector;
      With_Trivia : Boolean);
   --  Likewise, but extract tokens from an in-memory buffer. This never raises
   --  exceptions.

   function Token_Kind_Name (Token_Id : Token_Kind) return String;
   ${ada_doc('langkit.token_kind_name', 3)}

   function Token_Kind_Literal (Token_Id : Token_Kind) return Text_Type;
   --  Return the canonical literal corresponding to this token kind, or an
   --  empty string if this token has no literal.

   function Token_Error_Image (Token_Id : Token_Kind) return String;
   --  Return a string repr of token kind suitable in error messages

   function Text
     (TDH : Token_Data_Handler;
      T   : Token_Data_Type) return Text_Type
   is (TDH.Source_Buffer.all (T.Source_First .. T.Source_Last));
   --  Return the text associated to T, a token that belongs to TDH

   function Image
     (TDH : Token_Data_Handler;
      T   : Token_Data_Type) return String
   is (Image (Text (TDH, T)));
   --  Debug helper: return a human-readable representation of T, a token that
   --  belongs to TDH.

   type Symbolization_Result (Success : Boolean; Size : Natural) is record
      case Success is
         when True  => Symbol : Text_Type (1 .. Size);
         when False => Error_Message : Text_Type (1 .. Size);
      end case;
   end record;

   function Create_Symbol (Name : Text_Type) return Symbolization_Result is
     ((Success => True, Size => Name'Length, Symbol => Name));
   function Create_Error (Message : Text_Type) return Symbolization_Result is
     ((Success => False, Size => Message'Length, Error_Message => Message));

   function Force_Symbol
     (TDH : Token_Data_Handler;
      T   : in out Token_Data_Type) return Symbol_Type;
   --  If T has a symbol, return it. Otherwise, force its symbolization and
   --  return the symbol.

   function Internal_Charset return String;
   --  Return the charset used to store text internally

   Token_Kind_To_Family : array (Token_Kind) of Token_Family :=
     (${', '.join('{} => {}'.format(t.ada_name,
                                     lexer.tokens.token_to_family[t].ada_name)
                   for t in tokens)});

end ${ada_lib_name}.Lexer;
