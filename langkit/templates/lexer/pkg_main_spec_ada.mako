## vim: filetype=makoada

with Langkit_Support.Slocs;   use Langkit_Support.Slocs;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;
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

   % if lexer.track_indent:
   type Indent_Kind is (Indent, Dedent, Nodent, None);
   % endif

   type Token_Data_Type is record
      Kind         : Token_Kind;
      --  Kind for this token

      Source_First : Positive;
      Source_Last  : Natural;
      --  Bounds in the source buffer corresponding to this token

      Symbol       : Symbol_Type;
      --  Depending on the token kind (according to the lexer specification),
      --  this is either null or the symbolization of the token text.
      --
      --  For instance: null for keywords but actual text for identifiers.

      Sloc_Range   : Source_Location_Range;
      --  Source location range for this token. Note that the end bound is
      --  exclusive.

   end record;

   package Token_Data_Handlers is new Langkit_Support.Token_Data_Handlers
     (Token_Data_Type);
   use Token_Data_Handlers;

   Unknown_Charset : exception;
   --  Raised by Lex_From_* functions when the input charset is not supported

   Invalid_Input : exception;
   --  Raised by Lex_From_* functions when the input contains an invalid byte
   --  sequence.

   procedure Lex_From_Filename (Filename, Charset : String;
                                Read_BOM          : Boolean;
                                TDH               : in out Token_Data_Handler;
                                With_Trivia       : Boolean);
   --  Extract tokens out of Filename and store them into TDH. Raise a
   --  Name_Error exception if the file could not be open.

   procedure Lex_From_Buffer (Buffer, Charset : String;
                              Read_BOM        : Boolean;
                              TDH             : in out Token_Data_Handler;
                              With_Trivia     : Boolean);
   --  Likewise, but extract tokens from an in-memory buffer. This never raises
   --  an exception.

   function Token_Kind_Name (Token_Id : Token_Kind) return String;
   ${ada_doc('langkit.token_kind_name', 3)}

   function Token_Kind_Literal (Token_Id : Token_Kind) return String;
   --  Return the literal corresponding to this token kind

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

   function Force_Symbol
     (TDH : Token_Data_Handler;
      T   : in out Token_Data_Type) return Symbol_Type;
   --  If T has a symbol, return it. Otherwise, force its symbolization and
   --  return the symbol.

end ${ada_lib_name}.Lexer;
