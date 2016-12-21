## vim: filetype=makoada

with Interfaces;          use Interfaces;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;
with Langkit_Support.Token_Data_Handlers;

--  This package provides types and primitives to split text streams into lists
--  of tokens.

package ${_self.ada_api_settings.lib_name}.Lexer is

   <%
      lexer = ctx.lexer
      tokens = lexer.sorted_tokens
   %>

   type Token_Kind is (
      ${',\n'.join(lexer.ada_token_name(t) for t in tokens)}
   );

   type Token_Data_Type is record
      Kind       : Token_Kind;

      Text       : Text_Access;
      --  Text as found in original source file or null depending on the token
      --  kind (as decided in the lexer specification). For instance: null for
      --  keywords but actual text for identifiers.

      Sloc_Range : Source_Location_Range;

      Offset     : Unsigned_32;
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

   function Image (T : Token_Data_Type) return String is
     (if T.Text = null
      then ""
      else Image (T.Text.all));

end ${_self.ada_api_settings.lib_name}.Lexer;
