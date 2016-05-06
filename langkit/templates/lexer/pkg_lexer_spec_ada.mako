## vim: filetype=makoada

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;
with Langkit_Support.Token_Data_Handler;
use Langkit_Support.Token_Data_Handler;

--  This package provides types and primitives to split text streams into lists
--  of tokens.

package ${_self.ada_api_settings.lib_name}.Lexer is

   <%
      lexer = get_context().lexer
      tokens = lexer.sorted_tokens
   %>

   type Token_Kind is (
      ${',\n'.join(lexer.ada_token_name(t) for t in tokens)}
   );

   type Token_Data_Type is record
      Kind       : Token_Kind;
      Text       : Text_Access;
      Sloc_Range : Source_Location_Range;
   end record;

   Unknown_Charset : exception;
   --  Raised by Lex_From_* functions when the input charset is not supported

   Invalid_Input : exception;
   --  Raised by Lex_From_* functions when the input contains an invalid byte
   --  sequence.

   procedure Lex_From_Filename (Filename, Charset : String;
                                TDH               : in out Token_Data_Handler;
                                With_Trivia       : Boolean);
   --  Extract tokens out of Filename and store them into TDH. Raise a
   --  Name_Error exception if the file could not be open.

   procedure Lex_From_Buffer (Buffer, Charset : String;
                              TDH             : in out Token_Data_Handler;
                              With_Trivia     : Boolean);
   --  Likewise, but extract tokens from an in-memory buffer. This never raises
   --  an exception.

   function Token_Kind_Name (Token_Id : Token_Kind) return String;
   ${ada_doc('langkit.token_kind_name', 3)}

private

   Token_Kind_Names : constant array (Token_Kind) of String_Access := (
      % for tok in get_context().lexer.tokens_class:
          ${get_context().lexer.ada_token_name(tok)} =>
             new String'("${tok.name}")
          % if (not loop.last):
              ,
          % endif
      % endfor
   );

   function Token_Kind_Name (Token_Id : Token_Kind) return String is
     (Token_Kind_Names (Token_Id).all);

end ${_self.ada_api_settings.lib_name}.Lexer;
