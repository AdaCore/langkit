## vim: filetype=makoada

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.VFS;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

with ${ada_lib_name}.Common; use ${ada_lib_name}.Common;

--  This package provides types and primitives to split text streams into lists
--  of tokens.

package ${ada_lib_name}.Lexer is

   function To_Token_Kind (Raw : Raw_Token_Kind) return Token_Kind
      with Inline;
   function From_Token_Kind (Kind : Token_Kind) return Raw_Token_Kind
      with Inline;

   type Lexer_Input (Kind : Lexer_Input_Kind) is record
      case Kind is
      when File | Bytes_Buffer =>
         Charset : Unbounded_String;
         --  Name of the charset to use in order to decode the input source

         Read_BOM : Boolean;
         --  Whether the lexer should look for an optional Byte Order Mark

         case Kind is
         when File =>
            Filename : GNATCOLL.VFS.Virtual_File;
            --  Name of the file to read

         when Bytes_Buffer =>
            Bytes : String_Access;
            --  Source buffer to read

         when others => null;
         end case;

      when Text_Buffer =>
         Text : Text_Access;
         --  Source buffer to read
      end case;
   end record;

   procedure Extract_Tokens
     (Input       : Lexer_Input;
      With_Trivia : Boolean;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   --  Extract tokens out of the given Input and store them into TDH.
   --
   --  Raise a Name_Error exception if this involves reading a file that can
   --  not be open. Raise an Unknown_Charset exception if the requested
   --  charset is unknown. Raise an Invalid_Input exception if the source
   --  cannot be decoded using the given Charset.

   function Text
     (TDH : Token_Data_Handler;
      T   : Stored_Token_Data) return Text_Type
   is (TDH.Source_Buffer.all (T.Source_First .. T.Source_Last));
   --  Return the text associated to T, a token that belongs to TDH

   function Image
     (TDH : Token_Data_Handler;
      T   : Stored_Token_Data) return String
   is (Image (Text (TDH, T)));
   --  Debug helper: return a human-readable representation of T, a token that
   --  belongs to TDH.

end ${ada_lib_name}.Lexer;
