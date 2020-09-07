## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.VFS;

with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

with ${ada_lib_name}.Common; use ${ada_lib_name}.Common;

--  This package provides types and primitives to split text streams into lists
--  of tokens.

package ${ada_lib_name}.Lexer is

   use Support.Diagnostics, Support.Text;

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
            Bytes : Unbounded_String;
            --  Source buffer to read

         when others => null;
         end case;

      when Text_Buffer =>
         Text : Unbounded_Text_Type;
         --  Source buffer to read
      end case;
   end record;
   --  Input from which the lexer will read tokens

   procedure Extract_Tokens
     (Input       : Lexer_Input;
      With_Trivia : Boolean;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector)
      with Pre  => Initialized (TDH) and then not Has_Source_Buffer (TDH),
           Post => Has_Source_Buffer (TDH);
   --  Extract tokens out of the given ``Input`` and store them into ``TDH``.
   --
   --  Raise a ``Name_Error`` exception if this involves reading a file that
   --  can not be open. Raise an ``Unknown_Charset`` exception if the requested
   --  charset is unknown. Raise an ``Invalid_Input`` exception if the source
   --  cannot be decoded using the given ``Charset``.

   ${exts.include_extension(ctx.ext('lexer', 'public_decls'))}

end ${ada_lib_name}.Lexer;
