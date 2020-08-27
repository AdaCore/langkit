## vim: filetype=makoada

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;

with GNATCOLL.VFS;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Symbols;     use Langkit_Support.Symbols;

with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

with ${ada_lib_name}.Common; use ${ada_lib_name}.Common;

private package ${ada_lib_name}.Lexer_Implementation is

   type Internal_Lexer_Input (Kind : Lexer_Input_Kind) is record
      case Kind is
      when File | Bytes_Buffer =>
         Charset  : Unbounded_String;
         Read_BOM : Boolean;

         case Kind is
            when File =>
               Filename : GNATCOLL.VFS.Virtual_File;
            when Bytes_Buffer =>
               Bytes       : System.Address;
               Bytes_Count : Natural;
            when others =>
               null;
         end case;

      when Text_Buffer =>
         Text       : System.Address;
         Text_Count : Natural;
      end case;
   end record;
   --  See ${ada_lib_name}.Lexer.Lexer_Input for details. Resources pointed by
   --  access types must be free'd by Extract_Tokens's caller when done with
   --  it.

   procedure Extract_Tokens
     (Input       : Internal_Lexer_Input;
      Tab_Stop    : Positive;
      With_Trivia : Boolean;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   --  Implementation for ${ada_lib_name}.Lexer.Extract_Tokens

   function Get_Symbol
     (Token : Token_Or_Trivia_Index;
      TDH   : Token_Data_Handler) return Symbol_Type;
   --  Assuming that ``Token`` refers to a token that contains a symbol, return
   --  the corresponding symbol.

end ${ada_lib_name}.Lexer_Implementation;
