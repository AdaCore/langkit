
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;

with GNATCOLL.VFS;

with Liblktlang_Support.Diagnostics; use Liblktlang_Support.Diagnostics;
with Liblktlang_Support.Symbols;     use Liblktlang_Support.Symbols;

with Liblktlang_Support.Token_Data_Handlers;
use Liblktlang_Support.Token_Data_Handlers;

with Liblktlang.Common; use Liblktlang.Common;
limited with Liblktlang.Implementation;

private package Liblktlang.Lexer_Implementation is

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
   --  See Liblktlang.Lexer.Lexer_Input for details. Resources pointed by
   --  access types must be free'd by Extract_Tokens's caller when done with
   --  it.

   procedure Extract_Tokens
     (Input         : Internal_Lexer_Input;
      With_Trivia   : Boolean;
      File_Reader   : access Implementation.Internal_File_Reader'Class;
      TDH           : in out Token_Data_Handler;
      Diagnostics   : in out Diagnostics_Vectors.Vector;
      Old_TDH       : access constant Token_Data_Handler;
      Same_Contents : out Boolean);
   --  Implementation for Liblktlang.Lexer.Extract_Tokens.
   --
   --  In addition, set ``Same_Contents`` to whether if ``Old_TDH`` is not null
   --  and its contents is identical to the content of ``Input``.

   function Get_Symbol
     (Token : Token_Or_Trivia_Index;
      TDH   : Token_Data_Handler) return Symbol_Type;
   --  Assuming that ``Token`` refers to a token that contains a symbol, return
   --  the corresponding symbol.

end Liblktlang.Lexer_Implementation;
