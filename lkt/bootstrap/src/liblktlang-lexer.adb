


pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;
with Ada.Strings.Wide_Wide_Unbounded.Aux;
use Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");

with Liblktlang_Support.Internal.Analysis;
pragma Warnings (Off, "referenced");
with Liblktlang_Support.Symbols;
pragma Warnings (On, "referenced");
with Liblktlang_Support.Types; use Liblktlang_Support.Types;

with Liblktlang.Lexer_Implementation;
use Liblktlang.Lexer_Implementation;




package body Liblktlang.Lexer is

   --------------------
   -- Extract_Tokens --
   --------------------

   procedure Extract_Tokens
     (Input       : Lexer_Input;
      With_Trivia : Boolean;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      package Internal renames Liblktlang_Support.Internal.Analysis;

      Internal_Input : Internal.Lexer_Input (Input.Kind);
      Same_Contents  : Boolean;
   begin
      case Input.Kind is
         when File | Bytes_Buffer =>
            Internal_Input.Charset := Input.Charset;
            Internal_Input.Read_BOM := Input.Read_BOM;

            case Input.Kind is
               when File =>
                  Internal_Input.Filename := Input.Filename;
               when Bytes_Buffer =>
                  declare
                     Bytes : Big_String_Access;
                  begin
                     Get_String
                       (Input.Bytes, Bytes, Internal_Input.Bytes_Count);
                     Internal_Input.Bytes := Bytes.all'Address;
                  end;
               when others =>
                  raise Program_Error;
            end case;

         when Text_Buffer =>
            declare
               Text : Big_Wide_Wide_String_Access;
            begin
               Get_Wide_Wide_String
                 (Input.Text, Text, Internal_Input.Text_Count);
               Internal_Input.Text := Text.all'Address;
            end;
      end case;

      Extract_Tokens
        (Input         => Internal_Input,
         With_Trivia   => With_Trivia,
         File_Reader   => null,
         TDH           => TDH,
         Diagnostics   => Diagnostics,
         Old_TDH       => null,
         Same_Contents => Same_Contents);

      pragma Assert (not Same_Contents);
   end Extract_Tokens;

   


end Liblktlang.Lexer;
