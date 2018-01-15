## vim: filetype=makoada

with Ada.Strings.Wide_Wide_Unbounded;

with GNATCOLL.Iconv;

with ${ada_lib_name}.Analysis.Implementation;
use ${ada_lib_name}.Analysis.Implementation;
with ${ada_lib_name}.Lexer; use ${ada_lib_name}.Lexer;

package body ${ada_lib_name}.Unparsing is

   -------------
   -- Unparse --
   -------------

   function Unparse (Node : ${root_entity.api_name}'Class) return String is
      Buffer : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
      N      : constant ${root_node_type_name} := Bare_Node (Node);
   begin
      if Node.Is_Null then
         return "";
      end if;

      Unparse_Dispatch (N, Buffer);

      --  GNATCOLL.Iconv raises a Constraint_Error for empty strings: handle
      --  them here.
      if Ada.Strings.Wide_Wide_Unbounded.Length (Buffer) = 0 then
         return "";
      end if;

      declare
         use GNATCOLL.Iconv;

         State  : Iconv_T := Iconv_Open
           (To_Code   => Get_Charset (Get_Unit (Node)),
            From_Code => Internal_Charset);
         Status : Iconv_Result;

         To_Convert_WWS    : constant Wide_Wide_String :=
            Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (Buffer);
         To_Convert_String : constant String (1 .. 4 * To_Convert_WWS'Length)
            with Import     => True,
                 Convention => Ada,
                 Address    => To_Convert_WWS'Address;

         Result     : String (1 .. 4 * To_Convert_String'Length);
         --  Encodings should not take more than 4 bytes per code point, so
         --  this should be enough to hold the conversion.

         Input_Index  : Positive := To_Convert_String'First;
         Output_Index : Positive := Result'First;
      begin
         --  TODO??? Use GNATCOLL.Iconv to properly encode this wide wide
         --  string into a mere string using this unit's charset.
         Iconv
           (State, To_Convert_String, Input_Index, Result, Output_Index,
            Status);
         Iconv_Close (State);
         case Status is
            when Success => null;
            when others => raise Program_Error with "cannot encode result";
         end case;
         return Result (Result'First .. Output_Index - 1);
      end;
   end Unparse;

end ${ada_lib_name}.Unparsing;
