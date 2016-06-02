## vim: filetype=makoada

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

with System;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with ${_self.ada_api_settings.lib_name}.Analysis_Interfaces;
use ${_self.ada_api_settings.lib_name}.Analysis_Interfaces;

package body ${_self.ada_api_settings.lib_name}.Debug is

   --------
   -- PN --
   --------

   procedure PN (Node : ${root_node_type_name}) is
   begin
      Put_Line (Node.Short_Image);
   end PN;

   --------
   -- PT --
   --------

   procedure PT (Node : ${root_node_type_name}) is
   begin
      Node.Print;
   end PT;

   ----------
   -- PTok --
   ----------

   procedure PTok (Node : ${root_node_type_name}; T : Token_Index) is
      --  TODO??? This is very kludgy: in order to workaround visibility
      --  issues, we do gory unsafe conversion in order to turn token indices
      --  into token records.
      --
      --  We should get rid of this fixing GDB to make it easier to call
      --  tagged types primitives. This way we will be able to invoke regular
      --  AST node primitives directly from GDB to get token records.

      type Dummy_Node is tagged record
         Parent : System.Address;
         Unit   : Analysis_Unit_Interface;
      end record;

      type Dummy_Node_Access is access Dummy_Node;

      type Dummy_Token_Type is record
         TDH           : Token_Data_Handler_Access;
         Token, Trivia : Token_Index;
      end record;

      function Convert is new Ada.Unchecked_Conversion
        (Dummy_Token_Type, Token_Type);
      function Convert is new Ada.Unchecked_Conversion
        (${root_node_type_name}, Dummy_Node_Access);

      N   : constant Dummy_Node_Access := Convert (Node);
      Tok : constant Token_Type :=
         Convert ((TDH    => N.Unit.Token_Data,
                   Token  => T,
                   Trivia => No_Token_Index));
      D : constant Token_Data_Type := Data (Tok);

   begin
      Put (Token_Kind_Name (D.Kind));
      if D.Text /= null then
         Put (" " & Image (D.Text.all, With_Quotes => True));
      end if;
      Put_Line (" [" & Image (D.Sloc_Range) & "]");
   end PTok;

end ${_self.ada_api_settings.lib_name}.Debug;
