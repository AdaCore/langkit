with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Unparsing;
use Langkit_Support.Generic_API.Unparsing;

with Libfoolang.Generic_API; use Libfoolang.Generic_API;

procedure Invalid_Config is

   procedure Check (Filename : String; Check_All_Nodes : Boolean := False);

   -----------
   -- Check --
   -----------

   procedure Check (Filename : String; Check_All_Nodes : Boolean := False) is
      Diagnostics : Diagnostics_Vectors.Vector;
      Config      : Unparsing_Configuration;
      Indent      : constant String := "    ";
   begin
      Put_Line ("# " & Filename);
      Config := Load_Unparsing_Config
        (Self_Id, Filename, Diagnostics, Check_All_Nodes);
      if Config = No_Unparsing_Configuration then
         Put ((1 .. 4 => ' '));
         Print (Diagnostics, Prefix => "", Indent => 4);
      else
         Put_Line (Indent & "Success");
      end if;
   end Check;

begin
   Put_Line ("== Errors outside of templates ==");
   New_Line;
   Check ("no_such_file.json");
   Check ("invalid_syntax.json");
   Check ("missing_node_configs.json");
   Check ("invalid_type_name.json");
   Check ("invalid_node_name.json");
   Check ("invalid_member_name.json");
   Check ("invalid_field_name.json");
   Check ("invalid_node_sep.json");
   Check ("invalid_leading_sep.json");
   Check ("invalid_trailing_sep.json");
   Check ("invalid_max_empty_lines_1.json");
   Check ("invalid_max_empty_lines_2.json");
   Check ("invalid_flush_before_children_1.json");
   Check ("invalid_flush_before_children_2.json");
   New_Line;

   Put_Line ("== Decoding errors in templates ==");
   New_Line;
   Check ("invalid_template.json");
   Check ("invalid_string_template.json");
   Check ("invalid_kind_template.json");
   Check ("invalid_kind_template2.json");
   Check ("invalid_kind_template3.json");
   Check ("invalid_whitespace.json");
   Check ("invalid_whitespace2.json");
   Check ("invalid_align.json");
   Check ("invalid_align2.json");
   Check ("invalid_dedent.json");
   Check ("invalid_dedenttoroot.json");
   Check ("invalid_fill.json");
   Check ("invalid_group.json");
   Check ("invalid_group2.json");
   Check ("invalid_group3.json");
   Check ("invalid_group4.json");
   Check ("invalid_group5.json");
   Check ("invalid_group6.json");
   Check ("invalid_group7.json");
   Check ("invalid_ifbreak.json");
   Check ("invalid_ifbreak2.json");
   Check ("invalid_ifbreak3.json");
   Check ("invalid_ifbreak4.json");
   Check ("invalid_ifempty.json");
   Check ("invalid_ifempty2.json");
   Check ("invalid_ifempty3.json");
   Check ("invalid_ifkind.json");
   Check ("invalid_ifkind2.json");
   Check ("invalid_ifkind3.json");
   Check ("invalid_ifkind4.json");
   Check ("invalid_ifkind5.json");
   Check ("invalid_ifkind6.json");
   Check ("invalid_ifkind7.json");
   Check ("invalid_ifkind8.json");
   Check ("invalid_ifkind9.json");
   Check ("invalid_ifkind10.json");
   Check ("invalid_ifkind11.json");
   Check ("invalid_indent.json");
   Check ("invalid_markasroot.json");
   Check ("invalid_innerroot.json");
   Check ("invalid_continuationlineindent.json");
   Check ("invalid_recurse_field.json");
   Check ("invalid_recurse_field2.json");
   Check ("invalid_recurse_field3.json");
   Check ("invalid_recurse_field4.json");
   Check ("invalid_recurse_field5.json");
   Check ("invalid_recurse_field6.json");
   Check ("invalid_recurse_field7.json");
   Check ("invalid_recurse_field8.json");
   Check ("invalid_recurse_field9.json");
   Check ("invalid_recurse_field10.json");
   Check ("invalid_recurse_field11.json");
   Check ("invalid_recurse_flatten.json");
   Check ("invalid_recurse_flatten2.json");
   Check ("invalid_recurse_flatten3.json");
   Check ("invalid_table_join.json");
   Check ("invalid_table_join2.json");
   Check ("invalid_table_join3.json");
   Check ("invalid_table_join4.json");
   Check ("invalid_table_join5.json");
   New_Line;

   Put_Line ("== Invalid templates (too many/few ""recurse"") ==");
   New_Line;
   Check ("recurse_in_field_too_many.json");
   Check ("recurse_list_too_few.json");
   Check ("recurse_list_too_many.json");
   Check ("recurse_line.json");
   Check ("recurse_whitespace.json");
   Check ("recurse_indent_too_few.json");
   Check ("recurse_indent_too_many.json");
   Check ("recurse_if_empty.json");
   New_Line;

   Put_Line ("== Node config completeness ==");
   New_Line;
   Check ("root_node.json", Check_All_Nodes => True);
   Check ("missing_nodes.json", Check_All_Nodes => True);
   New_Line;

   Put_Line ("Done.");
end Invalid_Config;
