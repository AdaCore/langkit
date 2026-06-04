with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Unparsing;
use Langkit_Support.Generic_API.Unparsing;

with Libfoolang.Generic_API; use Libfoolang.Generic_API;

procedure Invalid_Config is

   procedure Check
     (Filename          : String;
      Check_All_Nodes   : Boolean := False;
      Config_Overriding : String := "");

   -----------
   -- Check --
   -----------

   procedure Check
     (Filename          : String;
      Check_All_Nodes   : Boolean := False;
      Config_Overriding : String := "")
   is
      Diagnostics : Diagnostics_Vectors.Vector;
      Config      : Unparsing_Configuration;
      Indent      : constant String := "    ";
   begin
      Put ("# " & Filename);
      if Config_Overriding /= "" then
         Put (" (overriding: " & Config_Overriding & ")");
      end if;
      New_Line;

      Config := Load_Unparsing_Config
        (Language        => Self_Id,
         Filename        => Filename,
         Diagnostics     => Diagnostics,
         Check_All_Nodes => Check_All_Nodes,
         Overridings     =>
           (if Config_Overriding = ""
            then Empty_File_Array
            else (1 => Create (+Config_Overriding))));

      if Config = No_Unparsing_Configuration then
         Put ((1 .. 4 => ' '));
         Print (Diagnostics, Prefix => "", Indent => 4);
      else
         Put_Line (Indent & "Success");
      end if;
   end Check;

begin
   if Argument_Count > 0 then
      for I in 1 .. Argument_Count loop
         Check (Argument (I));
      end loop;
      return;
   end if;

   Put_Line ("== Errors outside of templates ==");
   New_Line;
   Check ("no_such_file.json");
   Check ("invalid_syntax.json");
   Check ("missing_node_configs.json");
   Check ("invalid_node_configs.json");
   Check ("invalid_type_name.json");
   Check ("invalid_node_name.json");
   Check ("invalid_error_node.json");
   Check ("invalid_synthetic_node.json");
   Check ("invalid_member_name.json");
   Check ("invalid_field_name.json");
   Check ("invalid_node_sep.json");
   Check ("invalid_leading_sep.json");
   Check ("invalid_trailing_sep.json");
   Check ("invalid_max_empty_lines_1.json");
   Check ("invalid_max_empty_lines_2.json");
   Check ("invalid_flush_before_children_1.json");
   Check ("invalid_flush_before_children_2.json");
   Check ("invalid_independent_lines_1.json");
   Check ("invalid_independent_lines_2.json");
   New_Line;

   Put_Line ("== Errors in overridings ==");
   New_Line;
   Check ("root_node.json", Config_Overriding => "no_such_file.json");
   Check ("root_node.json", Config_Overriding => "invalid_syntax.json");
   Check ("root_node.json", Config_Overriding => "missing_node_configs.json");
   Check ("root_node.json", Config_Overriding => "invalid_node_configs.json");
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
   Check ("invalid_if.json");
   Check ("invalid_if2.json");
   Check ("invalid_if3.json");
   Check ("invalid_if4.json");
   Check ("invalid_isempty.json");
   Check ("invalid_isempty2.json");
   Check ("invalid_isempty3.json");
   Check ("invalid_match.json");
   Check ("invalid_match2.json");
   Check ("invalid_match3.json");
   Check ("invalid_match4.json");
   Check ("invalid_match5.json");
   Check ("invalid_match6.json");
   Check ("invalid_match7.json");
   Check ("invalid_match8.json");
   Check ("invalid_match9.json");
   Check ("invalid_match10.json");
   Check ("invalid_match11.json");
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
   Check ("invalid_is_a.json");
   Check ("invalid_is_a2.json");
   Check ("invalid_is_a3.json");
   Check ("invalid_is_a4.json");
   Check ("invalid_is_a5.json");
   Check ("invalid_is_a6.json");
   Check ("invalid_table_join.json");
   Check ("invalid_table_join2.json");
   Check ("invalid_table_join3.json");
   Check ("invalid_table_join4.json");
   Check ("invalid_table_join5.json");
   Check ("invalid_bubble_up.json");
   Check ("invalid_this_field.json");
   Check ("invalid_eval_member.json");
   Check ("invalid_eval_member2.json");
   Check ("invalid_eval_member3.json");
   Check ("invalid_eval_member4.json");
   Check ("invalid_eval_member5.json");
   Check ("invalid_eval_member6.json");
   Check ("invalid_eval_member7.json");
   Check ("invalid_eval_member8.json");
   Check ("invalid_eval_member9.json");
   Check ("invalid_eval_member10.json");
   Check ("invalid_eval_member11.json");
   Check ("invalid_node_text.json");
   Check ("invalid_node_text2.json");
   Check ("invalid_node_text3.json");
   Check ("invalid_node_text4.json");
   Check ("invalid_string_lit.json");
   Check ("invalid_string_lit2.json");
   Check ("invalid_bin_op.json");
   Check ("invalid_bin_op2.json");
   Check ("invalid_bin_op3.json");
   Check ("invalid_bin_op4.json");
   Check ("invalid_bin_op5.json");
   Check ("invalid_bin_op_eq.json");
   Check ("invalid_bin_op_eq2.json");
   Check ("invalid_bin_op_eq3.json");
   Check ("invalid_bin_op_and.json");
   Check ("invalid_not.json");
   Check ("invalid_not2.json");
   Check ("invalid_not3.json");
   Check ("invalid_symbol.json");
   Check ("invalid_symbol2.json");
   Check ("invalid_symbol3.json");
   Check ("invalid_node_symbol.json");
   Check ("invalid_node_symbol2.json");
   Check ("invalid_node_symbol3.json");
   Check ("invalid_cast.json");
   Check ("invalid_cast2.json");
   Check ("invalid_cast3.json");
   Check ("invalid_cast4.json");
   Check ("invalid_cast5.json");
   Check ("invalid_cast6.json");
   Check ("invalid_cast7.json");
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

   Put_Line ("== Errors in token configs ==");
   Check ("invalid_token_configs.json");
   Check ("invalid_token_default.json");
   Check ("invalid_token_default2.json");
   Check ("invalid_token_formattings.json");
   Check ("invalid_token_formattings_key.json");
   Check ("invalid_token_formattings_value1.json");
   Check ("invalid_token_formattings_value2.json");
   Check ("invalid_token_formattings_value3.json");
   New_Line;

   Put_Line ("Done.");
end Invalid_Config;
