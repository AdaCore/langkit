with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Generic_API.Unparsing;
use Langkit_Support.Generic_API.Unparsing;

with Libfoolang.Generic_API; use Libfoolang.Generic_API;

procedure Invalid_Config is

   procedure Check (Filename : String);

   -----------
   -- Check --
   -----------

   procedure Check (Filename : String) is
      Dummy : Unparsing_Configuration;
   begin
      Put_Line ("# " & Filename);
      Put ("    ");
      begin
         Dummy := Load_Unparsing_Config (Self_Id, Filename);
         Put_Line ("Success");
      exception
         when Exc : Invalid_Input =>
            Put_Line (Ada.Exceptions.Exception_Message (Exc));
      end;
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
   New_Line;

   Put_Line ("== Decoding errors in templates ==");
   New_Line;
   Check ("invalid_template.json");
   Check ("invalid_string_template.json");
   Check ("invalid_kind_template.json");
   Check ("invalid_kind_template2.json");
   Check ("invalid_kind_template3.json");
   Check ("invalid_whitespace.json");
   Check ("invalid_indent.json");
   New_Line;

   Put_Line ("== Invalid templates (too many/few ""recurse"") ==");
   New_Line;
   Check ("recurse_list_too_few.json");
   Check ("recurse_list_too_many.json");
   Check ("recurse_line.json");
   Check ("recurse_whitespace.json");
   Check ("recurse_indent_too_few.json");
   Check ("recurse_indent_too_many.json");
   New_Line;

   Put_Line ("Done.");
end Invalid_Config;
