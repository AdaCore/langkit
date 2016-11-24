## vim: filetype=makoada

with Ada.Unchecked_Deallocation;

with Langkit_Support.Text; use Langkit_Support.Text;

with ${_self.ada_api_settings.lib_name}.AST;
use ${_self.ada_api_settings.lib_name}.AST;

package ${_self.ada_api_settings.lib_name}.Unit_Files is

   type Unit_Kind is (Unit_Specification, Unit_Body);
   ${ada_doc('langkit.unit_kind_type', 3)}

   type Unit_File_Provider_Interface is limited interface;
   type Unit_File_Provider_Access is
      access Unit_File_Provider_Interface'Class;
   type Unit_File_Provider_Access_Cst is
      access constant Unit_File_Provider_Interface'Class;
   ${ada_doc('langkit.unit_file_provider_type', 3)}

   function Get_File
     (Provider : Unit_File_Provider_Interface;
      Node     : ${root_node_type_name};
      Kind     : Unit_Kind)
      return String is abstract;
   ${ada_doc('langkit.unit_file_provider_get_file_from_node', 3)}

   function Get_File
     (Provider : Unit_File_Provider_Interface;
      Name     : Text_Type;
      Kind     : Unit_Kind)
      return String is abstract;
   ${ada_doc('langkit.unit_file_provider_get_file_from_name', 3)}

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Unit_File_Provider_Interface'Class, Unit_File_Provider_Access);

end ${_self.ada_api_settings.lib_name}.Unit_Files;
