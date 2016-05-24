## vim: filetype=makoada

with Ada.Text_IO; use Ada.Text_IO;

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

end ${_self.ada_api_settings.lib_name}.Debug;
