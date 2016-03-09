## vim: filetype=makoada

--  Internal package used to provide interfaces types that help breaking
--  circular dependencies between Analysis_Unit and the root AST node
--  type definitions.

package ${_self.ada_api_settings.lib_name}.Analysis_Interfaces is

   type Analysis_Unit_Interface_Type is interface;
   type Analysis_Unit_Interface is
      access all Analysis_Unit_Interface_Type'Class;
   --  Internal interface type for Analysis_Unit, to be stored in AST nodes
   --  without trigerring circular dependencies.

end ${_self.ada_api_settings.lib_name}.Analysis_Interfaces;
