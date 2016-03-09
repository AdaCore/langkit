## vim: filetype=makoada

with Langkit_Support.Token_Data_Handler;
use Langkit_Support.Token_Data_Handler;

--  Internal package used to provide interfaces types that help breaking
--  circular dependencies between Analysis_Unit and the root AST node
--  type definitions.

package ${_self.ada_api_settings.lib_name}.Analysis_Interfaces is

   type Analysis_Unit_Interface_Type is interface;
   type Analysis_Unit_Interface is
      access all Analysis_Unit_Interface_Type'Class;
   --  Internal interface type for Analysis_Unit, to be stored in AST nodes
   --  without trigerring circular dependencies.

   function Token_Data
     (Unit : access Analysis_Unit_Interface_Type)
      return Token_Data_Handler_Access is abstract;
   --  Get an access to the token data handler bundled in Unit. Note that this
   --  token data handler is not valid anymore as soon as Unit is destroy or as
   --  it is reparsed.

end ${_self.ada_api_settings.lib_name}.Analysis_Interfaces;
