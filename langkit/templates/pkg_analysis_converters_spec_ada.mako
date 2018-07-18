## vim: filetype=makoada

with ${ada_lib_name}.Implementation;

--  Internal package: provide implementation helpers to switch between public
--  types and implementation ones.

package ${ada_lib_name}.Analysis.Converters is

   function Create_Entity
     (Node   : Implementation.${root_node_type_name};
      E_Info : Implementation.AST_Envs.Entity_Info
        := Implementation.AST_Envs.No_Entity_Info)
      return ${root_entity.api_name};

   function Bare_Node
     (Node : ${root_entity.api_name}'Class)
      return Implementation.${root_node_type_name};

   function To_Unit
     (Unit : Implementation.Internal_Unit) return Analysis_Unit;

   function To_Context
     (Context : Implementation.Internal_Context) return Analysis_Context;

   function Bare_Context
     (Context : Analysis_Context) return Implementation.Internal_Context;

   function Bare_Unit
     (Unit : Analysis_Unit) return Implementation.Internal_Unit;

end ${ada_lib_name}.Analysis.Converters;
