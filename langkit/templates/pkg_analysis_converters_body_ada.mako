## vim: filetype=makoada

package body ${ada_lib_name}.Analysis.Converters is

   ------------
   -- Create --
   ------------

   function Create_Entity
     (Node   : Implementation.${root_node_type_name};
      E_Info : Implementation.AST_Envs.Entity_Info
        := Implementation.AST_Envs.No_Entity_Info)
      return ${root_entity.api_name} is
   begin
      return (Internal => (Node, E_Info));
   end Create_Entity;

   ---------------
   -- Bare_Node --
   ---------------

   function Bare_Node
     (Node : ${root_entity.api_name}'Class)
      return Implementation.${root_node_type_name} is
   begin
      return Node.Internal.El;
   end Bare_Node;

   -------------
   -- To_Unit --
   -------------

   function To_Unit
     (Unit : Implementation.Internal_Unit) return Analysis_Unit
   is
     (Analysis_Unit (Unit));

   ----------------
   -- To_Context --
   ----------------

   function To_Context
     (Context : Implementation.Internal_Context) return Analysis_Context
   is
     (Analysis_Context (Context));

   ------------------
   -- Bare_Context --
   ------------------

   function Bare_Context
     (Context : Analysis_Context) return Implementation.Internal_Context
   is
     (Implementation.Internal_Context (Context));

   ---------------
   -- Bare_Unit --
   ---------------

   function Bare_Unit
     (Unit : Analysis_Unit) return Implementation.Internal_Unit
   is
     (Implementation.Internal_Unit (Unit));

end ${ada_lib_name}.Analysis.Converters;
