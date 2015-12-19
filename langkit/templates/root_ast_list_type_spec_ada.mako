## vim: filetype=makoada

with Langkit_Support.Bump_Ptr.Vectors;

--  TODO: We want to move this package to
--  ${_self.ada_api_settings.lib_name}.AST once we took care of moving type
--  definitions to another module than the root lang module (OC22-022).
generic
   type Node_Type is abstract new ${root_node_value_type} with private;
   type Node_Access is access all Node_Type'Class;
package AST.List is

   List_Kind : constant ${root_node_kind_name} := 1;

   package Node_Vectors is new Langkit_Support.Bump_Ptr.Vectors
     (Element_Type => Node_Access);

   type List_Type is new ${root_node_value_type} with record
      Vec : Node_Vectors.Vector;
   end record;

   overriding
   function Kind (Node : access List_Type) return ${root_node_kind_name};
   overriding
   function Kind_Name (Node : access List_Type) return String;
   overriding
   function Image (Node : access List_Type) return String;

   overriding
   function Child_Count (Node : access List_Type)
                         return Natural;
   overriding
   procedure Get_Child (Node   : access List_Type;
                        Index  : Natural;
                        Exists : out Boolean;
                        Result : out ${root_node_type_name});

   overriding
   procedure Validate (Node : access List_Type;
                       Parent : ${root_node_type_name} := null);

   overriding
   procedure Print (Node  : access List_Type;
                    Level : Natural := 0);

   overriding
   function Lookup_Children
     (Node : access List_Type;
      Sloc : Source_Location;
      Snap : Boolean := False) return ${root_node_type_name};

   overriding procedure Destroy (Node : access List_Type);

end AST.List;
