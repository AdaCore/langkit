## vim: filetype=makoada

with Langkit_Support.Bump_Ptr.Vectors;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

--  Generic package to create AST node types that just contain lists of other
--  AST node types.
--
--  This package is not part of the public API.

private generic
   type Node_Type is abstract new ${root_node_value_type} with private;
   type Node_Access is access all Node_Type'Class;
package ${_self.ada_api_settings.lib_name}.AST.List is

   --  See AST_Root for primitive operations documentations.
   --
   --  Note that as this package is instantiated in private parts and is itself
   --  private, there is no real need to create a private part here.

   package Node_Vectors is new Langkit_Support.Bump_Ptr.Vectors
     (Element_Type => Node_Access);

   type List_Type is new ${root_node_value_type} with record
      Vec : Node_Vectors.Vector;
   end record;

   overriding function Kind
     (Node : access List_Type) return ${root_node_kind_name};
   overriding function Kind_Name (Node : access List_Type) return String;
   overriding function Image (Node : access List_Type) return String;

   overriding function Child_Count
     (Node : access List_Type) return Natural;

   overriding procedure Get_Child 
     (Node   : access List_Type;
      Index  : Positive;
      Exists : out Boolean;
      Result : out ${root_node_type_name});

   overriding procedure Print
     (Node  : access List_Type;
      Level : Natural := 0);

   overriding function Lookup_Children
     (Node : access List_Type;
      Sloc : Source_Location;
      Snap : Boolean := False) return ${root_node_type_name};

   overriding procedure Destroy (Node : access List_Type);

end ${_self.ada_api_settings.lib_name}.AST.List;
