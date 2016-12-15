## vim: filetype=makoada

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

package body ${_self.ada_api_settings.lib_name}.AST.List is

   use Node_Vectors;

   ----------
   -- Kind --
   ----------

   overriding function Kind
     (Node : access List_Type) return ${root_node_kind_name}
   is
      pragma Unreferenced (Node);
   begin
      return Node_Kind;
   end Kind;

   ---------------
   -- Kind_Name --
   ---------------

   overriding function Kind_Name (Node : access List_Type) return String is
      pragma Unreferenced (Node);
   begin
      return Node_Kind_Name;
   end Kind_Name;

   -----------
   -- Image --
   -----------

   overriding function Image (Node : access List_Type) return String is
      Result : Unbounded_String;
   begin
      Append (Result, '[');
      for El of Node.Vec loop
         if Length (Result) > 0 then
            Append (Result, ", ");
         end if;
         Append (Result, El.Image);
      end loop;

      Append (Result, ']');
      return To_String (Result);
   end Image;

   -----------------
   -- Child_Count --
   -----------------

   overriding function Child_Count (Node : access List_Type) return Natural is
   begin
      return Length (Node.Vec);
   end Child_Count;

   ---------------
   -- Get_Child --
   ---------------

   overriding procedure Get_Child
     (Node            : access List_Type;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out ${root_node_type_name}) is
   begin
      if Index > Node_Vectors.Last_Index (Node.Vec) then
         Index_In_Bounds := False;
      else
         Index_In_Bounds := True;
         Result :=
           ${root_node_type_name} (Node_Vectors.Get_At_Index (Node.Vec, Index));
      end if;
   end Get_Child;

   -----------
   -- Print --
   -----------

   overriding procedure Print (Node : access List_Type; Prefix : String := "")
   is
      Class_Wide_Node : constant ${root_node_type_name} :=
         ${root_node_type_name} (Node);
   begin
      Put
        (Prefix & Class_Wide_Node.Kind_Name
         & "[" & Image (Node.Sloc_Range) & "]");
      if Length (Node.Vec) = 0 then
         Put_Line (": <empty list>");
         return;
      end if;

      New_Line;
      for Child of Node.Vec loop
         if Child /= null then
            Child.Print (Prefix & "|  ");
         end if;
      end loop;
   end Print;

   ------------------
   -- Destroy_Node --
   ------------------

   overriding procedure Destroy_Node (Node : access List_Type) is
   begin
      if Langkit_Support.Extensions.Has_Extensions then
         Node.Free_Extensions;
      end if;
   end Destroy_Node;

end ${_self.ada_api_settings.lib_name}.AST.List;
