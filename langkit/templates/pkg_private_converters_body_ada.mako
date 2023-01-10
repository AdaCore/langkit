## vim: filetype=makoada

with Langkit_Support.Internal.Analysis; use Langkit_Support.Internal.Analysis;
with Langkit_Support.Internal.Conversions;
use Langkit_Support.Internal.Conversions;

with ${ada_lib_name}.Generic_API; use ${ada_lib_name}.Generic_API;

package body ${ada_lib_name}.Private_Converters is

   function "+"
     (Entity : Langkit_Support.Internal.Analysis.Internal_Entity)
      return Implementation.${T.entity.name}
     with Import,
          External_Name => "${ada_lib_name}__from_generic_internal_entity";
   function "+"
     (Entity : Implementation.${T.entity.name})
      return Langkit_Support.Internal.Analysis.Internal_Entity
     with Import,
          External_Name => "${ada_lib_name}__to_generic_internal_entity";
   --  See the corresponding exports in $.Generic_Impl

   -----------------------
   -- From_Generic_Node --
   -----------------------

   function From_Generic_Node
     (Node : Lk_Node) return Implementation.${T.entity.name} is
   begin
      return +Unwrap_Node (Node);
   end From_Generic_Node;

   ---------------------
   -- To_Generic_Node --
   ---------------------

   function To_Generic_Node
     (Entity : Implementation.${T.entity.name}) return Lk_Node is
   begin
      return Wrap_Node (Self_Id, +Entity);
   end To_Generic_Node;

end ${ada_lib_name}.Private_Converters;
