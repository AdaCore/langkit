
with Liblktlang_Support.Internal.Analysis; use Liblktlang_Support.Internal.Analysis;
with Liblktlang_Support.Internal.Conversions;
use Liblktlang_Support.Internal.Conversions;

with Liblktlang.Generic_API; use Liblktlang.Generic_API;

package body Liblktlang.Private_Converters is

   function "+"
     (Entity : Liblktlang_Support.Internal.Analysis.Internal_Entity)
      return Implementation.Internal_Entity
     with Import,
          External_Name => "Liblktlang__from_generic_internal_entity";
   function "+"
     (Entity : Implementation.Internal_Entity)
      return Liblktlang_Support.Internal.Analysis.Internal_Entity
     with Import,
          External_Name => "Liblktlang__to_generic_internal_entity";
   --  See the corresponding exports in $.Generic_Impl

   -----------------------
   -- From_Generic_Node --
   -----------------------

   function From_Generic_Node
     (Node : Lk_Node) return Implementation.Internal_Entity is
   begin
      return +Unwrap_Node (Node);
   end From_Generic_Node;

   ---------------------
   -- To_Generic_Node --
   ---------------------

   function To_Generic_Node
     (Entity : Implementation.Internal_Entity) return Lk_Node is
   begin
      return Wrap_Node (Self_Id, +Entity);
   end To_Generic_Node;

end Liblktlang.Private_Converters;
