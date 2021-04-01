## vim: filetype=makoada

with ${ada_lib_name}.Analysis;       use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Common;         use ${ada_lib_name}.Common;
with ${ada_lib_name}.Implementation; use ${ada_lib_name}.Implementation;

--  Internal package: provide implementation helpers to switch between public
--  types and implementation ones.

private package ${ada_lib_name}.Public_Converters is

   use Support.Text;

   type Context_Wrapper is access function
     (Context : Internal_Context) return Analysis_Context;
   Wrap_Context : Context_Wrapper;

   type Context_Unwrapper is access function
     (Context : Analysis_Context'Class) return Internal_Context;
   Unwrap_Context : Context_Unwrapper;

   type Unit_Wrapper is access function
     (Unit : Internal_Unit) return Analysis_Unit;
   Wrap_Unit : Unit_Wrapper;

   type Unit_Unwrapper is access function
     (Unit : Analysis_Unit'Class) return Internal_Unit;
   Unwrap_Unit : Unit_Unwrapper;

   type Node_Wrapper is access function
     (Node : ${T.root_node.name};
      Info : ${T.entity_info.name} := ${T.entity_info.nullexpr})
      return ${root_entity.api_name};
   Wrap_Node : Node_Wrapper;

   type Node_Unwrapper is access function
     (Node : ${root_entity.api_name}'Class) return ${T.root_node.name};
   Unwrap_Node : Node_Unwrapper;

   type Entity_Unwrapper is access function
     (Entity : ${root_entity.api_name}'Class) return ${root_entity.name};
   Unwrap_Entity : Entity_Unwrapper;

   ---------------------------
   -- Unit_Provider_Wrapper --
   ---------------------------

   --  This wraps a unit provider using the public API into one that fits in
   --  our internal APIs.

   type Unit_Provider_Wrapper is new Internal_Unit_Provider with record
      Ref_Count : Natural;
      Internal  : Unit_Provider_Reference;
   end record;

   overriding procedure Inc_Ref (Provider : in out Unit_Provider_Wrapper);
   overriding function Dec_Ref
     (Provider : in out Unit_Provider_Wrapper) return Boolean;

   overriding function Get_Unit_Filename
     (Provider : Unit_Provider_Wrapper;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String;
   overriding function Get_Unit
     (Provider    : Unit_Provider_Wrapper;
      Context     : Internal_Context;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Internal_Unit;

   function Wrap_Public_Provider
     (Provider : Unit_Provider_Reference) return Internal_Unit_Provider_Access;
   --  Wrap a public unit provider inside an internal one. If Provider is a
   --  null reference, return null. Otherwise, this returns a new internal
   --  provider allocation, with a ref-count of 1.

end ${ada_lib_name}.Public_Converters;
