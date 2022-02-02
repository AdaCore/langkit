## vim: filetype=makoada

with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;

package ${ada_lib_name}.Generic_API is

   ${ctx.lang_name}_Lang_Id : constant Language_Id
     with Import, External_Name => "${ada_lib_name}__language_id";
   --  Unique identifier for ${ada_lib_name}

   Self_Id : Language_Id renames ${ctx.lang_name}_Lang_Id;
   --  Shortcut for convenience in code generation

   function To_Generic_Context (Context : Analysis_Context) return Lk_Context;
   --  Convert the given ``Context`` into a value suitable to use in the
   --  Langkit generic API.

   function From_Generic_Context
     (Context : Lk_Context) return Analysis_Context;
   --  Convert the ``Context`` value from the Langkit generic API into the
   --  ${ada_lib_name}-specific context type. Raise a
   --  ``Langkit_Support.Errors.Precondition_Failure`` if ``Context`` does not
   --  belong to ${ada_lib_name}.

   function To_Generic_Unit (Unit : Analysis_Unit) return Lk_Unit;
   --  Convert the given ``Unit`` into a value suitable to use in the Langkit
   --  generic API.

   function From_Generic_Unit (Unit : Lk_Unit) return Analysis_Unit;
   --  Convert the ``Unit`` value from the Langkit generic API into the
   --  ${ada_lib_name}-specific unit type. Raise a
   --  ``Langkit_Support.Errors.Precondition_Failure`` if ``Unit`` does not
   --  belong to ${ada_lib_name}.

end ${ada_lib_name}.Generic_API;
