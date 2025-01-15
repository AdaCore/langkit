## vim: filetype=makoada

with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Common;   use ${ada_lib_name}.Common;

package ${ada_lib_name}.Generic_API is

   <% lang_id_name = f"{cfg.library.language_name}_Lang_Id" %>

   ${lang_id_name} : constant Language_Id
     with Import, External_Name => "${ada_lib_name}__language_id";
   --  Unique identifier for ${ada_lib_name}

   Self_Id : Language_Id renames ${lang_id_name};
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

   function To_Generic_Grammar_Rule
     (Rule : Grammar_Rule) return Langkit_Support.Generic_API.Grammar_Rule_Ref;
   --  Convert the given ``rule`` into a value suitable to use in the Langkit
   --  generic API.

   function From_Generic_Grammar_Rule
     (Rule : Langkit_Support.Generic_API.Grammar_Rule_Ref) return Grammar_Rule;
   --  Convert the ``Rule`` value from the Langkit generic API into the
   --  ${ada_lib_name}-specific unit type. Raise a
   --  ``Langkit_Support.Errors.Precondition_Failure`` if ``Rule`` does not
   --  belong to ${ada_lib_name} or if it is ``No_Grammar_Rule_Ref``.

   function To_Generic_Node
     (Node : ${root_entity.api_name}'Class) return Lk_Node;
   --  Convert the given ``Node`` into a value suitable to use in the Langkit
   --  generic API.

   function From_Generic_Node (Node : Lk_Node) return ${root_entity.api_name};
   --  Convert the ``Node`` value from the Langkit generic API into the
   --  ${ada_lib_name}-specific unit type. Raise a
   --  ``Langkit_Support.Errors.Precondition_Failure`` if ``Node`` does not
   --  belong to ${ada_lib_name}.

end ${ada_lib_name}.Generic_API;
