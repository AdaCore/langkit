
with Liblktlang_Support.Generic_API; use Liblktlang_Support.Generic_API;
with Liblktlang_Support.Generic_API.Analysis;
use Liblktlang_Support.Generic_API.Analysis;

with Liblktlang.Analysis; use Liblktlang.Analysis;
with Liblktlang.Common;   use Liblktlang.Common;

package Liblktlang.Generic_API is

   

   Lkt_Lang_Id : constant Language_Id
     with Import, External_Name => "Liblktlang__language_id";
   --  Unique identifier for Liblktlang

   Self_Id : Language_Id renames Lkt_Lang_Id;
   --  Shortcut for convenience in code generation

   function To_Generic_Context (Context : Analysis_Context) return Lk_Context;
   --  Convert the given ``Context`` into a value suitable to use in the
   --  Langkit generic API.

   function From_Generic_Context
     (Context : Lk_Context) return Analysis_Context;
   --  Convert the ``Context`` value from the Langkit generic API into the
   --  Liblktlang-specific context type. Raise a
   --  ``Liblktlang_Support.Errors.Precondition_Failure`` if ``Context`` does not
   --  belong to Liblktlang.

   function To_Generic_Unit (Unit : Analysis_Unit) return Lk_Unit;
   --  Convert the given ``Unit`` into a value suitable to use in the Langkit
   --  generic API.

   function From_Generic_Unit (Unit : Lk_Unit) return Analysis_Unit;
   --  Convert the ``Unit`` value from the Langkit generic API into the
   --  Liblktlang-specific unit type. Raise a
   --  ``Liblktlang_Support.Errors.Precondition_Failure`` if ``Unit`` does not
   --  belong to Liblktlang.

   function To_Generic_Grammar_Rule
     (Rule : Grammar_Rule) return Liblktlang_Support.Generic_API.Grammar_Rule_Ref;
   --  Convert the given ``rule`` into a value suitable to use in the Langkit
   --  generic API.

   function From_Generic_Grammar_Rule
     (Rule : Liblktlang_Support.Generic_API.Grammar_Rule_Ref) return Grammar_Rule;
   --  Convert the ``Rule`` value from the Langkit generic API into the
   --  Liblktlang-specific unit type. Raise a
   --  ``Liblktlang_Support.Errors.Precondition_Failure`` if ``Rule`` does not
   --  belong to Liblktlang or if it is ``No_Grammar_Rule_Ref``.

   function To_Generic_Node
     (Node : Lkt_Node'Class) return Lk_Node;
   --  Convert the given ``Node`` into a value suitable to use in the Langkit
   --  generic API.

   function From_Generic_Node (Node : Lk_Node) return Lkt_Node;
   --  Convert the ``Node`` value from the Langkit generic API into the
   --  Liblktlang-specific unit type. Raise a
   --  ``Liblktlang_Support.Errors.Precondition_Failure`` if ``Node`` does not
   --  belong to Liblktlang.

end Liblktlang.Generic_API;
